
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Bindings.Unix.Constants; use Bindings.Unix.Constants;
with Bindings.Unix.Functions; use Bindings.Unix.Functions;
with Bindings.Unix.Types; use Bindings.Unix.Types;
with Bindings.X11.Functions; use Bindings.X11.Functions;

package body X11 is

	package Constants renames Bindings.X11.Constants;
	use Bindings.X11.Types;

	type Object_Pointer is access Object_Type'class;

	type Listener_Node;
	type Listener_Node_Pointer is access Listener_Node;
	type Listener_Node is record
		object   : Object_Pointer;
		next     : Listener_Node_Pointer;
	end record;

	type Timer_Node;
	type Timer_Node_Pointer is access Timer_Node;
	type Timer_Node is record
		handler   : Timer_Event_Handler_Type;
		object    : Object_Pointer;
		timeout   : Timeval_Type;
		last_time : Timeval_Type;
		next      : Timer_Node_Pointer;
	end record;

	timers      : Timer_Node_Pointer := null;
	min_timeout : Integer := -1;
	head        : Listener_Node_Pointer := null;
	count       : Integer := 0;
	should_stop : Boolean := false;
	button_mask : Button_Mask_Type := 0;

	procedure Free is
		new Ada.Unchecked_Deallocation(Listener_Node, Listener_Node_Pointer);

	procedure Free is
		new Ada.Unchecked_Deallocation(Timer_Node, Timer_Node_Pointer);

	procedure Free is
		new Ada.Unchecked_Deallocation(XEvent_Type, XEvent_Pointer);

	procedure Free is
		new Ada.Unchecked_Deallocation(Pollfd_Type, Pollfd_Pointer);

	procedure Compute_Timeout;
	procedure Run_Timers;
	procedure Open;
	procedure Close;
	function Intern_Atom(name : String) return Atom_Type;
	function gcd(a, b : Natural) return Natural;

	procedure Initialize(obj : in out Object_Type) is
		np : Listener_Node_Pointer;
	begin
		if count = 0 then
			Open;
		end if;
		count := count + 1;

		np := new Listener_Node;
		np.object := obj'unrestricted_access;
		np.next := head;
		head := np;

	end Initialize;

	procedure Finalize(obj : in out Object_Type) is
		np, last : Listener_Node_Pointer;
	begin
		if obj.id /= 0 then
			count := count - 1;
			if count = 0 then
				Close;
			end if;
		end if;
		obj.id := 0;

		np := head;
		last := null;
		while np /= null loop
			if np.object = obj'unrestricted_access then
				if last /= null then
					last.next := np.next;
				else
					head := np.next;
				end if;
				Free(np);
				exit;
			end if;
			last := np;
			np := np.next;
		end loop;

	end Finalize;

	procedure Run is
		event  : XEvent_Pointer := new XEvent_Type;
		id     : XID_Type;
		np     : Listener_Node_Pointer;
		pollfd : Pollfd_Pointer := new Pollfd_Type;
		rcode  : int;
	begin

		if display = null then
			return;
		end if;

		pollfd.fd := XConnectionNumber(display);
		pollfd.events := 1;

		should_stop := false;
		while not should_stop loop

			rcode := poll(pollfd, 1, int(min_timeout));

			if XPending(display) = 0 then
				Run_Timers;
			end if;

			while XPending(display) > 0 loop
				Run_Timers;
				XNextEvent(display, event);
				case event.t is
					when Constants.ButtonPress | Constants.ButtonRelease =>
						id := event.xbutton.window;
					when Constants.MotionNotify =>
						id := event.xmotion.window;
					when Constants.KeyPress | Constants.KeyRelease =>
						id := event.xkey.window;
					when Constants.ConfigureNotify =>
						id := event.xconfigure.window;
					when Constants.Expose =>
						id := event.xexpose.window;
					when Constants.ClientMessage =>
						id := event.xclient.window;
					when others =>
						id := Constants.None;
				end case;
				if id /= Constants.None then
					np := head;
					while np /= null loop
						if np.object.id = id then
							Handle_Event(np.object.all, event);
							exit;
						end if;
						np := np.next;
					end loop;
				end if;
			end loop;

		end loop;

		Free(pollfd);
		Free(event);

	end Run;

	procedure Stop is
	begin
		should_stop := true;
	end Stop;

	procedure Add_Timer(
		handler : in Timer_Event_Handler_Type;
		object  : in out Object_Type'class;
		timeout : in Positive) is
		np : Timer_Node_Pointer;
	begin
		np := new Timer_Node;
		np.handler := handler;
		np.object := object'unrestricted_access;
		np.timeout.tv_sec := long(timeout / 1000);
		np.timeout.tv_usec := long((timeout mod 1000) * 1000);
		np.last_time := Get_Time;
		np.next := timers;
		timers := np;
		Compute_Timeout;
	end Add_Timer;

	procedure Remove_Timer(
		handler : in Timer_Event_Handler_Type;
		object  : in Object_Type'class) is
		np : Timer_Node_Pointer;
		lp : Timer_Node_Pointer;
	begin
		lp := null;
		np := timers;
		while np /= null loop
			if np.handler = handler and then
				np.object = object'unrestricted_access then
				if lp /= null then
					lp.next := np.next;
				else
					timers := np.next;
				end if;
				Free(np);
				exit;
			end if;
			lp := np;
			np := np.next;
		end loop;
		Compute_Timeout;
	end Remove_Timer;

	procedure Compute_Timeout is
		np : Timer_Node_Pointer;
		ms : Natural;
	begin
		if timers = null then
			min_timeout := -1;
		else
			np := timers;
			min_timeout := Natural(timers.timeout.tv_sec * 1000
				+ timers.timeout.tv_usec / 1000);
			np := timers.next;
			while np /= null loop
				ms := Natural(np.timeout.tv_sec * 1000
					+ np.timeout.tv_usec / 1000);
				min_timeout := gcd(min_timeout, ms);
				np := np.next;
			end loop;
		end if;
	end Compute_Timeout;

	procedure Run_Timers is
		np           : Timer_Node_Pointer;
		current_time : aliased Timeval_Type;
	begin
		np := timers;
		current_time := Get_Time;
		while np /= null loop
			if current_time - np.last_time >= np.timeout then
				np.handler(np.object.all);
				np.last_time := current_time;
			end if;
			np := np.next;
		end loop;
	end Run_Timers;

	procedure Handle_Event(
		obj   : in out Object_Type;
		event : in Bindings.X11.Types.XEvent_Pointer) is
	begin
		null;
	end Handle_Event;

	procedure Open is
	begin
		display := XOpenDisplay(Null_Ptr);
		if display = null then
			raise Connection_Refused;
		end if;
		screen := display.default_screen;
		root := XDefaultRootWindow(display);
		colormap := XDefaultColormap(display, screen);

		protocols_atom := Intern_Atom("WM_PROTOCOLS");
		delete_atom := Intern_Atom("WM_DELETE_WINDOW");

	end Open;

	procedure Close is
	begin
		if display /= null then
			XCloseDisplay(display);
			display := null;
		end if;
	end Close;

	function Intern_Atom(name : String) return Atom_Type is
		c_str  : chars_ptr;
		result : Atom_Type;
	begin
		c_str := New_String(name);
		result := XInternAtom(display, c_str, 0);
		Free(c_str);
		return result;
	end Intern_Atom;

	function gcd(a, b : Natural) return Natural is
	begin
		if b = 0 then
			return a;
		else
			return gcd(b, a mod b);
		end if;
	end gcd;

end X11;

