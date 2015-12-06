
with Ada.Text_IO; use Ada.Text_IO;

with Interfaces.C; use Interfaces.C;

with Bindings.X11.Functions; use Bindings.X11.Functions;
with Bindings.X11.Constants;

package body X11.Panel is

   package Constants renames Bindings.X11.Constants;
   package Types renames Bindings.X11.Types;

   use Painter_List;
   use Button_Listener_List;
   use Motion_Listener_List;
   use Key_Listener_List;

   procedure Handle_Button(panel : in out Panel_Type'class;
      event : in Types.XEvent_Pointer);
   procedure Handle_Motion(panel : in out Panel_Type'class;
      event : in Types.XEvent_Pointer);
   procedure Handle_Key(panel : in out Panel_Type'class;
      event : in Types.XEvent_Pointer);
   procedure Handle_Client_Message(panel : in out Panel_Type'class;
      event : in Types.XEvent_Pointer);
   procedure Handle_Configure(panel : in out Panel_Type'class;
      event : in Types.XEvent_Pointer);
   procedure Handle_Expose(panel : in out Panel_Type'class;
      event : in Types.XEvent_Pointer);

   procedure Run_Painters(panel : in out Panel_Type'class);

   procedure Initialize(panel : in out Panel_Type) is
   begin
      Initialize(Object_Type(panel));

      panel.size.width := 1;
      panel.size.height := 1;
      panel.background := Default_Background;

      panel.id := XCreateSimpleWindow(display, root, 0, 0,
         unsigned(panel.size.width), unsigned(panel.size.height),
         0, 0, panel.background.pixel);
      Create(panel.graphics, panel.id);

      XSelectInput(display, panel.id,
         Constants.ExposureMask
         + Constants.StructureNotifyMask
         + Constants.KeyPressMask
         + Constants.KeyReleaseMask
         + Constants.ButtonPressMask
         + Constants.ButtonReleaseMask
         + Constants.PointerMotionMask);

      panel.initialized := true;

   end Initialize;

   procedure Finalize(panel : in out Panel_Type) is
      size : Natural;
      np   : Panel_Pointer;
   begin
      if panel.initialized then

         if panel.manager /= null then
            Release(panel.manager.all, panel);
         end if;

         size := Panel_List.Get_Size(panel.children);
         for x in 1 .. size loop
            np := Panel_List.Get(panel.children, x);
            if np /= null then
               Finalize(np.all);
            end if;
         end loop;

         Destroy(panel.graphics);
         XDestroyWindow(display, panel.id);
         Finalize(Object_Type(panel));
         panel.initialized := false;

      end if;
   end Finalize;

   procedure Set_Background(panel : in out Panel_Type'class;
      color : in Color_Type) is
   begin
      panel.background := color;
      XSetWindowBackground(display, panel.id, color.pixel);
      Run_Painters(panel);
   end Set_Background;

   function Get_Graphics(panel : Panel_Type'class) return Graphics_Type is
   begin
      return panel.graphics;
   end Get_Graphics;

   procedure Add(
      panel    : in out Panel_Type;
      child    : in out Panel_Type'class;
      location : in Positive := Border_Center) is
   begin
      if panel.manager /= null then
         Add(panel.manager.all, panel, child, location);
         XReparentWindow(display, child.id, panel.id, 0, 0);
      end if;
      child.parent := panel'unrestricted_access;
   end Add;

   procedure Remove(
      panel : in out Panel_Type;
      child : in out Panel_Type'class) is
   begin
      if panel.manager /= null then
         Remove(panel.manager.all, panel, child);
      end if;
      Hide(child);
      XReparentWindow(display, child.id, root, 0, 0);
      Run_Painters(panel);
   end Remove;

   procedure Add_Painter(panel : in out Panel_Type'class;
      painter : in Painter_Type) is
   begin
      Add(panel.painters, painter);
   end Add_Painter;

   procedure Add_Button_Listener(panel : in out Panel_Type'class;
      listener : in Button_Listener_Type) is
   begin
      Add(panel.button_listeners, listener);
   end Add_Button_Listener;

   procedure Add_Motion_Listener(panel : in out Panel_Type'class;
      listener : in Motion_Listener_Type) is
   begin
      Add(panel.motion_listeners, listener);
   end Add_Motion_Listener;

   procedure Add_Key_Listener(
      panel    : in out Panel_Type'class;
      listener : in Key_Listener_Type) is
   begin
      Add(panel.key_listeners, listener);
   end Add_Key_Listener;

   procedure Show(panel : in out Panel_Type'class) is
   begin
      if panel.manager /= null then
         Place(panel.manager.all, panel);
      else
         panel.size := panel.preferred_size;
      end if;
      Resize(panel, panel.size);
      XMapWindow(display, panel.id);
   end Show;

   procedure Hide(panel : in out Panel_Type'class) is
   begin
      XUnmapWindow(display, panel.id);
   end Hide;

   procedure Clear(panel : in out Panel_Type'class) is
   begin
      XClearWindow(display, panel.id);
   end Clear;

   function Get_Size(panel : Panel_Type'class) return Size_Type is
   begin
      return panel.size;
   end Get_Size;

   procedure Set_Preferred_Size(
      panel : in out Panel_Type'class;
      size  : in Size_Type) is
   begin
      panel.preferred_size := size;
   end Set_Preferred_size;

   function Get_Parent(panel : Panel_Type'class) return Panel_Pointer is
   begin
      return panel.parent;
   end Get_Parent;

   procedure Resize(panel : in out Panel_Type'class; size : in Size_Type) is
      temp : Size_Type := size;
   begin

      -- The size must be > 0, so fudge it with 1.
      if temp.width = 0 then
         temp.width := 1;
      end if;
      if temp.height = 0 then
         temp.height := 1;
      end if;

      XResizeWindow(display, panel.id, unsigned(temp.width),
         unsigned(temp.height));

      panel.size := temp;
   end Resize;

   procedure Move(panel : in out Panel_Type'class;
      position : in Position_Type) is
   begin
      XMoveWindow(display, panel.id, int(position.x), int(position.y));
      panel.position := position;
    end Move;

   procedure Set_Override_Redirect(
      panel : in out Panel_Type'class;
      value : in Boolean) is

      attr : aliased Bindings.X11.Types.XSetWindowAttributes_Type;
      mask : constant := Bindings.X11.Constants.CWOverrideRedirect;
      rc   : int;

   begin

      if value then
         attr.override_redirect := 1;
      else
         attr.override_redirect := 0;
      end if;

      rc := XChangeWindowAttributes(display, panel.id, mask,
         attr'unrestricted_access);

   end Set_Override_Redirect;

   procedure Handle_Event(
      panel : in out Panel_Type;
      event : in Types.XEvent_Pointer) is
   begin
      case event.t is
         when Constants.KeyPress | Constants.KeyRelease =>
            Handle_Key(Panel_Type(panel), event);
         when Constants.ButtonPress | Constants.ButtonRelease =>
            Handle_Button(Panel_Type(panel), event);
         when Constants.MotionNotify =>
            Handle_Motion(Panel_Type(panel), event);
         when Constants.Expose =>
            Handle_Expose(Panel_Type(panel), event);
         when Constants.ConfigureNotify =>
            Handle_Configure(Panel_Type(panel), event);
         when Constants.ClientMessage =>
            Handle_Client_Message(Panel_Type(panel), event);
         when others =>
            Put_Line("unknown event in X11.Panel.Handle_Event:"
               & int'image(event.t));
      end case;
   end Handle_Event;

   procedure Handle_Button(panel : in out Panel_Type'class;
      event : in Types.XEvent_Pointer) is
      strike   : Strike_Type;
      size     : Natural;
      listener : Button_Listener_Type;
   begin

      case event.t is
         when Constants.ButtonPress =>
            strike := Press;
         when Constants.ButtonRelease =>
            strike := Release;
         when others =>
            Put_Line("bad event in X11.Panel.Handle_Button");
            return;
      end case;

      size := Get_Size(panel.button_listeners);
      for x in 1 .. size loop
         listener := Get(panel.button_listeners, x);
         listener(panel, Integer(event.xbutton.x), Integer(event.xbutton.y),
            Positive(event.xbutton.button), strike);
      end loop;

   end Handle_Button;

   procedure Handle_Motion(panel : in out Panel_Type'class;
      event : in Types.XEvent_Pointer) is

      size     : Natural;
      listener : Motion_Listener_Type;
      mask     : Natural := 0;

   begin

      if event.t /= Constants.MotionNotify then
         Put_Line("bad event in X11.Panel.Handle_Motion");
         return;
      end if;

      -- Make sense of event.xmotion.state.
      mask := Natural(event.xmotion.state / 2 ** 8);

      size := Get_Size(panel.motion_listeners);
      for x in 1 .. size loop
         listener := Get(panel.motion_listeners, x);
         listener(panel, Integer(event.xmotion.x), Integer(event.xmotion.y),
            mask);
      end loop;

   end Handle_Motion;

   procedure Handle_Key(
      panel : in out Panel_Type'class;
      event : in Types.XEvent_Pointer) is

      strike   : Strike_Type;
      size     : Natural;
      listener : Key_Listener_Type;
      code     : Types.KeySym_Type;

   begin

      case event.t is
         when Constants.KeyPress =>
            strike := Press;
         when Constants.KeyRelease =>
            strike := Release;
         when others =>
            Put_Line("bad event in X11.Panel.Handle_Key");
            return;
      end case;

      code := XKeycodeToKeysym(display, event.xkey.keycode,
         event.xkey.state);

      size := Get_Size(panel.key_listeners);
      for x in 1 .. size loop
         listener := Get(panel.key_listeners, x);
         listener(panel, Natural(code), strike);
      end loop;

   end Handle_Key;

   procedure Handle_Client_Message(panel : in out Panel_Type'class;
      event : in Types.XEvent_Pointer) is
   begin

      if long(event.xclient.message_type) = long(protocols_atom) then
         if event.xclient.data.l(0) = long(delete_atom) then
            X11.Stop;
            return;
         end if;
      end if;

      Put_Line("Unknown ClientMessage:");
      Put_Line("  type:" & Types.Atom_Type'image(event.xclient.message_type));
      case event.xclient.data.format is
         when Types.Format_8 =>
            Put_Line("  Format: 8");
            for x in event.xclient.data.b'range loop
               Put_Line("  " & char'image(event.xclient.data.b(x)));
            end loop;
         when Types.Format_16 =>
            Put_Line("  Format: 16");
            for x in event.xclient.data.s'range loop
               Put_Line("  " & short'image(event.xclient.data.s(x)));
            end loop;
         when Types.Format_32 =>
            Put_Line("  Format: 32");
            for x in event.xclient.data.l'range loop
               Put_Line("  " & long'image(event.xclient.data.l(x)));
            end loop;
      end case;
   end Handle_Client_Message;

   procedure Handle_Configure(panel : in out Panel_Type'class;
      event : in Types.XEvent_Pointer) is
      nsize : Size_Type;
   begin
      if panel.manager /= null then
         nsize.width := Natural(event.xconfigure.width);
         nsize.height := Natural(event.xconfigure.height);
         Replace(panel.manager.all, panel, nsize);
      end if;
   end Handle_Configure;

   procedure Handle_Expose(panel : in out Panel_Type'class;
      event : in Types.XEvent_Pointer) is
   begin
      if event.xexpose.count = 0 then
         Run_Painters(panel);
      end if;
   end Handle_Expose;

   procedure Run_Painters(panel : in out Panel_Type'class) is
      size    : Natural;
      painter : Painter_Type;
   begin
      size := Get_Size(panel.painters);
      for x in 1 .. size loop
         painter := Get(panel.painters, x);
         painter(panel);
      end loop;
   end Run_Painters;
   
end X11.Panel;

