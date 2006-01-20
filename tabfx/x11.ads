
with Ada.Finalization; use Ada.Finalization;

with Interfaces.C;

with Bindings.X11.Constants;
with Bindings.X11.Types;

package X11 is

	type Size_Type is record
		width, height : Natural;
	end record;

	type Position_Type is record
		x, y : Integer;
	end record;

	type Alignment_Type is (
		Top_Left,
		Top_Center,
		Top_Right,

		Center_Left,
		Center_Center,
		Center_Right,

		Bottom_Left,
		Bottom_Center,
		Bottom_Right
	);

	type Border_Type is (
		No_Border,
		Line_Border,
		Up_Border,
		Down_Border,
		Bevel_Border
	);

	-- Up to 16 mouse buttons supported.
	type Button_Mask_Type is mod 2 ** 16;

	type Object_Type is new Limited_Controlled with private;

	display  : Bindings.X11.Types.Display_Pointer := null;
	screen   : Interfaces.C.int := 0;
	colormap : Bindings.X11.Types.Colormap_Type := Bindings.X11.Constants.None;
	root     : Bindings.X11.Types.Window_Type := Bindings.X11.Constants.None;

	Connection_Refused : exception;

	procedure Run;
	procedure Stop;

private

	type Timer_Event_Handler_Type is access
		procedure(obj : in out Object_Type'class);

	delete_atom    : Bindings.X11.Types.Atom_Type;
	protocols_atom : Bindings.X11.Types.Atom_Type;

	type Object_Type is new Limited_Controlled with record
		id : Bindings.X11.Types.XID_Type := Bindings.X11.Constants.None;
	end record;

	procedure Initialize(obj : in out Object_Type);
	procedure Finalize(obj : in out Object_Type);

	procedure Add_Timer(
		handler : in Timer_Event_Handler_Type;
		object  : in out Object_Type'class;
		timeout : in Positive);

	procedure Remove_Timer(
		handler : in Timer_Event_Handler_Type;
		object  : in Object_Type'class);

	procedure Handle_Event(
		obj   : in out Object_Type;
		event : in Bindings.X11.Types.XEvent_Pointer);

end X11;

