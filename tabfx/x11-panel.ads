
with Bindings.X11.Types;

with X11.Collections.List;
pragma Elaborate_All(X11.Collections.List);

with X11.Graphics; use X11.Graphics;
with X11.Color; use X11.Color;

package X11.Panel is

	type Panel_Type is new Object_Type with private;
	type Panel_Pointer is access all Panel_Type'class;

	Border_Center : constant Positive := 1;
	Border_North  : constant Positive := 2;
	Border_South  : constant Positive := 3;
	Border_East   : constant Positive := 4;
	Border_West   : constant Positive := 5;

	type Strike_Type is (Press, Release);

	type Painter_Type is access procedure(panel : in out Panel_Type'class);

	type Button_Listener_Type is access
		procedure(
			panel  : in out Panel_Type'class;
			x, y   : in Integer;
			button : in Positive;
			strike : in Strike_Type);

	type Motion_Listener_Type is access
		procedure(
			panel  : in out Panel_Type'class;
			x, y   : in Integer;
			button : in Natural);

	type Key_Listener_Type is access
		procedure(
			panel  : in out Panel_Type'class;
			key    : in Natural;
			strike : in Strike_Type);

	procedure Show(panel : in out Panel_Type'class);
	procedure Hide(panel : in out Panel_Type'class);

	procedure Set_Background(panel : in out Panel_Type'class;
		color : in Color_Type);

	function Get_Graphics(panel : Panel_Type'class) return Graphics_Type;

	procedure Add(panel, child : in out Panel_Type'class;
		location : in Positive := Border_Center);

	procedure Remove(panel, child : in out Panel_Type'class);

	procedure Add_Painter(
		panel   : in out Panel_Type'class;
		painter : in Painter_Type);

	procedure Add_Button_Listener(
		panel    : in out Panel_Type'class;
		listener : in Button_Listener_Type);

	procedure Add_Motion_Listener(
		panel    : in out Panel_Type'class;
		listener : in Motion_Listener_Type);

	procedure Add_Key_Listener(
		panel    : in out Panel_Type'class;
		listener : in Key_Listener_Type);

	procedure Clear(panel : in out Panel_Type'class);

	function Get_Size(panel : Panel_Type'class) return Size_Type;

	function Get_Parent(panel : Panel_Type'class) return Panel_Pointer;

	procedure Set_Preferred_Size(
		panel : in out Panel_Type'class;
		size  : in Size_Type);

private

	package Painter_List is
		new X11.Collections.List(Painter_Type);
	package Button_Listener_List is
		new X11.Collections.List(Button_Listener_Type);
	package Motion_Listener_List is
		new X11.Collections.List(Motion_Listener_Type);
	package Key_Listener_List is
		new X11.Collections.List(Key_Listener_Type);
	package Panel_List is
		new X11.Collections.List(Panel_Pointer);

	type Manager_Type is abstract tagged limited null record;
	type Manager_Pointer is access all Manager_Type'class;

	procedure Add(
		manager  : in out Manager_Type;
		panel    : in out Panel_Type'class;
		child    : in out Panel_Type'class;
		location : in Positive) is abstract;

	procedure Place(
		manager : in out Manager_Type;
		panel   : in out Panel_Type'class) is abstract;

	procedure Replace(
		manager : in out Manager_Type;
		panel   : in out Panel_Type'class;
		size    : in Size_Type) is abstract;

	procedure Remove(
		manager : in out Manager_Type;
		panel   : in out Panel_Type'class;
		child   : in out Panel_Type'class) is abstract;

	procedure Release(
		manager : in Manager_Type;
		panel   : in out Panel_Type'class) is abstract;

	type Panel_List_Pointer is access Panel_List.List_Type;

	type Panel_Type is new Object_Type with record
		initialized      : Boolean := false;
		size             : Size_Type;
		preferred_size   : Size_Type;
		position         : Position_Type;
		graphics         : Graphics_Type;
		background       : Color_Type;
		painters         : Painter_List.List_Type;
		button_listeners : Button_Listener_List.List_Type;
		motion_listeners : Motion_Listener_List.List_Type;
		key_listeners    : Key_Listener_List.List_Type;
		children         : Panel_List.List_Type;
		parent           : Panel_Pointer := null;
		manager          : Manager_Pointer := null;
	end record;

	procedure Move(
		panel    : in out Panel_Type'class;
		position : in Position_Type);

	procedure Resize(
		panel : in out Panel_Type'class;
		size  : in Size_Type);

	procedure Initialize(panel : in out Panel_Type);
	procedure Finalize(panel : in out Panel_Type);

	procedure Handle_Event(
		panel : in out Panel_Type;
		event : in Bindings.X11.Types.XEvent_Pointer);

end X11.Panel;

