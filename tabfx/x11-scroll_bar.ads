

with X11.Collections.List;
pragma Elaborate_All(X11.Collections.List);

with X11.Panel; use X11.Panel;
pragma Elaborate_All(X11.Panel);

with X11.Button; use X11.Button;
pragma Elaborate_All(X11.Button);

package X11.Scroll_Bar is

	type Orientation_Type is (Horizontal, Vertical);

	type Scroll_Bar_Type is new Panel_Type with private;

	type Adjust_Listener_Type is access
		procedure(bar : in out Scroll_Bar_Type'class);

	procedure Add_Adjust_Listener(
		bar      : in out Scroll_Bar_Type;
		listener : in Adjust_Listener_Type);

	procedure Set_Orientation(
		bar         : in out Scroll_Bar_Type;
		orientation : in Orientation_Type);

	procedure Set_Minimum(
		bar     : in out Scroll_Bar_Type;
		minimum : in Integer);

	procedure Set_Maximum(
		bar     : in out Scroll_Bar_Type;
		maximum : in Integer);

	procedure Set_Increment(
		bar       : in out Scroll_Bar_Type;
		increment : in Positive);

	procedure Set_Value(
		bar   : in out Scroll_Bar_Type;
		value : in Integer);

	function Get_Minimum(bar : Scroll_Bar_Type) return Integer;
	function Get_Maximum(bar : Scroll_Bar_Type) return Integer;
	function Get_Increment(bar : Scroll_Bar_Type) return Positive;
	function Get_Value(bar : Scroll_Bar_Type) return Integer;

	procedure Increment(bar : in out Scroll_Bar_Type);
	procedure Decrement(bar : in out Scroll_Bar_Type);

private

	package Listener_List is
		new X11.Collections.List(Adjust_Listener_Type);

	type Scroll_Bar_Pointer is access all Scroll_Bar_Type'class;

	type Scroll_Bar_Type is new Panel_Type with record
		orientation : Orientation_Type := Vertical;
		minimum     : Integer := 1;
		maximum     : Integer := 10;
		increment   : Positive := 1;
		value       : Integer := 1;
		start       : Button_Type;
		stop        : Button_Type;
		center      : Panel_Type;
		listeners   : Listener_List.List_Type;
	end record;

	procedure Initialize(bar : in out Scroll_Bar_Type);
	procedure Finalize(bar : in out Scroll_Bar_Type);

end X11.Scroll_Bar;

