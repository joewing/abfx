
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with X11.Panel.Label; use X11.Panel.Label;

package X11.Panel.Menu is

	type Menu_Type is new Panel_Type with private;

	procedure Set_Title(
		menu  : in out Menu_Type'class;
		title : in String);

	procedure Add(
		menu     : in out Menu_Type;
		child    : in out Panel_Type'class;
		location : in Positive := Border_Center);

	procedure Remove(
		menu  : in out Menu_Type;
		child : in out Panel_Type'class);

	procedure Popup(
		menu     : in out Menu_Type'class;
		position : in Position_Type);

private

	type Menu_Type is new Panel_Type with record
		label : Label_Type;
		menu  : Panel_Type;
	end record;

	procedure Initialize(menu : in out Menu_Type);

	procedure Finalize(menu : in out Menu_Type);

end X11.Panel.Menu;


