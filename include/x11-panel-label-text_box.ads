
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with X11.Collections.List;
with X11.Graphics; use X11.Graphics;
with X11.Label; use X11.Label;

package X11.Panel.Label.Text_Box is

	type Text_Box_Type is new Label_Type with private;

	type Text_Change_Listener_Type is access
		procedure(text_box : in out Text_Box_Type'class);

	procedure Add_Text_Change_Listener(
		text_box : in out Text_Box_Type'class;
		listener : in Text_Change_Listener_Type);

	procedure Set_Text(
		text_box : in out Text_Box_Type;
		text     : in String);

	procedure Set_Editable(
		text_box : in out Text_Box_Type;
		value    : in Boolean := true);

private

	package Text_Change_Listener_List is
		new X11.Collections.List(Text_Change_Listener_Type);

	type Text_Box_Type is new Label_Type with record
		start, stop           : Natural := 0;  -- Selection
		cursor_on             : Boolean := false;
		editable              : Boolean := true;
		text_change_listeners : Text_Change_Listener_List.List_Type;
	end record;

	procedure Initialize(text_box : in out Text_Box_Type);
	procedure Finalize(text_box : in out Text_Box_Type);

end X11.Panel.Label.Text_Box;

