
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with X11.Font; use X11.Font;

package body X11.Panel.Label.Text_Box is

	use Text_Change_Listener_List;

	procedure Handle_Timer_Event(object : in out Object_Type'class);
	procedure Handle_Key_Event(
		panel  : in out Panel_Type'class;
		key    : in Natural;
		strike : in Strike_Type);
	procedure Handle_Key_Press(
		tbox   : in out Text_Box_Type;
		key    : in Natural);
	procedure Show_Cursor(text_box : in out Text_Box_Type);
	procedure Hide_Cursor(text_box : in out Text_Box_Type);
	procedure Run_Text_Change_Listeners(panel : in out Panel_Type'class);

	procedure Initialize(text_box : in out Text_Box_Type) is
	begin
		Initialize(Label_Type(text_box));
		Set_Alignment(Label_Type(text_box), Top_Left);
		Set_Border(text_box, Bevel_Border);
		Add_Timer(Handle_Timer_Event'access, text_box, 250);
		Add_Key_Listener(text_box, Handle_Key_Event'access);
	end Initialize;

	procedure Finalize(text_box : in out Text_Box_Type) is
	begin
		Remove_Timer(Handle_Timer_Event'access, text_box);
		Finalize(Label_Type(text_box));
	end Finalize;

	procedure Show_Cursor(text_box : in out Text_Box_Type) is
		height : Natural;
		xstart : Natural;
	begin
		if text_box.size.height < 4 or else not text_box.editable then
			return;
		end if;
		height := text_box.size.height - 4;
		if text_box.stop = 0 then
			xstart := 0;
		else
			xstart := Get_Width(text_box.font,
				Slice(text_box.text, 1, text_box.stop));
		end if;
		xstart := xstart + 3;
		Set_Foreground(text_box.graphics, text_box.foreground);
		Draw_Line(text_box.graphics, xstart, 3, xstart, height);
		Draw_Line(text_box.graphics, xstart + 1, 3, xstart + 1, height);
		text_box.cursor_on := true;
	end Show_Cursor;

	procedure Hide_Cursor(text_box : in out Text_Box_Type) is
		height : Natural;
		xstart : Natural;
	begin
		if text_box.size.height < 4 then
			return;
		end if;
		height := text_box.size.height - 4;
		if text_box.stop = 0 then
			xstart := 0;
		else
			xstart := Get_Width(text_box.font,
				Slice(text_box.text, 1, text_box.stop));
		end if;
		xstart := xstart + 3;
		Set_Foreground(text_box.graphics, text_box.background);
		Draw_Line(text_box.graphics, xstart, 3, xstart, height);
		Draw_Line(text_box.graphics, xstart + 1, 3, xstart + 1, height);
		text_box.cursor_on := false;
	end Hide_Cursor;

	procedure Add_Text_Change_Listener(
		text_box : in out Text_Box_Type'class;
		listener : in Text_Change_Listener_Type) is
	begin
		Add(text_box.text_change_listeners, listener);
	end Add_Text_Change_Listener;

	procedure Set_Text(
		text_box : in out Text_Box_Type;
		text     : in String) is
	begin
		Set_Text(Label_Type(text_box), text);
		text_box.stop := Length(text_box.text);
	end Set_Text;

	procedure Set_Editable(
		text_box : in out Text_Box_Type;
		value    : in Boolean := true) is
	begin
		text_box.editable := value;
	end Set_Editable;

	procedure Handle_Timer_Event(object : in out Object_Type'class) is
	begin
		if Text_Box_Type(object).cursor_on then
			Hide_Cursor(Text_Box_Type(object));
		else
			Show_Cursor(Text_Box_Type(object));
		end if;
	end Handle_Timer_Event;

	procedure Handle_Key_Event(
		panel  : in out Panel_Type'class;
		key    : in Natural;
		strike : in Strike_Type) is
	begin
		if strike = Press then
			Handle_Key_Press(Text_Box_Type(panel), key);
		end if;
	end Handle_Key_Event;

	procedure Handle_Key_Press(
		tbox   : in out Text_Box_Type;
		key    : in Natural) is

		changed : Boolean := false;

	begin
		if not tbox.editable then
			return;
		end if;
		case key is
			when 16#FFFF# | 16#FF08# =>
				declare
					len : Natural := Length(tbox.text);
				begin
					if len > 0 then
						Delete(tbox.text, len, len);
						tbox.stop := tbox.stop - 1;
						changed := true;
					end if;
				end;
			when others =>
				if key >= 16#20# and key <= 16#7E# then
					Append(tbox.text, Character'val(key));
					changed := true;
					tbox.stop := tbox.stop + 1;
				end if;
		end case;
		if changed then
			Paint(tbox);
			Show_Cursor(tbox);
			Run_Text_Change_Listeners(tbox);
		end if;
	end Handle_Key_Press;

	procedure Run_Text_Change_Listeners(panel : in out Panel_Type'class) is
		size     : Natural;
		listener : Text_Change_Listener_Type;
	begin
		size := Get_Size(Text_Box_Type(panel).text_change_listeners);
		for x in 1 .. size loop
			listener := Get(Text_Box_Type(panel).text_change_listeners, x);
			listener(Text_Box_Type(panel));
		end loop;
	end Run_Text_Change_Listeners;

end X11.Panel.Label.Text_Box;

