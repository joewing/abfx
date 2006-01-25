
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Editing; use Ada.Text_IO.Editing;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings; use Ada.Strings;

with X11; use X11;
with X11.Label; use X11.Label;
with X11.Window; use X11.Window;
with X11.Button; use X11.Button;
with X11.Text_Box; use X11.Text_Box;
with X11.Panel; use X11.Panel;
with X11.Panel.Layout.Horizontal;
with X11.Panel.Layout.Vertical;
with X11.Panel.Layout.Grid;

with X11.Panel.Menu_Bar; use X11.Panel.Menu_Bar;
with X11.Panel.Menu; use X11.Panel.Menu;

package body Calc is

	type Value_Type is delta 0.000001 digits 18;

	format : constant Picture := To_Picture("ZZZZZZZZZZZZZZZZZ9.99999-");

	package Value_Format is new Ada.Text_IO.Editing.Decimal_Output(Value_Type);
	use Value_Format;

	-- Labels for the function buttons
	labels  : array(1 .. 4, 1 .. 4) of String(1 .. 1) := (
		( "7", "8", "9", "/" ),
		( "4", "5", "6", "*" ),
		( "1", "2", "3", "-" ),
		( "0", ".", "=", "+" )
	);

	-- GUI components
	win         : Window_Type;
	output      : Text_Box_Type;
	top_row     : Panel_Type;
	button_row  : Panel_Type;
	quit_button : Button_Type;
	ce_button   : Button_Type;
	ac_button   : Button_Type;
	buttons     : array(1 .. 4, 1 .. 4) of Button_Type;

	bar  : Menu_Bar_Type;
	menu : Menu_Type;

	value        : Value_Type := 0.0;
	last_op      : Character := ' ';
	should_clear : Boolean := true;


	function Contains(str : String; ch : Character) return Boolean is
	begin
		for x in str'range loop
			if str(x) = ch then
				return true;
			end if;
		end loop;
		return false;
	end Contains;

	function Get_Formated_Value return String is
		str : Unbounded_String;
	begin
		str := To_Unbounded_String(Image(value, format));
		str := Trim(str, Both);
		if Element(str, Length(str)) = '-' then
			return "-" & Slice(str, 1, Length(str) - 1);
		else
			return To_String(str);
		end if;
	end Get_Formated_Value;

	procedure Do_Operation is
	begin
		should_clear := true;
		case last_op is
			when '+' =>
				value := value + Value_Type'value(Get_Text(output));
				Set_Text(output, Get_Formated_Value);
			when '-' =>
				value := value - Value_Type'value(Get_Text(output));
				Set_Text(output, Get_Formated_Value);
			when '*' =>
				value := value * Value_Type'value(Get_Text(output));
				Set_Text(output, Get_Formated_Value);
			when '/' =>
				value := value / Value_Type'value(Get_Text(output));
				Set_Text(output, Get_Formated_Value);
			when ' ' =>
				value := Value_Type'value(Get_Text(output));
			when others =>
				Put_Line("invalid operation: " & Character'image(last_op));
		end case;
	exception
		when others =>
			Set_Text(output, "NaN");
	end Do_Operation;

	procedure Function_Listener(button : in out Button_Type'class) is
		prev : String := Get_Text(output);
		str  : String := Get_Text(button);
		ch   : Character := str(1);
	begin

		case ch is
			when '0' .. '9' =>
				if should_clear then
					should_clear := false;
					Set_Text(output, str);
				else
					Set_Text(output, prev & str);
				end if;
			when '.' =>
				if should_clear then
					should_clear := false;
					Set_Text(output, str);
				elsif not Contains(prev, '.') then
					Set_Text(output, prev & str);
				end if;
			when '+' | '-' | '*' | '/' =>
				Do_Operation;
				last_op := ch;
			when '=' =>
				Do_Operation;
				last_op := ' ';
			when others =>
				Put_Line("invalid button: " & str);
		end case;

	end Function_Listener;

	procedure Quit_Listener(button : in out Button_Type'class) is
	begin
		X11.Stop;
	end Quit_Listener;

	procedure Clear_Listener(button : in out Button_Type'class) is
	begin
		Set_Text(output, "0");
		should_clear := true;
	end Clear_Listener;

	procedure All_Clear_Listener(button : in out Button_Type'class) is
	begin
		Set_Text(output, "0");
		value := 0.0;
		last_op := ' ';
		should_clear := true;
	end All_Clear_Listener;

	procedure Run is
	begin

		X11.Panel.Layout.Vertical.Manage(win);
		Set_Title(win, "Calc");

		-- Add the menu bar.
		Add(win, bar);
		Add(bar, menu);
		Set_Title(menu, "File");

		-- Set up the output display
		Set_Text(output, "0");
		Set_Editable(output, false);
		Set_Alignment(output, Center_Right);
		Add(win, output);

		-- Set up the top row of buttons
		X11.Panel.Layout.Horizontal.Manage(top_row);
		Add(win, top_row);

		Set_Text(quit_button, "Quit");
		Add_Click_Listener(quit_button, Quit_Listener'access);
		Add(top_row, quit_button);

		Set_Text(ce_button, "CE");
		Add_Click_Listener(ce_button, Clear_Listener'access);
		Add(top_row, ce_button);

		Set_Text(ac_button, "AC");
		Add_Click_Listener(ac_button, All_Clear_Listener'access);
		Add(top_row, ac_button);

		-- Set up the function buttons
		X11.Panel.Layout.Grid.Manage(button_row);
		Add(win, button_row);
		for x in 1 .. 4 loop
			for y in 1 .. 4 loop
				Add(button_row, buttons(x, y));
				Add_Click_Listener(buttons(x, y), Function_Listener'access);
				Set_Text(buttons(x, y), labels(x, y));
			end loop;
		end loop;

		Show(win);
		X11.Run;

	end Run;

end Calc;


