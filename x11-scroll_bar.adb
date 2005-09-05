
with Ada.Text_IO; use Ada.Text_IO;

with X11.Panel.Layout.Border;
pragma Elaborate_All(X11.Panel.Layout.Border);

with X11.Graphics; use X11.Graphics;
pragma Elaborate_All(X11.Graphics);

with X11.Color; use X11.Color;

package body X11.Scroll_Bar is

	use Listener_List;

	procedure Paint(obj : in out Panel_Type'class);
	procedure Paint_Bar(bar : in out Scroll_Bar_Type);

	procedure Increase_Listener(button : in out Button_Type'class);
	procedure Decrease_Listener(button : in out Button_Type'class);

	procedure Run_Listeners(bar : in out Scroll_Bar_Type);

	procedure Initialize(bar : in out Scroll_Bar_Type) is
	begin
		Initialize(Panel_Type(bar));
		X11.Panel.Layout.Border.Manage(bar);

		Add_Painter(bar.center, Paint'access);

		Set_Text(bar.start, "-");
		Set_Text(bar.stop, "+");

		-- Default orientation is vertical
		Add(bar, bar.start, Border_North);
		Add(bar, bar.stop, Border_South);
		Add(bar, bar.center, Border_Center);
		Set_Preferred_size(bar.center, (1, 100));

		Add_Click_Listener(bar.start, Decrease_Listener'access);
		Add_Click_Listener(bar.stop, Increase_Listener'access);

	end Initialize;

	procedure Finalize(bar : in out Scroll_Bar_Type) is
	begin
		Finalize(Panel_Type(bar));
	end Finalize;

	procedure Add_Adjust_Listener(
		bar      : in out Scroll_Bar_Type;
		listener : in Adjust_Listener_Type) is
	begin
		Add(bar.listeners, listener);
	end Add_Adjust_Listener;

	procedure Set_Orientation(
		bar         : in out Scroll_Bar_Type;
		orientation : in Orientation_Type) is
	begin
		if bar.orientation /= orientation then
			bar.orientation := orientation;
			Remove(bar, bar.start);
			Remove(bar, bar.stop);
			if bar.orientation = Vertical then
				Add(bar, bar.start, Border_North);
				Add(bar, bar.stop, Border_South);
				Set_Preferred_Size(bar.center, (10, 100));
			else
				Add(bar, bar.start, Border_West);
				Add(bar, bar.stop, Border_East);
				Set_Preferred_Size(bar.center, (100, 10));
			end if;
			Paint_Bar(bar);
		end if;
	end Set_Orientation;

	procedure Set_Minimum(
		bar     : in out Scroll_Bar_Type;
		minimum : in Integer) is
	begin
		bar.minimum := minimum;
		Paint_Bar(bar);
	end Set_Minimum;

	procedure Set_Maximum(
		bar     : in out Scroll_Bar_Type;
		maximum : in Integer) is
	begin
		bar.maximum := maximum;
		Paint_Bar(bar);
	end Set_Maximum;

	procedure Set_Increment(
		bar       : in out Scroll_Bar_Type;
		increment : in Positive) is
	begin
		bar.increment := increment;
		Paint_Bar(bar);
	end Set_Increment;

	procedure Set_Value(
		bar   : in out Scroll_Bar_Type;
		value : in Integer) is
	begin
		bar.value := value;
		Paint_Bar(bar);
	end Set_Value;

	function Get_Maximum(bar : Scroll_Bar_Type) return Integer is
	begin
		return bar.maximum;
	end Get_Maximum;

	function Get_Minimum(bar : Scroll_Bar_Type) return Integer is
	begin
		return bar.minimum;
	end Get_Minimum;

	function Get_Increment(bar : Scroll_Bar_Type) return Positive is
	begin
		return bar.increment;
	end Get_Increment;

	function Get_Value(bar : Scroll_Bar_Type) return Integer is
	begin
		return bar.value;
	end Get_Value;

	procedure Increment(bar : in out Scroll_Bar_Type) is
	begin
		bar.value := bar.value + bar.increment;
		if bar.value > bar.maximum then
			bar.value := bar.maximum;
		end if;
		Paint_Bar(bar);
		Run_Listeners(bar);
	end Increment;

	procedure Decrement(bar : in out Scroll_Bar_Type) is
	begin
		bar.value := bar.value - bar.increment;
		if bar.value < bar.minimum then
			bar.value := bar.minimum;
		end if;
		Paint_Bar(bar);
		Run_Listeners(bar);
	end Decrement;

	procedure Paint(obj : in out Panel_Type'class) is
	begin
		Paint_Bar(Scroll_Bar_Type(Get_Parent(obj).all));
	end Paint;

	procedure Paint_Bar(bar : in out Scroll_Bar_Type) is
		offset : Float;
		len    : Integer;
		size   : Size_Type;
		gc     : Graphics_Type;
	begin

		size := Get_Size(bar.center);

		if bar.maximum > bar.minimum then
			len := bar.increment / (bar.maximum - bar.minimum);
			if len < 5 then
				len := 5;
			end if;
			offset := Float(bar.value) / Float(bar.maximum - bar.minimum);
		else
			len := 5;
			offset := 0.0;
		end if;

		if bar.orientation = Vertical then
			offset := offset * Float(size.height - len);
		else
			offset := offset * Float(size.width - len);
		end if;
		offset := offset - Float(len);
		if offset < 0.0 then
			offset := 0.0;
		end if;

		Clear(bar.center);
		Create(gc, Object_Type(bar.center).id);
		Set_Foreground(gc, Blue_Color);

		if len mod 2 /= 0 then
			len := len + 1;
		end if;
		if bar.orientation = Vertical then
			Draw_Rectangle(gc, 0, Integer(offset), size.width - 1, len);
		else
			Draw_Rectangle(gc, Integer(offset), 0, len, size.height - 1);
		end if;

		Destroy(gc);

	end Paint_Bar;

	procedure Increase_Listener(button : in out Button_Type'class) is
		panel     : Panel_Pointer := Get_Parent(button);
	begin
		Increment(Scroll_Bar_Type(panel.all));
	end Increase_Listener;

	procedure Decrease_Listener(button : in out Button_Type'class) is
		panel : Panel_Pointer := Get_Parent(button);
	begin
		Decrement(Scroll_Bar_Type(panel.all));
	end Decrease_Listener;

	procedure Run_Listeners(bar : in out Scroll_Bar_Type) is
		size : Natural;
	begin
		size := Get_Size(bar.listeners);
		for x in 1 .. size loop
			Get(bar.listeners, x)(bar);
		end loop;
	end Run_Listeners;

end X11.Scroll_Bar;

