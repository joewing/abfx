
with Ada.Text_IO; use Ada.Text_IO;

with X11.Panel.Layout.Border;
pragma Elaborate_All(X11.Panel.Layout.Border);

with X11.Graphics; use X11.Graphics;
pragma Elaborate_All(X11.Graphics);

with X11.Color; use X11.Color;

package body X11.Scroll_Bar is

   use Listener_List;

   type Indicator_Type is record
      offset : Natural;
      length : Natural;
   end record;

   procedure Paint(obj : in out Panel_Type'class);
   procedure Paint_Bar(bar : in out Scroll_Bar_Type);
   procedure Draw_Indicator(gc : in out Graphics_Type;
      x, y, width, height : in Integer);

   procedure Increase_Listener(button : in out Button_Type'class);
   procedure Decrease_Listener(button : in out Button_Type'class);
   procedure Button_Listener(
      panel  : in out Panel_Type'class;
      x, y   : in Integer;
      button : in Positive;
      strike : in Strike_Type);

   procedure Motion_Listener(
      panel  : in out Panel_Type'class;
      x, y   : in Integer;
      mask   : in Natural);

   procedure Run_Listeners(bar : in out Scroll_Bar_Type);

   function Compute_Indicator(bar : Scroll_Bar_Type) return Indicator_Type;

   procedure Initialize(bar : in out Scroll_Bar_Type) is
   begin
      Initialize(Panel_Type(bar));
      X11.Panel.Layout.Border.Manage(bar);

      Add_Painter(bar.center, Paint'access);

      Set_Text(bar.start, " - ");
      Set_Text(bar.stop, " + ");

      -- Default orientation is vertical
      Add(bar, bar.start, Border_North);
      Add(bar, bar.stop, Border_South);
      Add(bar, bar.center, Border_Center);
      Set_Preferred_size(bar.center, (1, 100));

      Add_Click_Listener(bar.start, Decrease_Listener'access);
      Add_Click_Listener(bar.stop, Increase_Listener'access);
      Add_Button_Listener(bar.center, Button_Listener'access);
      Add_Motion_Listener(bar.center, Motion_Listener'access);

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
      indicator : Indicator_Type;
      size      : Size_Type;
      gc        : Graphics_Type;
   begin

      size := Get_Size(bar.center);

      indicator := Compute_Indicator(bar);

      Clear(bar.center);
      Create(gc, Object_Type(bar.center).id);

      if bar.orientation = Vertical then
         Draw_Indicator(gc, 0, indicator.offset, size.width,
            indicator.length);
      else
         Draw_Indicator(gc, indicator.offset, 0, indicator.length,
            size.height);
      end if;

      Destroy(gc);

   end Paint_Bar;

   procedure Draw_Indicator(gc : in out Graphics_Type;
                            x, y, width, height : in Integer) is
   begin
      if width > 1 and then height > 1 then
         Draw_Border(gc, x, y, width, height);
      end if;
   end Draw_Indicator;

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

   procedure Button_Listener(
      panel  : in out Panel_Type'class;
      x, y   : in Integer;
      button : in Positive;
      strike : in Strike_Type) is

      bar       : Scroll_Bar_Pointer := Scroll_Bar_Pointer(Get_Parent(panel));
      diff      : Integer := bar.maximum - bar.minimum;
      size      : Integer;
      index     : Integer;

      indicator : Indicator_Type;

   begin

      if strike /= Press then
         return;
      end if;

      if diff <= 0 then
         return;
      end if;

      if bar.orientation = Vertical then
         size := Get_Size(bar.center).height;
         index := y;
      else
         size := Get_Size(bar.center).width;
         index := x;
      end if;
      indicator := Compute_Indicator(Scroll_Bar_Type(bar.all));
      index := index + indicator.length / 2;
      index := (index * diff) / size;
      index := index + bar.minimum;

      -- Now index should be the new value.
      Set_Value(bar.all, index);

   end Button_Listener;

   procedure Motion_Listener(
      panel : in out Panel_Type'class;
      x, y  : in Integer;
      mask  : in Natural) is
   begin
      if mask = 1 then
         -- Pretend to be a button event.
         Button_Listener(panel, x, y, 1, Press);
      end if;
   end Motion_Listener;

   procedure Run_Listeners(bar : in out Scroll_Bar_Type) is
      size : Natural;
   begin
      size := Get_Size(bar.listeners);
      for x in 1 .. size loop
         Get(bar.listeners, x)(bar);
      end loop;
   end Run_Listeners;

   function Compute_Indicator(bar : Scroll_Bar_Type) return Indicator_Type is

      result     : Indicator_Type;
      difference : Integer := bar.maximum - bar.minimum;

      length     : Integer;
      center     : Integer;
      size       : Integer;

   begin

      if bar.orientation = Vertical then
         length := Get_Size(bar.center).height;
      else
         length := Get_Size(bar.center).width;
      end if;

      if difference < 1 or else length < 1 then
         result.offset := 0;
         result.length := 1;
         return result;
      end if;


      center := ((bar.value - bar.minimum) * length) / difference;
      size := length / difference;
      if size < 5 then
         size := 5;
      end if;
      center := center - size / 2;

      if center + size > length then
         center := length - size;
      end if;
      if center < 0 then
         center := 0;
      end if;

      result.offset := center;
      result.length := size;

      return result;
   end Compute_Indicator;

end X11.Scroll_Bar;

