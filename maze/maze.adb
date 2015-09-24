
with Ada.Numerics.Discrete_Random;

with X11; use X11;
with X11.Color; use X11.Color;
with X11.Graphics; use X11.Graphics;
with X11.Label; use X11.Label;
with X11.Window; use X11.Window;
with X11.Button; use X11.Button;
with X11.Panel; use X11.Panel;
with X11.Panel.Layout.Vertical;

procedure Maze is

   width       : constant := 33; -- Must be odd.
   height      : constant := 17; -- Must be odd.

   win         : Window_Type;
   canvas      : Panel_Type;
   new_button  : Button_Type;

   subtype Random_Range is Integer range 0 .. 3;
   package Random4 is new Ada.Numerics.Discrete_Random(Random_Range);

   type Item_Type is (Empty, Wall);
   type Maze_Type is array(0 .. width - 1, 0 .. height - 1) of Item_Type;

   gen  : Random4.Generator;
   maze : Maze_Type := (others => (others => Wall));

   procedure Carve_Maze(x, y: Integer) is
      d        : Random_Range;
      dx, dy   : Integer;
      x1, y1   : Integer;
      x2, y2   : Integer;
   begin
      maze(x, y) := Empty;
      d := Random4.Random(gen);
      for c in 0 .. 3 loop
         dx := 0; dy := 0;
         case d is
            when 0 => dx := 1;
            when 1 => dx := -1;
            when 2 => dy := 1;
            when 3 => dy := -1;
         end case;
         x1 := x  + dx; y1 := y  + dy;
         x2 := x1 + dx; y2 := y1 + dy;
         if maze(x1, y1) = Wall and maze(x2, y2) = Wall then
            maze(x1, y1) := Empty;
            Carve_Maze(x2, y2);
         end if;
         d := (d + 1) mod 4;
      end loop;
   end Carve_Maze;

   procedure Generate_Maze is
   begin
      maze := (others => (others => Wall));
      for x in maze'range(1) loop
         maze(x, 0) := Empty;
         maze(x, height - 1) := Empty;
      end loop;
      for y in maze'range(2) loop
         maze(0, y) := Empty;
         maze(width - 1, y) := Empty;
      end loop;
      Carve_Maze(2, 2);
      maze(2, 1) := Empty;
      maze(width - 3, height - 2) := Empty;
   end Generate_Maze;

   procedure Paint_Maze(panel : in out Panel_Type'class) is
      g              : Graphics_Type := Get_Graphics(panel);
      size           : constant Size_Type := Get_Size(panel);
      item_height    : constant Natural := size.height / height;
      item_width     : constant Natural := size.width / width;
   begin
      for x in maze'range(1) loop
         for y in maze'range(2) loop
            if maze(x, y) = Empty then
               Set_Foreground(g, White_Color);
            else
               Set_Foreground(g, Black_Color);
            end if;
            Fill_Rectangle(g, x * item_width, y * item_width,
               item_width, item_height);
         end loop;
      end loop;
   end Paint_Maze;

   procedure New_Maze(button : in out Button_Type'class) is
   begin
      Generate_Maze;
      Paint_Maze(canvas);
   end New_Maze;

begin

   Generate_Maze;

   X11.Panel.Layout.Vertical.Manage(win);
   Set_Title(win, "Maze");

   Set_Text(new_button, "New Maze");
   Add_Click_Listener(new_button, New_Maze'unrestricted_access);

   Set_Preferred_Size(canvas, Size_Type'(width * 8, height * 8));
   Add_Painter(canvas, Paint_Maze'unrestricted_access);

   Add(win, canvas);
   Add(win, new_button);

   Show(win);

   X11.Run;

end Maze;
