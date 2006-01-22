
with Ada.Finalization; use Ada.Finalization;

with Bindings.X11.Types;
pragma Elaborate_All(Bindings.X11.Types);

with Bindings.X11.Constants;
pragma Elaborate_All(Bindings.X11.Constants);

with X11.Font; use X11.Font;
pragma Elaborate_All(X11.Font);

with X11.Color; use X11.Color;

package X11.Graphics is

	type Graphics_Type is private;

	package Types renames Bindings.X11.Types;
	package Constants renames Bindings.X11.Constants;

	procedure Create(g : out Graphics_Type; id : in Types.Drawable_Type);
	procedure Destroy(g : in out Graphics_Type);

	function Get_Drawable(g : Graphics_Type) return Types.Drawable_Type;

	procedure Set_Foreground(g : in out Graphics_Type; c : in Color_Type);
	procedure Set_Background(g : in out Graphics_Type; c : in Color_Type);

	procedure Set_Font(g : in out Graphics_Type; f : in Font_Type);

	procedure Print(g : in Graphics_Type; x, y : in Integer;
		str : in String);

	procedure Draw_Line(g : in Graphics_Type; x1, y1, x2, y2 : in Integer);

	procedure Draw_Rectangle(g : in Graphics_Type;
		x, y : in Integer; width, height : in Natural);

	procedure Draw_Border(g             : in out Graphics_Type;
	                      x, y          : in Integer;
	                      width, height : in Natural;
	                      border        : in Border_Type := Up_Border;
								 color         : in Color_Type := Gray_Color);

private

	type Graphics_Type is record
		drawable : Types.Drawable_Type := Constants.None;
		gc       : Types.GC_Type := Constants.None;
		font     : Font_Type;
		color    : Color_Type;
	end record;

end X11.Graphics;

