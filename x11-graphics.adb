
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Bindings.X11.Functions; use Bindings.X11.Functions;

package body X11.Graphics is

	procedure Create(g : out Graphics_Type; id : in Types.Drawable_Type) is
	begin
		g.drawable := id;
		g.gc := XCreateGC(display, id, unsigned_long(0), null);
		Set_Foreground(g, Default_Foreground);
		Set_Background(g, Default_Background);
	end Create;

	procedure Destroy(g : in out Graphics_Type) is
		rcode : int;
	begin
		if g.gc /= Constants.None then
			rcode := XFreeGC(display, g.gc);
			g.gc := Constants.None;
		end if;
	end Destroy;

	function Get_Drawable(g : Graphics_Type) return Types.Drawable_Type is
	begin
		return g.drawable;
	end Get_Drawable;

	procedure Set_Foreground(g : in Graphics_Type; c : in Color_Type) is
		rcode : int;
	begin
		rcode := XSetForeground(display, g.gc, c.pixel);
	end Set_Foreground;

	procedure Set_Background(g : in Graphics_Type; c : in Color_Type) is
		rcode : int;
	begin
		rcode := XSetBackground(display, g.gc, c.pixel);
	end Set_Background;

	procedure Set_Font(g : in Graphics_Type; f : in Font_Type) is
	begin
		XSetFont(display, g.gc, Get_Id(f));
	end Set_Font;

	procedure Print(g : in Graphics_Type; x, y : in Integer;
		str : in String) is
		rcode : int;
		c_str : chars_ptr;
	begin
		c_str := New_String(str);
		rcode := XDrawString(display, g.drawable, g.gc,
			int(x), int(y), c_str, int(str'length));
		Free(c_str);
	end Print;

	procedure Draw_Line(g : in Graphics_Type; x1, y1, x2, y2 : in Integer) is
	begin
		XDrawLine(display, g.drawable, g.gc,
			int(x1), int(y1), int(x2), int(y2));
	end Draw_Line;

	procedure Draw_Rectangle(g : in Graphics_Type;
		x, y : in Integer; width, height : in Natural) is
	begin
		XDrawRectangle(display, g.drawable, g.gc,
			int(x), int(y), int(width), int(height));
	end Draw_Rectangle;

end X11.Graphics;

