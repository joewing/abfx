
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Bindings.X11.Functions; use Bindings.X11.Functions;

package body X11.Font is

	Default_Font : constant String := "-*-courier-*-r-*-*-16-*-*-*-*-*-*-*";

	procedure Initialize(font : in out Font_Type) is
		c_str : chars_ptr;
	begin
		c_str := New_String(Default_Font);
		font.fs := XLoadQueryFont(display, c_str);
		Free(c_str);
	end Initialize;

	procedure Finalize(font : in out Font_Type) is
	begin
		if Types."/="(font.fs, null) then
			XFreeFont(display, font.fs);
			font.fs := null;
		end if;
	end Finalize;

	procedure Adjust(font : in out Font_Type) is
	begin
-- TODO
null;
	end Adjust;

	function Get_Id(font : Font_Type) return Types.Font_Type is
	begin
		return font.fs.fid;
	end Get_Id;

	function Get_Width(font : Font_Type; str : String) return Natural is
		width : int;
		c_str : chars_ptr;
	begin
		c_str := New_String(str);
		width := XTextWidth(font.fs, c_str, int(str'length));
		Free(c_str);
		return Natural(width);
	end Get_Width;

	function Get_Height(font : Font_Type) return Natural is
	begin
		return Natural(font.fs.ascent) + Natural(font.fs.descent);
	end Get_Height;

	function Get_Ascent(font : Font_Type) return Natural is
	begin
		return Natural(font.fs.ascent);
	end Get_Ascent;

	function Get_Descent(font : Font_Type) return Natural is
	begin
		return Natural(font.fs.descent);
	end Get_Descent;

end X11.Font;

