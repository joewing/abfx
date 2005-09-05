
with Ada.Finalization; use Ada.Finalization;

with Bindings.X11.Types;

package X11.Font is

	type Font_Type is new Controlled with private;

	function Get_Id(font : Font_Type) return Bindings.X11.Types.Font_Type;

	function Get_Width(font : Font_Type; str : String) return Natural;
	function Get_Height(font : Font_Type) return Natural;

	function Get_Ascent(font : Font_Type) return Natural;
	function Get_Descent(font : Font_Type) return Natural;

private

	package Types renames Bindings.X11.Types;

	type Font_Type is new Controlled with record
		fs : Types.XFontStruct_Pointer := null;
	end record;

	procedure Initialize(font : in out Font_Type);
	procedure Finalize(font : in out Font_Type);
	procedure Adjust(font : in out Font_Type);

end X11.Font;

