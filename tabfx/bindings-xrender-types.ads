
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package Bindings.XRender.Types is

	type XGlyphInfo_Type is record
		width  : unsigned_short;
		height : unsigned_short;
		x      : short;
		y      : short;
		xOff   : short;
		yOff   : short;
	end record;
	pragma Convention(C, XGlyphInfo_Type);

	type XGlyphInfo_Pointer is access all XGlyphInfo_Type;

	type XRenderColor_Type is record
		red   : unsigned_short;
		green : unsigned_short;
		blue  : unsigned_short;
		alpha : unsigned_short;
	end record;
	pragma Convention(C, XRenderColor_Type);

	type XRenderColor_Pointer is access all XRenderColor_Type;

end Bindings.XRender.Types;

