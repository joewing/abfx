
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Bindings.X11.Types; use Bindings.X11.Types;
with Bindings.XRender.Types; use Bindings.XRender.Types;
with Bindings.XFT.Types; use Bindings.XFT.Types;

package Bindings.XFT.Functions is

	function XftFontOpenName(
		display : Display_Pointer;
		screen  : int;
		name    : chars_ptr)
		return XftFont_Pointer;
	pragma Import(C, XftFontOpenName, "XftFontOpenName");

	function XftFontOpenXlfd(
		display : Display_Pointer;
		screen  : int;
		name    : chars_ptr)
		return XftFont_Pointer;
	pragma Import(C, XftFontOpenXlfd, "XftFontOpenXlfd");

	procedure XftFontClose(
		display : Display_Pointer;
		font    : XftFont_Pointer);
	pragma Import(C, XftFontClose, "XftFontClose");

	procedure XftTextExtents8(
		display : Display_Pointer;
		font    : XftFont_Pointer;
		str     : chars_ptr;
		length  : int;
		extents : XGlyphInfo_Pointer);
	pragma Import(C, XftTextExtents8, "XftTextExtents8");

	function XftDrawCreate(
		display  : Display_Pointer;
		drawable : Drawable_Type;
		visual   : Visual_Pointer;
		colormap : Colormap_Type)
		return XftDraw_Pointer;
	pragma Import(C, XftDrawCreate, "XftDrawCreate");

	procedure XftDrawString8(
		drawable : XftDraw_Pointer;
		color    : XftColor_Pointer;
		font     : XftFont_Pointer;
		x        : int;
		y        : int;
		str      : chars_ptr;
		length   : int);
	pragma Import(C, XftDrawString8, "XftDrawString8");

	procedure XftDrawDestroy(
		drawable : XftDraw_Pointer);
	pragma Import(C, XftDrawDestroy, "XftDrawDestroy");

	function XftColorAllocName(
		display : Display_Pointer;
		visual  : Visual_Pointer;
		cmap    : Colormap_Type;
		name    : chars_ptr;
		result  : XftColor_Pointer)
		return int;
	pragma Import(C, XftColorAllocName, "XftColorAllocName");

	function XftColorAllocValue(
		display : Display_Pointer;
		visual  : Visual_Pointer;
		cmap    : Colormap_Type;
		color   : XRenderColor_Pointer;
		result  : XftColor_Pointer)
		return int;
	pragma Import(C, XftColorAllocValue, "XftColorAllocValue");

	procedure XftColorFree(
		display : Display_Pointer;
		visual  : Visual_Pointer;
		cmap    : Colormap_Type;
		color   : XftColor_Pointer);
	pragma Import(C, XftColorFree, "XftColorFree");

end Bindings.XFT.Functions;

