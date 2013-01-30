
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Bindings.XRender.Types; use Bindings.XRender.Types;

package Bindings.XFT.Types is

   type XftFont_Type is record
      ascent             : int;
      descent            : int;
      height             : int;
      max_advance_width  : int;
      charset            : chars_ptr;
      pattern            : chars_ptr;
   end record;
   pragma Convention(C, XftFont_Type);

   type XftFont_Pointer is access all XftFont_Type;

   type XftDraw_Pointer is new chars_ptr;

   type XftColor_Type is record
      pixel : unsigned_long;
      color : XRenderColor_Type;
   end record;
   pragma Convention(C, XftColor_Type);

   type XftColor_Pointer is access all XftColor_Type;

end Bindings.XFT.Types;


