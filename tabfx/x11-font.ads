
with Ada.Finalization; use Ada.Finalization;

with Bindings.XFT.Types;

package X11.Font is

   type Font_Type is new Controlled with private;

   function Get_Width(font : Font_Type; str : String) return Natural;
   function Get_Height(font : Font_Type) return Natural;

   function Get_Ascent(font : Font_Type) return Natural;
   function Get_Descent(font : Font_Type) return Natural;

   function Get_Font_Data(font : Font_Type)
      return Bindings.XFT.Types.XftFont_Pointer;

private

   use Bindings.XFT.Types;

   type Font_Node is record
      font  : XftFont_Pointer := null;
      count : Natural := 1;
   end record;

   type Font_Node_Pointer is access Font_Node;

   type Font_Type is new Controlled with record
      node : Font_Node_Pointer := null;
   end record;

   procedure Initialize(font : in out Font_Type);
   procedure Finalize(font : in out Font_Type);
   procedure Adjust(font : in out Font_Type);

end X11.Font;

