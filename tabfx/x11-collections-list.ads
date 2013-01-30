
with Ada.Finalization;

generic
   type Element_Type is private;
package X11.Collections.List is

   type List_Type is limited private;

   procedure Add(list : in out List_Type; element : Element_Type);

   procedure Remove(list : in out List_Type; index : Positive);

   function Get(list : List_Type; index : Positive) return Element_Type;
   procedure Set(list : in out List_Type; element : in Element_Type;
      index : in Positive);

   procedure Clear(list : in out List_Type);

   function Get_Size(list : List_Type) return Natural;

   procedure Initialize(list : in out List_Type);
   procedure Adjust(list : in out List_Type);
   procedure Finalize(list : in out List_Type);

private

   type List_Array is array(Natural range <>) of Element_Type;
   type List_Array_Pointer is access List_Array;

   type List_Type is new Ada.Finalization.Controlled with record
      size     : Natural            := 0;
      data     : List_Array_Pointer := null;
   end record;

end X11.Collections.List;

