
with X11.Collections.List;

generic
   type Element_Type is private;
   type Key_Type is private;
   type Hash_Function_Type is access function(key : Key_Type) return Natural;
   type Equals_Type is access function(a, b : Key_Type) return Boolean;
   Get_Hash  : Hash_Function_Type;
   Equal     : Equals_Type;
   Hash_Size : Natural := 16;
package X11.Collections.Hash is

   type Hash_Type is limited private;

   Not_Found : exception;

   procedure Add(hash : in out Hash_Type; key : Key_Type;
      element : Element_Type);

   procedure Remove(hash : in out Hash_Type; key : Key_Type);

   function Find(hash : Hash_Type; key : Key_Type) return Element_Type;

   procedure Clear(hash : in out Hash_Type);

private

   package Hash_List is new List(Element_Type);
   package Key_List is new List(Key_Type);

   type Hash_List_Array is array(1 .. Hash_Size) of Hash_List.List_Type;
   type Key_List_Array is array(1 .. Hash_Size) of Key_List.List_Type;

   type Hash_Type is record
      data : Hash_List_Array;
      keys : Key_List_Array;
   end record;
   
end X11.Collections.Hash;

