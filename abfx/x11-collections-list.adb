
with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;

package body X11.Collections.List is

   procedure Free is
      new Ada.Unchecked_Deallocation(List_Array, List_Array_Pointer);

   Block_Size : constant := 8;

   procedure Add(list : in out List_Type; element : Element_Type) is
   begin

      if list.size = 0 or else list.size >= list.data'length then
         if list.size > 0 then
            declare
               temp : List_Array_Pointer := list.data;
            begin
               list.data := new List_Array(1 .. temp'length + Block_Size);
               for x in 1 .. list.size loop
                  list.data(x) := temp(x);
               end loop;
               Free(temp);
            end;
         else
            list.data := new List_Array(1 .. Block_Size);
         end if;
      end if;

      list.size := list.size + 1;
      list.data(list.size) := element;

   end Add;

   procedure Remove(list : in out List_Type; index : Positive) is
   begin
      list.size := list.size - 1;
      for x in index .. list.size loop
         list.data(x) := list.data(x + 1);
      end loop;
   end Remove;

   procedure Clear(list : in out List_Type) is
   begin
      list.size := 0;
   end Clear;

   function Get(list : List_Type; index : Positive) return Element_Type is
   begin
      return list.data(index);
   end Get;

   procedure Set(list : in out List_Type; element : in Element_Type;
      index : in Positive) is

      size : Positive;
      temp : List_Array_Pointer;

   begin

      if index > list.size then
         size := index - (index mod Block_Size) + Block_Size;
         temp := new List_Array(1 .. size);
         if list.data /= null then
            for x in 1 .. list.size loop
               temp(x) := list.data(x);
            end loop;
            Free(list.data);
         end if;
         list.data := temp;
         list.size := index;
      end if;

      list.data(index) := element;

   end Set;

   function Get_Size(list : List_Type) return Natural is
   begin
      return list.size;
   end Get_Size;

   procedure Initialize(list : in out List_Type) is
   begin
      null;
   end Initialize;

   procedure Adjust(list : in out List_Type) is
      temp : List_Array_Pointer;
   begin
      temp := list.data;
      list.data := new List_Array(1 .. temp'length);
      for x in 1 .. list.size loop
         list.data(x) := temp(x);
      end loop;
   end Adjust;

   procedure Finalize(list : in out List_Type) is
   begin
      if list.data /= null then
         Free(list.data);
      end if;
   end Finalize;

end X11.Collections.List;

