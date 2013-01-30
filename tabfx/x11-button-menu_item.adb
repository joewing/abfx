
package body X11.Button.Menu_Item is

   procedure Initialize(item : in out Menu_Item_Type) is
   begin
      Initialize(Button_Type(item));
   end Initialize;

   procedure Finalize(item : in out Menu_Item_Type) is
   begin
      Finalize(Button_Type(item));
   end Finalize;

end X11.Button.Menu_Item;

