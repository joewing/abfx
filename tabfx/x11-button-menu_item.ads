
package X11.Button.Menu_Item is

   type Menu_Item_Type is new Button_Type with private;

private

   type Menu_Item_Type is new Button_Type with record
      null;
   end record;

   procedure Initialize(item : in out Menu_Item_Type);

   procedure Finalize(item : in out Menu_Item_Type);

end X11.Button.Menu_Item;

