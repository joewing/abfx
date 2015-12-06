
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package X11.Panel.Window is

   type Window_Type is new Panel_Type with private;

   procedure Set_Title(win : in out Window_Type; title : in String);
   function Get_Title(win : Window_Type) return String;

private

   type Window_Type is new Panel_Type with record
      title : Unbounded_String;
   end record;

   procedure Initialize(win : in out Window_Type);
   procedure Finalize(win : in out Window_Type);

end X11.Panel.Window;

