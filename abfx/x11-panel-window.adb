
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Bindings.X11.Constants; use Bindings.X11.Constants;
with Bindings.X11.Functions; use Bindings.X11.Functions;

package body X11.Panel.Window is

   procedure Initialize(win : in out Window_Type) is
   begin
      Initialize(Panel_Type(win));

      XChangeProperty(display, win.id, protocols_atom,
         XA_ATOM, 32, PropModeReplace,
         delete_atom'unrestricted_access, 1);

   end Initialize;

   procedure Finalize(win : in out Window_Type) is
   begin
      Finalize(Panel_Type(win));
   end Finalize;

   procedure Set_Title(win : in out Window_Type; title : in String) is
      c_str : chars_ptr;
   begin
      win.title := To_Unbounded_String(title);
      c_str := New_String(title);
      XStoreName(display, win.id, c_str);
      Free(c_str);
   end Set_Title;

   function Get_Title(win : Window_Type) return String is
   begin
      return To_String(win.title);
   end Get_Title;

end X11.Panel.Window;

