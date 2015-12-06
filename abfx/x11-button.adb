
with X11.Panel; use X11.Panel;

package body X11.Button is

   use Click_Listener_List;

   procedure Run_Click_Listeners(button : in out Button_Type'class);

   procedure Listener(win : in out Panel_Type'class;
      x, y : in Integer; button : in Positive; strike : in Strike_Type);

   procedure Initialize(button : in out Button_Type) is
   begin
      Initialize(Label_Type(button));
      Set_Border(button, Up_Border);
      Add_Button_Listener(button, Listener'access);
   end Initialize;

   procedure Finalize(button : in out Button_Type) is
   begin
      Finalize(Label_Type(button));
   end Finalize;

   procedure Add_Click_Listener(button : in out Button_Type'class;
      listener : in Click_Listener_Type) is
   begin
      Add(button.click_listeners, listener);
   end Add_Click_Listener;

   procedure Listener(win : in out Panel_Type'class;
      x, y : in Integer; button : in Positive; strike : in Strike_Type) is
   begin
      case strike is
         when Press =>
            Set_Border(Label_Type(win), Down_Border);
         when Release =>
            Run_Click_Listeners(Button_Type(win));
            Set_Border(Label_Type(win), Up_Border);
      end case;
   end Listener;

   procedure Run_Click_Listeners(button : in out Button_Type'class) is
      listener : Click_Listener_Type;
      size     : Natural;
   begin
      size := Get_Size(button.click_listeners);
      for x in 1 .. size loop
         listener := Get(button.click_listeners, x);
         listener(button);
      end loop;
   end Run_Click_Listeners;

end X11.Button;

