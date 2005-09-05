
with X11.Collections.List;
with X11.Label; use X11.Label;

pragma Elaborate_All(X11.Label);

package X11.Button is

	type Button_Type is new Label_Type with private;

	type Click_Listener_Type is access
		procedure(button : in out Button_Type'class);

	procedure Add_Click_Listener(button : in out Button_Type'class;
		listener : in Click_Listener_Type);

private

	package Click_Listener_List is
		new X11.Collections.List(Click_Listener_Type);

	type Button_Type is new Label_Type with record
		click_listeners : Click_Listener_List.List_Type;
	end record;

	procedure Initialize(button : in out Button_Type);
	procedure Finalize(button : in out Button_Type);

end X11.Button;

