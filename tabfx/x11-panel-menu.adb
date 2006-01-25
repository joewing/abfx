
with X11.Panel.Layout.Vertical;

package body X11.Panel.Menu is

	procedure Click_Listener(
		panel  : in out Panel_Type'class;
		x, y   : in Integer;
		button : in Positive;
		strike : in Strike_Type);

	procedure Menu_Button_Listener(
		panel  : in out Panel_Type'class;
		x, y   : in Integer;
		button : in Positive;
		strike : in Strike_Type);

	procedure Initialize(menu : in out Menu_Type) is
	begin
		Initialize(Panel_Type(menu));
		X11.Panel.Layout.Vertical.Manage(menu);
		Add_Button_Listener(menu.label, Click_Listener'access);
		Add_Button_Listener(menu.menu, Menu_Button_Listener'access);
		Add(Panel_Type(menu), menu.label);
	end Initialize;

	procedure Finalize(menu : in out Menu_Type) is
	begin
		Finalize(Panel_Type(menu));
	end Finalize;

	procedure Set_Title(
		menu  : in out Menu_Type'class;
		title : in String) is
	begin
		Set_Text(menu.label, title);
	end Set_Title;

	procedure Add(
		menu     : in out Menu_Type;
		child    : in out Panel_Type'class;
		location : in Positive := Border_Center) is
	begin
		Add(menu.menu, child);
	end Add;

	procedure Remove(
		menu  : in out Menu_Type;
		child : in out Panel_Type'class) is
	begin
		Remove(menu.menu, child);
	end Remove;

	procedure Popup(
		menu     : in out Menu_Type'class;
		position : in Position_Type) is
	begin
		Move(menu.menu, position);
		Show(menu.menu);
	end Popup;

	procedure Click_Listener(
		panel  : in out Panel_Type'class;
		x, y   : in Integer;
		button : in Positive;
		strike : in Strike_Type) is

		position : Position_Type := panel.position;

	begin

		position.y := position.y + panel.size.height;
		Popup(Menu_Type(panel), position);

	end Click_Listener;

	procedure Menu_Button_Listener(
		panel  : in out Panel_Type'class;
		x, y   : in Integer;
		button : in Positive;
		strike : in Strike_Type) is
	begin

		if strike = Release then
			Hide(panel);
		end if;

	end Menu_Button_Listener;

end X11.Panel.Menu;

