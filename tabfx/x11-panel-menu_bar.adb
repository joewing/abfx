
with X11.Panel.Layout.Horizontal;

package body X11.Panel.Menu_Bar is

	procedure Initialize(bar : in out Menu_Bar_Type) is
	begin
		Initialize(Panel_Type(bar));
		X11.Panel.Layout.Horizontal.Manage(bar);
	end Initialize;

	procedure Finalize(bar : in out Menu_Bar_Type) is
	begin
		Finalize(Panel_Type(bar));
	end Finalize;

	procedure Add(
		bar   : in out Menu_Bar_Type;
		child : in out Menu_Item_Type'class) is
	begin
		null;
	end Add;

end X11.Panel.Menu_Bar;

