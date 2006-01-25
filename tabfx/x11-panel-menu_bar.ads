
with X11.Button.Menu_Item; use X11.Button.Menu_Item;

package X11.Panel.Menu_Bar is

	type Menu_Bar_Type is new Panel_Type with private;

	procedure Add(
		bar   : in out Menu_Bar_Type;
		child : in out Menu_Item_Type'class);

private

	type Menu_Bar_Type is new Panel_Type with record
		null;
	end record;

	procedure Initialize(bar : in out Menu_Bar_Type);

	procedure Finalize(bar : in out Menu_Bar_Type);

end X11.Panel.Menu_Bar;

