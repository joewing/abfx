
with Ada.Text_IO; use Ada.Text_IO;

package body X11.Panel.Layout.Single is

	use Panel_List;

	procedure Add(panel, child : in out Panel_Type'class;
		location : in Positive);
	procedure Place(panel : in out Panel_Type'class);
	procedure Replace(panel : in out Panel_Type'class; size : in Size_Type);
	procedure Remove(panel, child : in out Panel_Type'class);

	procedure Manage(panel : in out Panel_Type'class) is
	begin
		Clear(panel.children);
		panel.adder := Add'access;
		panel.placer := Place'access;
		panel.replacer := Replace'access;
		panel.remover := Remove'access;
	end Manage;

	procedure Add(panel, child : in out Panel_Type'class;
		location : in Positive) is
	begin
		Clear(panel.children);
		Add(panel.children, child'unrestricted_access);
	end Add;

	procedure Place(panel : in out Panel_Type'class) is
		child : Panel_Pointer;
	begin
		if Get_Size(panel.children) = 1 then
			child := Get(panel.children, 1);
			Show(child.all);
			panel.size := child.preferred_size;
			panel.preferred_size := panel.size;
		end if;
	end Place;

	procedure Replace(panel : in out Panel_Type'class; size : in Size_Type) is
		child : Panel_Pointer;
	begin
		if Get_Size(panel.children) = 1 then
			child := Get(panel.children, 1);
			Resize(child.all, size);
			if child.replacer /= null then
				child.replacer(child.all, size);
			end if;
		end if;
		panel.size := size;
	end Replace;

	procedure Remove(panel, child : in out Panel_Type'class) is
	begin
		if Get(panel.children, 1) = child'unrestricted_access then
			Set(panel.children, null, 1);
		end if;
	end Remove;

end X11.Panel.Layout.Single;

