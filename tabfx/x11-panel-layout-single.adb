
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body X11.Panel.Layout.Single is

	use Panel_List;

	type Single_Manager_Type is new Manager_Type with null record;
	type Single_Manager_Pointer is access all Single_Manager_Type;

	procedure Add(
		manager  : in out Single_Manager_Type;
		panel    : in out Panel_Type'class;
		child    : in out Panel_Type'class;
		location : in Positive);

	procedure Place(
		manager : in out Single_Manager_Type;
		panel   : in out Panel_Type'class);

	procedure Replace(
		manager : in out Single_Manager_Type;
		panel   : in out Panel_Type'class;
		size    : in Size_Type);

	procedure Remove(
		manager : in out Single_Manager_Type;
		panel   : in out Panel_Type'class;
		child   : in out Panel_Type'class);

	procedure Release(
		manager : in Single_Manager_Type;
		panel   : in out Panel_Type'class);

	procedure Free is new Ada.Unchecked_Deallocation(
		Single_Manager_Type, Single_Manager_Pointer);

	procedure Manage(panel : in out Panel_Type'class) is
	begin

		if panel.manager /= null then
			Release(panel.manager.all, panel);
		end if;

		Clear(panel.children);
		panel.manager := new Single_Manager_Type;

	end Manage;

	procedure Add(
		manager  : in out Single_Manager_Type;
		panel    : in out Panel_Type'class;
		child    : in out Panel_Type'class;
		location : in Positive) is
	begin
		Clear(panel.children);
		Add(panel.children, child'unrestricted_access);
	end Add;

	procedure Place(
		manager : in out Single_Manager_Type;
		panel   : in out Panel_Type'class) is

		child : Panel_Pointer;

	begin
		if Get_Size(panel.children) = 1 then
			child := Get(panel.children, 1);
			Show(child.all);
			panel.size := child.preferred_size;
			panel.preferred_size := panel.size;
		end if;
	end Place;

	procedure Replace(
		manager : in out Single_Manager_Type;
		panel   : in out Panel_Type'class;
		size    : in Size_Type) is

		child : Panel_Pointer;

	begin
		if Get_Size(panel.children) = 1 then
			child := Get(panel.children, 1);
			Resize(child.all, size);
			if child.manager /= null then
				Replace(child.manager.all, child.all, size);
			end if;
		end if;
		panel.size := size;
	end Replace;

	procedure Remove(
		manager : in out Single_Manager_Type;
		panel   : in out Panel_Type'class;
		child   : in out Panel_Type'class) is
	begin
		if Get(panel.children, 1) = child'unrestricted_access then
			Set(panel.children, null, 1);
		end if;
	end Remove;

	procedure Release(
		manager : in Single_Manager_Type;
		panel   : in out Panel_Type'class) is
	begin
		Free(Single_Manager_Pointer(panel.manager));
		panel.manager := null;
	end Release;

end X11.Panel.Layout.Single;

