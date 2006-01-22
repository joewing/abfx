
with Ada.Text_IO; use Ada.Text_IO;

package body X11.Panel.Layout.Horizontal is

	use Panel_List;

	type Horizontal_Manager_Type is new Manager_Type with null record;

	procedure Add(
		manager  : in out Horizontal_Manager_Type;
		panel    : in out Panel_Type'class;
		child    : in out Panel_Type'class;
		location : in Positive);

	procedure Place(
		manager : in out Horizontal_Manager_Type;
		panel   : in out Panel_Type'class);

	procedure Replace(
		manager : in out Horizontal_Manager_type;
		panel   : in out Panel_Type'class;
		size    : in Size_Type);

	procedure Remove(
		manager : in out Horizontal_Manager_Type;
		panel   : in out Panel_Type'class;
		child   : in out Panel_Type'class);

	procedure Manage(panel : in out Panel_Type'class) is
	begin
		Clear(panel.children);
		panel.manager := new Horizontal_Manager_Type;
	end Manage;

	procedure Add(
		manager  : in out Horizontal_Manager_Type;
		panel    : in out Panel_Type'class;
		child    : in out Panel_Type'class;
		location : in Positive) is
	begin
		Add(panel.children, child'unrestricted_access);
	end Add;

	procedure Place(
		manager : in out Horizontal_Manager_Type;
		panel   : in out Panel_Type'class) is

		sizes  : array(1 .. Get_Size(panel.children)) of Size_Type;
		offset : Natural;
		child  : Panel_Pointer;

	begin

		for x in sizes'range loop
			child := Get(panel.children, x);
			Show(child.all);
			sizes(x) := child.preferred_size;
		end loop;

		-- Compute the height and width.
		panel.size.height := 0;
		panel.size.width := 0;
		for x in sizes'range loop
			if sizes(x).height > panel.size.height then
				panel.size.height := sizes(x).height;
			end if;
			panel.size.width := panel.size.width + sizes(x).width;
		end loop;
		panel.preferred_size := panel.size;

		-- Resize subpanels
		for x in sizes'range loop
			sizes(x).height := panel.size.height;
			Resize(Get(panel.children, x).all, sizes(x));
		end loop;

		-- Move subpanels
		offset := 0;
		for x in sizes'range loop
			Move(Get(panel.children, x).all, (offset, 0));
			offset := offset + sizes(x).width;
		end loop;

	end Place;

	procedure Replace(
		manager : in out Horizontal_Manager_type;
		panel   : in out Panel_Type'class;
		size    : in Size_Type) is

		sizes  : array(1 .. Get_Size(panel.children)) of Size_Type;
		total  : Natural;
		ratio  : Float;
		offset : Natural;
		child  : Panel_Pointer;

	begin

		total := 0;
		for x in sizes'range loop
			sizes(x) := Get(panel.children, x).preferred_size;
			total := total + sizes(x).width;
		end loop;

		if total = 0 then
			return;
		end if;

		offset := 0;
		for x in sizes'range loop

			child := Get(panel.children, x);

			ratio := Float(sizes(x).width) / Float(total);
			sizes(x).width := Natural(Float(size.width) * ratio);
			sizes(x).height := size.height;

			Resize(child.all, sizes(x));
			Move(child.all, (offset, 0));
			if child.manager /= null then
				Replace(child.manager.all, child.all, sizes(x));
			end if;

			offset := offset + sizes(x).width;

		end loop;

	end Replace;

	procedure Remove(
		manager : in out Horizontal_Manager_Type;
		panel   : in out Panel_Type'class;
		child   : in out Panel_Type'class) is
	begin
		for x in 1 .. Get_Size(panel.children) loop
			if Get(panel.children, x) = child'unrestricted_access then
				Remove(panel.children, x);
			end if;
		end loop;
	end Remove;

end X11.Panel.Layout.Horizontal;

