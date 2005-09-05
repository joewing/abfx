
with Ada.Text_IO; use Ada.Text_IO;

package body X11.Panel.Layout.Vertical is

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
		Add(panel.children, child'unrestricted_access);
	end Add;

	procedure Place(panel : in out Panel_Type'class) is
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
			if sizes(x).width > panel.size.width then
				panel.size.width := sizes(x).width;
			end if;
			panel.size.height := panel.size.height + sizes(x).height;
		end loop;
		panel.preferred_size := panel.size;

		-- Resize subpanels
		for x in sizes'range loop
			sizes(x).width := panel.size.width;
			Resize(Get(panel.children, x).all, sizes(x));
		end loop;

		-- Move subpanels
		offset := 0;
		for x in sizes'range loop
			Move(Get(panel.children, x).all, (0, offset));
			offset := offset + sizes(x).height;
		end loop;

	end Place;

	procedure Replace(panel : in out Panel_Type'class; size : in Size_Type) is
		sizes    : array(1 .. Get_Size(panel.children)) of Size_Type;
		total    : Natural;
		ratio    : Float;
		offset   : Natural;
		child    : Panel_Pointer;
	begin

		total := 0;
		for x in sizes'range loop
			sizes(x) := Get(panel.children, x).preferred_size;
			total := total + sizes(x).height;
		end loop;

		if total = 0 then
			return;
		end if;

		offset := 0;
		for x in sizes'range loop

			child := Get(panel.children, x);

			ratio := Float(sizes(x).height) / Float(total);
			sizes(x).height := Natural(Float(size.height) * ratio);
			sizes(x).width := size.width;

			Resize(child.all, sizes(x));
			Move(child.all, (0, offset));
			if child.replacer /= null then
				child.replacer(child.all, sizes(x));
			end if;

			offset := offset + sizes(x).height;

		end loop;

	end Replace;

	procedure Remove(panel, child : in out Panel_Type'class) is
	begin
		for x in 1 .. Get_Size(panel.children) loop
			if Get(panel.children, x) = child'unrestricted_access then
				Remove(panel.children, x);
			end if;
		end loop;
	end Remove;

end X11.Panel.Layout.Vertical;

