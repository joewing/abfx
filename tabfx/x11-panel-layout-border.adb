
package body X11.Panel.Layout.Border is

	use Panel_List;

	type Border_Manager_Type is new Manager_Type with null record;

	procedure Add(
		manager  : in out Border_Manager_Type;
		panel    : in out Panel_Type'class;
		child    : in out Panel_Type'class;
		location : in Positive);

	procedure Place(
		manager : in out Border_Manager_Type;
		panel   : in out Panel_Type'class);

	procedure Replace(
		manager : in out Border_Manager_Type;
		panel   : in out Panel_Type'class;
		size    : in Size_Type);

	procedure Remove(
		manager : in out Border_Manager_Type;
		panel   : in out Panel_Type'class;
		child   : in out Panel_Type'class);

	procedure Manage(panel : in out Panel_Type'class) is
	begin
		Clear(panel.children);
		for x in 1 .. 5 loop
			Set(panel.children, null, x);
		end loop;
		panel.manager := new Border_Manager_Type;
	end Manage;

	procedure Add(
		manager  : in out Border_Manager_Type;
		panel    : in out Panel_Type'class;
		child    : in out Panel_Type'class;
		location : in Positive) is
	begin
		Set(panel.children, child'unrestricted_access, location);
	end Add;

	procedure Place(
		manager : in out Border_Manager_Type;
		panel   : in out Panel_Type'class) is

		sizes : array(1 .. 5) of Size_Type;
		tpos  : Position_Type;

	begin

		for x in sizes'range loop
			if Get(panel.children, x) /= null then
				Show(Get(panel.children, x).all);
				sizes(x) := Get(panel.children, x).preferred_size;
			else
				sizes(x) := (0, 0);
			end if;
		end loop;

		-- Compute the height
		if sizes(Border_West).height > sizes(Border_East).height then
			panel.size.height := sizes(Border_West).height;
		else
			panel.size.height := sizes(Border_East).height;
		end if;
		if sizes(Border_Center).height > panel.size.height then
			panel.size.height := sizes(Border_Center).height;
		end if;
		panel.size.height := panel.size.height
			+ sizes(Border_North).height + sizes(Border_South).height;

		-- Compute the width
		panel.size.width := sizes(Border_West).width
			+ sizes(Border_Center).width + sizes(Border_East).width;
		if sizes(Border_North).width > panel.size.width then
			panel.size.width := sizes(Border_North).width;
		end if;
		if sizes(Border_South).width > panel.size.width then
			panel.size.width := sizes(Border_South).width;
		end if;
		panel.preferred_size := panel.size;

		-- Resize subpaneldows
		if Get(panel.children, Border_North) /= null then
			sizes(Border_North).width := panel.size.width;
		end if;
		if Get(panel.children, Border_South) /= null then
			sizes(Border_South).width := panel.size.width;
		end if;
		if sizes(Border_East).height > sizes(Border_West).height then
			sizes(Border_West).height := sizes(Border_East).height;
		else
			sizes(Border_East).height := sizes(Border_West).height;
		end if;
		if sizes(Border_East).height > sizes(Border_Center).height then
			sizes(Border_Center).height := sizes(Border_East).height;
		else
			sizes(Border_East).height := sizes(Border_Center).height;
			sizes(Border_West).height := sizes(Border_Center).height;
		end if;

		-- Here we need to make a choice of which paneldow to expand in the
		-- center to take up any excess width. The center is this paneldow
		-- if it exist. If it does not, expand east and west to be as
		-- close to the same size as possible.
		if Get(panel.children, Border_Center) /= null then
			sizes(Border_Center).width := panel.size.width
				- sizes(Border_East).width - sizes(Border_West).width;
		else
			-- Check if we can make east and west equal size.
			-- If not, expand the smaller one. 
			if sizes(Border_East).width > sizes(Border_West).width then
				if sizes(Border_East).width * 2 <= panel.size.width then
					sizes(Border_East).width := panel.size.width / 2;
					sizes(Border_West).width := panel.size.width / 2;
					-- If we have a pixel left over give it to the west paneldow.
					if panel.size.width mod 2 /= 0 then
						sizes(Border_West).width := sizes(Border_West).width + 1;
					end if;
				else
					sizes(Border_West).width := panel.size.width
						- sizes(Border_East).width;
				end if;
			else
				if sizes(Border_West).width * 2 <= panel.size.width then
					sizes(Border_East).width := panel.size.width / 2;
					sizes(Border_West).width := panel.size.width / 2;
					-- If we have a pixel left over give it to the west paneldow.
					if panel.size.width mod 2 /= 0 then
						sizes(Border_West).width := sizes(Border_West).width + 1;
					end if;
				else
					sizes(Border_East).width := panel.size.width
						- sizes(Border_West).width;
				end if;
			end if;
		end if;

		for x in 1 .. 5 loop
			if Get(panel.children, x) /= null then
				Resize(Get(panel.children, x).all, sizes(x));
			end if;
		end loop;

		-- Move subpaneldows
		if Get(panel.children, Border_North) /= null then
			Move(Get(panel.children, Border_North).all, (0, 0));
		end if;
		if Get(panel.children, Border_South) /= null then
			tpos.x := 0;
			tpos.y := sizes(Border_North).height + sizes(Border_West).height;
			Move(Get(panel.children, Border_South).all, tpos);
		end if;
		if Get(panel.children, Border_West) /= null then
			tpos.x := 0;
			tpos.y := sizes(Border_North).height;
			Move(Get(panel.children, Border_West).all, tpos);
		end if;
		if Get(panel.children, Border_Center) /= null then
			tpos.x := sizes(Border_West).width;
			tpos.y := sizes(Border_North).height;
			Move(Get(panel.children, Border_Center).all, tpos);
		end if;
		if Get(panel.children, Border_East) /= null then
			tpos.x := sizes(Border_West).width + sizes(Border_Center).width;
			tpos.y := sizes(Border_North).height;
			Move(Get(panel.children, Border_East).all, tpos);
		end if;

	end Place;

	procedure Replace(
		manager : in out Border_Manager_Type;
		panel   : in out Panel_Type'class;
		size    : in Size_Type) is

		sizes : array(1 .. 5) of Size_Type;
		tpos  : Position_Type;

	begin
	
		for x in sizes'range loop
			if Get(panel.children, x) /= null then
				sizes(x) := Get(panel.children, x).preferred_size;
			else
				sizes(x) := (0, 0);
			end if;
		end loop;

		-- North and South widths must be this paneldow's width
		sizes(Border_North).width := size.width;
		sizes(Border_South).width := size.width;

		-- Precedence for width is: West, East, Center
		if sizes(Border_West).width <= size.width then
			if sizes(Border_West).width + sizes(Border_East).width
				<= size.width then
				sizes(Border_Center).width := size.width
					- sizes(Border_West).width - sizes(Border_East).width;
			else
				sizes(Border_East).width := size.width - sizes(Border_West).width;
				sizes(Border_Center).width := 0;
			end if;
		else
			sizes(Border_West).width := size.width;
			sizes(Border_East).width := 0;
			sizes(Border_Center).width := 0;
		end if;

		-- Precedence for height is: North, South, { Center, East, West }
		if sizes(Border_North).height <= size.height then
			if sizes(Border_North).height + sizes(Border_South).height
				<= size.height then
				sizes(Border_Center).height := size.height
					- sizes(Border_North).height - sizes(Border_South).height;
				sizes(Border_East).height := sizes(Border_Center).height;
				sizes(Border_West).height := sizes(Border_Center).height;
			else
				sizes(Border_South).height := size.height
					- sizes(Border_North).height;
				sizes(Border_Center).height := 0;
				sizes(Border_East).height := 0;
				sizes(Border_West).height := 0;
			end if;
		else
			sizes(Border_North).height := size.height;
			sizes(Border_South).height := 0;
			sizes(Border_Center).height := 0;
			sizes(Border_East).height := 0;
			sizes(Border_West).height := 0;
		end if;

		-- Do the resizing
		for x in sizes'range loop
			if Get(panel.children, x) /= null then
				Resize(Get(panel.children, x).all, sizes(x));
			end if;
		end loop;

		-- Move
		if Get(panel.children, Border_North) /= null then
			Move(Get(panel.children, Border_North).all, (0, 0));
		end if;
		if Get(panel.children, Border_South) /= null then
			tpos.x := 0;
			tpos.y := sizes(Border_North).height + sizes(Border_West).height;
			Move(Get(panel.children, Border_South).all, tpos);
		end if;
		if Get(panel.children, Border_West) /= null then
			tpos.x := 0;
			tpos.y := sizes(Border_North).height;
			Move(Get(panel.children, Border_West).all, tpos);
		end if;
		if Get(panel.children, Border_Center) /= null then
			tpos.x := sizes(Border_West).width;
			tpos.y := sizes(Border_North).height;
			Move(Get(panel.children, Border_Center).all, tpos);
		end if;
		if Get(panel.children, Border_East) /= null then
			tpos.x := sizes(Border_West).width + sizes(Border_Center).width;
			tpos.y := sizes(Border_North).height;
			Move(Get(panel.children, Border_East).all, tpos);
		end if;

		panel.size := size;
	end Replace;

	procedure Remove(
		manager : in out Border_Manager_Type;
		panel   : in out Panel_Type'class;
		child   : in out Panel_Type'class) is

		np   : Panel_Pointer;

	begin
		for x in 1 .. 5 loop
			np := Get(panel.children, x);
			if np = child'unrestricted_access then
				Set(panel.children, null, x);
			end if;
		end loop;
	end Remove;

end X11.Panel.Layout.Border;

