
with Ada.Unchecked_Deallocation;

package body X11.Panel.Layout.Grid is

   use Panel_List;

   type Grid_Manager_Type is new Manager_Type with record
      width  : Natural;
      height : Natural;
   end record;

   type Grid_Manager_Pointer is access all Grid_Manager_Type;

   procedure Add(
      manager  : in out Grid_Manager_Type;
      panel    : in out Panel_Type'class;
      child    : in out Panel_Type'class;
      location : in Positive);

   procedure Place(
      manager : in out Grid_Manager_Type;
      panel   : in out Panel_Type'class);

   procedure Replace(
      manager : in out Grid_Manager_Type;
      panel   : in out Panel_Type'class;
      size    : in Size_Type);

   procedure Remove(
      manager : in out Grid_Manager_Type;
      panel   : in out Panel_Type'class;
      child   : in out Panel_Type'class);

   procedure Release(
      manager : in Grid_Manager_Type;
      panel   : in out Panel_Type'class);

   procedure Free is new Ada.Unchecked_Deallocation(
      Grid_Manager_Type, Grid_Manager_Pointer);

   function Get_Dimensions(
      manager : Grid_Manager_Type'class;
      count   : Natural) return Size_Type;

   procedure Manage(
      panel  : in out Panel_Type'class;
      width  : in Natural := 0;
      height : in Natural := 0) is

      mp : Grid_Manager_Pointer := new Grid_Manager_Type;

   begin

      if panel.manager /= null then
         Release(panel.manager.all, panel);
      end if;

      Clear(panel.children);
      mp.width := width;
      mp.height := height;
      panel.manager := Manager_Pointer(mp);

   end Manage;

   procedure Add(
      manager  : in out Grid_Manager_Type;
      panel    : in out Panel_Type'class;
      child    : in out Panel_Type'class;
      location : in Positive) is
   begin
      Add(panel.children, child'unrestricted_access);
   end Add;

   procedure Place(
      manager : in out Grid_Manager_Type;
      panel   : in out Panel_Type'class) is

      type Fill_Mode_Type is (Left_Right, Top_Bottom);

      dim        : Size_Type;
      count      : Natural := Get_Size(panel.children);

      child      : Panel_Pointer;
      max        : Size_Type := Size_Type'(1, 1);

   begin

      if count = 0 then
         return;
      end if;

      dim := Get_Dimensions(manager, count);

      -- Determine the size of each grid component.
      for x in 1 .. count loop
         child := Get(panel.children, x);
         Show(child.all);
         if child.preferred_size.width > max.width then
            max.width := child.preferred_size.width;
         end if;
         if child.preferred_size.height > max.height then
            max.height := child.preferred_size.height;
         end if;
      end loop;
      panel.preferred_size
         := Size_Type'(max.width * dim.width, max.height * dim.height);

      -- Resize subpanels.
      for x in 1 .. count loop
         child := Get(panel.children, x);
         Resize(child.all, max);
      end loop;

      -- Move subpanels.
      declare
         item    : Natural;
         offsetx : Natural;
         offsety : Natural;
      begin
         item := 1;
         offsety := 0;
         for y in 1 .. dim.height loop
            offsetx := 0;
            for x in 1 .. dim.width loop
               if item <= count then
                  child := Get(panel.children, item);
                  Move(child.all, (offsetx, offsety));
                  item := item + 1;
               end if;
               offsetx := offsetx + max.width;
            end loop;
            offsety := offsety + max.height;
         end loop;
      end;

   end Place;

   procedure Replace(
      manager : in out Grid_Manager_Type;
      panel   : in out Panel_Type'class;
      size    : in Size_Type) is

      count     : Natural := Get_Size(panel.children);
      dim       : Size_Type;
      max       : Size_Type;
      remainder : Size_Type;

   begin

      if count = 0 then
         return;
      end if;

      dim := Get_Dimensions(manager, count);

      max.width := size.width / dim.width;
      max.height := size.height / dim.height;
      remainder.width := size.width mod dim.width;
      remainder.height := size.height mod dim.height;

      for x in 1 .. count loop
         Resize(Get(panel.children, x).all, max);
      end loop;

      declare
         item    : Natural;
         offsetx : Natural;
         offsety : Natural;
         rx, ry  : Natural;
         child   : Panel_Pointer;
         size    : Size_Type;
      begin

         item := 1;
         offsety := 0;
         ry := remainder.height;
         for y in 1 .. dim.height loop

            offsetx := 0;
            rx := remainder.width;
            size.height := max.height;
            if ry > 0 then
               ry := ry - 1;
               size.height := size.height + 1;
            end if;

            for x in 1 .. dim.width loop
               size.width := max.width;
               if rx > 0 then
                  rx := rx - 1;
                  size.width := size.width + 1;
               end if;
               if item <= count then
                  child := Get(panel.children, item);
                  Move(child.all, (offsetx, offsety));
                  Resize(child.all, size);
                  item := item + 1;
               end if;
               offsetx := offsetx + size.width;
            end loop;
            offsety := offsety + size.height;

         end loop;

      end;
      
   end Replace;

   procedure Remove(
      manager : in out Grid_Manager_Type;
      panel   : in out Panel_Type'class;
      child   : in out Panel_Type'class) is
   begin
      for x in 1 .. Get_Size(panel.children) loop
         if Get(panel.children, x) = child'unrestricted_access then
            Remove(panel.children, x);
         end if;
      end loop;
   end Remove;

   procedure Release(
      manager : in Grid_Manager_Type;
      panel   : in out Panel_Type'class) is
   begin
      Free(Grid_Manager_Pointer(panel.manager));
      panel.manager := null;
   end Release;

   function Get_Dimensions(
      manager : Grid_Manager_Type'class;
      count   : Natural) return Size_Type is

      width  : Natural := manager.width;
      height : Natural := manager.height;

   begin

      if width = 0 and height = 0 then

         -- Square
         loop
            if width * height >= count then
               exit;
            end if;
            width := width + 1;
            if width * height >= count then
               exit;
            end if;
            height := height + 1;
         end loop;

      elsif width = 0 then

         -- Unconstrained width.
         width := count / height;
         if count mod height /= 0 then
            width := width + 1;
         end if;

      elsif height = 0 then

         -- Unconstrained height.
         height := count / width;
         if count mod width /= 0 then
            height := height + 1;
         end if;

      end if;

      return Size_Type'(width, height);

   end Get_Dimensions;

end X11.Panel.Layout.Grid;

