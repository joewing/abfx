
package body X11.Collections.Hash is

	use Hash_List;
	use Key_List;

	procedure Add(
		hash    : in out Hash_Type;
		key     : in Key_Type;
		element : in Element_Type) is

		index : Positive := (Get_Hash(key) mod Hash_Size) + 1;

	begin

		Add(hash.data(index), element);
		Add(hash.keys(index), key);

	end Add;

	procedure Remove(
		hash : in out Hash_Type;
		key  : in Key_Type) is

		index : Positive := (Get_Hash(key) mod Hash_Size) + 1;
		size  : Natural := Get_Size(hash.data(index));

	begin

		for x in 1 .. size loop
			if Equal(Get(hash.keys(index), x), key) then
				Remove(hash.keys(index), x);
				Remove(hash.data(index), x);
				return;
			end if;
		end loop;

		raise Not_Found;

	end Remove;

	function Find(hash : Hash_Type; key : Key_Type) return Element_Type is

		index : Positive := (Get_Hash(key) mod Hash_Size) + 1;
		size  : Natural := Get_Size(hash.data(index));

	begin

		for x in 1 .. size loop
			if Equal(Get(hash.keys(index), x), key) then
				return Get(hash.data(index), x);
			end if;
		end loop;

		raise Not_Found;

	end Find;

	procedure Clear(hash : in out Hash_Type) is
	begin
		for x in hash.data'range loop
			Clear(hash.data(x));
			Clear(hash.keys(x));
		end loop;
	end Clear;

end X11.Collections.Hash;

