
with Interfaces.C; use Interfaces.C;

with Bindings.X11.Types; use Bindings.X11.Types;
with Bindings.X11.Functions; use Bindings.X11.Functions;

with X11.Collections.Hash;
pragma Elaborate_All(X11.Collections.Hash);

with X11.Logging; use X11.Logging;

package body X11.Color is

	type RGB_Type is record
		red   : unsigned_short;
		green : unsigned_short;
		blue  : unsigned_short;
	end record;

	type Color_Hash_Type is access function(key : RGB_Type) return Natural;
	type Color_Equal_Type is access function(a, b : RGB_Type) return Boolean;

	function Get_Color_Hash(key : RGB_Type) return Natural is
		result : Natural := 0;
	begin
		-- Combine the lower 3 bits of each element.
		result := result + Natural((key.red   and 7) * (2 ** 6));
		result := result + Natural((key.green and 7) * (2 ** 3));
		result := result + Natural((key.blue  and 7) * (2 ** 0));
		return result;
	end Get_Color_Hash;

	function Equals(a, b : RGB_Type) return Boolean is
	begin
		if          a.red   = b.red
			and then a.green = b.green
			and then a.blue  = b.blue then
			return true;
		else
			return false;
		end if;
	end Equals;

	package Color_Hash_Package is new X11.Collections.Hash(
		Element_Type       => Color_Type,
		Key_Type           => RGB_Type,
		Hash_Function_Type => Color_Hash_Type,
		Equals_Type        => Color_Equal_Type,
		Get_Hash           => Get_Color_Hash'access,
		Equal              => Equals'access);

	color_hash  : Color_Hash_Package.Hash_Type;
	color_delta : constant Float := 0.45;

	function Get_Color(red, green, blue : Float) return Color_Type is
		key   : RGB_Type;
		color : aliased Color_Type;
		rc    : Status_Type;
	begin
		key.red := unsigned_short(red * 65535.0);
		key.green := unsigned_short(green * 65535.0);
		key.blue := unsigned_short(blue * 65535.0);
		return Color_Hash_Package.Find(color_hash, key);
	exception
		when Color_Hash_Package.Not_Found =>
			color.red := key.red;
			color.green := key.green;
			color.blue := key.blue;
			rc := XAllocColor(display, colormap, color'unchecked_access);
			Color_Hash_Package.Add(color_hash, key, color);
			return color;
	end Get_Color;

	function White_Color return Color_Type is
	begin
		return Get_Color(1.0, 1.0, 1.0);
	end White_Color;

	function Black_Color return Color_Type is
	begin
		return Get_Color(0.0, 0.0, 0.0);
	end Black_Color;

	function Red_Color return Color_Type is
	begin
		return Get_Color(1.0, 0.0, 0.0);
	end Red_Color;

	function Green_Color return Color_Type is
	begin
		return Get_Color(0.0, 1.0, 0.0);
	end Green_Color;

	function Blue_Color return Color_Type is
	begin
		return Get_Color(0.0, 0.0, 1.0);
	end Blue_Color;

	function Yellow_Color return Color_Type is
	begin
		return Get_Color(1.0, 1.0, 0.0);
	end Yellow_Color;

	function Cyan_Color return Color_Type is
	begin
		return Get_Color(0.0, 1.0, 1.0);
	end Cyan_Color;

	function Gray_Color return Color_Type is
	begin
		return Get_Color(0.75, 0.75, 0.75);
	end Gray_Color;

	function Default_Foreground return Color_Type is
	begin
		return Black_Color;
	end Default_Foreground;

	function Default_Background return Color_Type is
	begin
		return Gray_Color;
	end Default_Background;

	function Lighten_Color(color : Color_Type) return Color_Type is
		a, b, c : Float;
	begin

		a := Float(color.red) / 65535.0;
		b := Float(color.green) / 65535.0;
		c := Float(color.blue) / 65535.0;

		a := a * (1.0 + color_delta);
		b := b * (1.0 + color_delta);
		c := c * (1.0 + color_delta);

		if a > 1.0 then
			a := 1.0;
		end if;
		if b > 1.0 then
			b := 1.0;
		end if;
		if c > 1.0 then
			c := 1.0;
		end if;

		return Get_Color(a, b, c);
	end Lighten_Color;

	function Darken_Color(color : Color_Type) return Color_Type is
		a, b, c : Float;
	begin

		a := Float(color.red) / 65535.0;
		b := Float(color.green) / 65535.0;
		c := Float(color.blue) / 65535.0;

		a := a * color_delta;
		b := b * color_delta;
		c := c * color_delta;

		return Get_Color(a, b, c);

	end Darken_Color;

end X11.Color;

