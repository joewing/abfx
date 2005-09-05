
with Bindings.X11.Types;
pragma Elaborate_All(Bindings.X11.Types);

package X11.Color is

	subtype Color_Type is Bindings.X11.Types.XColor_Type;

	function Get_Color(red, green, blue : Float) return Color_Type;

	function White_Color return Color_Type;
	function Black_Color return Color_Type;
	function Red_Color return Color_Type;
	function Green_Color return Color_Type;
	function Blue_Color return Color_Type;
	function Yellow_Color return Color_Type;
	function Cyan_Color return Color_Type;
	function Gray_Color return Color_Type;

	function Default_Foreground return Color_Type;
	function Default_Background return Color_Type;

	function Lighten_Color(color : Color_Type) return Color_Type;
	function Darken_Color(color : Color_Type) return Color_Type;

end X11.Color;

