
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with X11.Font; use X11.Font;

package X11.Panel.Label is

   type Label_Type is new Panel_Type with private;

   procedure Set_Text(label : in out Label_Type; text : in String);

   function Get_Text(label : Label_Type'class) return String;

   procedure Set_Font(label : in out Label_Type'class; font : in Font_Type);

   procedure Set_Foreground(label : in out Label_Type'class;
      color : in Color_Type);

   procedure Set_Alignment(label : in out Label_Type'class;
      alignment : in Alignment_Type);

   procedure Set_Border(label : in out Label_Type'class;
      border : in Border_Type);

private

   type Label_Type is new Panel_Type with record
      text       : Unbounded_String;
      font       : Font_Type;
      alignment  : Alignment_Type := Center_Center;
      border     : Border_Type    := No_Border;
      foreground : Color_Type     := Default_Foreground;
   end record;

   procedure Initialize(label : in out Label_Type);
   procedure Finalize(label : in out Label_Type);

   procedure Paint(win : in out Panel_Type'class);

end X11.Panel.Label;

