
package body X11.Panel.Label is

   procedure Compute_Size(label : in out Label_Type'class);
   procedure Paint_Label(np : in out Label_Type'class);

   procedure Initialize(label : in out Label_Type) is
   begin
      Initialize(Panel_Type(label));
      Compute_Size(label);
      Add_Painter(label, Paint'access);
   end Initialize;

   procedure Finalize(label : in out Label_Type) is
   begin
      Finalize(Panel_Type(label));
   end Finalize;

   procedure Set_Text(label : in out Label_Type; text : in String) is
   begin
      label.text := To_Unbounded_String(text);
      Compute_Size(label);
      Paint_Label(label);
   end Set_Text;

   function Get_Text(label : Label_Type'class) return String is
   begin
      return To_String(label.text);
   end Get_Text;

   procedure Set_Font(label : in out Label_Type'class; font : in Font_Type) is
   begin
      label.font := font;
      Compute_Size(label);
      Paint_Label(label);
   end Set_Font;

   procedure Set_Foreground(label : in out Label_Type'class;
      color : in Color_Type) is
   begin
      label.foreground := color;
      Compute_Size(label);
      Paint_Label(label);
   end Set_Foreground;

   procedure Set_Alignment(label : in out Label_Type'class;
      alignment : in Alignment_Type) is
   begin
      label.alignment := alignment;
      Compute_Size(label);
      Paint_Label(label);
   end Set_Alignment;

   procedure Set_Border(label : in out Label_Type'class;
      border : in Border_Type) is
   begin
      label.border := border;
      Compute_Size(label);
      Paint_Label(label);
   end Set_Border;

   procedure Paint(win : in out Panel_Type'class) is
   begin
      Paint_Label(Label_Type(win));
   end Paint;

   procedure Paint_Label(np : in out Label_Type'class) is
      x, y : Integer;
   begin

      case np.border is
         when No_Border =>
            x := 0;
            y := 0;
         when Line_Border =>
            x := 2;
            y := 2;
         when Up_Border | Down_Border =>
            x := 3;
            y := 3;
         when Bevel_Border =>
            x := 4;
            y := 4;
      end case;
      case np.alignment is
         when Top_Right =>
            x := np.size.width - x
               - Get_Width(np.font, To_String(np.text));
            y := Get_Ascent(np.font) + y;
         when Top_Center =>
            x := np.size.width / 2
               - Get_Width(np.font, To_String(np.text)) / 2;
            y := Get_Ascent(np.font) + y;
         when Top_Left =>
            y := Get_Ascent(np.font) + y;
         when Center_Right =>
            x := np.size.width - x
               - Get_Width(np.font, To_String(np.text));
            y := Get_Ascent(np.font) + np.size.height / 2
               - Get_Height(np.font) / 2;
         when Center_Center =>
            x := np.size.width / 2
               - Get_Width(np.font, To_String(np.text)) / 2;
            y := Get_Ascent(np.font) + np.size.height / 2
               - Get_Height(np.font) / 2;
         when Center_Left =>
            y := Get_Ascent(np.font) + np.size.height / 2
               - Get_Height(np.font) / 2;
         when Bottom_Right =>
            x := np.size.width - x
               - Get_Width(np.font, To_String(np.text));
            y := np.size.height - y - Get_Descent(np.font);
         when Bottom_Center =>
            x := np.size.width / 2
               - Get_Width(np.font, To_String(np.text)) / 2;
            y := np.size.height - y - Get_Descent(np.font);
         when Bottom_Left =>
            y := np.size.height - y - Get_Descent(np.font);
      end case;

      Clear(Panel_Type(np));

      Set_Foreground(np.graphics, np.foreground);
      Set_Font(np.graphics, np.font);
      Print(np.graphics, x, y, To_String(np.text));

      Draw_Border(np.graphics, 0, 0, np.size.width, np.size.height,
         np.border, np.background);

   end Paint_Label;

   procedure Compute_Size(label : in out Label_Type'class) is
   begin
      label.preferred_size.height := Get_Height(label.font) + 5;
      label.preferred_size.width
         := Get_Width(label.font, To_String(label.text)) + 5;
      case label.border is
         when No_Border =>
            null;
         when Line_Border =>
            label.preferred_size.height :=
               label.preferred_size.height + 4;
            label.preferred_size.width :=
               label.preferred_size.width + 4;
         when Up_Border | Down_Border =>
            label.preferred_size.height :=
               label.preferred_size.height + 6;
            label.preferred_size.width :=
               label.preferred_size.width + 6;
         when Bevel_Border =>
            label.preferred_size.height :=
               label.preferred_size.height + 8;
            label.preferred_size.width :=
               label.preferred_size.width + 8;
      end case;
   end Compute_Size;

end X11.Panel.Label;

