
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Bindings.X11.Types; use Bindings.X11.Types;

package Bindings.X11.Functions is

   function XOpenDisplay(name : chars_ptr) return Display_Pointer;
   pragma Import(C, XOpenDisplay, "XOpenDisplay");

   function XDisplayName(str : chars_ptr) return chars_ptr;
   pragma Import(C, XDisplayName, "XDisplayName");

   function XConnectionNumber(display : Display_Pointer) return int;
   pragma Import(C, XConnectionNumber, "XConnectionNumber");

   function XPending(display : Display_Pointer) return int;
   pragma Import(C, XPending, "XPending");

   procedure XStoreName(
      display     : Display_Pointer;
      w           : Window_Type;
      window_name : chars_ptr);
   pragma Import(C, XStoreName, "XStoreName");

   procedure XSetWMName(
      display   : Display_Pointer;
      w         : Window_Type;
      text_prop : XTextProperty_Pointer);
   pragma Import(C, XSetWMName, "XSetWMName");

   function XInternAtom(
      display        : Display_Pointer;
      atom_name      : chars_ptr;
      only_if_exists : Bool_Type) return Atom_Type;
   pragma Import(C, XInternAtom, "XInternAtom");

   procedure XChangeProperty(
      display     : Display_Pointer;
      w           : Window_Type;
      property, t : Atom_Type;
      format      : int;
      mode        : int;
      data        : Atom_Pointer;
      nelements   : int);
   pragma Import(C, XChangeProperty, "XChangeProperty");

   function XLoadFont(display : Display_Pointer; name : chars_ptr)
      return Font_Type;
   pragma Import(C, XLoadFont, "XLoadFont");

   function XLoadQueryFont(
      display : Display_Pointer;
      name    : chars_ptr) return XFontStruct_Pointer;
   pragma Import(C, XLoadQueryFont, "XLoadQueryFont");

   function XTextWidth(
      fs    : XFontStruct_Pointer;
      str   : chars_ptr;
      count : int) return int;
   pragma Import(C, XTextWidth, "XTextWidth");

   procedure XFreeFont(display : Display_Pointer; fs : XFontStruct_Pointer);
   pragma Import(C, XFreeFont, "XFreeFont");

   procedure XSetFont(
      display : Display_Pointer;
      g       : GC_Type;
      font    : Font_Type);
   pragma Import(C, XSetFont, "XSetFont");

   function XCreateGC(
      display   : Display_Pointer;
      d         : Drawable_Type;
      valuemask : unsigned_long;
      values    : XGCValues_Pointer)
      return GC_Type;
   pragma Import(C, XCreateGC, "XCreateGC");

   function XFreeGC(
      display   : Display_Pointer;
      g         : GC_Type)
      return int;
   pragma Import(C, XFreeGC, "XFreeGC");

   function XCreateSimpleWindow(
      display       : Display_Pointer;
      parent        : Window_Type;
      x, y          : int;
      width, height : unsigned;
      border_width  : unsigned;
      border        : unsigned_long;
      background    : unsigned_long)
      return Window_Type;
   pragma Import(C, XCreateSimpleWindow, "XCreateSimpleWindow");

   procedure XDestroyWindow(
      display : Display_Pointer;
      w       : Window_Type);
   pragma Import(C, XDestroyWindow, "XDestroyWindow");

   function XRootWindow(display : Display_Pointer; screen_number : int)
      return Window_Type;
   pragma Import(C, XRootWindow, "XRootWindow");

   function XDefaultRootWindow(display : Display_Pointer)
      return Window_Type;
   pragma Import(C, XDefaultRootWindow, "XDefaultRootWindow");

   function XDefaultColormap(display : Display_Pointer;
                             screen  : int)
      return Colormap_Type;
   pragma Import(C, XDefaultColormap, "XDefaultColormap");

   function XAddToSaveSet(display : Display_Pointer; w : Window_Type)
      return int;
   pragma Import(C, XAddToSaveSet, "XAddToSaveSet");

   function XAllocColor(display : Display_Pointer;
      colormap : Colormap_Type; screen_in_out : XColor_Pointer)
      return Status_Type;
   pragma Import(C, XAllocColor, "XAllocColor");

   procedure XCloseDisplay(display : Display_Pointer);
   pragma Import(C, XCloseDisplay, "XCloseDisplay");

   procedure XMapWindow(display : Display_Pointer; w : Window_Type);
   pragma Import(C, XMapWindow, "XMapWindow");

   procedure XUnmapWindow(display : Display_Pointer; w : Window_Type);
   pragma Import(C, XUnmapWindow, "XUnmapWindow");

   procedure XNextEvent(
      display      : Display_Pointer;
      event_return : XEvent_Pointer);
   pragma Import(C, XNextEvent, "XNextEvent"); 

   function XDrawString(
      display : Display_Pointer;
      d       : Drawable_Type;
      gc      : GC_Type;
      x, y    : int;
      str     : chars_ptr;
      length  : int)
      return int;
   pragma Import(C, XDrawString, "XDrawString");

   procedure XSelectInput(display : Display_Pointer;
      w : Window_Type; event_mask : long);
   pragma Import(C, XSelectInput, "XSelectInput");

   procedure XDrawLine(display : Display_Pointer;
      d : Drawable_Type; g : GC_Type;
      x1, y1, x2, y2 : int);
   pragma Import(C, XDrawLine, "XDrawLine");

   procedure XDrawRectangle(
      display       : Display_Pointer;
      d             : Drawable_Type;
      g             : GC_Type;
      x, y          : int;
      width, height : int);
   pragma Import(C, XDrawRectangle, "XDrawRectangle");

   function XSetBackground(display : Display_Pointer;
      g : GC_Type; color : unsigned_long) return int;
   pragma Import(C, XSetBackground, "XSetBackground");

   function XSetForeground(display : Display_Pointer;
      g : GC_Type; color : unsigned_long) return int;
   pragma Import(C, XSetForeground, "XSetForeground");

   procedure XSetWindowBackground(display : Display_Pointer;
      w : Window_Type; color : unsigned_long);
   pragma Import(C, XSetWindowBackground, "XSetWindowBackground");

   procedure XResizeWindow(display : Display_Pointer;
      w : Window_Type; width, height : unsigned);
   pragma Import(C, XResizeWindow, "XResizeWindow");

   procedure XMoveWindow(display : Display_Pointer;
      w : Window_Type; x, y : int);
   pragma Import(C, XMoveWindow, "XMoveWindow");

   procedure XReparentWindow(display : Display_Pointer;
      w, p : Window_Type; x, y : int);
   pragma Import(C, XReparentWindow, "XReparentWindow");

   procedure XClearWindow(display : Display_Pointer; w : Window_Type);
   pragma Import(C, XClearWindow, "XClearWindow");

   function XKeycodeToKeysym(
      display : Display_Pointer;
      keycode : KeyCode_Type;
      index   : int) return KeySym_Type;
   pragma Import(C, XKeycodeToKeysym, "XKeycodeToKeysym");

   function XChangeWindowAttributes(
      display : Display_Pointer;
      window  : Window_Type;
      mask    : unsigned_long;
      attr    : XSetWindowAttributes_Pointer) return int;
   pragma Import(C, XChangeWindowAttributes, "XChangeWindowAttributes");

end Bindings.X11.Functions;

