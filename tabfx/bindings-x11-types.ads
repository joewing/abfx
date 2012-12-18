
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Bindings.X11.Constants; use Bindings.X11.Constants;

package Bindings.X11.Types is

	subtype XID_Type is unsigned_long;
	subtype Mask_Type is unsigned_long;
	subtype Atom_Type is unsigned_long;
	subtype VisualID_Type is unsigned_long;
	subtype Time_Type is unsigned_long;
	subtype KeyCode_Type is unsigned_long;

	subtype Window_Type is XID_Type;
	subtype Drawable_Type is XID_Type;
	subtype Font_Type is XID_Type;
	subtype Pixmap_Type is XID_Type;
	subtype Cursor_Type is XID_Type;
	subtype Colormap_Type is XID_Type;
	subtype GContext_Type is XID_Type;
	subtype KeySym_Type is XID_Type;
	subtype GC_Type is XID_Type;

	subtype Bool_Type is int;
	subtype Status_Type is int;

	subtype XPointer_Type is chars_ptr;

	type Atom_Pointer is access all Atom_Type;

	type Free_Private_Pointer is access function return int;

	type XExtData_Type;
	type XExtData_Pointer is access XExtData_Type;
	type XExtData_Type is record
		number       : int;
		next         : XExtData_Pointer;
		free_private : Free_Private_Pointer;
		private_data : XPointer_Type;
	end record;
	pragma Convention(C, XExtData_Type);

	type XExtCodes_Type is record
		extension    : int;
		major_opcode : int;
		first_event  : int;
		first_error  : int;
	end record;
	pragma Convention(C, XExtCodes_Type);

	type XPixmapFormatValues_Type is record
		depth          : int;
		bits_per_pixel : int;
		scanline_pad   : int;
	end record;
	pragma Convention(C, XPixmapFormatValues_Type);

	type XGCValues_Type is record
		func               : int;
		plane_mask         : unsigned_long;
		foreground         : unsigned_long;
		background         : unsigned_long;
		line_width         : int;
		line_style         : int;
		cap_style          : int;
		join_style         : int;
		fill_style         : int;
		fill_rule          : int;
		arc_mode           : int;
		tile               : Pixmap_Type;
		stipple            : Pixmap_Type;
		ts_x_origin        : int;
		ts_y_origin        : int;
		font               : Font_Type;
		subwindow_mode     : int;
		graphics_exposures : Bool_Type;
		clip_x_origin      : int;
		clip_y_origin      : int;
		clip_mask          : Pixmap_Type;
		dash_offset        : int;
		dashes             : Char;
	end record;
	pragma Convention(C, XGCValues_Type);

	type XGCValues_Pointer is access XGCValues_Type;

	type Visual_Type is record
		ext_data           : XExtData_Pointer;
		visualid           : VisualID_Type;
		class              : int;
		red_mask           : unsigned_long;
		green_mask         : unsigned_long;
		blue_mask          : unsigned_long;
		bits_per_rgb       : int;
		map_entries        : int;
	end record;
	pragma Convention(C, Visual_Type);

	type Visual_Pointer is access Visual_Type;

	type Depth_Type is record
		depth     : int;
		nvisuals  : int;
		visuals   : Visual_Pointer;
	end record;
	pragma Convention(C, Depth_Type);

	type Depth_Pointer is access Depth_Type;

	type Display_Type;
	type Display_Pointer is access Display_Type;

	type Screen_Type is record
		ext_data        : XExtData_Pointer;
		display         : Display_Pointer;
		root            : Window_Type;
		width           : int;
		height          : int;
		mwidth          : int;
		mheight         : int;
		ndepths         : int;
		depths          : Depth_Pointer;
		root_depth      : int;
		root_visual     : Visual_Pointer;
		default_gc      : GC_Type;
		cmap            : Colormap_Type;
		white_pixel     : unsigned_long;
		black_pixel     : unsigned_long;
		max_maps        : int;
		min_maps        : int;
		backing_store   : int;
		save_unders     : Bool_Type;
		root_input_mask : Long;
	end record;
	pragma Convention(C, Screen_Type);

	type Screen_Array is array(0 .. int'last) of Screen_Type;
	pragma Convention(C, Screen_Array);
	type Screen_Pointer is access all Screen_Array;
	pragma Convention(C, Screen_Pointer);

	type ScreenFormat_Type is record
		ext_data        : XExtData_Pointer;
		depth           : int;
		bits_per_pixel  : int;
		scanline_pad    : int;
	end record;
	pragma Convention(C, ScreenFormat_Type);

	type XSetWindowAttributes_Type is record
		background_pixmap     : Pixmap_Type;
		background_pixel      : unsigned_long;
		border_pixmap         : Pixmap_Type;
		border_pixel          : unsigned_long;
		bit_gravity           : int;
		win_gravity           : int;
		backing_store         : int;
		backing_planes        : unsigned_long;
		backing_pixel         : unsigned_long;
		save_under            : Bool_Type;
		event_mask            : Long;
		do_not_propagate_mask : Long;
		override_redirect     : Bool_Type;
		colormap              : Colormap_Type;
		cursor                : Cursor_Type;
	end record;
	pragma Convention(C, XSetWindowAttributes_Type);

	type XSetWindowAttributes_Pointer is access all XSetWindowAttributes_Type;

	type XWindowAttributes_Type is record
		x, y                  : int;
		width, height         : int;
		border_width          : int;
		depth                 : int;
		visual                : Visual_Pointer;
		root                  : Window_Type;
		class                 : int;
		bit_gravity           : int;
		win_gravity           : int;
		backing_store         : int;
		backing_planes        : unsigned_long;
		backing_pixel         : unsigned_long;
		save_under            : Bool_Type;
		colormap              : Colormap_Type;
		map_installed         : Bool_Type;
		map_state             : int;
		all_event_masks       : Long;
		your_event_mask       : Long;
		do_not_propage_mask   : Long;
		override_redirect     : Bool_Type;
		screen                : Screen_Pointer;
	end record;
	pragma Convention(C, XWindowAttributes_Type);

	type XHostAddress_Type is record
		family                : int;
		length                : int;
		addresses             : chars_ptr;
	end record;
	pragma Convention(C, XHostAddress_Type);

	type XImage_Type;
	type XImage_Pointer is access XImage_Type;

	type Destroy_Image_Type is access
		function (image : XImage_Pointer) return int;
	type Get_Pixel_Type is access
		function (image : XImage_Pointer; x, y : Int) return unsigned_long;
	type Put_Pixel_Type is access
		function (image : XImage_Pointer; x, y : int; p : unsigned_long)
		return int;
	type Sub_Image_Type is access
		function (image : XImage_Pointer; x, y : int;
		width, height : unsigned) return XImage_Pointer;
	type Add_Pixel_Type is access
		function (image : XImage_Pointer; p : Long) return int;

	type XImage_Functions_Type is record
		destroy_image         : Destroy_Image_Type;
		get_pixel             : Get_Pixel_Type;
		put_pixel             : Put_Pixel_Type;
		sub_image             : Sub_Image_Type;
		add_pixel             : Add_Pixel_Type;
	end record;

	type XImage_Type is record
		width, height         : int;
		xoffset               : int;
		format                : int;
		data                  : chars_ptr;
		byte_order            : int;
		bitmap_unit           : int;
		bitmap_pad            : int;
		depth                 : int;
		bytes_per_line        : int;
		bits_per_pixel        : int;
		red_mask              : unsigned_long;
		green_mask            : unsigned_long;
		blue_mask             : unsigned_long;
		obdata                : XPointer_Type;
		f                     : XImage_Functions_Type;
	end record;
	pragma Convention(C, XImage_Type);

	type XWindowChanges_Type is record
		x, y                  : int;
		width, height         : int;
		border_width          : int;
		sibling               : Window_Type;
		stack_mode            : int;
	end record;
	pragma Convention(C, XWindowChanges_Type);

	type XColor_Type is record
		pixel                 : unsigned_long;
		red, green, blue      : unsigned_Short;
		flags                 : Char;
		pad                   : Char;
	end record;
	pragma Convention(C, XColor_Type);

	type XColor_Pointer is access all XColor_Type;

	type XSegment_Type is record
		x1, y1, x2, y2        : Short;
	end record;
	pragma Convention(C, XSegment_Type);

	type XPoint_Type is record
		x, y                  : Short;
	end record;
	pragma Convention(C, XPoint_Type);

	type XRectangle_Type is record
		x, y                  : Short;
		width, height         : unsigned_Short;
	end record;
	pragma Convention(C, XRectangle_Type);

	type XArc_Type is record
		x, y                  : Short;
		width, height         : unsigned_Short;
		angle1, angle2        : Short;
	end record;
	pragma Convention(C, XArc_Type);

	type XKeyboardControl_Type is record
		key_click_percent     : int;
		bell_percent          : int;
		bell_pitch            : int;
		bell_duration         : int;
		led                   : int;
		led_mode              : int;
		key                   : int;
		auto_repeat_mode      : int;
	end record;
	pragma Convention(C, XKeyboardControl_Type);

	type Key_State_Type is array(0 .. 31) of Char;
	type Key_State_Pointer is access Key_State_Type;

	type XKeyboardState_Type is record
		key_click_percent     : int;
		bell_percent          : int;
		bell_pitch            : unsigned;
		bell_duration         : unsigned;
		led_mask              : unsigned_long;
		global_auto_repeat    : int;
		auto_repeats          : Key_State_Pointer;
	end record;
	pragma Convention(C, XKeyboardState_Type);

	type XTimeCoord_Type is record
		time                  : Time_Type;
		x, y                  : Short;
	end record;
	pragma Convention(C, XTimeCoord_Type);

	type XModifierKeymap_Type is record
		max_keypermod         : int;
		modifiermap           : chars_ptr;
	end record;
	pragma Convention(C, XModifierKeymap_Type);

	type ScreenFormat_Pointer is access ScreenFormat_Type;

	type Resource_Alloc_Type is access
		function (d : Display_Pointer) return XID_Type;

	type Private15_Type is access
		function return chars_ptr;

	type Display_Type is record
		ext_data              : XExtData_Pointer;
		private1              : chars_ptr;
		fd                    : int;
		private2              : int;
		proto_major_version   : int;
		proto_minor_version   : int;
		vender                : chars_ptr;
		private3              : XID_Type;
		private4              : XID_Type;
		private5              : XID_Type;
		private6              : int;
		resource_alloc        : Resource_Alloc_Type;
		byte_order            : int;
		bitmap_unit           : int;
		bitmap_pad            : int;
		bitmap_bit_order      : int;
		nformats              : int;
		pixmap_format         : ScreenFormat_Pointer;
		private8              : int;
		release               : int;
		private9, private10   : chars_ptr;
		qlen                  : int;
		last_request_read     : unsigned_long;
		request               : unsigned_long;
		private11             : XPointer_Type;
		private12             : XPointer_Type;
		private13             : XPointer_Type;
		private14             : XPointer_Type;
		max_request_size      : unsigned;
		db                    : chars_ptr;
		private15             : Private15_Type;
		display_name          : chars_ptr;
		default_screen        : int;
		nscreens              : int;
		screens               : Screen_Pointer;
		motion_buffer         : unsigned_long;
		private16             : unsigned_long;
		min_keycode           : int;
		max_keycode           : int;
		private17             : XPointer_Type;
		private18             : XPointer_Type;
		private19             : int;
		xdefaults             : chars_ptr;
	end record;
	pragma Convention(C, Display_Type);

	type XKeyEvent_Type is record
		window                : Window_Type;
		root                  : Window_Type;
		subwindow             : Window_Type;  
		time                  : Time_Type;
		x, y                  : int;
		x_root, y_root        : int;
		state                 : int;
		keycode               : KeyCode_Type;
		same_screen           : Bool_Type;
	end record;
	pragma Convention(C, XKeyEvent_Type);

	subtype XKeyPressedEvent_Type is XKeyEvent_Type;
	subtype XKeyReleasedEvent_Type is XKeyEvent_Type;

	type XKeyEvent_Pointer is access XKeyEvent_Type;
	type XKeyPressedEvent_Pointer is access XKeyPressedEvent_Type;
	type XKeyReleasedEvent_Pointer is access XKeyPressedEvent_Type;

	type XButtonEvent_Type is record
		window                : Window_Type;
		root                  : Window_Type;
		subwindow             : Window_Type;
		time                  : Time_Type;
		x, y                  : int;
		x_root, y_root        : int;
		state                 : unsigned;
		button                : unsigned;
		same_screen           : Bool_Type;
	end record;
	pragma Convention(C, XButtonEvent_Type);

	subtype XButtonPressedEvent_Type is XButtonEvent_Type;
	subtype XButtonReleasedEvent_Type is XButtonEvent_Type;

	type XButtonEvent_Pointer is access XButtonEvent_Type;
	type XButtonPressedEvent_Pointer is access XButtonPressedEvent_Type;
	type XButtonReleasedEvent_Pointer is access XButtonReleasedEvent_Type;

	type XMotionEvent_Type is record
		window                : Window_Type;
		root                  : Window_Type;
		subwindow             : Window_Type;
		time                  : Time_Type;
		x, y                  : int;
		x_root, y_root        : int;
		state                 : unsigned;
		is_hint               : char;
		same_screen           : Bool_Type;
	end record;
	pragma Convention(C, XMotionEvent_Type);

	subtype XPointerMovedEvent_Type is XMotionEvent_Type;

	type XMotionEvent_Pointer is access XMotionEvent_Type;
	type XPointerMovedEvent_Pointer is access XPointerMovedEvent_Type;

	type XCrossingEvent_Type is record
		window                : Window_Type;
		root                  : Window_Type;
		subwindow             : Window_Type;
		time                  : Time_Type;
		x, y                  : int;
		x_root, y_root        : int;
		mode                  : int;
		detail                : int;
		same_screen           : Bool_Type;
		focus                 : Bool_Type;
		state                 : unsigned;
	end record;
	pragma Convention(C, XCrossingEvent_Type);

	subtype XEnterWindowEvent_Type is XCrossingEvent_Type;
	subtype XLeaveWindowEvent_Type is XCrossingEvent_Type;

	type XCrossingEvent_Pointer is access XCrossingEvent_Type;
	type XEnterWindowEvent_Pointer is access XEnterWindowEvent_Type;
	type XLeaveWindowEvent_Pointer is access XLeaveWindowEvent_Type;

	type XFocusChangeEvent_Type is record
		window                : Window_Type;
		mode                  : int;
		detail                : int;
	end record;
	pragma Convention(C, XFocusChangeEvent_Type);

	subtype XFocusInEvent_Type is XFocusChangeEvent_Type;
	subtype XFocusOutEvent_Type is XFocusChangeEvent_Type;

	type XFocusChangeEvent_Pointer is access XFocusChangeEvent_Type;
	type XFocusInEvent_Pointer is access XFocusInEvent_Type;
	type XFocusOutEvent_Pointer is access XFocusOutEvent_Type;

	type XKeymapEvent_Type is record
		window                : Window_Type;
		key_vector            : Key_State_Type;
	end record;
	pragma Convention(C, XKeymapEvent_Type);

	type XKeymapEvent_Pointer is access XKeymapEvent_Type;

	type XExposeEvent_Type is record
		window                : Window_Type;
		x, y                  : int;
		width, height         : int;
		count                 : int;
	end record;
	pragma Convention(C, XExposeEvent_Type);

	type XExposeEvent_Pointer is access XExposeEvent_Type;

	type XGraphicsExposeEvent_Type is record
		drawable              : Drawable_Type;
		x, y                  : int;
		width, height         : int;
		count                 : int;
		major_code            : int;
		minor_code            : int;
	end record;
	pragma Convention(C, XGraphicsExposeEvent_Type);

	type XGraphicsExposeEvent_Pointer is access XGraphicsExposeEvent_Type;

	type XNoExposeEvent_Type is record
		drawable              : Drawable_Type;
		major_code            : int;
		minor_code            : int;
	end record;
	pragma Convention(C, XNoExposeEvent_Type);

	type XVisibilityEvent_Type is record
		window                : Window_Type;
		state                 : int;
	end record;
	pragma Convention(C, XVisibilityEvent_Type);

	type XCreateWindowEvent_Type is record
		parent                : Window_Type;
		window                : Window_Type;
		x, y                  : int;
		width, height         : int;
		border_width          : int;
		override_redirect     : Bool_Type;
	end record;
	pragma Convention(C, XCreateWindowEvent_Type);

	type XDestroyWindowEvent_Type is record
		event                 : Window_Type;
		window                : Window_Type;
	end record;
	pragma Convention(C, XDestroyWindowEvent_Type);

	type XUnmapEvent_Type is record
		event                 : Window_Type;
		window                : Window_Type;
		from_configure        : Bool_Type;
	end record;
	pragma Convention(C, XUnmapEvent_Type);

	type XMapEvent_Type is record
		event                 : Window_Type;
		window                : Window_Type;
		override_redirect     : Bool_Type;
	end record;
	pragma Convention(C, XMapEvent_Type);

	type XMapRequestEvent_Type is record
		event                 : Window_Type;
		window                : Window_Type;
	end record;
	pragma Convention(C, XMapRequestEvent_Type);

	type XReparentEvent_Type is record
		event                 : Window_Type;
		window                : Window_Type;
		parent                : Window_Type;
		x, y                  : int;
		override_redirect     : Bool_Type;
	end record;
	pragma Convention(C, XReparentEvent_Type);

	type XConfigureEvent_Type is record
		event                 : Window_Type;
		window                : Window_Type;
		x, y                  : int;
		width, height         : int;
		border_width          : int;
		above                 : Window_Type;
		override_redirect     : Bool_Type;
	end record;
	pragma Convention(C, XConfigureEvent_Type);

	type XGravityEvent_Type is record
		event                 : Window_Type;
		window                : Window_Type;
		x, y                  : int;
	end record;
	pragma Convention(C, XGravityEvent_Type);

	type XResizeRequestEvent_Type is record
		window                : Window_Type;
		width, height         : int;
	end record;
	pragma Convention(C, XResizeRequestEvent_Type);

	type XConfigureRequestEvent_Type is record
		parent                : Window_Type;
		window                : Window_Type;
		x, y                  : int;
		width, height         : int;
		border_width          : int;
		above                 : Window_Type;
		detail                : int;
		value_mask            : unsigned_long;
	end record;
	pragma Convention(C, XConfigureRequestEvent_Type);

	type XCirculateEvent_Type is record
		event                 : Window_Type;
		window                : Window_Type;
		place                 : int;
	end record;
	pragma Convention(C, XCirculateEvent_Type);

	type XCirculateRequestEvent_Type is record
		parent                : Window_Type;
		window                : Window_Type;
		place                 : int;
	end record;
	pragma Convention(C, XCirculateRequestEvent_Type);

	type XPropertyEvent_Type is record
		window                : Window_Type;
		atom                  : Atom_Type;
		time                  : Time_Type;
		state                 : int;
	end record;
	pragma Convention(C, XPropertyEvent_Type);

	type XSelectionClearEvent_Type is record
		window                : Window_Type;
		selection             : Atom_Type;
		time                  : Time_Type;
	end record;
	pragma Convention(C, XSelectionClearEvent_Type);

	type XSelectionRequestEvent_Type is record
		owner                 : Window_Type;
		requestor             : Window_Type;
		selection             : Atom_Type;
		target                : Atom_Type;
		property              : Atom_Type;
		time                  : Time_Type;
	end record;
	pragma Convention(C, XSelectionRequestEvent_Type);

	type XSelectionEvent_Type is record
		requestor             : Window_Type;
		selection             : Atom_Type;
		target                : Atom_Type;
		property              : Atom_Type;
		time                  : Time_Type;
	end record;
	pragma Convention(C, XSelectionEvent_Type);

	type XColormapEvent_Type is record
		window                : Window_Type;
		colormap              : Colormap_Type;
		c_new                 : Bool_Type;
		state                 : int;
	end record;
	pragma Convention(C, XColormapEvent_Type);

	type Format_Type is (Format_8, Format_16, Format_32);
	for Format_Type use (Format_8 => 8, Format_16 => 16, Format_32 => 32);
	for Format_Type'size use int'size;

	type Char_Array20 is array(0 .. 19) of char;
	type Short_Array10 is array(0 .. 9) of short;
	type Long_Array5 is array(0 .. 4) of long;

	type XClientMessageEvent_Union(f : Format_Type := Format_32) is record
		format : Format_Type := f;
		case f is
			when Format_8 =>
				b : Char_Array20;
			when Format_16 =>
				s : Short_Array10;
			when Format_32 =>
				l : Long_Array5;
		end case;
	end record;
        pragma Unchecked_Union(XClientMessageEvent_Union);
	pragma Convention(C, XClientMessageEvent_Union);

	type XClientMessageEvent_Type is record
		window                : Window_Type;
		message_type          : Atom_Type;
		data                  : XClientMessageEvent_Union;
	end record;
	pragma Convention(C, XClientMessageEvent_Type);

	type XMappingEvent_Type is record
		window                : Window_Type;
		request               : int;
		first_keycode         : int;
		count                 : int;
	end record;
	pragma Convention(C, XMappingEvent_Type);

	type XErrorEvent_Type is record
		t                     : int;
		display               : Display_Pointer;
		resourceid            : XID_Type;
		serial                : unsigned_long;
		error_code            : unsigned_char;
		request_code          : unsigned_char;
		minor_code            : unsigned_char;
	end record;
	pragma Convention(C, XErrorEvent_Type);

	type XAnyEvent_Type is record
		window                : Window_Type;
	end record;
	pragma Convention(C, XAnyEvent_Type);

	type Pad24_Type is array(0 .. 24) of long;

	type XEvent_Type(event_type : int := InvalidEvent) is record
		t                     : int := event_type;
		serial                : unsigned_long;
		send_event            : Bool_Type;
		display               : Display_Pointer;
		case event_type is
			when KeyPress | KeyRelease =>
				xkey : XKeyEvent_Type;
			when ButtonPress | ButtonRelease =>
				xbutton : XButtonEvent_Type;
			when MotionNotify =>
				xmotion : XMotionEvent_Type;
			when EnterNotify | LeaveNotify =>
				xcrossing : XCrossingEvent_Type;
			when FocusIn | FocusOut =>
				xfocus : XFocusChangeEvent_Type;
			when Expose =>
				xexpose : XExposeEvent_Type;
			when GraphicsExpose =>
				xgraphicsexpose : XGraphicsExposeEvent_Type;
			when NoExpose =>
				xnoexpose : XNoExposeEvent_Type;
			when VisibilityNotify =>
				xvisibility : XVisibilityEvent_Type;
			when CreateNotify =>
				xcreatewindow : XCreateWindowEvent_Type;
			when DestroyNotify =>
				xdestroywindow : XDestroyWindowEvent_Type;
			when UnmapNotify =>
				xunmap : XUnmapEvent_Type;
			when MapNotify =>
				xmap : XMapEvent_Type;
			when MapRequest =>
				xmaprequest : XMapRequestEvent_Type;
			when ReparentNotify =>
				xreparent : XReparentEvent_Type;
			when ConfigureNotify =>
				xconfigure : XConfigureEvent_Type;
			when GravityNotify =>
				xgravity : XGravityEvent_Type;
			when ResizeRequest =>
				xresizerequest : XResizeRequestEvent_Type;
			when CirculateNotify =>
				xcirculate : XCirculateEvent_Type;
			when CirculateRequest =>
				xcirculaterequest : XCirculateRequestEvent_Type;
			when PropertyNotify =>
				xproperty : XPropertyEvent_Type;
			when SelectionClear =>
				xselectionclear : XSelectionClearEvent_Type;
			when SelectionRequest =>
				xselectionrequest : XSelectionRequestEvent_Type;
			when SelectionNotify =>
				xselection : XSelectionEvent_Type;
			when ColormapNotify =>
				xcolormap : XColormapEvent_Type;
			when ClientMessage =>
				xclient : XClientMessageEvent_Type;
			when MappingNotify =>
				xmapping : XMappingEvent_Type;
			when KeymapNotify =>
				xkeymap : XKeymapEvent_Type;
			when InvalidEvent =>
				pad  : Pad24_Type;
			when others =>
				xany : XAnyEvent_Type;
		end case;
	end record;
	pragma Unchecked_Union(XEvent_Type);
	pragma Convention(C, XEvent_Type);

	type XEvent_Pointer is access XEvent_Type;

	type XCharStruct_Type is record
		lbearing              : short;
		rbearing              : short;
		width                 : short;
		ascent                : short;
		descent               : short;
		attributes            : unsigned_short;
	end record;
	pragma Convention(C, XCharStruct_Type);

	type XCharStruct_Pointer is access XCharStruct_Type;

	type XFontProp_Type is record
		name                  : Atom_Type;
		card32                : unsigned_long;
	end record;
	pragma Convention(C, XFontProp_Type);

	type XFontProp_Pointer is access XFontProp_Type;

	type XFontStruct_Type is record
		ext_data              : XExtData_Pointer;
		fid                   : Font_Type;
		direction             : unsigned;
		min_char_or_byte2     : unsigned;
		max_char_or_byte2     : unsigned;
		min_byte1             : unsigned;
		max_byte1             : unsigned;
		all_chars_exist       : Bool_Type;
		default_char          : unsigned;
		n_properties          : int;
		properties            : XFontProp_Pointer;
		min_bounds            : XCharStruct_Type;
		max_bounds            : XCharStruct_Type;
		per_char              : XCharStruct_Pointer;
		ascent                : int;
		descent               : int;
	end record;
	pragma Convention(C, XFontStruct_Type);

	type XFontStruct_Pointer is access XFontStruct_Type;

	type XTextItem_Type is record
		chars                 : chars_ptr;
		nchars                : int;
		c_delta               : int;
		font                  : Font_Type;
	end record;
	pragma Convention(C, XTextItem_Type);

	type XChar2b_Type is record
		byte1                 : unsigned_char;
		byte2                 : unsigned_char;
	end record;
	pragma Convention(C, XChar2b_Type);

	type XChar2b_Pointer is access XChar2b_Type;

	type XTextItem16_Type is record
		chars                 : XChar2b_Pointer;
		nchars                : int;
		c_delta               : int;
		font                  : Font_Type;
	end record;
	pragma Convention(C, XTextItem16_Type);

	type XTextProperty is record
		value    : chars_ptr;
		encoding : Atom_Type;
		format   : int;
		nitems   : unsigned_long;
	end record;
	pragma Convention(C, XTextProperty);

	type XTextProperty_Pointer is access XTextProperty;

end Bindings.X11.Types;

