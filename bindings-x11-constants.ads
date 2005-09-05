
package Bindings.X11.Constants is

	X_PROTOCOL                  : constant := 11;
	X_PROTOCOL_REVISION         : constant := 0;

	XA_PRIMARY                  : constant := 1;
	XA_SECONDARY                : constant := 2;
	XA_ARC                      : constant := 3;
	XA_ATOM                     : constant := 4;
	XA_BITMAP                   : constant := 5;
	XA_CARDINAL                 : constant := 6;
	XA_COLORMAP                 : constant := 7;
	XA_CURSOR                   : constant := 8;
	XA_CUT_BUFFER0              : constant := 9;
	XA_CUT_BUFFER1              : constant := 10;
	XA_CUT_BUFFER2              : constant := 11;
	XA_CUT_BUFFER3              : constant := 12;
	XA_CUT_BUFFER4              : constant := 13;
	XA_CUT_BUFFER5              : constant := 14;
	XA_CUT_BUFFER6              : constant := 15;
	XA_CUT_BUFFER7              : constant := 16;
	XA_DRAWABLE                 : constant := 17;
	XA_FONT                     : constant := 18;
	XA_INTEGER                  : constant := 19;
	XA_PIXMAP                   : constant := 20;
	XA_POINT                    : constant := 21;
	XA_RECTANGLE                : constant := 22;
	XA_RESOURCE_MANAGER         : constant := 23;
	XA_RGB_COLOR_MAP            : constant := 24;
	XA_RGB_BEST_MAP             : constant := 25;
	XA_RGB_BLUE_MAP             : constant := 26;
	XA_RGB_DEFAULT_MAP          : constant := 27;
	XA_RGB_GRAY_MAP             : constant := 28;
	XA_RGB_GREEN_MAP            : constant := 29;
	XA_RGB_RED_MAP              : constant := 30;
	XA_STRING                   : constant := 31;
	XA_VISUALID                 : constant := 32;
	XA_WINDOW                   : constant := 33;
	XA_WM_COMMAND               : constant := 34;
	XA_WM_HINTS                 : constant := 35;
	XA_WM_CLIENT_MACHINE        : constant := 36;
	XA_WM_ICON_NAME             : constant := 37;
	XA_WM_ICON_SIZE             : constant := 38;
	XA_WM_NAME                  : constant := 39;
	XA_WM_NORMAL_HINTS          : constant := 40;
	XA_WM_SIZE_HINTS            : constant := 41;
	XA_WM_ZOOM_HINTS            : constant := 42;
	XA_MIN_SPACE                : constant := 43;
	XA_NORM_SPACE               : constant := 44;
	XA_MAX_SPACE                : constant := 45;
	XA_END_SPACE                : constant := 46;
	XA_SUPERSCRIPT_X            : constant := 47;
	XA_SUPERSCRIPT_Y            : constant := 48;
	XA_SUBSCRIPT_X              : constant := 49;
	XA_SUBSCRIPT_Y              : constant := 50;
	XA_UNDERLINE_POSITION       : constant := 51;
	XA_UNDERLINE_THICKNESS      : constant := 52;
	XA_STRIKEOUT_ASCENT         : constant := 53;
	XA_STRIKEOUT_DESCENT        : constant := 54;
	XA_ITALIC_ANGLE             : constant := 55;
	XA_X_HEIGHT                 : constant := 56;
	XA_QUAD_WIDTH               : constant := 57;
	XA_WEIGHT                   : constant := 58;
	XA_POINT_SIZE               : constant := 59;
	XA_RESOLUTION               : constant := 60;
	XA_COPYRIGHT                : constant := 61;
	XA_NOTICE                   : constant := 62;
	XA_FONT_NAME                : constant := 63;
	XA_FAMILY_NAME              : constant := 64;
	XA_FULL_NAME                : constant := 65;
	XA_CAP_HEIGHT               : constant := 66;
	XA_WM_CLASS                 : constant := 67;
	XA_WM_TRANSIENT_FOR         : constant := 68;
	XA_LAST_PREDEFINED          : constant := 68;

	None                        : constant := 0;
	ParentRelative              : constant := 1;
	CopyFromParent              : constant := 0;
	PointerWindow               : constant := 0;
	InputFocus                  : constant := 1;
	PointerRoot                 : constant := 1;
	AnyPropertyType             : constant := 0;
	AnyKey                      : constant := 0;
	AnyButton                   : constant := 0;
	AllTemporary                : constant := 0;
	CurrentTime                 : constant := 0;
	NoSymbol                    : constant := 0;

	NoEventMask                 : constant := 0;
	KeyPressMask                : constant := 2 ** 0;
	KeyReleaseMask              : constant := 2 ** 1;
	ButtonPressMask             : constant := 2 ** 2;
	ButtonReleaseMask           : constant := 2 ** 3;
	EnterWindowMask             : constant := 2 ** 4;
	LeaveWindowMask             : constant := 2 ** 5;
	PointerMotionMask           : constant := 2 ** 6;
	PointerMotionHintMask       : constant := 2 ** 7;
	Button1MotionMask           : constant := 2 ** 8;
	Button2MotionMask           : constant := 2 ** 9;
	Button3MotionMask           : constant := 2 ** 10;
	Button4MotionMask           : constant := 2 ** 11;
	Button5MotionMask           : constant := 2 ** 12;
	ButtonMotionMask            : constant := 2 ** 13;
	KeymapStateMask             : constant := 2 ** 14;
	ExposureMask                : constant := 2 ** 15;
	VisibilityChangeMask        : constant := 2 ** 16;
	StructureNotifyMask         : constant := 2 ** 17;
	ResizeRedirectMask          : constant := 2 ** 18;
	SubstructureNotifyMask      : constant := 2 ** 19;
	SubstructureRedirectMask    : constant := 2 ** 20;
	FocusChangeMask             : constant := 2 ** 21;
	PropertyChangeMask          : constant := 2 ** 22;
	ColormapChangeMask          : constant := 2 ** 23;
	OwnerGrabButtonMask         : constant := 2 ** 24;

	KeyPress                    : constant := 2;
	KeyRelease                  : constant := 3;
	ButtonPress                 : constant := 4;
	ButtonRelease               : constant := 5;
	MotionNotify                : constant := 6;
	EnterNotify                 : constant := 7;
	LeaveNotify                 : constant := 8;
	FocusIn                     : constant := 9;
	FocusOut                    : constant := 10;
	KeymapNotify                : constant := 11;
	Expose                      : constant := 12;
	GraphicsExpose              : constant := 13;
	NoExpose                    : constant := 14;
	VisibilityNotify            : constant := 15;
	CreateNotify                : constant := 16;
	DestroyNotify               : constant := 17;
	UnmapNotify                 : constant := 18;
	MapNotify                   : constant := 19;
	MapRequest                  : constant := 20;
	ReparentNotify              : constant := 21;
	ConfigureNotify             : constant := 22;
	ConfigureRequest            : constant := 23;
	GravityNotify               : constant := 24;
	ResizeRequest               : constant := 25;
	CirculateNotify             : constant := 26;
	CirculateRequest            : constant := 27;
	PropertyNotify              : constant := 28;
	SelectionClear              : constant := 29;
	SelectionRequest            : constant := 30;
	SelectionNotify             : constant := 31;
	ColormapNotify              : constant := 32;
	ClientMessage               : constant := 33;
	MappingNotify               : constant := 34;
	InvalidEvent                : constant := 35;

	ShiftMask                   : constant := 2 ** 0;
	LockMask                    : constant := 2 ** 1;
	ControlMask                 : constant := 2 ** 2;
	Mod1Mask                    : constant := 2 ** 3;
	Mod2Mask                    : constant := 2 ** 4;
	Mod3Mask                    : constant := 2 ** 5;
	Mod4Mask                    : constant := 2 ** 6;
	Mod5Mask                    : constant := 2 ** 7;

	ShiftMapIndex               : constant := 0;
	LockMapIndex                : constant := 1;
	ControlMapIndex             : constant := 2;
	Mod1MapIndex                : constant := 3;
	Mod2MapIndex                : constant := 4;
	Mod3MapIndex                : constant := 5;
	Mod4MapIndex                : constant := 6;
	Mod5MapIndex                : constant := 7;

	Button1Mask                 : constant := 2 ** 8;
	Button2Mask                 : constant := 2 ** 9;
	Button3Mask                 : constant := 2 ** 10;
	Button4Mask                 : constant := 2 ** 11;
	Button5Mask                 : constant := 2 ** 12;

	AnyModifier                 : constant := 2 ** 15;

	Button1                     : constant := 1;
	Button2                     : constant := 2;
	Button3                     : constant := 3;
	Button4                     : constant := 4;
	Button5                     : constant := 5;

	NotifyNormal                : constant := 0;
	NotifyGrab                  : constant := 1;
	NotifyUngrab                : constant := 2;
	NotifyWhileGrabbed          : constant := 3;

	NotifyHint                  : constant := 1;

	NotifyAncestor              : constant := 0;
	NotifyVirtual               : constant := 1;
	NotifyInferior              : constant := 2;
	NotifyNonlinear             : constant := 3;
	NotifyNonlinearVirtual      : constant := 4;
	NotifyPointer               : constant := 5;
	NotifyPointerRoot           : constant := 6;
	NotifyDetailNone            : constant := 7;

	VisibilityUnobscured        : constant := 0;
	VisibilityPartiallyObscured : constant := 1;
	VisibilityFullyObscured     : constant := 2;

	PlaceOnTop                  : constant := 0;
	PlaceOnBottom               : constant := 1;

	FamilyInternet              : constant := 0;
	FamilyDECnet                : constant := 1;
	FamilyChaos                 : constant := 2;

	PropertyNewValue            : constant := 0;
	PropertyDelete              : constant := 1;

	ColormapUninstalled         : constant := 0;
	ColormapInstalled           : constant := 1;

	GrabModeSync                : constant := 0;
	GrabModeAsync               : constant := 1;

	GrabSuccess                 : constant := 0;
	AlreadyGrabbed              : constant := 1;
	GrabInvalidTime             : constant := 2;
	GrabNotViewable             : constant := 3;
	GrabFrozen                  : constant := 4;

	AsyncPointer                : constant := 0;
	SyncPointer                 : constant := 1;
	ReplayPointer               : constant := 2;
	AsyncKeyboard               : constant := 3;
	SyncKeyboard                : constant := 4;
	ReplayKeyboard              : constant := 5;
	AsyncBoth                   : constant := 6;
	SyncBoth                    : constant := 7;

	RevertToNone                : constant := None;
	RevertToPointerRoot         : constant := PointerRoot;
	RevertToParent              : constant := 2;

	Success                     : constant := 0;
	BadRequest                  : constant := 1;
	BadValue                    : constant := 2;
	BadWindow                   : constant := 3;
	BadPixmap                   : constant := 4;
	BadAtom                     : constant := 5;
	BadCursor                   : constant := 6;
	BadFont                     : constant := 7;
	BadMatch                    : constant := 8;
	BadDrawable                 : constant := 9;
	BadAccess                   : constant := 10;
	BadAlloc                    : constant := 11;
	BadColor                    : constant := 12;
	BadGC                       : constant := 13;
	BadIDChoice                 : constant := 14;
	BadName                     : constant := 15;
	BadLength                   : constant := 16;
	BadImplementation           : constant := 17;

	FirstExtensionError         : constant := 128;
	LastExtensionError          : constant := 255;

	InputOutput                 : constant := 1;
	InputOnly                   : constant := 2;

	CWBackPixmap                : constant := 2 ** 0;
	CWBackPixel                 : constant := 2 ** 1;
	CWBorderPixmap              : constant := 2 ** 2;
	CWBorderPixel               : constant := 2 ** 3;
	CWBitGravity                : constant := 2 ** 4;
	CWWinGravity                : constant := 2 ** 5;
	CWBackingStore              : constant := 2 ** 6;
	CWBackingPlanes             : constant := 2 ** 7;
	CWBackingPixel              : constant := 2 ** 8;
	CWOverrideRedirect          : constant := 2 ** 9;
	CWSaveUnder                 : constant := 2 ** 10;
	CWEventMask                 : constant := 2 ** 11;
	CWDontPropagate             : constant := 2 ** 12;
	CWColormap                  : constant := 2 ** 13;
	CWCursor                    : constant := 2 ** 14;

	CWX                         : constant := 2 ** 0;
	CWY                         : constant := 2 ** 1;
	CWWidth                     : constant := 2 ** 2;
	CWHeight                    : constant := 2 ** 3;
	CWBorderWidth               : constant := 2 ** 4;
	CWSibling                   : constant := 2 ** 5;
	CWStackMode                 : constant := 2 ** 6;

	ForgetGravity               : constant := 0;
	NorthWestGravity            : constant := 1;
	NorthGravity                : constant := 2;
	NorthEastGravity            : constant := 3;
	WestGravity                 : constant := 4;
	CenterGravity               : constant := 5;
	EastGravity                 : constant := 6;
	SouthWestGravity            : constant := 7;
	SouthGravity                : constant := 8;
	SouthEastGravity            : constant := 9;
	StaticGravity               : constant := 10;

	UnmapGravity                : constant := 0;

	NotUseful                   : constant := 0;
	WhenMapped                  : constant := 1;
	Always                      : constant := 2;

	IsUnmapped                  : constant := 0;
	IsUnviewable                : constant := 1;
	IsViewable                  : constant := 2;

	SetModeInsert               : constant := 0;
	SetModeDelete               : constant := 1;

	DestroyAll                  : constant := 0;
	RetainPermanent             : constant := 1;
	RetainTemporary             : constant := 2;

	Above                       : constant := 0;
	Below                       : constant := 1;
	TopIf                       : constant := 2;
	BottomIf                    : constant := 3;
	Opposite                    : constant := 4;

	RaiseLowest                 : constant := 0;
	LowerHighest                : constant := 1;

	PropModeReplace             : constant := 0;
	PropModePrepend             : constant := 1;
	PropModeAppend              : constant := 2;

	GXclear                     : constant := 0;
	GXand                       : constant := 1;
	GXandReverse                : constant := 2;
	GXcopy                      : constant := 3;
	GXandInverted               : constant := 4;
	GXnoop                      : constant := 5;
	GXxor                       : constant := 6;
	GXor                        : constant := 7;
	GXnor                       : constant := 8;
	GXequiv                     : constant := 9;
	GXinvert                    : constant := 10;
	GXorReverse                 : constant := 11;
	GXcopyInverted              : constant := 12;
	GXorInverted                : constant := 13;
	GXnand                      : constant := 14;
	GXset                       : constant := 15;

	LineSolid                   : constant := 0;
	LineOnOffDash               : constant := 1;
	LineDoubleDash              : constant := 2;

	CapNotLast                  : constant := 0;
	CapButt                     : constant := 1;
	CapRound                    : constant := 2;
	CapProjecting               : constant := 3;

	JoinMiter                   : constant := 0;
	JoinRound                   : constant := 1;
	JoinBevel                   : constant := 2;

	FillSolid                   : constant := 0;
	FillTiled                   : constant := 1;
	FillStippled                : constant := 2;
	FillOpaqueStippled          : constant := 3;

	EvenOddRule                 : constant := 0;
	WindingRule                 : constant := 1;

	ClipByChildren              : constant := 0;
	IncludeInferiors            : constant := 1;

	Unsorted                    : constant := 0;
	YSorted                     : constant := 1;
	YXSorted                    : constant := 2;
	YXBanded                    : constant := 3;

	CoordModeOrigin             : constant := 0;
	CoordModePrevious           : constant := 1;

	Complex                     : constant := 0;
	Nonconvex                   : constant := 1;
	Convex                      : constant := 2;

	ArcChord                    : constant := 0;
	ArcPieSlice                 : constant := 1;

	GCFunction                  : constant := 2 ** 0;
	GCPlaneMask                 : constant := 2 ** 1;
	GCForeground                : constant := 2 ** 2;
	GCBackground                : constant := 2 ** 3;
	GCLineWidth                 : constant := 2 ** 4;
	GCLineStyle                 : constant := 2 ** 5;
	GCCapStyle                  : constant := 2 ** 6;
	GCJoinStyle                 : constant := 2 ** 7;
	GCFillStyle                 : constant := 2 ** 8;
	GCFillRule                  : constant := 2 ** 9;
	GCTile                      : constant := 2 ** 10;
	GCStipple                   : constant := 2 ** 11;
	GCTileStipXOrigin           : constant := 2 ** 12;
	GCTileStipYOrigin           : constant := 2 ** 13;
	GCFont                      : constant := 2 ** 14;
	GCSubwindowMode             : constant := 2 ** 15;
	GCGraphicsExposures         : constant := 2 ** 16;
	GCClipXOrigin               : constant := 2 ** 17;
	GCClipYOrigin               : constant := 2 ** 18;
	GCClipMask                  : constant := 2 ** 19;
	GCDashOffset                : constant := 2 ** 20;
	GCDashList                  : constant := 2 ** 21;
	GCArcMode                   : constant := 2 ** 22;
	GCLastBit                   : constant := 22;

	FontLeftToRight             : constant := 0;
	FontRightToLeft             : constant := 1;
	FontChange                  : constant := 255;

	XYBitmap                    : constant := 0;
	XYPixmap                    : constant := 1;
	ZPixmap                     : constant := 2;

	AllocNone                   : constant := 0;
	AllocAll                    : constant := 1;

	DoRed                       : constant := 2 ** 0;
	DoGreen                     : constant := 2 ** 1;
	DoBlue                      : constant := 2 ** 2;

	CursorShape                 : constant := 0;
	TileShape                   : constant := 1;
	StippleShape                : constant := 2;

	AutoRepeatModeOff           : constant := 0;
	AutoRepeatModeOn            : constant := 1;
	AutoRepeatModeDefault       : constant := 2;

	LedModeOff                  : constant := 0;
	LedModeOn                   : constant := 1;

	KBKeyClickPercent           : constant := 2 ** 0;
	KBBellPercent               : constant := 2 ** 1;
	KBBellPitch                 : constant := 2 ** 2;
	KBBellDuration              : constant := 2 ** 3;
	KBLed                       : constant := 2 ** 4;
	KBLedMode                   : constant := 2 ** 5;
	KBKey                       : constant := 2 ** 6;
	KBAutoRepeatMode            : constant := 2 ** 7;

	MappingSuccess              : constant := 0;
	MappingBusy                 : constant := 1;
	MappingFailed               : constant := 2;

	MappingModifier             : constant := 0;
	MappingKeyboard             : constant := 1;
	MappingPointer              : constant := 2;

	DontPreferBlanking          : constant := 0;
	PreferBlanking              : constant := 1;
	DefaultBlanking             : constant := 2;

	DisableScreenSaver          : constant := 0;
	DisableScreenInterval       : constant := 0;

	DontAllowExposures          : constant := 0;
	AllowExposures              : constant := 1;
	DefaultExposures            : constant := 2;

	ScreenSaverReset            : constant := 0;
	ScreenSaverActive           : constant := 1;

	HostInsert                  : constant := 0;
	HostDelete                  : constant := 1;

	EnableAccess                : constant := 1;
	DisableAccess               : constant := 0;

	StaticGray                  : constant := 0;
	GrayScale                   : constant := 1;
	StaticColor                 : constant := 2;
	PseudoColor                 : constant := 3;
	TrueColor                   : constant := 4;
	DirectColor                 : constant := 5;

	LSBFirst                    : constant := 0;
	MSBFirst                    : constant := 1;

end Bindings.X11.Constants;

