
with Ada.Text_IO; use Ada.Text_IO;

package body X11.Logging is

	procedure Debug(msg : in String) is
	begin
		Put_Line(msg);
	end Debug;


end X11.Logging;

