
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package Bindings.Unix.Types is

   type Pollfd_Type is record
      fd      : int;
      events  : short;
      revents : short;
   end record;
   pragma Convention(C, Pollfd_Type);

   type Pollfd_Pointer is access Pollfd_Type;
   type Pollfd_Array is array(Natural range <>) of Pollfd_Type;

   type Timeval_Type is record
      tv_sec  : long;
      tv_usec : long;
   end record;
   pragma Convention(C, Timeval_Type);

   type Timeval_Pointer is access Timeval_Type;

end Bindings.Unix.Types;

