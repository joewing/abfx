
package Bindings.Unix.Constants is

   POLLIN     : constant := 16#0001#;
   POLLPRI    : constant := 16#0002#;
   POLLOUT    : constant := 16#0004#;
   POLLRDNORM : constant := 16#0040#;
   POLLWRNORM : constant := POLLOUT;
   POLLRDBAND : constant := 16#0000#;
   POLLWRBAND : constant := 16#0100#;
   POLLNORM   : constant := POLLRDNORM;

   POLLERR    : constant := 16#0008#;
   POLLHUP    : constant := 16#0010#;
   POLLNVAL   : constant := 16#0020#;
   POLLREMOVE : constant := 16#0800#;

end Bindings.Unix.Constants;

