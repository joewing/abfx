
package body Bindings.Unix.Functions is

	procedure gettimeofday(tp : Timeval_Pointer; vp : chars_ptr);
	pragma Import(C, gettimeofday, "gettimeofday");

	procedure Normalize(t : in out Timeval_Type);

	function Get_Time return Timeval_Type is
		result : aliased Timeval_Type;
	begin
		gettimeofday(result'unrestricted_access, Null_Ptr);
		Normalize(result);
		return result;
	end Get_Time;

	function "="(t1, t2 : Timeval_Type) return Boolean is
		a : Timeval_Type := t1;
		b : Timeval_Type := t2;
	begin
		Normalize(a);
		Normalize(b);
		return (a.tv_sec = b.tv_sec) and (a.tv_usec = b.tv_usec);
	end "=";

	function "<"(t1, t2 : Timeval_Type) return Boolean is
		a : Timeval_Type := t1;
		b : Timeval_Type := t2;
	begin
		Normalize(a);
		Normalize(b);
		if a.tv_sec < b.tv_sec then
			return true;
		elsif a.tv_usec < b.tv_usec then
			return true;
		else
			return false;
		end if;
	end "<";

	function "<="(t1, t2 : Timeval_Type) return Boolean is
	begin
		if t1 < t2 or else t1 = t2 then
			return true;
		else
			return false;
		end if;
	end "<=";

	function ">"(t1, t2 : Timeval_Type) return Boolean is
	begin
		return not (t1 <= t2);
	end ">";

	function ">="(t1, t2 : Timeval_Type) return Boolean is
	begin
		return not (t1 < t2);
	end ">=";

	function "-"(t1, t2 : Timeval_Type) return Timeval_Type is
		result : Timeval_Type;
	begin
		result.tv_sec := t1.tv_sec - t2.tv_sec;
		result.tv_usec := t1.tv_usec - t2.tv_usec;
		Normalize(result);
		return result;
	end "-";

	function "+"(t1, t2 : Timeval_Type) return Timeval_Type is
		result : Timeval_Type;
	begin
		result.tv_sec := t1.tv_sec + t2.tv_sec;
		result.tv_usec := t1.tv_usec + t2.tv_usec;
		Normalize(result);
		return result;
	end "+";

	procedure Normalize(t : in out Timeval_Type) is
	begin
		while t.tv_usec > 1000000 loop
			t.tv_sec := t.tv_sec + 1;
			t.tv_usec := t.tv_usec - 1000000;
		end loop;
		while t.tv_usec < 0 loop
			t.tv_sec := t.tv_sec - 1;
			t.tv_usec := t.tv_usec + 1000000;
		end loop;
	end Normalize;

end Bindings.Unix.Functions;

