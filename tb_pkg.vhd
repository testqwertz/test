library ieee;
use ieee.math_real.all;
use ieee.std_logic_1164.all;

package tb_pkg is 

	function rand_num return real;
	function rand_sign(sign_val : real) return real;
	function std_logic_to_int(val : std_logic) return integer;

end package tb_pkg;

package body tb_pkg is

	function rand_num return real is
		variable seed1, seed2	: positive;
		variable rand_val	: real;
	begin
		uniform(seed1, seed2, rand_val);
		return rand_val;
	end function;

	function rand_sign(sign_val : real) return real is
		variable sign 	: real;
		variable rand_val	: real;
	begin
		if (sign_val > 0.5) then
			sign := -1.0;
		else
			sign := 1.0;
		end if;

		return sign;
	end function;

	function std_logic_to_int(val : std_logic) return integer is
		variable val_conv	: integer;
	begin
		if val = '1' then
			val_conv := 1;
		else
			val_conv := 0;
		end if;

		return val_conv;
	end;
end package body tb_pkg;
