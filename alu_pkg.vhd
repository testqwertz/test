library ieee;
use ieee.math_real.all;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package alu_pkg is 

	constant CMD_USUM	: std_logic_vector(4 - 1 downto 0) := "0000";
	constant CMD_SSUM	: std_logic_vector(4 - 1 downto 0) := "0001";
	constant CMD_USUB	: std_logic_vector(4 - 1 downto 0) := "0010";
	constant CMD_SSUB	: std_logic_vector(4 - 1 downto 0) := "0011";
	constant CMD_UCMP	: std_logic_vector(4 - 1 downto 0) := "0100";
	constant CMD_SCMP	: std_logic_vector(4 - 1 downto 0) := "0101";
	constant CMD_AND	: std_logic_vector(4 - 1 downto 0) := "0110";
	constant CMD_OR		: std_logic_vector(4 - 1 downto 0) := "0111";
	constant CMD_XOR	: std_logic_vector(4 - 1 downto 0) := "1000";
	constant CMD_NOT	: std_logic_vector(4 - 1 downto 0) := "1001";
	constant CMD_SHIFT	: std_logic_vector(4 - 1 downto 0) := "1010";

	function calc_length_multiplier (op1_l, op2_l, base : integer; multiplicand : integer) return integer;
	function sel_multiplicand (op1_l, op2_l : integer) return integer;
	function count_length(op2_l : integer) return integer;

	component mul
	generic (
		OP1_L	: positive := 16;
		OP2_L	: positive := 16
	);
	port (
		rst	: in std_logic;
		clk	: in std_logic;
		Op1	: in std_logic_vector(OP1_L - 1 downto 0);
		Op2	: in std_logic_vector(OP2_L - 1 downto 0);
		Start	: in std_logic;
		Done	: out std_logic;
		Res	: out std_logic_vector(OP1_L+OP2_L-1 downto 0)
	);
	end component;

	component div
	generic (
		OP1_L	: positive := 16;
		OP2_L	: positive := 16
	);
	port (
		rst		: in std_logic;
		clk		: in std_logic;
		Dividend	: in std_logic_vector(OP1_L - 1 downto 0);
		Divisor		: in std_logic_vector(OP2_L - 1 downto 0);
		Start		: in std_logic;
		Done		: out std_logic;
		Quotient	: out std_logic_vector(OP1_L-1 downto 0);
		Remainder	: out std_logic_vector(OP2_L - 1 downto 0)
	);
	end component;

	component alu
	generic (
		OP1_L	: positive := 16;
		OP2_L	: positive := 16;
		CMD_L	: positive := 4
	);
	port (
		rst	: in std_logic;
		clk	: in std_logic;
		Op1	: in std_logic_vector(OP1_L - 1 downto 0);
		Op2	: in std_logic_vector(OP2_L - 1 downto 0);
		Cmd	: in std_logic_vector(CMD_L - 1 downto 0);
		Start	: in std_logic;
		Done	: out std_logic;
		Ovfl	: out std_logic;
		Unfl	: out std_logic;
		UnCmd	: out std_logic;
		Res	: out std_logic_vector(OP1_L-1 downto 0)
	);
	end component;

end package alu_pkg;

package body alu_pkg is

	function calc_length_multiplier (op1_l, op2_l, base : integer; multiplicand : integer) return integer is
		variable multiplier, rem_multiplier	: integer;
		variable length_multiplier	: integer;
		variable length_op		: integer;
	begin
		if (op1_l = multiplicand) then
			multiplier := op2_l;
		elsif (op2_l = multiplicand) then
			multiplier := op1_l;
		else
			report "Error: cannot determine selected input" severity error;
		end if;

		rem_multiplier := (multiplier mod base);

		assert rem_multiplier = 0 report ("op length must be a multiple of " & integer'image(base) & ". It is " & integer'image(multiplier) & ". Input will be extended") severity warning;

		if (rem_multiplier = 0) then
			length_op := multiplier;
		else
			length_op := multiplier - rem_multiplier + base;
		end if;

		return length_op;

	end;

	function sel_multiplicand (op1_l, op2_l : integer) return integer is
		variable rem_op	: integer;
		variable multiplicand	: integer;
	begin
		if (op1_l <= op2_l) then
			multiplicand := op1_l;
		else
			multiplicand := op2_l;
		end if;

		return multiplicand;
	end;

	function count_length(op2_l : integer) return integer is
		variable nbit, tmp	: integer;
	begin
		tmp := integer(ceil(log2(real(op2_l))));
		if (2**tmp = op2_l) then
			nbit := tmp + 1;
		else
			nbit := tmp;
		end if;

		return nbit;
	end;
end package body alu_pkg;
