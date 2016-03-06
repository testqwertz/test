library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

library work;
use work.alu_pkg.all;
use work.tb_pkg.all;

entity alu_tb is
end entity alu_tb;

architecture bench of alu_tb is

	constant CLK_PERIOD	: time := 10 ns;
	constant NUM_TEST	: integer := 1000;

	signal clk_tb	: std_logic := '0';
	signal stop	: boolean := false;
	signal rst_tb	: std_logic;

	constant OP1_L_TB	: integer := 16;
	constant OP2_L_TB	: integer := 16;
	constant CMD_L_TB	: integer := 4;

	signal Op1_tb	: std_logic_vector(OP1_L_TB - 1 downto 0);
	signal Op2_tb	: std_logic_vector(OP2_L_TB - 1 downto 0);
	signal Cmd_tb	: std_logic_vector(CMD_L_TB - 1 downto 0);

	signal Ovfl_tb	: std_logic;
	signal Unfl_tb	: std_logic;
	signal UnCmd_tb	: std_logic;

	signal Start_tb	: std_logic;
	signal Done_tb	: std_logic;

	signal Res_tb	: std_logic_vector(OP1_L_TB - 1 downto 0);

begin

	DUT: alu generic map(
		OP1_L => OP1_L_TB,
		OP2_L => OP2_L_TB,
		CMD_L => CMD_L_TB
	)
	port map (
		rst => rst_tb,
		clk => clk_tb,
		Op1 => Op1_tb,
		Op2 => Op2_tb,
		Cmd => Cmd_tb,
		Start => Start_tb,
		Done => Done_tb,
		Ovfl => Ovfl_tb,
		Unfl => Unfl_tb,
		UnCmd => UnCmd_tb,
		Res => Res_tb
	);

	clk_tb <= not clk_tb after CLK_PERIOD/2 when not stop;

	test: process

		procedure reset is
		begin
			rst_tb <= '0';
			wait until rising_edge(clk_tb);
			rst_tb <= '1';
			Op1_tb <= (others => '0');
			Op2_tb <= (others => '0');
			Start_tb <= '0';
			wait until rising_edge(clk_tb);
			rst_tb <= '0';
		end procedure reset;

		procedure push_op(variable Op1_int : out integer; variable Op2_int: out integer; variable Cmd: out std_logic_vector(CMD_L_TB-1 downto 0); variable seed1, seed2: inout positive) is
			variable Op1_in, Op2_in, Cmd_in	: integer;
			variable rand_val, sign_val	: real;
			variable Cmd_int: std_logic_vector(CMD_L_TB-1 downto 0);
		begin
			uniform(seed1, seed2, rand_val);
			Cmd_in := integer(rand_val*(2.0**(real(CMD_L_TB)) - 1.0));

			Cmd_tb <= std_logic_vector(to_unsigned(Cmd_in, CMD_L_TB));
			Cmd := std_logic_vector(to_unsigned(Cmd_in, CMD_L_TB));
			Cmd_int := std_logic_vector(to_unsigned(Cmd_in, CMD_L_TB));

			if (Cmd_int = CMD_SSUM) or (Cmd_int = CMD_SSUB) or (Cmd_int = CMD_SCMP) then
				uniform(seed1, seed2, rand_val);
				uniform(seed1, seed2, sign_val);
				Op1_in := integer(rand_sign(sign_val)*rand_val*(2.0**(real(OP1_L_TB) - 1.0) - 1.0));
				uniform(seed1, seed2, rand_val);
				uniform(seed1, seed2, sign_val);
				Op2_in := integer(rand_sign(sign_val)*rand_val*(2.0**(real(OP2_L_TB) - 1.0) - 1.0));

				Op1_tb <= std_logic_vector(to_signed(Op1_in, OP1_L_TB));
				Op2_tb <= std_logic_vector(to_signed(Op2_in, OP2_L_TB));
			else
				uniform(seed1, seed2, rand_val);
				Op1_in := integer(rand_val*(2.0**(real(OP1_L_TB)) - 1.0));
				uniform(seed1, seed2, rand_val);
				Op2_in := integer(rand_val*(2.0**(real(OP2_L_TB)) - 1.0));

				Op1_tb <= std_logic_vector(to_unsigned(Op1_in, OP1_L_TB));
				Op2_tb <= std_logic_vector(to_unsigned(Op2_in, OP2_L_TB));
			end if;

			Op1_int := Op1_in;
			Op2_int := Op2_in;

			Start_tb <= '1';

			wait until rising_edge(clk_tb);
			Start_tb <= '0';
		end procedure push_op;

		procedure reference(variable Op1_int : in integer; variable Op2_int: in integer; variable Cmd: in std_logic_vector(CMD_L_TB-1 downto 0); variable Res_ideal: out integer; variable Ovfl_ideal : out integer; variable Unfl_ideal : out integer) is
			variable tmp_op1	: std_logic_vector(OP1_L_TB-1 downto 0);
			variable tmp_op2	: std_logic_vector(OP2_L_TB-1 downto 0);
			variable tmp_res	: std_logic_vector(OP1_L_TB-1 downto 0);
			variable rand_val, sign_val	: real;
			variable Res_tmp	: integer;
		begin
			Ovfl_ideal := 0;
			Unfl_ideal := 0;
			case Cmd is
				when CMD_USUM =>
					Res_tmp := Op1_int + Op2_int;
					Res_ideal := Res_tmp;
					if (Res_tmp > (2**(OP1_L_TB) - 1)) then
						Ovfl_ideal := 1;
						Res_ideal := Res_tmp - 2**(OP1_L_TB);
					end if;
				when CMD_SSUM =>
					Res_tmp := Op1_int + Op2_int;
					Res_ideal := Res_tmp;
					if (Res_tmp > (2**(OP1_L_TB-1) - 1)) then
						Ovfl_ideal := 1;
						Res_ideal := -2*(2**(OP1_L_TB-1)) + Res_tmp;
					elsif (Res_tmp < (-(2**(OP1_L_TB-1)))) then
						Unfl_ideal := 1;
						Res_ideal := 2*(2**(OP1_L_TB-1)) + Res_tmp;
					end if;
				when CMD_USUB =>
					Res_tmp := Op1_int - Op2_int;
					Res_ideal := Res_tmp;
					if (Res_tmp < 0) then
						Unfl_ideal := 1;
						Res_ideal := 2**(OP1_L_TB) + Res_tmp;
					end if;
				when CMD_SSUB =>
					Res_tmp := Op1_int - Op2_int;
					Res_ideal := Res_tmp;
					if (Res_tmp > (2**(OP1_L_TB-1) - 1)) then
						Ovfl_ideal := 1;
						Res_ideal := -2*(2**(OP1_L_TB-1)) + Res_tmp;
					elsif (Res_tmp < (-(2**(OP1_L_TB-1)))) then
						Unfl_ideal := 1;
						Res_ideal := 2*(2**(OP1_L_TB-1)) + Res_tmp;
					end if;
				when CMD_UCMP =>
					if (Op1_int = Op2_int) then
						Res_ideal := 0;
					elsif (Op1_int > Op2_int) then
						Res_ideal := 1;
					else
						Res_ideal := (2**(OP1_L_TB)) - 1;
					end if;
				when CMD_SCMP =>
					if (Op1_int = Op2_int) then
						Res_ideal := 0;
					elsif (Op1_int > Op2_int) then
						Res_ideal := 1;
					else
						Res_ideal := - 1;
					end if;
				when CMD_AND =>
					tmp_op1 := std_logic_vector(to_unsigned(Op1_int, OP1_L_TB));
					tmp_op2 := std_logic_vector(to_unsigned(Op2_int, OP2_L_TB));
					for i in 0 to OP1_L_TB-1 loop
						tmp_res(i) := tmp_op1(i) and tmp_op2(i);
					end loop;
					Res_ideal := to_integer(unsigned(tmp_res));
				when CMD_OR =>
					tmp_op1 := std_logic_vector(to_unsigned(Op1_int, OP1_L_TB));
					tmp_op2 := std_logic_vector(to_unsigned(Op2_int, OP2_L_TB));
					for i in 0 to OP1_L_TB-1 loop
						tmp_res(i) := tmp_op1(i) or tmp_op2(i);
					end loop;
					Res_ideal := to_integer(unsigned(tmp_res));
				when CMD_XOR =>
					tmp_op1 := std_logic_vector(to_unsigned(Op1_int, OP1_L_TB));
					tmp_op2 := std_logic_vector(to_unsigned(Op2_int, OP2_L_TB));
					for i in 0 to OP1_L_TB-1 loop
						tmp_res(i) := tmp_op1(i) xor tmp_op2(i);
					end loop;
					Res_ideal := to_integer(unsigned(tmp_res));
				when CMD_NOT =>
					tmp_op1 := std_logic_vector(to_unsigned(Op1_int, OP1_L_TB));
					for i in 0 to OP1_L_TB-1 loop
						tmp_res(i) := not tmp_op1(i);
					end loop;
					Res_ideal := to_integer(unsigned(tmp_res));
				when others =>
					Res_ideal := 0;
			end case;
		end procedure reference;

		procedure std_vect_to_txt(variable Cmd: in std_logic_vector(CMD_L_TB-1 downto 0); Cmd_txt : out string) is
		begin
			case Cmd is
				when CMD_USUM =>
					Cmd_txt := "USUM";
				when CMD_SSUM =>
					Cmd_txt := "SSUM";
				when CMD_USUB =>
					Cmd_txt := "USUB";
				when CMD_SSUB =>
					Cmd_txt := "SSUB";
				when CMD_UCMP =>
					Cmd_txt := "UCMP";
				when CMD_SCMP =>
					Cmd_txt := "SCMP";
				when CMD_AND =>
					Cmd_txt := "BAND";
				when CMD_OR =>
					Cmd_txt := "B_OR";
				when CMD_XOR =>
					Cmd_txt := "BXOR";
				when CMD_NOT =>
					Cmd_txt := "BNOT";
				when others =>
					Cmd_txt := "UCMD";
			end case;
		end procedure std_vect_to_txt;

		procedure verify(variable Op1_int, Op2_int, Res_ideal, Res_rtl, Ovfl_ideal, Ovfl_rtl, Unfl_ideal, Unfl_rtl: in integer; Cmd_txt : in string; variable pass: out integer) is
		begin
			if (Res_rtl = Res_ideal) and (Ovfl_ideal = Ovfl_rtl) and (Unfl_ideal = Unfl_rtl) then
				report "ALU operation " & Cmd_txt & " of " & integer'image(Op1_int) & " and " & integer'image(Op2_int) & " gives: RTL Result:" & integer'image(Res_rtl) & ", overflow:" & integer'image(Ovfl_rtl) & ", underflow:" & integer'image(Unfl_rtl) & " and reference: Result " & integer'image(Res_ideal) & ", overflow:" & integer'image(Ovfl_ideal) & ", underflow:" & integer'image(Unfl_ideal) & ": PASSED";
				pass := 1;
			elsif (Ovfl_ideal = Ovfl_rtl) and (Unfl_ideal = Unfl_rtl) then
				report "ALU operation " & Cmd_txt & " of " & integer'image(Op1_int) & " and " & integer'image(Op2_int) & " gives: RTL Result:" & integer'image(Res_rtl) & ", overflow:" & integer'image(Ovfl_rtl) & ", underflow:" & integer'image(Unfl_rtl) & " and reference: Result " & integer'image(Res_ideal) & ", overflow:" & integer'image(Ovfl_ideal) & ", underflow:" & integer'image(Unfl_ideal) & ": FAILED (Result)" severity warning;
				pass := 0;
			elsif (Res_ideal = Res_rtl) and (Unfl_ideal = Unfl_rtl) then
				report "ALU operation " & Cmd_txt & " of " & integer'image(Op1_int) & " and " & integer'image(Op2_int) & " gives: RTL Result:" & integer'image(Res_rtl) & ", overflow:" & integer'image(Ovfl_rtl) & ", underflow:" & integer'image(Unfl_rtl) & " and reference: Result " & integer'image(Res_ideal) & ", overflow:" & integer'image(Ovfl_ideal) & ", underflow:" & integer'image(Unfl_ideal) & ": FAILED (Overflow)" severity warning;
				pass := 0;
			elsif (Ovfl_ideal = Ovfl_rtl) and (Res_ideal = Res_rtl) then
				report "ALU operation " & Cmd_txt & " of " & integer'image(Op1_int) & " and " & integer'image(Op2_int) & " gives: RTL Result:" & integer'image(Res_rtl) & ", overflow:" & integer'image(Ovfl_rtl) & ", underflow:" & integer'image(Unfl_rtl) & " and reference: Result " & integer'image(Res_ideal) & ", overflow:" & integer'image(Ovfl_ideal) & ", underflow:" & integer'image(Unfl_ideal) & ": FAILED (Underflow)" severity warning;
				pass := 0;
			elsif (Ovfl_ideal = Ovfl_rtl) then
				report "ALU operation " & Cmd_txt & " of " & integer'image(Op1_int) & " and " & integer'image(Op2_int) & " gives: RTL Result:" & integer'image(Res_rtl) & ", overflow:" & integer'image(Ovfl_rtl) & ", underflow:" & integer'image(Unfl_rtl) & " and reference: Result " & integer'image(Res_ideal) & ", overflow:" & integer'image(Ovfl_ideal) & ", underflow:" & integer'image(Unfl_ideal) & ": FAILED (Result and underflow)" severity warning;
				pass := 0;
			elsif (Unfl_ideal = Unfl_rtl) then
				report "ALU operation " & Cmd_txt & " of " & integer'image(Op1_int) & " and " & integer'image(Op2_int) & " gives: RTL Result:" & integer'image(Res_rtl) & ", overflow:" & integer'image(Ovfl_rtl) & ", underflow:" & integer'image(Unfl_rtl) & " and reference: Result " & integer'image(Res_ideal) & ", overflow:" & integer'image(Ovfl_ideal) & ", underflow:" & integer'image(Unfl_ideal) & ": FAILED (Result and overflow)" severity warning;
				pass := 0;
			elsif (Res_ideal = Res_rtl) then
				report "ALU operation " & Cmd_txt & " of " & integer'image(Op1_int) & " and " & integer'image(Op2_int) & " gives: RTL Result:" & integer'image(Res_rtl) & ", overflow:" & integer'image(Ovfl_rtl) & ", underflow:" & integer'image(Unfl_rtl) & " and reference: Result " & integer'image(Res_ideal) & ", overflow:" & integer'image(Ovfl_ideal) & ", underflow:" & integer'image(Unfl_ideal) & ": FAILED (Underflow and overflow)" severity warning;
				pass := 0;
			else
				report "ALU operation " & Cmd_txt & " of " & integer'image(Op1_int) & " and " & integer'image(Op2_int) & " gives: RTL Result:" & integer'image(Res_rtl) & ", overflow:" & integer'image(Ovfl_rtl) & ", underflow:" & integer'image(Unfl_rtl) & " and reference: Result " & integer'image(Res_ideal) & ", overflow:" & integer'image(Ovfl_ideal) & ", underflow:" & integer'image(Unfl_ideal) & ": FAILED (Result, overflow and underflow)" severity warning;
				pass := 0;
			end if;
		end procedure verify;

		variable Res_rtl, Res_ideal	: integer;
		variable Ovfl_rtl, Ovfl_ideal	: integer;
		variable Unfl_rtl, Unfl_ideal	: integer;
		variable Op1_int, Op2_int	: integer;
		variable seed1, seed2	: positive;
		variable Op2	: std_logic_vector(OP2_L_TB-1 downto 0);
		variable Op1	: std_logic_vector(OP1_L_TB-1 downto 0);
		variable Cmd	: std_logic_vector(CMD_L_TB-1 downto 0);
		variable Cmd_txt	: string(1 to 4);
		variable pass	: integer;
		variable num_pass	: integer;

	begin

		wait for 1 ns;

		num_pass := 0;

		reset;

		for i in 0 to NUM_TEST-1 loop
			push_op(Op1_int, Op2_int, Cmd, seed1, seed2);

			wait on Done_tb;

			if (Cmd = CMD_SSUM) or (Cmd = CMD_SSUB) or (Cmd = CMD_SCMP) then
				Res_rtl := to_integer(signed(Res_tb));
			else
				Res_rtl := to_integer(unsigned(Res_tb));
			end if;

			Ovfl_rtl := std_logic_to_int(Ovfl_tb);
			Unfl_rtl := std_logic_to_int(Unfl_tb);

			std_vect_to_txt(Cmd, Cmd_txt);
			reference(Op1_int, Op2_int, Cmd, Res_ideal, Ovfl_ideal, Unfl_ideal);
			verify(Op1_int, Op2_int, Res_ideal, Res_rtl, Ovfl_ideal, Ovfl_rtl, Unfl_ideal, Unfl_rtl, Cmd_txt, pass);

			num_pass := num_pass + pass;

			wait until rising_edge(clk_tb);
		end loop;

		report "PASSED: " & integer'image(num_pass) & " out of " & integer'image(NUM_TEST);

		stop <= true;

	end process test;

end bench;
