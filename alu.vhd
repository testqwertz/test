library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.alu_pkg.all;

entity alu is
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
end entity alu;

architecture rtl of alu is

	constant ZERO	: unsigned(OP1_L+1 - 1 downto 0) := to_unsigned(0, OP1_L+1);

	signal Op1N, Op1C	: unsigned(OP1_L - 1 downto 0);
	signal Op2N, Op2C	: unsigned(OP2_L - 1 downto 0);
	signal CmdN, CmdC	: std_logic_vector(CMD_L - 1 downto 0);
	signal ResN, ResC	: unsigned(OP1_L - 1 downto 0);

	signal SCmp, UCmp	: unsigned(OP1_L - 1 downto 0);

	signal USum		: unsigned(OP1_L+1 - 1 downto 0);
	signal USubN, USubC	: unsigned(OP1_L+1 - 1 downto 0);
	signal SSum		: unsigned(OP1_L+1 - 1 downto 0);
	signal SSubN, SSubC	: unsigned(OP1_L+1 - 1 downto 0);

	signal BAnd, BOr, BXor, BNot	: unsigned(OP1_L - 1 downto 0);

	signal OvflN, OvflC	: std_logic;
	signal UnflN, UnflC	: std_logic;

	signal DoneOp		: std_logic;
	signal UnCmdN, UnCmdC, UnCmdInt		: std_logic;

	type state_list is (IDLE, COMPUTE, COMPARE, OUTPUT);
	signal StateC, StateN: state_list;

begin

	reg: process(rst, clk)
	begin
		if (rst = '1') then
			Op1C <= (others => '0');
			Op2C <= (others => '0');
			CmdC <= (others => '0');
			SSubC <= (others => '0');
			USubC <= (others => '0');
			OvflC <= '0';
			UnflC <= '0';
			UnCmdC <= '0';
			ResC <= (others => '0');
			StateC <= IDLE;
		elsif (rising_edge(clk)) then
			Op1C <= Op1N;
			Op2C <= Op2N;
			CmdC <= CmdN;
			SSubC <= SSubN;
			USubC <= USubN;
			OvflC <= OvflN;
			UnflC <= UnflN;
			UnCmdC <= UnCmdN;
			ResC <= ResN;
			StateC <= StateN;
		end if;
	end process reg;

	state_det: process(StateC, Start, DoneOp, UnCmdInt, CmdC)
	begin
		StateN <= StateC; -- avoid latches
		case StateC is
			when IDLE =>
				if (Start = '1') then
					StateN <= COMPUTE;
				else
					StateN <= IDLE;
				end if;
			when COMPUTE =>
				if DoneOp = '1' then
					StateN <= OUTPUT;
				elsif (CmdC = CMD_UCMP) or (CmdC = CMD_SCMP) then
					StateN <= COMPARE;
				elsif (UnCmdC = '1') then
					StateN <= OUTPUT;
				else
					StateN <= COMPUTE;
				end if;
			when COMPARE =>
				StateN <= OUTPUT;
			when OUTPUT =>
				StateN <= IDLE;
		end case;
	end process state_det;

	UnCmdInt <= '0' when (StateC = COMPUTE ) and ((CmdC = CMD_USUM) or (CmdC = CMD_SSUM) or (CmdC = CMD_USUB) or (CmdC = CMD_SSUB) or (CmdC = CMD_UCMP) or (CmdC = CMD_SCMP) or (CmdC = CMD_AND) or (CmdC = CMD_OR) or (CmdC = CMD_NOT) or (CmdC = CMD_XOR)) else '1';

	USum <= ("0" & Op1C) + ("0" & Op2C);
	USubN <= ("0" & Op1C) - ("0" & Op2C);

	UCmp <=	(others => '0')			when USubC = ZERO else
		to_unsigned(1, UCmp'length)	when USubC(USubC'length-1) = '0' else
		(others => '1');

	SSum <= unsigned(signed(Op1C(Op1C'length-1 downto Op1C'length-1) & Op1C) + signed(Op2C(Op2C'length-1 downto Op2C'length-1) & Op2C));
	SSubN <= unsigned(signed(Op1C(Op1C'length-1 downto Op1C'length-1) & Op1C) - signed(Op2C(Op2C'length-1 downto Op2C'length-1) & Op2C));

	SCmp <=	(others => '0')			when SSubC = ZERO else
		to_unsigned(1, SCmp'length)	when SSubC(SSubC'length-1) = '0' else
		(others => '1');

	and_bit: for k in 0 to OP1_L-1 generate 
		BAnd(k) <= Op1C(k) and Op2C(k);
	end generate and_bit;

	or_bit: for k in 0 to OP1_L-1 generate 
		BOr(k) <= Op1C(k) or Op2C(k);
	end generate or_bit;

	xor_bit: for k in 0 to OP1_L-1 generate 
		BXor(k) <= Op1C(k) xor Op2C(k);
	end generate xor_bit;

	not_bit: for k in 0 to OP1_L-1 generate 
		BNot(k) <= not Op1C(k);
	end generate not_bit;

	data : process(StateC, SSum, SSubN, USum, USubN, SCmp, UCmp, BNot, BAnd, BOr, BXor, ResC, Op1, Op2, CmdC)
	begin
		ResN <= ResC;
		Op1N <= Op1C;
		Op2N <= Op2C;
		CmdN <= CmdC;
		OvflN <= OvflC;
		UnflN <= UnflC;
		UnCmdN <= UnCmdC;
		DoneOp <= '0';

		case StateC is
			when IDLE =>
				Op1N <= unsigned(Op1);
				Op2N <= unsigned(Op2);
				CmdN <= Cmd;
				UnflN <= '0';
				OvflN <= '0';
				UnCmdN <= UnCmdInt;
			when COMPUTE =>
				if (CmdC = CMD_USUM) then
					ResN <= USum(USum'length-1 - 1 downto 0);
					if (USum(USum'length-1) = '1') then
						OvflN <= '1';
					else
						OvflN <= '0';
					end if;
					DoneOp <= '1';
				elsif (CmdC = CMD_SSUM) then
					ResN <= SSum(SSum'length-1 - 1 downto 0);
					if ((Op1C(Op1C'length-1) = '0') and ((Op2C(Op2C'length-1)) = '0') and (SSum(SSum'length-2) = '1')) then
						OvflN <= '1';
						UnflN <= '0';
					elsif (((Op1C(Op1C'length-1) and Op2C(Op2C'length-1)) = '1') and (SSum(SSum'length-2) = '0')) then
						UnflN <= '1';
						OvflN <= '0';
					else
						OvflN <= '0';
						UnflN <= '0';
					end if;
					DoneOp <= '1';
				elsif (CmdC = CMD_USUB) then
					ResN <= USubN(USubN'length-1 - 1 downto 0);
					if (USubN(USubN'length-1) = '1') then
						UnflN <= '1';
					else
						UnflN <= '0';
					end if;
					DoneOp <= '1';
				elsif (CmdC = CMD_SSUB) then
					ResN <= SSubN(SSubN'length-1 - 1 downto 0);
					if (((Op2C(Op2C'length-1) or SSubN(SSubN'length-2)) = '0') and (Op1C(Op1C'length-1) = '1')) then
						OvflN <= '0';
						UnflN <= '1';
					elsif (((SSubN(SSubN'length-2) and Op2C(Op2C'length-1)) = '1') and (Op1C(Op1C'length-1) = '0')) then
						UnflN <= '0';
						OvflN <= '1';
					else
						UnflN <= '0';
						OvflN <= '0';
					end if;
					DoneOp <= '1';
				elsif(CmdC = CMD_AND) then
					ResN <= BAnd;
					DoneOp <= '1';
				elsif(CmdC = CMD_NOT) then
					ResN <= BNot;
					DoneOp <= '1';
				elsif(CmdC = CMD_OR) then
					ResN <= BOr;
					DoneOp <= '1';
				elsif(CmdC = CMD_XOR) then
					ResN <= BXor;
					DoneOp <= '1';
				else
					ResN <= (others => '0');
					DoneOp <= '0';
				end if;
			when COMPARE =>
				if (CmdC = CMD_SCMP) then
					ResN <= SCmp;
					DoneOp <= '1';
				elsif (CmdC = CMD_UCMP) then
					ResN <= UCmp;
					DoneOp <= '1';
				else
					ResN <= (others => '0');
				end if;
			when OUTPUT =>
				ResN <= ResC;
		end case;
	end process data;

	Unfl <= UnflC when (StateC = OUTPUT) else '0';
	Ovfl <= OvflC when (StateC = OUTPUT) else '0';
	UnCmd <= UnCmdC when (StateC = OUTPUT) else '0';

	Done <= '1' when (StateC = OUTPUT) else '0';

	Res <= std_logic_vector(ResC) when StateC = OUTPUT else (others => '0');

end rtl; 
