----------------------------------------------------------------------------------
-- Company: 
-- Engineer: zpekic@hotmail.com
-- 
-- Create Date:    23:55:21 03/10/2019 
-- Design Name: 
-- Module Name:    tms0800 - Behavioral 
-- Project Name: 	https://hackaday.io/project/167457-tms0800-fpga-implementation-in-vhdl
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;
use work.tms0800_package.all;

entity tms0800 is
    Port ( -- present in original
			  reset : in  STD_LOGIC;
           clk_cpu : in  STD_LOGIC;
           nDigit : out  STD_LOGIC_VECTOR (8 downto 0);
           segment : out  STD_LOGIC_VECTOR(7 downto 0);
           ka : in  STD_LOGIC;
           kb : in  STD_LOGIC;
           kc : in  STD_LOGIC;
           kd : in  STD_LOGIC;
			  -- select TI Datamath (0) or Sinclair Scientific (1) mode 
			  sinclair: in STD_LOGIC;
			  -- debug, trace additions
           trace_enable : in  STD_LOGIC;
			  trace_ascii: out STD_LOGIC_VECTOR(7 downto 0);
			  trace_ready: in STD_LOGIC;
			  breakpoint_req: out STD_LOGIC;
			  breakpoint_ack: in STD_LOGIC;
			  singlestep_disable: out STD_LOGIC;
			  dbg_mpc: out STD_LOGIC_VECTOR(8 downto 0);
			  dbg_upc: out STD_LOGIC_VECTOR(7 downto 0)
			);
end tms0800;

architecture Behavioral of tms0800 is

component display_ti is
    Port ( clk : in  STD_LOGIC;
			  reset: in STD_LOGIC;
           reg_a : in  STD_LOGIC_VECTOR (35 downto 0);
			  show_error: in STD_LOGIC;
           nDPoint : in STD_LOGIC_VECTOR (9 downto 0);
           nDigit : in  STD_LOGIC_VECTOR (9 downto 0);
           segment : out  STD_LOGIC_VECTOR (7 downto 0)
			);
end component;

component display_sinclair is
    Port ( clk : in  STD_LOGIC;
			  reset: in STD_LOGIC;
           reg_a : in  STD_LOGIC_VECTOR (35 downto 0);
			  show_error: in STD_LOGIC;
           nDPoint : in STD_LOGIC_VECTOR (9 downto 0);
           nDigit : in  STD_LOGIC_VECTOR (9 downto 0);
           segment : out  STD_LOGIC_VECTOR (7 downto 0)
			);
end component;

component rom512x12 is
	 Generic (
			fill_value: std_logic_vector(11 downto 0);
			sinclair_mode: boolean;
			asm_filename: string;
			lst_filename: string
		);
    Port (           
           address : in  STD_LOGIC_VECTOR (8 downto 0);
			  data : out  STD_LOGIC_VECTOR (11 downto 0)
			 );
end component;

component controlunit is
    Port ( clk : in  STD_LOGIC;
           reset : in  STD_LOGIC;
           instruction : in  STD_LOGIC_VECTOR (6 downto 0);
           condition : in  STD_LOGIC_VECTOR (15 downto 0);
           u_code : out  STD_LOGIC_VECTOR (31 downto 0);
			  -- debug only
			  u_addr: out STD_LOGIC_VECTOR(7 downto 0));
end component;

component samdigit is
    Port ( clk : in  STD_LOGIC;
           sel : in  STD_LOGIC_VECTOR(1 downto 0);
           in1 : in  STD_LOGIC_VECTOR(3 downto 0);
           in2 : in  STD_LOGIC_VECTOR(3 downto 0);
			  in3 : in  STD_LOGIC_VECTOR(3 downto 0);
           nEnable : in  STD_LOGIC;
           mleft : in  STD_LOGIC;
           m : in  STD_LOGIC;
           mright : in  STD_LOGIC;
           digit : out STD_LOGIC_VECTOR(3 downto 0));
end component;

component sambit is
    Port ( clk : in  STD_LOGIC;
           sel : in  STD_LOGIC_VECTOR (1 downto 0);
           nEnable : in  STD_LOGIC;
           m : in  STD_LOGIC;
			  inp: in STD_LOGIC;
           flag : buffer  STD_LOGIC);
end component;

component bcdalu is
    Port ( fun : in  STD_LOGIC_VECTOR (2 downto 0);
           sel : in  STD_LOGIC_VECTOR (2 downto 0);
           cin : in  STD_LOGIC;
           a : in  STD_LOGIC_VECTOR (3 downto 0);
           b : in  STD_LOGIC_VECTOR (3 downto 0);
           c : in  STD_LOGIC_VECTOR (3 downto 0);
           k : in  STD_LOGIC_VECTOR (3 downto 0);
           y : out  STD_LOGIC_VECTOR (3 downto 0);
           cout : out  STD_LOGIC);
end component;

component mux16x4 is
    Port ( s : in  STD_LOGIC_VECTOR (3 downto 0);
           x : in  STD_LOGIC_VECTOR (43 downto 0);
           y : out  STD_LOGIC_VECTOR (3 downto 0));
end component;

type decodepattern is array (0 to 15) of std_logic_vector(9 downto 0);
constant decode: decodepattern := (
					"1111111101",
					"1111111011",
					"1111110111",
					"1111101111",
					"1111011111",
					"1110111111",
					"1101111111",
					"1011111111",
					"0111111111",
					"1111111111",
					"1111111111",
					"1111111111",
					"1111111111",
					"1111111111",
					"1111111111",
					"1111111111"
);

type mem16x8 is array (0 to 15) of std_logic_vector(7 downto 0);
constant h2a: mem16x8 :=(
	std_logic_vector(to_unsigned(character'pos('0'), 8)),
	std_logic_vector(to_unsigned(character'pos('1'), 8)),
	std_logic_vector(to_unsigned(character'pos('2'), 8)),
	std_logic_vector(to_unsigned(character'pos('3'), 8)),
	std_logic_vector(to_unsigned(character'pos('4'), 8)),
	std_logic_vector(to_unsigned(character'pos('5'), 8)),
	std_logic_vector(to_unsigned(character'pos('6'), 8)),
	std_logic_vector(to_unsigned(character'pos('7'), 8)),
	std_logic_vector(to_unsigned(character'pos('8'), 8)),
	std_logic_vector(to_unsigned(character'pos('9'), 8)),
	std_logic_vector(to_unsigned(character'pos('A'), 8)),
	std_logic_vector(to_unsigned(character'pos('B'), 8)),
	std_logic_vector(to_unsigned(character'pos('C'), 8)),
	std_logic_vector(to_unsigned(character'pos('D'), 8)),
	std_logic_vector(to_unsigned(character'pos('E'), 8)),
	std_logic_vector(to_unsigned(character'pos('F'), 8))
	);
	
constant selring: mem16x8 :=(
	x"1B",	-- 0
	x"20",	
	x"31",
	x"42",
	x"53",
	x"64",
	x"75",
	x"86",
	x"97",
	x"A8",
	x"B9",
	x"0A",	-- 11
	x"0B",	-- illegal
	x"0B",	-- illegal
	x"0B",	-- illegal
	x"0B"		-- illegal
	);
	
type masktable is array (0 to 31) of std_logic_vector(43 downto 0);

constant km: masktable :=(
--	TI Datamath masks ----------------
    X"FFFFFFFFFF7", -- M0 	=	F0/DPT7
    X"FFFFFFFFF4F", -- M1	=	F1/EXPD
    X"FFFFFFFF1FF", -- M2	= 	F2/LSD
    X"FFFFFFF0FFF", -- M3	=	F3
    X"FFFFFF0FFFF", -- M4	=	F4
    X"FFFFF0FFFFF", -- M5	=	F5
    X"FFFF0FFFFFF", -- M6	=	F6
    X"F0FFFFFFFFF", -- M7	=	F9
    X"1FFFFFFFFFF", -- M8	=	F10/OV1
    X"FFFFFFFF000", -- M9	=	OPFGS
    X"01FFFFFFFFF", -- M10	=	MSD1
    X"000000001FF", -- M11	=	MANT1
    X"000000000FF", -- M12	=	MANT
    X"FFFFFFFFF01", -- M13	=	EXP1
    X"FFFFFFFFF00", -- M14	=	EXP
    X"00000000000", -- M15	=	ALL
-- Sinclair Scientific masks ---------
    X"00000000000", -- M0 = ALL
    X"5FFFFFFFFFF", -- M1 = MANT_S5
    X"FF00FFFFFFF", -- M2 = EXP
    X"FFFF1FFFFFF", -- M3 = DIGIT1
    X"FFFF0000000", -- M4 = MANT
    X"FFFFFFFFFF1", -- M5 = MANTLOW1
    X"FF01FFFFFFF", -- M6 = EXP1
    X"F5FFFFFFFFF", -- M7 = EXP_S5
    X"000000FFFFF", -- M8 = TOPSTUFF
    X"0001FFFFFFF", -- M9 = EXPSGNS1
    X"FFFF0000001", -- M10 = MANT1
    X"FFFFF1FFFFF", -- M11 = MASKA1
    X"FFFF00005FF", -- M12 = MANTD5
    X"FFFF00001FF", -- M13 = MANTD1
    X"FFFF4FFFFFF", -- M14 = DIGIT4
    X"FFFF0FFFFFF"  -- M15 = DIGIT
);
	
constant NOP16: 			std_logic_vector(11 downto 0) := "01001000XXXX";  
constant NOP30: 			std_logic_vector(11 downto 0) := "01011110XXXX";
constant BREAK: 			std_logic_vector(11 downto 0) := "100000000000"; -- OR mask for breakpoint!
--1 0011 1111 013F	00 10001 0110          BIE    ALOGDIV ; BET divide and finish up ALOG
constant BIE_ALOGDIV:	std_logic_vector(11 downto 0) := "000100010110"; 
	
signal mask: std_logic_vector(10 downto 0);
signal k, km_current: std_logic_vector(43 downto 0);

-- Main data registers -------------------------
signal reg_a, reg_b, reg_c: std_logic_vector(43 downto 0);
signal flag_a: std_logic_vector(10 downto 0);
signal flag_b: std_logic_vector(10 downto 0);
signal cflag: std_logic;

-- flag xor for EXF ---------------------------
signal af_xor_bf: std_logic_vector(10 downto 0);

-- selected data to feed ALU and tracer unit ---
signal a_selected, b_selected, c_selected, k_selected: std_logic_vector(3 downto 0);
signal af_selected, bf_selected: std_logic;
signal mask_selected: std_logic;
signal digit1, digit10: std_logic;
signal hex: std_logic_vector(3 downto 0);

-- instruction being executed
signal instruction_ti, instruction_sinclair, instruction: std_logic_vector(11 downto 0); -- 11 bit instruction (+ 1 bit breakpoint)
alias i_breakpoint: std_logic is instruction(11);
alias i_class_and_opcode: std_logic_vector(6 downto 0) is instruction(10 downto 4);
alias i_jumpaddress: std_logic_vector(8 downto 0) is instruction(8 downto 0);
alias i_mask: std_logic_vector(3 downto 0) is instruction(3 downto 0);

-- Other internal registers --------------------
signal pc: std_logic_vector(8 downto 0);			-- 9 bit program counter
signal e_dig: std_logic_vector(11 downto 0); 	-- 12 bit enable register (usually only 1 hot bit to enable digit position registers)
signal e_sel: std_logic_vector(3 downto 0);		--	points to 1 hot bit in e_dig
signal e_selring: std_logic_vector(7 downto 0);	-- next value of e_sel from lookup table
alias  e_sel_left: std_logic_vector(3 downto 0) is e_selring(7 downto 4);	-- <<
alias  e_sel_right: std_logic_vector(3 downto 0) is e_selring(3 downto 0);	-- >>
signal e_reg: std_logic_vector(4 downto 0);		-- 5 bit enable register for A, B, C, AF, BF
signal alu_sel: std_logic_vector(2 downto 0);	-- make ALU selection "sticky" until changed
signal enable_a: std_logic_vector(10 downto 0);
signal enable_b: std_logic_vector(10 downto 0);
signal enable_c: std_logic_vector(10 downto 0);
signal enable_af: std_logic_vector(10 downto 0);
signal enable_bf: std_logic_vector(10 downto 0);

-- 4-bit ALU outputs ----------------------------
signal alu_y: std_logic_vector(3 downto 0);
signal alu_cout: std_logic;

-- microinstruction
signal u_code: std_logic_vector(31 downto 0);
-- microinstruction fields, byte 3
alias pc_verb: 		std_logic_vector(1 downto 0) is u_code(31 downto 30);
alias e_verb: 			std_logic_vector(1 downto 0) is u_code(29 downto 28);
alias reg_verb: 		std_logic_vector(1 downto 0) is u_code(27 downto 26);
alias flag_verb:		std_logic_vector(1 downto 0) is u_code(25 downto 24);
-- microinstruction fields, byte 2
alias ss_disable:		std_logic is u_code(23);	-- this goes outside too
alias update_sam:		std_logic is u_code(22);
alias dst_verb: 		std_logic_vector(2 downto 0) is u_code(21 downto 19);
alias cflag_verb: 	std_logic_vector(2 downto 0) is u_code(18 downto 16);
-- microinstruction fields, byte 1
alias sync_verb:		std_logic_vector(1 downto 0) is u_code(15 downto 14);
alias alu_fun: 		std_logic_vector(2 downto 0) is u_code(13 downto 11);
alias alu_inp: 		std_logic_vector(2 downto 0) is u_code(10 downto 8);
-- microinstruction fields, byte 0
alias tu_char:			std_logic_vector(7 downto 0) is u_code(7 downto 0);

-- keyboard / display
signal keystrobe, keypressed: std_logic;
signal segment_sinclair, segment_ti: std_logic_vector(7 downto 0);
signal scanentry: std_logic_vector(9 downto 0);

-- clocks and sync
signal clk_scan: std_logic;

begin

dbg_mpc <= pc;	-- make PC available at debug output port

-- display units differ for TI and Sinclair, but are driven by same scanentry ring counter
du_ti: display_ti port map 
(
		clk => clk_scan,
		reset => reset,
		reg_a => reg_a(43 downto 8),
		show_error => flag_b(5), -- when set, this flag indicates error (overflow or divide by zero)
		nDPoint => decode(to_integer(unsigned(reg_a(3 downto 0)))),
		nDigit => scanentry,
		segment => segment_ti
);

du_sinclair: display_sinclair port map 
(
		clk => clk_scan,
		reset => reset,
		reg_a => reg_a(43 downto 8),
		show_error => '0',			-- error is not shown in Sinclair mode
		nDPoint => "1011111111",	-- decimal point always after 1st digit
		nDigit => scanentry,
		segment => segment_sinclair
);

segment <= segment_sinclair when (sinclair = '1') else segment_ti;
		
-- Program counter and instruction -------------------------------------------------------
update_pc: process(clk_cpu, reset, pc_verb, instruction)
begin
	if (reset = '1') then
		pc <= (others => '0');
	else
		if (rising_edge(clk_cpu)) then
			case pc_verb is
				when pc_clear =>
					pc <= (others => '0');
				when pc_load => 
					pc <= i_jumpaddress;
				when pc_next =>
					pc <= std_logic_vector(unsigned(pc) + 1);
				when others =>
					pc <= pc;
			end case;
		end if;
	end if;
end process;

-- note there is no instruction register, the output of program memory is used directly --
prog_ti: rom512x12
		generic map
		(
			fill_value => NOP30,
			sinclair_mode => false,	-- hint to show correct disassembled listing (TI instructions)
			asm_filename => "./sourceCode_ti.asm",
			lst_filename => "./tms0800/output/sourceCode_ti.lst"
		)	
		port map
		(
			address => pc,
			data => instruction_ti
		);

prog_sinclair: rom512x12
		generic map
		(
-- HACKHACK: Why is this "random instruction" being passed in to intitialize the ROM???
-- Last instruction in Sinclair ROM as 0x13F is "BINE ALOGDIV" - however in some cases
-- CF is 0 which means it will not be executed and execution will continue at 0x140, which 
-- will bring it back to the right place, instead of executing bad opcodes, or NOPs which 
-- would wrap up to reset location 0. This is indication of another bug but for now this
-- "works". 
			fill_value => BIE_ALOGDIV,
			sinclair_mode => true, -- hint to show correct disassembled listing (Sinclair mode)
			asm_filename => "./sourceCode_sinclair.asm",
			lst_filename => "./tms0800/output/sourceCode_sinclair.lst"
		)	
		port map
		(
			address => pc,
			data => instruction_sinclair
		);

-- feed either TI or Sinclair prog and masks into the processor ---
instruction <= instruction_sinclair when (sinclair = '1') else instruction_ti;
-- expose breakpoint bit to the host
breakpoint_req <= i_breakpoint;
-- get encoded K (constants) and M (masks) from lookup ROM --		
km_current <= km(to_integer(unsigned(sinclair & i_mask)));

-- process to determine if any key pressed
set_keystrobe: process(reset, clk_scan, digit10, ka, kb, kc, kd)
begin
	if (reset = '1') then
		keystrobe <= '0';
	else
		if (rising_edge(clk_scan)) then
			if (digit10 = '1') then
				keystrobe <= '0';
			else
				keystrobe <= keystrobe or (not ka) or (not kb) or (not kc) or (not kd);
			end if;
		end if;
	end if;
end process;

-- display / keyboard sync ----------------------------
clk_scan <= '0' when (sync_verb = pulse) else '1';
			 
-- drive scan - note that even in debug output mode, nDigit lines drive keyboard correctly
nDigit 	<= scanentry(9 downto 1);
digit1	<= not scanentry(9);
digit10 	<= not scanentry(0);

drive_scan: process(clk_scan, reset)
begin
	if (reset = '1') then
		scanentry <= "0111111111";
	else
		if (rising_edge(clk_scan)) then
			scanentry <= scanentry(0) & scanentry(9 downto 1);
		end if;
	end if;
end process;

-- microcontrol unit
cu: controlunit port map 
	( 
		clk => clk_cpu,
      reset => reset,
      instruction => i_class_and_opcode,		 	-- don't care for the mask selector here
      condition(cond_false) => '0', 				-- hard-code "false" for condition 15 
      condition(cond_charsent) => trace_ready,	-- from external tracer circuit(s)
      condition(cond_enabletrace) => trace_enable,
      condition(cond_cflag) => cflag,
      condition(cond_e11) => e_dig(11),			-- e register is at starting bit (== no digits are enabled)
      condition(cond_kp) => ka,						-- not used, should be '1' (inactive)
      condition(cond_ko) => kb,						-- '0' if any operation key pressed
      condition(cond_kn) => kc,						-- '0' if numeric key pressed
      condition(cond_keystrobe) => keystrobe,	-- '1' is any key was pressed in this scan
      condition(cond_digit10) => digit10,			-- '1' after least significant displayed digit
      condition(cond_sinclair) => sinclair,						
      condition(cond_dk) => '1',						-- as if DisplayKey is stuck == display always on
      condition(cond_3) => '0',
      condition(cond_digit1) => digit1,
      condition(cond_breakpoint) => breakpoint_ack,
      condition(cond_true) => '1', -- hard-code "true" for condition 0
		u_code => u_code,
		-- debug only
		u_addr => dbg_upc
	);

-- used in EXF instruction
af_xor_bf <= flag_a xor flag_b;

-- hint to clock logic outside (disable single step when in trace code, or when microcode does it)
singlestep_disable <= ss_disable;

-- enables for each digit or bit in the SAM are at "intersection" of digit and register enabled line --
-- when the "master write" from microcode is 1. Note these are all active 0 																		  --
enable_a  <= e_dig(10 downto 0) when ((update_sam and (not e_reg(4))) = '1') else "11111111111";
enable_b  <= e_dig(10 downto 0) when ((update_sam and (not e_reg(3))) = '1') else "11111111111";
enable_c  <= e_dig(10 downto 0) when ((update_sam and (not e_reg(2))) = '1') else "11111111111";
enable_af <= e_dig(10 downto 0) when ((update_sam and (not e_reg(1))) = '1') else "11111111111";
enable_bf <= e_dig(10 downto 0) when ((update_sam and (not e_reg(0))) = '1') else "11111111111";

-- generate SAM (regs a, b, c), and a and b flags, plus (k)onstants and masks ---
sam_generate: for i in 0 to 10 generate
begin
	-- mask and constants
	mask(i) <= not (km_current(4 *i + 3));
	k(4 * i + 3 downto 4 * i) <= "0000" when mask(i) = '0' else km_current(4 * i + 3 downto 4 * i);
	
	-- af, bf flags are the same regardless of the position
	af: sambit port map (
		clk => clk_cpu,
		sel => flag_verb,
		nEnable => enable_af(i),
		m => mask(i),
		inp => af_xor_bf(i),
		flag => flag_a(i)
	);

	bf: sambit port map (
		clk => clk_cpu,
		sel => flag_verb,
		nEnable => enable_bf(i),
		m => mask(i),
		inp => af_xor_bf(i),
		flag => flag_b(i)
	);
	
	-- rightmost digits a, b, c of mantissa or exponent are connected to "0" for left << shift --
	lsd: if (i = 0) generate
		a: samdigit Port map ( 
				clk => clk_cpu,
				sel => reg_verb,
				in1 => reg_a(4 * (i + 1) + 3 downto 4 * (i + 1)),
				in2 => "0000",
				in3 => alu_y,
				nEnable => enable_a(i),
				mleft => mask(i + 1), 
				m => mask(i),
				mright => '0',
				digit => reg_a(4 * i + 3 downto 4 * i));

		b: samdigit Port map ( 
				clk => clk_cpu,
				sel => reg_verb,
				in1 => reg_b(4 * (i + 1) + 3 downto 4 * (i + 1)),
				in2 => "0000",
				in3 => alu_y,
				nEnable => enable_b(i),
				mleft => mask(i + 1), 
				m => mask(i),
				mright => '0',
				digit => reg_b(4 * i + 3 downto 4 * i));

		c: samdigit Port map ( 
				clk => clk_cpu,
				sel => reg_verb,
				in1 => reg_c(4 * (i + 1) + 3 downto 4 * (i + 1)),
				in2 => "0000",
				in3 => alu_y,
				nEnable => enable_c(i),
				mleft => mask(i + 1), 
				m => mask(i),
				mright => '0',
				digit => reg_c(4 * i + 3 downto 4 * i));

	end generate;

	-- in the middle of mantissa --
	isd: if ((i > 0) and (i < 10)) generate
		a: samdigit Port map ( 
				clk => clk_cpu,
				sel => reg_verb,
				in1 => reg_a(4 * (i + 1) + 3 downto 4 * (i + 1)),
				in2 => reg_a(4 * (i - 1) + 3 downto 4 * (i - 1)),
				in3 => alu_y,
				nEnable => enable_a(i),
				mleft => mask(i + 1),
				m => mask(i),
				mright => mask(i - 1),
				digit => reg_a(4 * i + 3 downto 4 * i));

		b: samdigit Port map ( 
				clk => clk_cpu,
				sel => reg_verb,
				in1 => reg_b(4 * (i + 1) + 3 downto 4 * (i + 1)),
				in2 => reg_b(4 * (i - 1) + 3 downto 4 * (i - 1)),
				in3 => alu_y,
				nEnable => enable_b(i),
				mleft => mask(i + 1),
				m => mask(i),
				mright => mask(i - 1),
				digit => reg_b(4 * i + 3 downto 4 * i));

		c: samdigit Port map ( 
				clk => clk_cpu,
				sel => reg_verb,
				in1 => reg_c(4 * (i + 1) + 3 downto 4 * (i + 1)),
				in2 => reg_c(4 * (i - 1) + 3 downto 4 * (i - 1)),
				in3 => alu_y,
				nEnable => enable_c(i),
				mleft => mask(i + 1),
				m => mask(i),
				mright => mask(i - 1),
				digit => reg_c(4 * i + 3 downto 4 * i));

	end generate;

	-- leftmost digits of mantissa or exponent of a, b, c are connected to "0" for right >> shift --
	msd: if (i = 10) generate
		a: samdigit Port map ( 
				clk => clk_cpu,
				sel => reg_verb,
				in1 => "0000",
				in2 => reg_a(4 * (i - 1) + 3 downto 4 * (i - 1)),
				in3 => alu_y,
				nEnable => enable_a(i),
				mleft => '0',
				m => mask(i),
				mright => mask(i - 1),
				digit => reg_a(4 * i + 3 downto 4 * i));

		b: samdigit Port map ( 
				clk => clk_cpu,
				sel => reg_verb,
				in1 => "0000",
				in2 => reg_b(4 * (i - 1) + 3 downto 4 * (i - 1)),
				in3 => alu_y,
				nEnable => enable_b(i),
				mleft => '0',
				m => mask(i),
				mright => mask(i - 1),
				digit => reg_b(4 * i + 3 downto 4 * i));

		c: samdigit Port map ( 
				clk => clk_cpu,
				sel => reg_verb,
				in1 => "0000",
				in2 => reg_c(4 * (i - 1) + 3 downto 4 * (i - 1)),
				in3 => alu_y,
				nEnable => enable_c(i),
				mleft => '0',
				m => mask(i),
				mright => mask(i - 1),
				digit => reg_c(4 * i + 3 downto 4 * i));
	end generate;

end generate;

-- Multiplexors to bring A, B, C, K to ALU and trace unit ---
amux: mux16x4 port map 
	(
--		e => e_dig(10 downto 0),
		s => e_sel,
		x => reg_a,
		y => a_selected
	);

bmux: mux16x4 port map 
	(
--		e => e_dig(10 downto 0),
		s => e_sel,
		x => reg_b,
		y => b_selected
	);

cmux: mux16x4 port map 
	(
--		e => e_dig(10 downto 0),
		s => e_sel,
		x => reg_c,
		y => c_selected
	);

kmux: mux16x4 port map 
	(
--		e => e_dig(10 downto 0),
		s => e_sel,
		x => k,
		y => k_selected
	);

fmux: mux16x4 port map 
	(
--		e => e_dig(10 downto 0),
		s => e_sel,
		x(43 downto 40) => '0' & mask(10) & flag_b(10) & flag_a(10),
		x(39 downto 36) => '0' & mask(9) & flag_b(9) & flag_a(9),
		x(35 downto 32) => '0' & mask(8) & flag_b(8) & flag_a(8),
		x(31 downto 28) => '0' & mask(7) & flag_b(7) & flag_a(7),
		x(27 downto 24) => '0' & mask(6) & flag_b(6) & flag_a(6),
		x(23 downto 20) => '0' & mask(5) & flag_b(5) & flag_a(5),
		x(19 downto 16) => '0' & mask(4) & flag_b(4) & flag_a(4),
		x(15 downto 12) => '0' & mask(3) & flag_b(3) & flag_a(3),
		x(11 downto 8)  => '0' & mask(2) & flag_b(2) & flag_a(2),
		x(7 downto 4)   => '0' & mask(1) & flag_b(1) & flag_a(1),
		x(3 downto 0)   => '0' & mask(0) & flag_b(0) & flag_a(0),
		y(3) => open,
		y(2) => mask_selected, 
		y(1) => bf_selected,
		y(0) => af_selected
	);

alu: bcdalu port map
	( 
		fun => alu_fun,
      sel => alu_sel, 
      cin => cflag,
      a => a_selected,
      b => b_selected,
      c => c_selected,
      k => k_selected,
      y => alu_y,
      cout => alu_cout
	);

-- cond (condition) register --------------------------------------
update_cflag: process(clk_cpu, cflag_verb)
begin
	if (rising_edge(clk_cpu)) then
			case cflag_verb is
				when cf_zero =>
					cflag <= '0';	-- clear regardless of the mask
				when cf_one =>
					cflag <= '1';	-- set regardless of the mask
				when cf_cout =>
					if (mask_selected = '1') then
						cflag <= alu_cout; 
					end if;
				when cf_or_af =>
					if (mask_selected = '1') then
						cflag <= cflag or af_selected; -- used in TFA (test flag a)
					end if;
				when cf_or_bf =>
					if (mask_selected = '1') then
						cflag <= cflag or bf_selected; -- used in TFB (test flag b)
					end if;
				when cf_or_af_xor_bf =>
					if (mask_selected = '1') then
						cflag <= cflag or (af_selected xor bf_selected);	-- used in CF (compare flags)
					end if;
				when others =>
					cflag <= cflag;
			end case;
	end if;
end process;

-- SRC (source) register sets the ALU input multiplexor selection
update_src: process(clk_cpu, alu_inp)
begin
	if (rising_edge(clk_cpu)) then
		if (alu_inp = src_nop) then
			alu_sel <= alu_sel;	-- no change
		else
			alu_sel <= alu_inp;	-- capture from microinstruction for future use
		end if;
	end if;
end process;

-- DST (destination) register - selects 0 or 1 out of 5 regs (A, B, C, AF, BF) ---
update_dst: process(clk_cpu, dst_verb)
begin
	if (rising_edge(clk_cpu)) then
		case dst_verb is
			when dst_nul =>
				e_reg <= "11111";	-- no register will be enabled for writing!
			when dst_a =>
				e_reg <= "01111";
			when dst_b =>
				e_reg <= "10111";
			when dst_c =>
				e_reg <= "11011";
			when dst_af =>
				e_reg <= "11101";
			when dst_bf =>
				e_reg <= "11110";
			when others => 		-- no change in destination register
				e_reg <= e_reg;
		end case;
	end if;
end process;

-- Next value from lookup table 
e_selring <= selring(to_integer(unsigned(e_sel)));

-- E (enable) register - selects 1 out of 10 digits 9 .. 0 ------------------
update_e: process(clk_cpu, e_verb)
begin
	if (rising_edge(clk_cpu)) then
		case e_verb is
			when e_init =>
				e_dig <= "011111111111"; -- note that e(11) does not enable any register
				e_sel <= x"B";	-- 11 (beyond left-most existing digit)
			when e_rol =>
				e_dig <= e_dig(10 downto 0) & e_dig(11); -- rotate left (11 << 0), used when calculating
				e_sel <= e_sel_left;
			when e_ror =>
				e_dig <= e_dig(0) & e_dig(11 downto 1); -- rotate right (11 >> 0), used for driving tracer
				e_sel <= e_sel_right;
			when others =>
				e_dig <= e_dig;
		end case;
	end if;
end process;

-- drive the tracer output, on which a string of ASCII characters appear
--select hex/bcd character to display
with tu_char(3 downto 0) select
	hex <= 	"000" & af_selected 					when t_af,
				"000" & bf_selected 					when t_bf,
				"000" & cflag 							when t_cf,
				a_selected 								when t_a,
				b_selected								when t_b,
				c_selected 								when t_c,
				instruction(3 downto 0) 			when t_instr0,
				instruction(7 downto 4) 			when t_instr1,
				instruction(11 downto 8)			when t_instr2, -- note that instruction(11) is breakpoint flag!
				pc(3 downto 0) 						when t_pc0,
				pc(7 downto 4) 						when t_pc1,
				"000" & pc(8) 							when t_pc2;
	
-- if input >127 then it is from mux, otherwise transmit directly
setTrace: process(clk_cpu, tu_char, hex)
begin
	if (falling_edge(clk_cpu)) then
		if (tu_char(7) = '0') then
			-- ASCII code to be transmitted is directly in the microcode
			trace_ascii <= tu_char;
		else
			-- convert 4-bit HEX to 8-bit ASCII
			--trace_ascii <= std_logic_vector(unsigned(ascii_hex) + unsigned(ascii_offset));
			trace_ascii <= h2a(to_integer(unsigned(hex)));
		end if;
	end if;
end process;	

end Behavioral;

