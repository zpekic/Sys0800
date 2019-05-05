----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    23:55:21 03/10/2019 
-- Design Name: 
-- Module Name:    tms0800 - Behavioral 
-- Project Name: 
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
    Port ( reset : in  STD_LOGIC;
           clk_cpu : in  STD_LOGIC;
           clk_du : in  STD_LOGIC;
           clk_txd : in  STD_LOGIC;
           enable_trace : in  STD_LOGIC;
           show_debug : in  STD_LOGIC;
           nDigit : out  STD_LOGIC_VECTOR (8 downto 0);
           segment : out  STD_LOGIC_VECTOR(7 downto 0);
           ka : in  STD_LOGIC;
           kb : in  STD_LOGIC;
           kc : in  STD_LOGIC;
           kd : in  STD_LOGIC;
			  trace_txd: out STD_LOGIC;
			  trace_rxd: in STD_LOGIC;
			  disableSingleStep: out STD_LOGIC;
			  ps2: in STD_LOGIC_VECTOR(15 downto 0); -- TODO: remove!
			  dbg_select: in STD_LOGIC_VECTOR(2 downto 0);
			  dbg_state: out STD_LOGIC_VECTOR(3 downto 0));
end tms0800;

architecture Behavioral of tms0800 is

component displayunit is
    Port ( clk : in  STD_LOGIC;
			  reset: in STD_LOGIC;
           a : in  STD_LOGIC_VECTOR (35 downto 0);
           debug : in  STD_LOGIC_VECTOR (31 downto 0);
           dp_pos : in  STD_LOGIC_VECTOR(3 downto 0);
           show_debug : in  STD_LOGIC;
           nDigit : out  STD_LOGIC_VECTOR (8 downto 0);
           segment : out  STD_LOGIC_VECTOR (7 downto 0);
			  digit10: out STD_LOGIC;
			  dbg_select: in STD_LOGIC_VECTOR(2 downto 0);
			  dbg_state: out STD_LOGIC_VECTOR(3 downto 0));
end component;

component traceunit is
    Port ( clk : in  STD_LOGIC;
			  clk_txd: in STD_LOGIC;
           reset : in  STD_LOGIC;
           char : in  STD_LOGIC_VECTOR(7 downto 0);
           pc : in  STD_LOGIC_VECTOR (8 downto 0);
           instruction : in  STD_LOGIC_VECTOR (11 downto 0);
			  a : in STD_LOGIC_VECTOR(3 downto 0);
			  b : in STD_LOGIC_VECTOR(3 downto 0);
			  c : in STD_LOGIC_VECTOR(3 downto 0);
			  af : in  STD_LOGIC;
			  bf : in  STD_LOGIC;
			  cf : in  STD_LOGIC;
           char_sent : out  STD_LOGIC;
           txd : out  STD_LOGIC);
end component;

component rom512x12 is
	 Generic (
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
           digit : buffer STD_LOGIC_VECTOR(3 downto 0));
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

component mux11x4 is
    Port ( e : in  STD_LOGIC_VECTOR (10 downto 0);
           x : in  STD_LOGIC_VECTOR (43 downto 0);
           y : out  STD_LOGIC_VECTOR (3 downto 0));
end component;

component freqmux is
    Port ( reset : in  STD_LOGIC;
           f0in : in  STD_LOGIC;
           f1in : in  STD_LOGIC;
           sel : in  STD_LOGIC;
           fout : out  STD_LOGIC);
end component;

type masktable is array (0 to 15) of std_logic_vector(43 downto 0);
constant km: masktable :=(
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
    X"00000000000"  -- M15	=	ALL
	 );
	 
signal mask: std_logic_vector(10 downto 0);
signal k, km_current: std_logic_vector(43 downto 0);

signal tu_charsent: std_logic;

-- Main data registers -------------------------
signal reg_a, reg_b, reg_c: std_logic_vector(43 downto 0);
signal flag_a: std_logic_vector(10 downto 0);
signal flag_b: std_logic_vector(10 downto 0);
signal cflag: std_logic := '1';	-- TODO remove value

-- flag xor for EXF ---------------------------
signal af_xor_bf: std_logic_vector(10 downto 0);

-- selected data to feed ALU and tracer unit ---
signal a_selected, b_selected, c_selected, k_selected: std_logic_vector(3 downto 0);
signal af_selected, bf_selected: std_logic;
signal mask_selected: std_logic;
signal digit10: std_logic;

-- Other internal registers --------------------
signal pc: std_logic_vector(8 downto 0); -- 9 bit program counter
signal instruction: std_logic_vector(10 downto 0); -- 11 bit instruction
signal e_dig: std_logic_vector(11 downto 0); -- 12 bit enable register (usually only 1 hot bit to enable digit position registers)
signal e_reg: std_logic_vector(4 downto 0); -- 5 bit enable register for A, B, C, AF, BF
signal alu_sel: std_logic_vector(2 downto 0); -- make ALU selection "sticky" until changed
signal enable_a: std_logic_vector(10 downto 0);
signal enable_b: std_logic_vector(10 downto 0);
signal enable_c: std_logic_vector(10 downto 0);
signal enable_af: std_logic_vector(10 downto 0);
signal enable_bf: std_logic_vector(10 downto 0);
--------------------------------------------
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
signal sync_on, sync_off, sync_pulse: std_logic;

-- clocks and sync
signal clk_scan, clk_calc: std_logic;
--signal SyncMode: std_logic := '0';
--signal nSyncMode: std_logic := '1';

-- DEBUG only
signal u_addr: std_logic_vector(7 downto 0);

begin

du: displayunit port map 
(
		clk => clk_scan,
		reset => reset,
		a => reg_a(43 downto 8),
		debug(15 downto 0) => pc(7 downto 0) & u_addr,
		debug(31 downto 16) => ps2, --X"000" & kd & kc & kb & ka,
		dp_pos => reg_a(3 downto 0),
		show_debug => show_debug,
		nDigit => nDigit,
		segment => segment,
		digit10 => digit10,
		dbg_select => dbg_select,
		dbg_state => open --dbg_state
);

tu: traceunit port map
( 
	clk => clk_calc,
	clk_txd => clk_txd,
   reset => reset,
   char => tu_char,
   pc => pc, 
   instruction => '0' & instruction,
	a => a_selected,
	b => b_selected,
	c => c_selected,
	af => af_selected,
	bf => bf_selected,
	cf => cflag,
   char_sent => tu_charsent,
   txd => trace_txd
);
			  
-- Program counter and instruction -------------------------------------------------------
update_pc: process(clk_calc, reset, pc_verb, instruction)
begin
	if (reset = '1') then
		pc <= (others => '0');
	else
		if (rising_edge(clk_calc)) then
			case pc_verb is
				when pc_clear =>
					pc <= (others => '0');
				when pc_next =>
					pc <= std_logic_vector(unsigned(pc) + 1);
				when pc_load => 
					pc <= instruction(8 downto 0);
				when others =>
					null;
			end case;
		end if;
	end if;
end process;

-- note there is no instruction register, the output of program memory is used directly --
program: rom512x12
		generic map
		(
			asm_filename => "./calculator_source_edit_6.asm",
			lst_filename => "./tms0800/output/calculator_source_edit_6.lst"
		)	
		port map
		(
			address => pc,
			data(11) => open,
			data(10 downto 0) => instruction
		);
-----------------------------------------------------------------------------------------
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
-- TODO: explain why needed

--sync_on 		<= '1' when (sync_verb = turnon) else '0';
--sync_off 	<= '1' when (sync_verb = turnoff) else '0';
sync_pulse 	<= '0' when (sync_verb = pulse) else '1';

--cpu_freqmux: freqmux Port map ( 
--				reset => reset,
--				f0in => clk_cpu,
--				f1in => clk_du,
--				sel => syncMode,
--				fout => clk_calc
--			 );
--			 
--du_freqmux: freqmux Port map ( 
--				reset => reset,
--				f0in => clk_du,
--				f1in => sync_pulse,
--				sel => syncMode,
--				fout => clk_scan
--			 );
			 
clk_calc <= clk_cpu;
clk_scan <= sync_pulse; --'1' when (u_addr = X"02") else '0';

--nSyncMode <= not (sync_on or syncMode);
--syncMode <= not (sync_off or reset or nSyncMode);

--dbg_state <= ka & kb & kc & kd;
dbg_state <= kb & kc & keystrobe & digit10;

-------------------------------------------------------
cu: controlunit port map 
	( 
		clk => clk_calc,
      reset => reset,
      instruction => instruction(10 downto 4), 	-- don't care for the mask selector here
      condition(cond_false) => '0', 				-- hard-code "false" for condition 15 
      condition(cond_charsent) => tu_charsent,
      condition(cond_enabletrace) => enable_trace,
      condition(cond_cflag) => cflag,
      condition(cond_e11) => e_dig(11),			-- e register is at starting bit (== no digits are enabled)
      condition(cond_kp) => ka,						-- not used, should be '1' (inactive)
      condition(cond_ko) => kb,						-- '0' if any operation key pressed
      condition(cond_kn) => kc,						-- '0' if numeric key pressed
      condition(cond_keystrobe) => keystrobe,	-- '1' is any key was pressed in this scan
      condition(cond_digit10) => digit10,			-- '1' after least significant displayed digit
      condition(cond_5) => '0',						
      condition(cond_dk) => '1',						-- as if DisplayKey is stuck == display always on
      condition(cond_3) => '0',
      condition(cond_2) => '0',
      condition(cond_1) => '0',
      condition(cond_true) => '1', -- hard-code "true" for condition 0
		u_code => u_code,
		-- debug only
		u_addr => u_addr
	);

-- used in EXF instruction
af_xor_bf <= flag_a xor flag_b;

-- hint to clock logic outside (disable single step when in trace code, or when microcode does it)
disableSingleStep <= '1' when ((unsigned(u_addr) > 9) and (unsigned(u_addr) < 64)) else ss_disable;

-- get encoded K (constants) and M (masks) from lookup ROM --		
km_current <= km(to_integer(unsigned(instruction(3 downto 0))));

-- enables for each digit or bit in the SAM are at "intersection" of digit and register enabled line --
-- when the "master write" from microcode is 1. Note these are all active 0 																		  --
enable_a  <= e_dig(10 downto 0) when (update_sam = '1' and e_reg(4) = '0') else "11111111111";
enable_b  <= e_dig(10 downto 0) when (update_sam = '1' and e_reg(3) = '0') else "11111111111";
enable_c  <= e_dig(10 downto 0) when (update_sam = '1' and e_reg(2) = '0') else "11111111111";
enable_af <= e_dig(10 downto 0) when (update_sam = '1' and e_reg(1) = '0') else "11111111111";
enable_bf <= e_dig(10 downto 0) when (update_sam = '1' and e_reg(0) = '0') else "11111111111";

-- generate SAM (regs a, b, c), and a and b flags, plus (k)onstants and masks ---
sam_generate: for i in 0 to 10 generate
begin
	-- mask and constants
	mask(i) <= not (km_current(4 *i + 3));
	k(4 * i + 3 downto 4 * i) <= "0000" when mask(i) = '0' else km_current(4 * i + 3 downto 4 * i);
	
	-- af, bf flags are the same regardless of the position
	af: sambit port map (
		clk => clk_calc,
		sel => flag_verb,
		nEnable => enable_af(i),
		m => mask(i),
		inp => af_xor_bf(i),
		flag => flag_a(i)
	);

	bf: sambit port map (
		clk => clk_calc,
		sel => flag_verb,
		nEnable => enable_bf(i),
		m => mask(i),
		inp => af_xor_bf(i),
		flag => flag_b(i)
	);
	
	-- rightmost digits a, b, c are connected to "0" for left << shift --
	lsd: if (i = 0) generate
		a: samdigit Port map ( 
				clk => clk_calc,
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
				clk => clk_calc,
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
				clk => clk_calc,
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

	-- in the middle --
	isd: if (i > 0 and i < 10) generate
		a: samdigit Port map ( 
				clk => clk_calc,
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
				clk => clk_calc,
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
				clk => clk_calc,
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

	-- leftmost digits a, b, c are connected to "0" for right >> shift --
	msd: if (i = 10) generate
		a: samdigit Port map ( 
				clk => clk_calc,
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
				clk => clk_calc,
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
				clk => clk_calc,
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
amux: mux11x4 port map 
	(
		e => e_dig(10 downto 0),
		x => reg_a,
		y => a_selected
	);

bmux: mux11x4 port map 
	(
		e => e_dig(10 downto 0),
		x => reg_b,
		y => b_selected
	);

cmux: mux11x4 port map 
	(
		e => e_dig(10 downto 0),
		x => reg_c,
		y => c_selected
	);

kmux: mux11x4 port map 
	(
		e => e_dig(10 downto 0),
		x => k,
		y => k_selected
	);

fmux: mux11x4 port map 
	(
		e => e_dig(10 downto 0),
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
update_cflag: process(clk_calc, cflag_verb)
begin
	if (rising_edge(clk_calc)) then
		case cflag_verb is
			when cf_zero =>
				cflag <= '0';
			when cf_one =>
				cflag <= '1';
			when cf_cout =>
				cflag <= alu_cout;
			when cf_or_af_and_mask =>
				cflag <= cflag or (af_selected and mask_selected); -- used in TFA (test flag a)
			when cf_or_bf_and_mask =>
				cflag <= cflag or (bf_selected and mask_selected);	-- used in TFB (test flag b)
			when cf_or_af_xor_bf =>
				cflag <= cflag or (af_selected xor bf_selected);	-- used in CF (compare flags)
			when others =>
				null;
		end case;
	end if;
end process;

-- SRC (source) register sets the ALU input multiplexor selection
update_src: process(clk_calc, alu_inp)
begin
	if (rising_edge(clk_calc)) then
		if (alu_inp = src_nop) then
			alu_sel <= alu_sel;	-- no change
		else
			alu_sel <= alu_inp;	-- capture from microinstruction for future use
		end if;
	end if;
end process;

-- DST (destination) register - selects 0 or 1 out of 5 regs (A, B, C, AF, BF) ---
update_dst: process(clk_calc, dst_verb)
begin
	if (rising_edge(clk_calc)) then
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
				null;
		end case;
	end if;
end process;

-- E (enable) register - selects 1 out of 10 digits 9 .. 0 ------------------
update_e: process(clk_calc, e_verb)
begin
	if (rising_edge(clk_calc)) then
		case e_verb is
			when e_init =>
				e_dig <= "011111111111"; -- note that e(11) does not enable any register
			when e_rol =>
				e_dig <= e_dig(10 downto 0) & e_dig(11); -- rotate left (11 << 0), used when calculating
			when e_ror =>
				e_dig <= e_dig(0) & e_dig(11 downto 1); -- rotate right (11 >> 0), used for driving tracer
			when others =>
				null;
		end case;
	end if;
end process;

end Behavioral;

