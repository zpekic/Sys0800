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
           clk_calc : in  STD_LOGIC;
           clk_scan : in  STD_LOGIC;
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
			  scan_start: out STD_LOGIC;
			  scan_end: out STD_LOGIC;
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

component mux11x4 is
    Port ( e : in  STD_LOGIC_VECTOR (10 downto 0);
           x : in  STD_LOGIC_VECTOR (43 downto 0);
           y : out  STD_LOGIC_VECTOR (3 downto 0));
end component;

type masktable is array (0 to 15) of std_logic_vector(43 downto 0);
constant km: masktable :=(
    X"FFFFFFFFFF7", -- M0
    X"FFFFFFFFF4F", -- M1
    X"FFFFFFFF1FF", -- M2
    X"FFFFFFF0FFF", -- M3
    X"FFFFFF0FFFF", -- M4
    X"FFFFF0FFFFF", -- M5
    X"FFFF0FFFFFF", -- M6
    X"F0FFFFFFFFF", -- M7
    X"1FFFFFFFFFF", -- M8
    X"FFFFFFFF000", -- M9
    X"01FFFFFFFFF", -- M10
    X"000000001FF", -- M11
    X"000000000FF", -- M12
    X"FFFFFFFFF01", -- M13
    X"FFFFFFFFF00", -- M14
    X"00000000000"  -- M15
	 );
	 
signal mask: std_logic_vector(10 downto 0);
signal k, km_current: std_logic_vector(43 downto 0);

signal tu_charsent: std_logic;

-- TODO: remove hard-coding!! ------------------

-- Main data registers -------------------------
signal reg_a, reg_b, reg_c: std_logic_vector(43 downto 0);
signal flag_a: std_logic_vector(10 downto 0) := "10101010101";
signal flag_b: std_logic_vector(10 downto 0) := "01010101010";
signal cond: std_logic := '1';

-- selected data to feed ALU and tracer unit ---
signal a_selected, b_selected, c_selected, k_selected: std_logic_vector(3 downto 0);
signal af_selected, bf_selected: std_logic;

-- Other internal registers --------------------
signal pc: std_logic_vector(8 downto 0); -- 9 bit program counter
signal instruction: std_logic_vector(10 downto 0); -- 11 bit instruction
signal e: std_logic_vector(11 downto 0); -- 12 bit enable register (usually only 1 hot bit to enable digit position registers)
--------------------------------------------
signal alu_y: std_logic_vector(3 downto 0) := X"9";


signal u_addr: std_logic_vector(7 downto 0);
signal u_instruction: std_logic_vector(31 downto 0);
alias pc_verb: std_logic_vector(1 downto 0) is u_instruction(31 downto 30);
alias e_verb: std_logic_vector(1 downto 0) is u_instruction(29 downto 28);
alias a_verb: std_logic_vector(1 downto 0) is u_instruction(27 downto 26);
alias b_verb: std_logic_vector(1 downto 0) is u_instruction(25 downto 24);
alias c_verb: std_logic_vector(1 downto 0) is u_instruction(23 downto 22);
alias t_verb: std_logic_vector(1 downto 0) is u_instruction(21 downto 20);
alias tu_char: std_logic_vector(7 downto 0) is u_instruction(7 downto 0);

begin

du: displayunit port map 
(
		clk => clk_scan,
		reset => reset,
		a => reg_a(43 downto 8),
		debug(15 downto 0) => pc(7 downto 0) & u_addr,
		debug(31 downto 16) => pc_verb & e_verb & "0000" & tu_char,
		dp_pos => reg_a(3 downto 0),
		show_debug => show_debug,
		nDigit => nDigit,
		segment => segment,
		scan_start => open,
		scan_end => open,
		dbg_state => dbg_state
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
	cf => cond,
   char_sent => tu_charsent,
   txd => trace_txd
);
			  
-- Program counter and instruction -------------------------------------------------------
update_pc: process(clk_calc, pc_verb, instruction)
begin
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
end process;
-- note there is no instruction register, the output of program memory is used directly --
program: rom512x12
		generic map
		(
			asm_filename => "./calculator_source_edit_6.asm",
			lst_filename => "./tms0800/calculator_source_edit_6.lst"
		)	
		port map
		(
			address => pc,
			data(11) => open,
			data(10 downto 0) => instruction
		);
-----------------------------------------------------------------------------------------

cu: controlunit Port map ( 
		clk => clk_calc,
      reset => reset,
      instruction => instruction(10 downto 4), -- don't care for the mask selector here
      condition(cond_false) => '0', -- hard-code "false" for condition 15 
      condition(cond_charsent) => tu_charsent,
      condition(cond_enabletrace) => enable_trace,
      condition(cond_cond) => cond,
      condition(cond_11) => '0',
      condition(cond_10) => '0',
      condition(cond_9) => '0',
      condition(cond_8) => '0',
      condition(cond_7) => '0',
      condition(cond_6) => '0',
      condition(cond_5) => '0',
      condition(cond_4) => '0',
      condition(cond_3) => '0',
      condition(cond_2) => '0',
      condition(cond_1) => '0',
      condition(cond_true) => '1', -- hard-code "true" for condition 0
		u_code => u_instruction,
		-- debug only
		u_addr => u_addr
		);


-- generate K (constants) and M (masks) -----------		
km_current <= km(to_integer(unsigned(instruction(3 downto 0))));

km_generate: for i in 0 to 10 generate
begin
	mask(i) <= not (km_current(4 *i + 3));
	k(4 * i + 3 downto 4 * i) <= "0000" when mask(i) = '0' else km_current(4 * i + 3 downto 4 * i);
end generate;  
---------------------------------------------------

-- generate SAM (regs a, b, c, plus additional register t), and a and b flags ---
sam_generate: for i in 0 to 10 generate
begin
	-- all --
--	t: samdigit Port map ( 
--			clk => clk_calc,
--			sel => t_verb,
--			in1 => "000" & flag_a(i),
--			in2 => "000" & flag_b(i),
--			in3 => alu_y,
--			enable => e(i),
--			mleft => '1', 
--			m => '1',
--			mright => '1',
--			digit => reg_t(4 * i + 3 downto 4 * i));

	-- rightmost --
	lsd: if (i = 0) generate
		a: samdigit Port map ( 
				clk => clk_calc,
				sel => a_verb,
				in1 => reg_a(4 * (i + 1) + 3 downto 4 * (i + 1)),
				in2 => "0000",
				in3 => alu_y,
				nEnable => e(i),
				mleft => mask(i + 1), 
				m => mask(i),
				mright => '0',
				digit => reg_a(4 * i + 3 downto 4 * i));

		b: samdigit Port map ( 
				clk => clk_calc,
				sel => b_verb,
				in1 => reg_b(4 * (i + 1) + 3 downto 4 * (i + 1)),
				in2 => "0000",
				in3 => alu_y,
				nEnable => e(i),
				mleft => mask(i + 1), 
				m => mask(i),
				mright => '0',
				digit => reg_b(4 * i + 3 downto 4 * i));

		c: samdigit Port map ( 
				clk => clk_calc,
				sel => c_verb,
				in1 => reg_c(4 * (i + 1) + 3 downto 4 * (i + 1)),
				in2 => "0000",
				in3 => alu_y,
				nEnable => e(i),
				mleft => mask(i + 1), 
				m => mask(i),
				mright => '0',
				digit => reg_c(4 * i + 3 downto 4 * i));

	end generate;

	-- in the middle --
	isd: if (i > 0 and i < 10) generate
		a: samdigit Port map ( 
				clk => clk_calc,
				sel => a_verb,
				in1 => reg_a(4 * (i + 1) + 3 downto 4 * (i + 1)),
				in2 => reg_a(4 * (i - 1) + 3 downto 4 * (i - 1)),
				in3 => alu_y,
				nEnable => e(i),
				mleft => mask(i + 1),
				m => mask(i),
				mright => mask(i - 1),
				digit => reg_a(4 * i + 3 downto 4 * i));

		b: samdigit Port map ( 
				clk => clk_calc,
				sel => b_verb,
				in1 => reg_b(4 * (i + 1) + 3 downto 4 * (i + 1)),
				in2 => reg_b(4 * (i - 1) + 3 downto 4 * (i - 1)),
				in3 => alu_y,
				nEnable => e(i),
				mleft => mask(i + 1),
				m => mask(i),
				mright => mask(i - 1),
				digit => reg_b(4 * i + 3 downto 4 * i));

		c: samdigit Port map ( 
				clk => clk_calc,
				sel => c_verb,
				in1 => reg_c(4 * (i + 1) + 3 downto 4 * (i + 1)),
				in2 => reg_c(4 * (i - 1) + 3 downto 4 * (i - 1)),
				in3 => alu_y,
				nEnable => e(i),
				mleft => mask(i + 1),
				m => mask(i),
				mright => mask(i - 1),
				digit => reg_c(4 * i + 3 downto 4 * i));

	end generate;

	-- leftmost --
	msd: if (i = 10) generate
		a: samdigit Port map ( 
				clk => clk_calc,
				sel => a_verb,
				in1 => "0000",
				in2 => reg_a(4 * (i - 1) + 3 downto 4 * (i - 1)),
				in3 => alu_y,
				nEnable => e(i),
				mleft => '0',
				m => mask(i),
				mright => mask(i - 1),
				digit => reg_a(4 * i + 3 downto 4 * i));

		b: samdigit Port map ( 
				clk => clk_calc,
				sel => b_verb,
				in1 => "0000",
				in2 => reg_b(4 * (i - 1) + 3 downto 4 * (i - 1)),
				in3 => alu_y,
				nEnable => e(i),
				mleft => '0',
				m => mask(i),
				mright => mask(i - 1),
				digit => reg_b(4 * i + 3 downto 4 * i));

		c: samdigit Port map ( 
				clk => clk_calc,
				sel => c_verb,
				in1 => "0000",
				in2 => reg_c(4 * (i - 1) + 3 downto 4 * (i - 1)),
				in3 => alu_y,
				nEnable => e(i),
				mleft => '0',
				m => mask(i),
				mright => mask(i - 1),
				digit => reg_c(4 * i + 3 downto 4 * i));
	end generate;

end generate;

-- Multiplexors to bring A, B, C, K to ALU and trace unit ---
amux: mux11x4 port map 
	(
		e => e(10 downto 0),
		x => reg_a,
		y => a_selected
	);

bmux: mux11x4 port map 
	(
		e => e(10 downto 0),
		x => reg_b,
		y => b_selected
	);

cmux: mux11x4 port map 
	(
		e => e(10 downto 0),
		x => reg_c,
		y => c_selected
	);

kmux: mux11x4 port map 
	(
		e => e(10 downto 0),
		x => k,
		y => k_selected
	);

fmux: mux11x4 port map 
	(
		e => e(10 downto 0),
		x(43 downto 40) => "00" & flag_b(10) & flag_a(10),
		x(39 downto 36) => "00" & flag_b(9) & flag_a(9),
		x(35 downto 32) => "00" & flag_b(8) & flag_a(8),
		x(31 downto 28) => "00" & flag_b(7) & flag_a(7),
		x(27 downto 24) => "00" & flag_b(6) & flag_a(6),
		x(23 downto 20) => "00" & flag_b(5) & flag_a(5),
		x(19 downto 16) => "00" & flag_b(4) & flag_a(4),
		x(15 downto 12) => "00" & flag_b(3) & flag_a(3),
		x(11 downto 8)  => "00" & flag_b(2) & flag_a(2),
		x(7 downto 4)   => "00" & flag_b(1) & flag_a(1),
		x(3 downto 0)   => "00" & flag_b(0) & flag_a(0),
		y(3) => open,
		y(2) => open,
		y(1) => bf_selected,
		y(0) => af_selected
	);

-- E (enable) register --------------------------------------
update_e: process(clk_calc, e_verb)
begin
	if (rising_edge(clk_calc)) then
		case e_verb is
			when e_init =>
				e <= "011111111111"; -- note that e(11) does not enable any register
			when e_shl =>
				e <= e(10 downto 0) & e(11); -- shift left
			when e_shr =>
				e <= e(0) & e(11 downto 1); -- shift right
			when others =>
				null;
		end case;
	end if;
end process;


end Behavioral;

