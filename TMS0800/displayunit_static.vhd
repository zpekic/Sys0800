----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    23:19:16 03/11/2019 
-- Design Name: 
-- Module Name:    displayunit - Behavioral 
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

entity displayunit is
    Port ( clk : in  STD_LOGIC;
			  reset: in STD_LOGIC;
			  sinclair: in STD_LOGIC;
           reg_a : in  STD_LOGIC_VECTOR (35 downto 0);
           debug : in  STD_LOGIC_VECTOR (31 downto 0);
           dp_pos : in  STD_LOGIC_VECTOR(3 downto 0);
           show_debug : in  STD_LOGIC;
			  show_error: in STD_LOGIC;
           segment : out  STD_LOGIC_VECTOR (7 downto 0);
           nDigit : out  STD_LOGIC_VECTOR (8 downto 0);
			  digit10: out STD_LOGIC;
			  dbg_select: in STD_LOGIC_VECTOR(2 downto 0));
end displayunit;

architecture Behavioral of displayunit is

component mux11x4 is
    Port ( e : in  STD_LOGIC_VECTOR (10 downto 0);
           x : in  STD_LOGIC_VECTOR (43 downto 0);
           y : out  STD_LOGIC_VECTOR (3 downto 0));
end component;

type anodepattern is array (0 to 15) of std_logic_vector(7 downto 0);
constant hexfont: anodepattern :=(
					pattern0,   --0
					pattern1,   --1
					pattern2,   --2
					pattern3,   --3
					pattern4,   --4
					pattern5,   --5
					pattern6,   --6
					pattern7,   --7
					pattern8,   --8
					pattern9,   --9
					"01110111",   --A
					"01111100",   --b
					"00111001",   --C
					"01011110",   --d
					"01111001",   --E
					"01110001"    --F
);

constant bcdfont: anodepattern :=(
					pattern0,   --0
					pattern1,   --1
					pattern2,   --2
					pattern3,   --3
					pattern4,   --4
					pattern5,   --5
					pattern6,   --6
					pattern7,   --7
					pattern8,   --8
					pattern9,   --9
					"01111001",   -- A (E for "Error")
					"01010000",   -- B (r for "Error")
					"01011100",   -- C (o for "Error")
					"00001000",   --(single segment, should never appear)
					pattern_minus,   --(show minus sign)
					pattern_blank    --blanking
);
--
--type scanpattern is array (0 to 15) of std_logic_vector(13 downto 0);
--constant scantable: scanpattern := (
--					"1011111111" & X"7",
--					"1101111111" & X"6",
--					"1110111111" & X"5",
--					"1111011111" & X"4",
--					"1111101111" & X"3",
--					"1111110111" & X"2",
--					"1111111011" & X"1",
--					"1111111101" & X"0",
--					"1111111110" & X"F",
--					"0111111111" & X"F",
--					"1111111111" & X"F",
--					"1111111111" & X"F",
--					"1111111111" & X"F",
--					"1111111111" & X"F",
--					"1111111111" & X"F",
--					"1111111111" & X"F"
--);

signal scan: std_logic_vector(9 downto 0);

signal hex: std_logic_vector(3 downto 0);
signal seg_hex: std_logic_vector(7 downto 0);

signal bcd: std_logic_vector(3 downto 0);
signal seg_bcd, seg_data, seg_stable: std_logic_vector(7 downto 0);
signal dpoint, dp_sinclair, dp_ti: std_logic;
signal dp: std_logic_vector(7 downto 0);
signal a, a_ti: std_logic_vector(35 downto 0);
-- for blanking logic
signal show, show_sinclair, show_ti, show_propagate, notzero: std_logic;

-- Sinclair specific
signal sign_mantissa: std_logic_vector(3 downto 0);
alias  mantissa: std_logic_vector(19 downto 0) is reg_a(19 downto 0);
signal sign_exponent: std_logic_vector(3 downto 0);
alias  exponent: std_logic_vector(7 downto 0) is reg_a(27 downto 20);

begin

-- drive scan - note that even in debug output mode, nDigit lines drive keyboard correctly
-- but the segments display debug selection 
nDigit 	<= scan(9 downto 1);
digit10 	<= not scan(0);

-- data path			
--seg_dp <= (not sinclair) when (scantable(scan_cnt)(3 downto 0) = dp_pos) else '0'; -- no DP in sinclair mode
--seg_bcd <= bcdfont(to_integer(unsigned(bcd)));
--blank_generate <= (not seg_dp and blank_propagate) when (bcd = X"0") else '0';

a_ti <= X"0000ABBCB" when (show_error = '1') else reg_a; 	-- replace value with "Error" when in TI mode
sign_mantissa <= X"E" when (reg_a(35 downto 32) = X"5") else X"F";	-- for sinclair, minus is encoded as "5", otherwise don't display
sign_exponent <= X"E" when (reg_a(31 downto 28) = X"5") else X"F";	-- for sinclair, minus is encoded as "5", otherwise don't display
a <= sign_mantissa & mantissa & sign_exponent & exponent when (sinclair = '1') else a_ti; 

bcd_mux: mux11x4 port map (
		e => scan & "1",
		x(43 downto 8) => a,
		x(7 downto 0) => X"FF",
		y => bcd
	);
	
notzero <= '0' when (bcd = X"0") else '1';
dp(7) <= (dp_pos(2) 		 and 			dp_pos(1) and 			dp_pos(0));
dp(6) <= (dp_pos(2) 		 and 			dp_pos(1) and 	(not dp_pos(0)));
dp(5) <= (dp_pos(2) 		 and  (not dp_pos(1)) and 			dp_pos(0));
dp(4) <= (dp_pos(2) 		 and  (not dp_pos(1)) and 	(not dp_pos(0)));
dp(3) <= ((not dp_pos(2)) and 			dp_pos(1) and 			dp_pos(0));
dp(2) <= ((not dp_pos(2)) and 			dp_pos(1) and 	(not dp_pos(0)));
dp(1) <= ((not dp_pos(2)) and 	(not dp_pos(1)) and 			dp_pos(0));
dp(0) <= ((not dp_pos(2)) and 	(not dp_pos(1)) and	(not dp_pos(0)));
	
dpblank_mux: mux11x4 port map (
		e => scan & "1",
		x(43 downto 40) => "01" & '0'   &  notzero,
		x(39 downto 36) => "11" & dp(7) &  show_propagate,
		x(35 downto 32) => "01" & dp(6) &  show_propagate,
		x(31 downto 28) => "01" & dp(5) &  show_propagate,
		x(27 downto 24) => "01" & dp(4) &  show_propagate,
		x(23 downto 20) => "01" & dp(3) &  show_propagate,
		x(19 downto 16) => "01" & dp(2) &  show_propagate,
		x(15 downto 12) => "01" & dp(1) &  show_propagate,
		x(11 downto  8) => "01" & dp(0) & '1',
		x( 7 downto  0) => X"FF",
		y(3) => dp_sinclair,
		y(2) => show_sinclair,
		y(1) => dp_ti,
		y(0) => show_ti
	);

dpoint <= dp_sinclair when (sinclair = '1') else dp_ti;
show <= show_sinclair when (sinclair = '1') else show_ti;
seg_bcd <= "00000000" when ((clk and show) = '0') else dpoint & bcdfont(to_integer(unsigned(bcd)))(6 downto 0);
	
-- note that "clk" is a single low pulse coming after each instruction to move the scan forward
-- (toward LSD) - otherwise it is in high state
drive_scan: process(clk, reset, scan)
begin
	if (reset = '1') then
		scan <= "1111111110";
	else
		if (falling_edge(clk)) then
			scan <= scan(0) & scan(9 downto 1);
			if (scan(9) = '0') then
				show_propagate <= '0';
			else
				show_propagate <= show_propagate or dp_ti or notzero;
			end if;
		end if;
	end if;
end process;

-- debug path ---
with dbg_select select
	hex <= 	debug(3 downto 0) 	when "000", 
				debug(7 downto 4) 	when "001", 
				debug(11 downto 8) 	when "010", 
				debug(15 downto 12) 	when "011", 
				debug(19 downto 16) 	when "100", 
				debug(23 downto 20) 	when "101", 
				debug(27 downto 24) 	when "110", 
				debug(31 downto 28) 	when "111";

seg_hex <= hexfont(to_integer(unsigned(hex)));

-- select debug or data path for output to segments
segment <= seg_hex when (show_debug = '1') else seg_bcd;
							
end Behavioral;

