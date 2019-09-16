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
			  digit0: out STD_LOGIC;
			  digit10: out STD_LOGIC;
			  dbg_select: in STD_LOGIC_VECTOR(2 downto 0));
end displayunit;

architecture Behavioral of displayunit is

--component mux11x4 is
--    Port ( e : in  STD_LOGIC_VECTOR (10 downto 0);
--           x : in  STD_LOGIC_VECTOR (43 downto 0);
--           y : out  STD_LOGIC_VECTOR (3 downto 0));
--end component;

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

type scanpattern is array (0 to 15) of std_logic_vector(13 downto 0);
constant scantable: scanpattern := (
					"1111111110" & X"F",
					"1111111101" & X"0",
					"1111111011" & X"1",
					"1111110111" & X"2",
					"1111101111" & X"3",
					"1111011111" & X"4",
					"1110111111" & X"5",
					"1101111111" & X"6",
					"1011111111" & X"7",
					"0111111111" & X"8",
					"1111111111" & X"F",
					"1111111111" & X"F",
					"1111111111" & X"F",
					"1111111111" & X"F",
					"1111111111" & X"F",
					"1111111111" & X"F"
);

signal scan_cnt: integer range 0 to 15;
signal scan: std_logic_vector(9 downto 0);
signal blank, blank_propagate: std_logic;

signal hex: std_logic_vector(3 downto 0);
signal seg_hex: std_logic_vector(7 downto 0);

signal bcd: std_logic_vector(3 downto 0);
signal seg_data: std_logic_vector(7 downto 0);
signal dp, dp_ti, dp_sinclair: std_logic;
signal a, a_ti: std_logic_vector(35 downto 0);
signal scanentry: std_logic_vector(13 downto 0);

-- Sinclair specific
signal sign_mantissa: std_logic_vector(3 downto 0);
alias  mantissa: std_logic_vector(19 downto 0) is reg_a(19 downto 0);
signal sign_exponent: std_logic_vector(3 downto 0);
alias  exponent: std_logic_vector(7 downto 0) is reg_a(27 downto 20);

begin

-- drive scan - note that even in debug output mode, nDigit lines drive keyboard correctly
-- but the segments display debug selection 
nDigit 	<= scanentry(13 downto 5);
digit0	<= not scanentry(13);
digit10 	<= not scanentry(4);

-- data path
scanentry  <= scantable(scan_cnt);

a_ti <= X"0000ABBCB" when (show_error = '1') else reg_a; 	-- replace value with "Error" when in TI mode
sign_mantissa <= X"E" when (reg_a(35 downto 32) = X"5") else X"F";	-- for sinclair, minus is encoded as "5", otherwise don't display
sign_exponent <= X"E" when (reg_a(31 downto 28) = X"5") else X"F";	-- for sinclair, minus is encoded as "5", otherwise don't display
a <= sign_mantissa & mantissa & sign_exponent & exponent when (sinclair = '1') else a_ti; 

with scan_cnt select
	bcd <=	a(35 downto 32) when 9, -- digit8
				a(31 downto 28) when 8, -- digit7
				a(27 downto 24) when 7, -- digit6
				a(23 downto 20) when 6, -- digit5
				a(19 downto 16) when 5, -- digit4
				a(15 downto 12) when 4, -- digit3
				a(11 downto  8) when 3, -- digit2
				a( 7 downto  4) when 2, -- digit1
				a( 3 downto  0) when 1, -- digit0
				"0000" when others;

dp_sinclair <= '1' when (scan_cnt = 8) else '0';
dp_ti <= '1' when (scanentry(3 downto 0) = dp_pos) else '0';
dp <= dp_sinclair when (sinclair = '1') else dp_ti;

blank <= '0' when (scan_cnt = 1) else not(dp or bcd(3) or bcd(2) or bcd(1) or bcd(0));
seg_data <= pattern_blank when ((blank and blank_propagate) = '1') else (dp & bcdfont(to_integer(unsigned(bcd)))(6 downto 0));
			
-- note that "clk" is a single low pulse coming after each instruction to move the scan forward
-- (toward LSD) - otherwise it is in high state
drive_scan: process(clk, reset, scan_cnt, sinclair)
begin
	if (reset = '1') then
		scan_cnt <= 9;
		--scanentry  <= scantable(9);
	else
		if (rising_edge(clk)) then
			if (scan_cnt = 0) then
				scan_cnt <= 9;
				--scanentry  <= scantable(9);
				blank_propagate <= not sinclair;
			else
				scan_cnt <= scan_cnt - 1;
				--scanentry  <= scantable(scan_cnt - 1);
				blank_propagate <= blank_propagate and blank;
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
					
-- select debug or data
segment <= seg_hex when (show_debug = '1') else seg_data;
							
end Behavioral;

