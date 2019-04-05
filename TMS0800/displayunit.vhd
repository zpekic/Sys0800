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
           a : in  STD_LOGIC_VECTOR (35 downto 0);
           debug : in  STD_LOGIC_VECTOR (31 downto 0);
           dp_pos : in  STD_LOGIC_VECTOR(3 downto 0);
           show_debug : in  STD_LOGIC;
           segment : out  STD_LOGIC_VECTOR (7 downto 0);
           nDigit : out  STD_LOGIC_VECTOR (8 downto 0);
			  scan_start: out STD_LOGIC;
			  scan_end: out STD_LOGIC;
			  dbg_state: out STD_LOGIC_VECTOR(3 downto 0));
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
					"00000001",   --(single segment, should never appear)
					"00000010",   --(single segment, should never appear)
					"00000100",   --(single segment, should never appear)
					"00001000",   --(single segment, should never appear)
					pattern_minus,   --(show minus sign)
					"00010000"    --(single segment, should never appear)
);

--signal scan_counter: std_logic_vector(3 downto 0);
signal scan: std_logic_vector(10 downto 0);
signal blank, blank_candidate, blank_cascade: std_logic;

signal hex: std_logic_vector(3 downto 0);
signal seg_hex: std_logic_vector(7 downto 0);

signal bcd: std_logic_vector(3 downto 0);
signal seg_bcd, seg_data: std_logic_vector(7 downto 0);
signal seg_dp: std_logic;

begin

scan_start  <= not scan(10);
nDigit 		<= scan(9 downto 1);
scan_end 	<= not scan(0);
							
-- debug path ---
hexmux: mux11x4 port map 
	(
		e => scan,
		x(3 downto 0) => "0000",
		x(35 downto 4) => debug,
		x(43 downto 36) => X"00",
		y => hex
	);

--with scan select
--	hex <= 	debug(3  downto  0) when "11111111101", --when X"0",
--				debug(7  downto  4) when "11111111011", --when X"1",
--				debug(11 downto  8) when "11111110111", --when X"2",
--				debug(15 downto 12) when "11111101111", --when X"3",
--				debug(19 downto 16) when "11111011111", --when X"4",
--				debug(23 downto 20) when "11110111111", --when X"5",
--				debug(27 downto 24) when "11101111111", --when X"6",
--				debug(31 downto 28) when "11011111111", --when X"7",
--				"0000" when others;

seg_hex <= hexfont(to_integer(unsigned(hex)));

-- data path ---
bcdmux: mux11x4 port map 
	(
		e => scan,
		x(3 downto 0) => "0000",
		x(39 downto 4) => a,
		x(43 downto 40) => "0000",
		y => bcd
	);
	
--with scan select
--	bcd <= 	a(3  downto  0) when "11111111101", --X"0",
--				a(7  downto  4) when "11111111011", --X"1",
--				a(11 downto  8) when "11111110111", --X"2",
--				a(15 downto 12) when "11111101111", --X"3",
--				a(19 downto 16) when "11111011111", --X"4",
--				a(23 downto 20) when "11110111111", --X"5",
--				a(27 downto 24) when "11101111111", --X"6",
--				a(31 downto 28) when "11011111111", --X"7",
--				a(35 downto 32) when "10111111111", --X"8",
--				"0000" when others;
				
seg_bcd <= bcdfont(to_integer(unsigned(bcd)));

-- decimal point dot
with dp_pos select
	seg_dp <= 	not scan(1) when X"0",
					not scan(2) when X"1",
					not scan(3) when X"2",
					not scan(4) when X"3",
					not scan(5) when X"4",
					not scan(6) when X"5",
					not scan(7) when X"6",
					not scan(8) when X"7",
					'0' when others;

-- blanking logic
blank_candidate <= scan(0) and not(seg_dp or bcd(3) or bcd(2) or bcd(1) or bcd(0));

blanking: process(clk, blank_candidate, scan(10))
begin
	if (scan(10) = '0') then
		blank_cascade <= '1';
	else
		if (rising_edge(clk)) then
			blank_cascade <= blank_cascade and blank_candidate;
		end if;
	end if;
end process;

blank <= blank_candidate and blank_cascade;

seg_data <= pattern_blank when (blank = '1') else seg_dp & seg_bcd(6 downto 0);

dbg_state <= blank & blank_candidate & blank_cascade & scan(10);

-- select path to display
segment <= seg_hex when (show_debug = '1') else seg_data;

drive_scan: process(clk, scan, reset)
begin
	if (reset = '1') then
		scan <= "01111111111";
	else
		if (rising_edge(clk)) then
			scan <= scan(0) & scan(10 downto 1);
		end if;
	end if;
end process;

		
---- count digit downwards to allow zero blanking		
--scan: process(clk, scan_counter, show_debug, is_blankable)
--begin
--	if (rising_edge(clk)) then
--		scan_counter <= std_logic_vector(unsigned(scan_counter) - 1);
--		case scan_counter is
--			when X"9" => -- set initial blanking enable before first digit is displayed
--				enable_blanking <= not show_debug;
--			when X"8" | X"7" | X"6" | X"5" | X"4" | X"3" | X"2" => -- continue enabling based on current value
--				enable_blanking <= enable_blanking and not (is_blankable);
--			when X"1" => -- LSD always shows, even if 0
--				enable_blanking <= '0';
--			when others =>
--				null;
--		end case;
--	end if;
--end process;

end Behavioral;

