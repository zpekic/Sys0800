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
           reg_a : in  STD_LOGIC_VECTOR (35 downto 0);
           debug : in  STD_LOGIC_VECTOR (31 downto 0);
           dp_pos : in  STD_LOGIC_VECTOR(3 downto 0);
           show_debug : in  STD_LOGIC;
			  show_error: in STD_LOGIC;
           segment : out  STD_LOGIC_VECTOR (7 downto 0);
           nDigit : out  STD_LOGIC_VECTOR (8 downto 0);
			  digit10: out STD_LOGIC;
			  dbg_select: in STD_LOGIC_VECTOR(2 downto 0);
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
					"01111001",   -- A (E for "Error")
					"01010000",   -- B (r for "Error")
					"01011100",   -- C (o for "Error")
					"00001000",   --(single segment, should never appear)
					pattern_minus,   --(show minus sign)
					"00010000"    --(single segment, should never appear)
);

signal scan_cnt: integer range 0 to 15;
signal scan: std_logic_vector(9 downto 0);
signal blank: std_logic;

signal hex: std_logic_vector(3 downto 0);
signal seg_hex: std_logic_vector(7 downto 0);

signal bcd: std_logic_vector(3 downto 0);
signal seg_bcd, seg_data: std_logic_vector(7 downto 0);
signal seg_dp: std_logic;
signal dp_match: std_logic;
signal a: std_logic_vector(35 downto 0);

begin

-- drive scan - note that even in debug output mode, nDigit lines drive keyboard correctly
-- but the segments display debug selection 
nDigit 	<= scan(9 downto 1);
digit10 	<= not scan(0);

-- data path
dp_match <= '1' when (scan_cnt = unsigned(dp_pos) + 2) else '0';
seg_bcd <= bcdfont(to_integer(unsigned(bcd)));
seg_data <= pattern_blank when (blank = '1') else seg_dp & seg_bcd(6 downto 0);

a <= X"0000ABBCB" when (show_error = '1') else reg_a; -- replace value with "Error"

drive_scan: process(clk, scan_cnt, dp_match, reset, a)
begin
	if (reset = '1') then
		scan_cnt <= 0;
		scan <= "1111111111";
	else
		if (rising_edge(clk)) then
			-- count down 9 ... 0
			if (scan_cnt = 0) then
				scan_cnt <= 9;
			else
				scan_cnt <= scan_cnt - 1;
			end if;
			-- light-up decimal dot if the next position will match
			seg_dp <= dp_match;
			-- bring down one scan line at the time and multiplex in the right digit of A register
			case scan_cnt is
				when 9 =>
					scan <= "1011111111";
					bcd <= a(31 downto 28);
					blank <= blank and not(dp_match or a(31) or a(30) or a(29) or a(28));
				when 8 =>
					scan <= "1101111111";
					bcd <= a(27 downto 24);
					blank <= blank and not(dp_match or a(27) or a(26) or a(25) or a(24));
				when 7 =>
					scan <= "1110111111";
					bcd <= a(23 downto 20);
					blank <= blank and not(dp_match or a(23) or a(22) or a(21) or a(20));
				when 6 =>
					scan <= "1111011111";
					bcd <= a(19 downto 16);
					blank <= blank and not(dp_match or a(19) or a(18) or a(17) or a(16));
				when 5 =>
					scan <= "1111101111";
					bcd <= a(15 downto 12);
					blank <= blank and not(dp_match or a(15) or a(14) or a(13) or a(12));
				when 4 =>
					scan <= "1111110111";
					bcd <= a(11 downto 8);
					blank <= blank and not(dp_match or a(11) or a(10) or a(9) or a(8));
				when 3 =>
					scan <= "1111111011";
					bcd <= a(7 downto 4);
					blank <= blank and not(dp_match or a(7) or a(6) or a(5) or a(4));
				when 2 =>
					scan <= "1111111101";
					bcd <= a(3 downto 0);
					blank <= '0';
				when 1 =>
					scan <= "1111111110";
					bcd <= X"0";
					blank <= '1';
				when 0 =>
					scan <= "0111111111";
					bcd <= a(35 downto 32);
					blank <= blank and not(dp_match or a(35) or a(34) or a(33) or a(32));
				when others =>
					scan <= "1111111111";
					bcd <= X"0";
					blank <= '1';
			end case;
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
dbg_state <= scan(3 downto 0); --blank & blank_candidate & blank_cascade & scan(10);
					
-- select path to display
segment <= seg_hex when (show_debug = '1') else seg_data;

end Behavioral;

