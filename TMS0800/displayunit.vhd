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
					"00000001",   --(single segment, should never appear)
					"00000010",   --(single segment, should never appear)
					"00000100",   --(single segment, should never appear)
					"00001000",   --(single segment, should never appear)
					pattern_minus,   --(show minus sign)
					"00010000"    --(single segment, should never appear)
);

--signal scan_counter: std_logic_vector(3 downto 0);
signal scan: std_logic_vector(9 downto 0);
signal blank, blank_candidate, blank_cascade: std_logic;

signal hex: std_logic_vector(3 downto 0);
signal seg_hex: std_logic_vector(7 downto 0);

signal bcd: std_logic_vector(3 downto 0);
signal seg_bcd, seg_data: std_logic_vector(7 downto 0);
signal seg_dp: std_logic;

begin

-- drive scan - note that even in debug output mode, nDigit lines drive keyboard correctly
-- but the segments display debug selection 
nDigit 	<= scan(9 downto 1);
digit10 	<= not scan(0);

drive_scan: process(clk, scan, reset)
begin
	if (reset = '1') then
		scan <= "1111111110";
	else
		if (rising_edge(clk)) then
			scan <= scan(0) & scan(9 downto 1);
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

-- data path ---
bcdmux: mux11x4 port map 
	(
		e => '1' & scan,
		x(3 downto 0) => "0000",
		x(39 downto 4) => a,
		x(43 downto 40) => "0000",
		y => bcd
	);
					
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
--blank_candidate <= scan(0) and not(seg_dp or bcd(3) or bcd(2) or bcd(1) or bcd(0));
blank_candidate <= (scan(0) and not (seg_dp)) when (bcd = X"0") else '0';

blanking: process(clk, blank_candidate, scan(0))
begin
	if (scan(0) = '0') then
		blank_cascade <= '1';
	else
		if (rising_edge(clk)) then
			blank_cascade <= blank_cascade and blank_candidate;
		end if;
	end if;
end process;

-- enable blanking
blank <= blank_candidate and blank_cascade;

seg_data <= pattern_blank when (blank = '1') else seg_dp & seg_bcd(6 downto 0);

dbg_state <= scan(3 downto 0); --blank & blank_candidate & blank_cascade & scan(10);

-- select path to display
segment <= seg_hex when (show_debug = '1') else seg_data;


end Behavioral;

