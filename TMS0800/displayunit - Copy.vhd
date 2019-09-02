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
					"1011111111" & X"7",
					"1101111111" & X"6",
					"1110111111" & X"5",
					"1111011111" & X"4",
					"1111101111" & X"3",
					"1111110111" & X"2",
					"1111111011" & X"1",
					"1111111101" & X"0",
					"1111111110" & X"F",
					"0111111111" & X"F",
					"1111111111" & X"F",
					"1111111111" & X"F",
					"1111111111" & X"F",
					"1111111111" & X"F",
					"1111111111" & X"F",
					"1111111111" & X"F"
);

signal scan_cnt: integer range 0 to 15;
signal scan: std_logic_vector(9 downto 0);
signal blank_generate, blank_propagate: std_logic;

signal hex: std_logic_vector(3 downto 0);
signal seg_hex: std_logic_vector(7 downto 0);

signal bcd: std_logic_vector(3 downto 0);
signal seg_bcd, seg_data, seg_stable: std_logic_vector(7 downto 0);
signal seg_dp: std_logic;
signal a, a_ti: std_logic_vector(35 downto 0);

-- Sinclair specific
signal sign_mantissa: std_logic_vector(3 downto 0);
alias  mantissa: std_logic_vector(19 downto 0) is reg_a(19 downto 0);
signal sign_exponent: std_logic_vector(3 downto 0);
alias  exponent: std_logic_vector(7 downto 0) is reg_a(27 downto 20);

-- delay line for clock
signal clk1, clk2, clk3, clk4, clk5, clk6, clk7, clk8: std_logic;
attribute keep: boolean;
attribute keep of clk1, clk2, clk3, clk4, clk5, clk6, clk7, clk8: signal is true;

begin

-- drive scan - note that even in debug output mode, nDigit lines drive keyboard correctly
-- but the segments display debug selection 
nDigit 	<= scan(9 downto 1);
digit10 	<= not scan(0);

-- data path
with scan_cnt select
	bcd <=	a(35 downto 32) when 8, -- digit8
				a(31 downto 28) when 0, -- digit7
				a(27 downto 24) when 1, -- digit6
				a(23 downto 20) when 2, -- digit5
				a(19 downto 16) when 3, -- digit4
				a(15 downto 12) when 4, -- digit3
				a(11 downto  8) when 5, -- digit2
				a( 7 downto  4) when 6, -- digit1
				a( 3 downto  0) when 7, -- digit0
				"0000" when others;
			
seg_dp <= (not sinclair) when (scantable(scan_cnt)(3 downto 0) = dp_pos) else '0'; -- no DP in sinclair mode
seg_bcd <= bcdfont(to_integer(unsigned(bcd)));
blank_generate <= (not seg_dp and blank_propagate) when (bcd = X"0") else '0';

a_ti <= X"0000ABBCB" when (show_error = '1') else reg_a; 	-- replace value with "Error" when in TI mode
sign_mantissa <= X"E" when (reg_a(35 downto 32) = X"5") else X"F";	-- for sinclair, minus is encoded as "5", otherwise don't display
sign_exponent <= X"E" when (reg_a(31 downto 28) = X"5") else X"F";	-- for sinclair, minus is encoded as "5", otherwise don't display
a <= sign_mantissa & mantissa & sign_exponent & exponent when (sinclair = '1') else a_ti; 

--bcd_mux: mux11x4 port map (
--		e => scan & "1",
--		x(43 downto 8) => a,
--		x(7 downto 0) => X"FF",
--		y => bcd
--	);
	
-- note that "clk" is a single low pulse coming after each instruction to move the scan forward
-- (toward LSD) - otherwise it is in high state
drive_scan: process(clk, reset, scan_cnt, sinclair)
begin
	if (reset = '1') then
		scan_cnt <= 0;
	else
		if (rising_edge(clk)) then
			scan <= scantable(scan_cnt)(13 downto 4);
			if (blank_generate = '1') then
				seg_data <= pattern_blank;
			else
				seg_data <= seg_dp & seg_bcd(6 downto 0);
			end if;
			if (scan_cnt = 9) then
				scan_cnt <= 0;
				blank_propagate <= not sinclair;
			else
				scan_cnt <= scan_cnt + 1;
				blank_propagate <= blank_generate;
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
					
-- select path to display
--clk1 <= not clk;
--clk2 <= not clk1;
--clk3 <= not clk2;
--clk4 <= not clk3;
--clk5 <= not clk4;
--clk6 <= not clk5;
--clk7 <= not clk6;
--clk8 <= not clk7;

seg_stable <= "00000000" when (clk = '0') else seg_data;
segment <= seg_hex when (show_debug = '1') else seg_stable;

--drive_scan: process(clk, scan_cnt, dp_match, reset, a)
--begin
--	if (reset = '1') then
--		scan_cnt <= 0;
--		scan <= "1111111111";
--	else
--		if (rising_edge(clk)) then
--			-- light-up decimal dot if the next position will match
--			seg_dp <= dp_match;
--			-- bring down one scan line at the time and multiplex in the right digit of A register
--			case scan_cnt is
--				when 9 =>
--					scan <= "1011111111";
--					if (sinclair = '1') then
--						seg_bcd <= bcdfont(to_integer(unsigned(a(19 downto 16))));
--						blank <= '0';
--					else
--						seg_bcd <= bcdfont(to_integer(unsigned(a(31 downto 28))));
--						blank <= blank and not(dp_match or a(31) or a(30) or a(29) or a(28));
--					end if;
--					scan_cnt <= 8;
--				when 8 =>
--					scan <= "1101111111";
--					if (sinclair = '1') then
--						seg_bcd <= bcdfont(to_integer(unsigned(a(15 downto 12))));
--						blank <= '0';
--					else
--						seg_bcd <= bcdfont(to_integer(unsigned(a(27 downto 24))));
--						blank <= blank and not(dp_match or a(27) or a(26) or a(25) or a(24));
--					end if;
--					scan_cnt <= 7;
--				when 7 =>
--					scan <= "1110111111";
--					if (sinclair = '1') then
--						seg_bcd <= bcdfont(to_integer(unsigned(a(11 downto 8))));
--						blank <= '0';
--					else
--						seg_bcd <= bcdfont(to_integer(unsigned(a(23 downto 20))));
--						blank <= blank and not(dp_match or a(23) or a(22) or a(21) or a(20));
--					end if;
--					scan_cnt <= 6;
--				when 6 =>
--					scan <= "1111011111";
--					if (sinclair = '1') then
--						seg_bcd <= bcdfont(to_integer(unsigned(a(7 downto 4))));
--						blank <= '0';
--					else
--						seg_bcd <= bcdfont(to_integer(unsigned(a(19 downto 16))));
--						blank <= blank and not(dp_match or a(19) or a(18) or a(17) or a(16));
--					end if;
--					scan_cnt <= 5;
--				when 5 =>
--					scan <= "1111101111";
--					if (sinclair = '1') then
--						seg_bcd <= bcdfont(to_integer(unsigned(a(3 downto 0))));
--						blank <= '0';
--					else
--						seg_bcd <= bcdfont(to_integer(unsigned(a(15 downto 12))));
--						blank <= blank and not(dp_match or a(15) or a(14) or a(13) or a(12));
--					end if;
--					scan_cnt <= 4;
--				when 4 =>
--					scan <= "1111110111";
--					if (sinclair = '1') then -- exponent sign
--						seg_bcd <= bcdfont(14);	-- display minus if value is "5"
--						blank <= not(a(31) and (not a(30)) and (not a(29)) and a(28));
--					else
--						seg_bcd <= bcdfont(to_integer(unsigned(a(11 downto 8))));
--						blank <= blank and not(dp_match or a(11) or a(10) or a(9) or a(8));
--					end if;
--					scan_cnt <= 3;
--				when 3 =>
--					scan <= "1111111011";
--					if (sinclair = '1') then
--						seg_bcd <= bcdfont(to_integer(unsigned(a(27 downto 24)))); -- exponent MSD
--						blank <= '0';
--					else
--						seg_bcd <= bcdfont(to_integer(unsigned(a(7 downto 4))));
--						blank <= blank and not(dp_match or a(7) or a(6) or a(5) or a(4));
--					end if;
--					scan_cnt <= 2;
--				when 2 =>
--					scan <= "1111111101";
--					blank <= '0';
--					if (sinclair = '1') then
--						seg_bcd <= bcdfont(to_integer(unsigned(a(23 downto 20))));	-- exponent LSD
--					else
--						seg_bcd <= bcdfont(to_integer(unsigned(a(3 downto 0))));	-- mantissa LSD
--					end if;
--					scan_cnt <= 1;
--				when 1 =>
--					scan <= "1111111110";
--					seg_bcd <= bcdfont(0);
--					blank <= '1';
--					scan_cnt <= 0;
--				when 0 =>
--					scan <= "0111111111";
--					if (sinclair = '1') then -- mantissa sign
--						seg_bcd <= bcdfont(14);	-- display minus if value is "5"
--						blank <= not(a(35) and (not a(34)) and (not a(33)) and a(32));
--					else
--						seg_bcd <= bcdfont(to_integer(unsigned(a(35 downto 32))));
--						blank <= blank and not(dp_match or a(35) or a(34) or a(33) or a(32));
--					end if;
--					scan_cnt <= 9;
--				when others =>
--					scan <= "1111111111";
--					seg_bcd <= bcdfont(0);
--					blank <= '1';
--					scan_cnt <= 9;
--			end case;
--		end if;
--	end if;
--end process;
							
end Behavioral;

