----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    23:19:16 03/11/2019 
-- Design Name: 
-- Module Name:    display_ti - Behavioral 
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

entity display_ti is
    Port ( clk : in  STD_LOGIC;
			  reset: in STD_LOGIC;
           reg_a : in  STD_LOGIC_VECTOR (35 downto 0);
			  show_error: in STD_LOGIC;
           nDPoint : in  STD_LOGIC_VECTOR (9 downto 0);
           nDigit  : in  STD_LOGIC_VECTOR (9 downto 0);
           segment : out  STD_LOGIC_VECTOR (7 downto 0)
			);
end display_ti;

architecture Behavioral of display_ti is

component mux11x4 is
    Port ( e : in  STD_LOGIC_VECTOR (10 downto 0);
           x : in  STD_LOGIC_VECTOR (43 downto 0);
           y : out  STD_LOGIC_VECTOR (3 downto 0));
end component;

signal bcd: std_logic_vector(3 downto 0);
signal dp: std_logic;

-- TI specific
signal a_ti: std_logic_vector(35 downto 0);
signal blank, blank_propagate: std_logic;

begin

-- data path

a_ti <= X"0000ABBCB" when (show_error = '1') else reg_a; 	-- replace value with "Error" when in TI mode

digit_mux: mux11x4 port map 
	(
		e => nDigit & "1",
		x(43 downto 8) => a_ti,
		x( 7 downto  0) => X"FF",
		y => bcd
	);

dp <= '1' when (nDigit = nDPoint) else '0';
blank <= '0' when (nDigit = "1111111101") else not(dp or bcd(3) or bcd(2) or bcd(1) or bcd(0));
segment <= pattern_blank when ((blank and blank_propagate) = '1') else (dp & bcdfont(to_integer(unsigned(bcd)))(6 downto 0));

drive_blanking: process(clk, reset, nDigit)
begin
	if (rising_edge(clk)) then
		if (nDigit = "1111111110") then
			blank_propagate <= '1';
		else
			blank_propagate <= blank_propagate and blank;
		end if;
	end if;
end process;
							
end Behavioral;

