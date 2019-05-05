----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    22:24:08 04/03/2019 
-- Design Name: 
-- Module Name:    mux11x4 - Behavioral 
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
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity mux11x4 is
    Port ( e : in  STD_LOGIC_VECTOR (10 downto 0);
           x : in  STD_LOGIC_VECTOR (43 downto 0);
           y : out  STD_LOGIC_VECTOR (3 downto 0));
end mux11x4;

architecture Behavioral of mux11x4 is

begin
with e select
	y <= 	x(3  downto  0) when "11111111110", --X"0",
			x(7  downto  4) when "11111111101", --X"1",
			x(11 downto  8) when "11111111011", --X"2",
			x(15 downto 12) when "11111110111", --X"3",
			x(19 downto 16) when "11111101111", --X"4",
			x(23 downto 20) when "11111011111", --X"5",
			x(27 downto 24) when "11110111111", --X"6",
			x(31 downto 28) when "11101111111", --X"7",
			x(35 downto 32) when "11011111111", --X"8",
			x(39 downto 36) when "10111111111", --X"9",
			x(43 downto 40) when "01111111111", --X"A",
			"1111" when others; -- TODO change to 0 or F?

end Behavioral;

