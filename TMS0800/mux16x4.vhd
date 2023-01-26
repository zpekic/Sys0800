----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    21:32:01 01/23/2023 
-- Design Name: 
-- Module Name:    mux16x4 - Behavioral 
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

entity mux16x4 is
    Port ( s : in  STD_LOGIC_VECTOR (3 downto 0);
           x : in  STD_LOGIC_VECTOR (43 downto 0);
           y : out  STD_LOGIC_VECTOR (3 downto 0));
end mux16x4;

architecture Behavioral of mux16x4 is

begin

with s select y <=	
	x(43 downto 40) when "1010", -- 10
	x(39 downto 36) when "1001", -- 9
	x(35 downto 32) when "1000", -- 8
	x(31 downto 28) when "0111", -- 7
	x(27 downto 24) when "0110", -- 6
	x(23 downto 20) when "0101", -- 5
	x(19 downto 16) when "0100", -- 4
	x(15 downto 12) when "0011", -- 3
	x(11 downto  8) when "0010", -- 2
	x( 7 downto  4) when "0001", -- 1
	x( 3 downto  0) when "0000", -- 0
	X"0" when others;

end Behavioral;

