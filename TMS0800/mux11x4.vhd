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

mux_generate: for i in 0 to 3 generate
begin
	y(i) <= 	(e(10) or x(40 + i)) and 
				(e(9) or x(36 + i)) and
				(e(8) or x(32 + i)) and
				(e(7) or x(28 + i)) and
				(e(6) or x(24 + i)) and
				(e(5) or x(20 + i)) and
				(e(4) or x(16 + i)) and
				(e(3) or x(12 + i)) and
				(e(2) or x(8 + i)) and
				(e(1) or x(4 + i)) and
				(e(0) or x(0 + i));
end generate;

end Behavioral;

