----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    10:33:10 04/07/2019 
-- Design Name: 
-- Module Name:    sambit - Behavioral 
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
use work.tms0800_package.all;

entity sambit is
    Port ( clk : in  STD_LOGIC;
           sel : in  STD_LOGIC_VECTOR (1 downto 0);
           nEnable : in  STD_LOGIC;
           m : in  STD_LOGIC;
			  inp: in STD_LOGIC;
           flag : out  STD_LOGIC);
end sambit;

architecture Behavioral of sambit is

signal fbit: std_logic := '1'; -- TODO remove value

begin

flag <= fbit;

flag_update: process(clk, nEnable, sel, m, inp)
begin
	if (nEnable = '0' and m = '1') then
		if (rising_edge(clk)) then
			case sel is
				when bit_zero =>
					fbit <= '0';
				when bit_load =>
					fbit <= inp;
				when bit_invert =>
					fbit <= not fbit;
				when others =>
					null;
			end case;
		end if;
	end if;
end process;

end Behavioral;

