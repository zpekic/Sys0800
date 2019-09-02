----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    22:00:47 04/01/2019 
-- Design Name: 
-- Module Name:    samdigit - Behavioral 
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

entity samdigit is
    Port ( clk : in  STD_LOGIC;
           sel : in  STD_LOGIC_VECTOR(1 downto 0);
           in1 : in  STD_LOGIC_VECTOR(3 downto 0);
           in2 : in  STD_LOGIC_VECTOR(3 downto 0);
			  in3 : in  STD_LOGIC_VECTOR(3 downto 0);
           nEnable : in  STD_LOGIC;
           mleft : in  STD_LOGIC;
           m : in  STD_LOGIC;
           mright : in  STD_LOGIC;
           digit : out STD_LOGIC_VECTOR(3 downto 0));
end samdigit;

architecture Behavioral of samdigit is

signal in_l, in_r: std_logic_vector(3 downto 0);
signal val: std_logic_vector(3 downto 0);-- := X"9"; -- TODO remove value
--signal effectiveSel: std_logic_vector(1 downto 0);

begin

in_l <= in1 when (mleft = '1') else "0000";
in_r <= in2 when (mright = '1') else "0000";
digit <= val;

--effectiveSel <= sel when (m = '1' and nEnable = '0') else bcd_nop;

y_update: process(clk, sel, m, nEnable, in_l, in_r)
begin
	if (m = '1' and nEnable = '0') then
		if (rising_edge(clk)) then
			case sel is
				when bcd_fromleft =>
					val <= in_l;
				when bcd_fromright =>
					val <= in_r;
				when bcd_fromalu =>
					val <= in3;
				when others =>
					val <= val;
			end case;
		end if;
	end if;
end process;

end Behavioral;

