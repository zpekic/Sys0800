----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    22:52:00 04/20/2019 
-- Design Name: 
-- Module Name:    freqmux - Behavioral 
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

entity freqmux is
    Port ( reset : in  STD_LOGIC;
           f0in : in  STD_LOGIC;
           f1in : in  STD_LOGIC;
           sel : in  STD_LOGIC;
           fout : out  STD_LOGIC);
end freqmux;

architecture Behavioral of freqmux is

signal s1, s0, i1, i0, f: std_logic;

begin

f <= (s0 and (i0 xor f0in)) or (s1 and (i1 xor f1in));
fout <= f;

switch: process(reset, f, sel, s1, s0)
begin
	if (reset = '1') then
		s0 <= '1';
		s1 <= '0';
		i0 <= '0';
		i1 <= '0';
	else
		if (rising_edge(f)) then
			if (sel = '0' and s1 = '1') then -- switching from source 1 to 0
				s0 <= '1';
				s1 <= '0';
				i0 <= not f0in; -- flip new input f to keep the high level during switch
			end if;
			if (sel = '1' and s0 = '1') then -- switching from source 0 to 1
				s0 <= '0';
				s1 <= '1';
				i1 <= not f1in; -- flip new input f to keep the high level during switch
			end if;
		end if;
	end if;
end process;

end Behavioral;

