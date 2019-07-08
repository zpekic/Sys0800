----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    11:37:53 05/19/2019 
-- Design Name: 
-- Module Name:    sio0800 - Behavioral 
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

entity sio0800 is
    Port ( reset : in  STD_LOGIC;
           clk : in  STD_LOGIC;
           txChar : in  STD_LOGIC_VECTOR (7 downto 0);
           txChar_sent : buffer STD_LOGIC;
           txd : out  STD_LOGIC);
end sio0800;

architecture Behavioral of sio0800 is

signal bitSel: std_logic_vector(3 downto 0);

begin
-- drive simple UART data output with mux
with bitSel select 
		txd <= 		'1'		 when "0000", -- delay 0
						'1'		 when "0001",
						'1'		 when "0010",
						'1' 		 when "0011", -- delay 3
						'0' 		 when "0100", -- start bit
						txChar(0) when "0101", -- data
						txChar(1) when "0110",
						txChar(2) when "0111",
						txChar(3) when "1000",
						txChar(4) when "1001",
						txChar(5) when "1010",
						txChar(6) when "1011",
						txChar(7) when "1100",
						'1' 		 when "1101",	-- stop
						'1' 		 when "1110",	-- additional stop or parity
						'1' when others;			-- delay

-- drive high when all bits transmitted, this signal is fed as a condition to microcode	
txChar_sent <= bitSel(3) and bitSel(2) and bitSel(1); 					

-- when char goes to non-zero, char is being sent to txd, until hits last 2 bitSel values
drivebitSel: process(reset, bitsel, clk, txChar)
begin
	if (txChar = X"00" or reset = '1') then
		bitSel <= X"0";
	else
		if (rising_edge(clk) and bitSel /= X"F") then
			bitSel <= std_logic_vector(unsigned(bitSel) + 1);
		end if;
	end if;
end process;

end Behavioral;

