----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    12:08:56 03/24/2019 
-- Design Name: 
-- Module Name:    traceunit - Behavioral 
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

entity traceunit is
    Port ( clk : in  STD_LOGIC;
			  clk_txd: in STD_LOGIC;
           reset : in  STD_LOGIC;
           char : in  STD_LOGIC_VECTOR(7 downto 0);
           pc : in  STD_LOGIC_VECTOR (8 downto 0);
           instruction : in  STD_LOGIC_VECTOR (11 downto 0);
           a : in  STD_LOGIC_VECTOR(3 downto 0);
           b : in  STD_LOGIC_VECTOR(3 downto 0);
           c : in  STD_LOGIC_VECTOR(3 downto 0);
           af : in  STD_LOGIC;
           bf : in  STD_LOGIC;
           cf : in  STD_LOGIC;
           char_sent : buffer STD_LOGIC;
           txd : out  STD_LOGIC);
end traceunit;

architecture Behavioral of traceunit is

signal bitSel, hex: std_logic_vector(3 downto 0);
signal muxAscii, asciiOffset, txChar: std_logic_vector(7 downto 0);
signal over9: std_logic;

begin

--select hex/bcd character to display
with char(3 downto 0) select
	hex <= 	"000" & af 						when t_af,
				"000" & bf 						when t_bf,
				"000" & cf 						when t_cf,
				a 									when t_a,
				b 									when t_b,
				c 									when t_c,
				instruction(3 downto 0) 	when t_instr0,
				instruction(7 downto 4) 	when t_instr1,
				instruction(11 downto 8)	when t_instr2,
				pc(3 downto 0) 				when t_pc0,
				pc(7 downto 4) 				when t_pc1,
				"000" & pc(8) 					when t_pc2,
				"0000" when others;

-- convert 4-bit HEX to 8-bit ASCII
over9 <= (hex(3) and hex(2)) or (hex(3) and (not hex(2)) and hex(1)); -- 101X, 11XX
asciiOffset <= X"37" when over9 = '1' else X"30";
muxAscii <= std_logic_vector(unsigned("0000" & hex) + unsigned(asciiOffset));
	
-- if input >127 then it is from mux, otherwise transmit directly
setChar: process(clk, char, muxAscii)
begin
	if (falling_edge(clk)) then
		if (char(7) = '0') then
			txChar <= char;
		else
			txChar <= muxAscii;
		end if;
	end if;
end process;	

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
char_sent <= '1' when bitSel(3 downto 1) = "111" else '0'; 					

-- when char goes to non-zero, char is being sent to txd, until hits last 2 bitSel values
drivebitSel: process(reset, bitsel, clk_txd, txChar)
begin
	if (txChar = X"00" or reset = '1') then
		bitSel <= X"0";
	else
		if (rising_edge(clk_txd) and bitSel /= X"F") then
			bitSel <= std_logic_vector(unsigned(bitSel) + 1);
		end if;
	end if;
end process;

end Behavioral;

