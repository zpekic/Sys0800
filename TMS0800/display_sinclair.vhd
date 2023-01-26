----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    23:19:16 03/11/2019 
-- Design Name: 
-- Module Name:    display_sinclair - Behavioral 
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

entity display_sinclair is
    Port ( clk : in  STD_LOGIC;
			  reset: in STD_LOGIC;
           reg_a : in  STD_LOGIC_VECTOR (35 downto 0);
			  show_error: in STD_LOGIC;
           nDPoint : in  STD_LOGIC_VECTOR (9 downto 0);
           nDigit  : in  STD_LOGIC_VECTOR (9 downto 0);
           segment : out  STD_LOGIC_VECTOR (7 downto 0)
			);
end display_sinclair;

architecture Behavioral of display_sinclair is

component mux11x4 is
    Port ( e : in  STD_LOGIC_VECTOR (10 downto 0);
           x : in  STD_LOGIC_VECTOR (43 downto 0);
           y : out  STD_LOGIC_VECTOR (3 downto 0));
end component;

signal bcd: std_logic_vector(3 downto 0);
signal dp: std_logic;

-- Sinclair specific
signal sign_mantissa: std_logic_vector(3 downto 0);
signal sign_exponent: std_logic_vector(3 downto 0);

begin

sign_mantissa <= X"E" when (reg_a(35 downto 32) = X"5") else X"F";	-- for sinclair, minus is encoded as "5", otherwise don't display
sign_exponent <= X"E" when (reg_a(31 downto 28) = X"5") else X"F";	-- for sinclair, minus is encoded as "5", otherwise don't display

digit_mux: mux11x4 port map 
	(
		e => nDigit & "1",
		x(43 downto 40) => sign_mantissa,
		x(39 downto 20) => reg_a(19 downto 0),		-- mantissa
		x(19 downto 16) => sign_exponent,
		x(15 downto  8) => reg_a(27 downto 20),	-- exponent
		x( 7 downto  0) => X"FF",
		y => bcd
	);

dp <= '1' when (nDigit = nDPoint) else '0';

segment <= dp & bcdfont(to_integer(unsigned(bcd)))(6 downto 0);
						
end Behavioral;

