----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    12:11:41 04/06/2019 
-- Design Name: 
-- Module Name:    bcdalu - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 	See "Digital Computer Arithmetic" by Joseph J.F. Cavanagh, 
-- 					p. 311 (5.2.3. Addition Using Multiplexer)  
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

entity bcdalu is
    Port ( fun : in  STD_LOGIC_VECTOR (2 downto 0);
           sel : in  STD_LOGIC_VECTOR (2 downto 0);
           cin : in  STD_LOGIC;
           a : in  STD_LOGIC_VECTOR (3 downto 0);
           b : in  STD_LOGIC_VECTOR (3 downto 0);
           c : in  STD_LOGIC_VECTOR (3 downto 0);
           k : in  STD_LOGIC_VECTOR (3 downto 0);
           y : out  STD_LOGIC_VECTOR (3 downto 0);
           cout : out  STD_LOGIC);
end bcdalu;

architecture Behavioral of bcdalu is

type lookup is array (0 to 15) of std_logic_vector(31 downto 0);
constant offset: lookup :=
(
		X"9010F010",   --0 
		X"8921EF21",   --1
		X"7832DE32",   --2
		X"6743CD43",   --3
		X"5654BC54",   --4
		X"4565AB65",   --5
		X"34769A76",   --6
		X"23878987",   --7
		X"12987898",   --8
		X"01A967A9",   --9 
		X"000056BA",   --A
		X"000045CB",   --B
		X"000034DC",   --C
		X"000023ED",   --D
		X"000012FE",   --E
		X"0000010F"    --F 00000010 
);

-- some "clever" encoding of function selection
alias arithmetic: std_logic is fun(2);
alias subtract: std_logic is fun(1);
alias bcdmode: std_logic is fun(0);

-- internal inputs
signal rs: std_logic_vector(7 downto 0);
alias r: std_logic_vector(3 downto 0) is rs(7 downto 4);
alias s: std_logic_vector(3 downto 0) is rs(3 downto 0);
signal s_offset: std_logic_vector(31 downto 0);
signal offset_mode: std_logic_vector(2 downto 0); 
signal s_effective: std_logic_vector(4 downto 0);
signal adjust, s0, s9, sf, over9: std_logic;
signal sum1, sum2, correction: std_logic_vector(4 downto 0); -- 5 bits == cout & 4 bits

begin

-- input selections
with sel select
		rs <= a & b when src_ab,
				a & k when src_ak,
				c & k when src_ck,
				c & b when src_cb,
				X"00" when others;
				
-- evaluate s
s0 <= '1' when s = X"0" else '0';
s9 <= '1' when s = X"9" else '0';
sf <= '1' when s = X"F" else '0';

s_offset <= offset(to_integer(unsigned(s)));
offset_mode <= bcdmode & subtract & cin;

with offset_mode select
		s_effective <= '0' & s_offset( 3 downto  0) when "000", -- hex adc, no carry (cin = 0)
							sf & s_offset( 7 downto  4) when "001",	-- hex adc, carry (cin = 1)
							'0' & s_offset(11 downto  8) when "010",	-- hex sbc, no borrow (cin = 0)
							'0' & s_offset(15 downto 12) when "011", -- hex sbc, borrow (cin = 1)
							'0' & s_offset(19 downto 16) when "100", -- bcd adc, no carry (cin = 0)
							'0' & s_offset(23 downto 20) when "101",	-- bcd adc, carry (cin = 1)
							'0' & s_offset(27 downto 24) when "110",	-- bcd sbc, no borrow (cin = 0)
							'0' & s_offset(31 downto 28) when "111"; -- bcd sbc, borrow (cin = 1)

-- 1st level adder
sum1 <= std_logic_vector(unsigned('0' & r) + unsigned(s_effective));
over9 <= sum1(4) or (sum1(3) and sum1(2)) or (sum1(3) and sum1(1)); -- check for sum > 9

-- 2nd level adder
correction <= "00110" when (bcdmode = '1' and over9 = '1') else "00000"; -- add "6" when adjustment needed
sum2 <= std_logic_vector(unsigned(correction) + unsigned(sum1));

-- select outputs
cout <= (sum2(4) xor subtract) when arithmetic = '1' else cin;

with fun select
	y <= "0000" 					when fun_zero,
			s							when fun_s,
			r							when fun_r,
			(s xor r)				when fun_xor,
			sum2(3 downto 0)		when others;

end Behavioral;

