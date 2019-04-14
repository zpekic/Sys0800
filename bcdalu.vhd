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

type complement is array (0 to 15) of std_logic_vector(7 downto 0);
constant lookup: complement :=
(
					X"9F",   --0
					X"8E",   --1
					X"7D",   --2
					X"6C",   --3
					X"5B",   --4
					X"4A",   --5
					X"39",   --6
					X"28",   --7
					X"17",   --8
					X"06",   --9
					X"05",   --A
					X"04",   --B
					X"03",   --C
					X"02",   --D
					X"01",   --E
					X"00"    --F
);

-- some "clever" encoding of function selection
alias arithmetic: std_logic is fun(2);
alias subtract: std_logic is fun(1);
alias bcdmode: std_logic is fun(0);

-- internal inputs
signal rs: std_logic_vector(7 downto 0);
alias r: std_logic_vector(3 downto 0) is rs(7 downto 4);
alias s: std_logic_vector(3 downto 0) is rs(3 downto 0);
signal s_complement: std_logic_vector(7 downto 0); 
signal s_effective: std_logic_vector(3 downto 0);
signal cin_effective, adjust: std_logic;
signal sum1: std_logic_vector(5 downto 0); -- 6 bits == cout & 4 bits & cin
signal correction, sum2: std_logic_vector(4 downto 0); -- 5 bits == cout & 4 bits

begin

-- ALU selections
with sel select
		rs <= a & b when inp_ab,
				a & k when inp_ak,
				c & k when inp_ck,
				c & b when inp_cb,
				X"00" when others;
				
-- carry in 
cin_effective <= cin xor subtract;

-- evaluate s
s_complement <= lookup(to_integer(unsigned(s)));
with fun select
	s_effective <= s_complement(3 downto 0) when fun_sbchex,
						s_complement(7 downto 4) when fun_sbcbcd,
						s when others;

-- 1st level adder
sum1 <= std_logic_vector(unsigned('0' & r & cin_effective) + unsigned('0' & s_effective & cin_effective));
adjust <= (sum1(5) or (sum1(4) and sum1(3)) or (sum1(4) and sum1(2))) when bcdmode = '1' else '0'; -- adjustment needed if bcd and sum > 9

-- 2nd level adder
correction <= "00110" when adjust = '1' else "00000"; -- add "6" when adjustment needed
sum2 <= std_logic_vector(unsigned(correction) + unsigned('0' & sum1(4 downto 1)));

-- adder
--hexsum <= std_logic_vector(unsigned('0' & r & cin_effective) + unsigned('0' & s_effective & cin_effective));
--adjustedsum <= std_logic_vector(unsigned('0' & binarysum(4 downto 1) & '0') + "001100");
--adjust <= '1' when (hexsum(4 downto 2) = "101" or hexsum(4 downto 3) = "11") else hexsum(5);
--bcdsum <= adjustedsum when adjust = '1' else hexsum;

-- select output
cout <= (sum2(4) xor subtract) when arithmetic = '1' else cin;

with fun select
	y <= "0000" 					when fun_zero,
			s							when fun_s,
			r							when fun_r,
			(s xor r)				when fun_xor,
			sum2(3 downto 0)		when others;

end Behavioral;

