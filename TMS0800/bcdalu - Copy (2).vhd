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

-- internal inputs
signal rs: std_logic_vector(7 downto 0);
alias r: std_logic_vector(3 downto 0) is rs(7 downto 4);
alias s: std_logic_vector(3 downto 0) is rs(3 downto 0);
signal s_compl9: std_logic_vector(3 downto 0);
signal c_effective, over9: std_logic;
signal sum1, s_effective: std_logic_vector(5 downto 0);				 -- 6 bits == cout & 4 bits & cin 
signal sum2, correction: std_logic_vector(3 downto 0); -- 5 bits == cout & 4 bits

begin

-- input selections
with sel select
		rs <= a & b when src_ab,
				a & k when src_ak,
				c & k when src_ck,
				c & b when src_cb,
				X"00" when others;
				
-- evaluate modify s and carry if needed
s_compl9(3) <= (not s(3)) and (not s(2)) and (not s(1));
s_compl9(2) <= s(2) xor s(1);
s_compl9(1) <= s(1);
s_compl9(0) <= not s(0);

with fun select
	c_effective <= not cin when fun_sbchex,
						not cin when fun_sbcbcd,
						cin	  when others;
with fun select
	s_effective <=	'0' & s & c_effective			when fun_adchex,
						'0' & s & c_effective			when fun_adcbcd,
						'0' & (s xor "1111") & c_effective when fun_sbchex,
						'0' & s_compl9	& c_effective	when fun_sbcbcd,
						'0' & s &'0'						when others;

-- 1st level adder
sum1 <= std_logic_vector(unsigned('0' & r & c_effective) + unsigned(s_effective));
over9 <= sum1(5) or (sum1(4) and sum1(3)) or (sum1(4) and sum1(2)); -- check for sum > 9

-- 2nd level adder
correction <= "0110" when (over9 = '1') else "0000"; -- add "6" when adjustment needed
sum2 <= std_logic_vector(unsigned(correction) + unsigned(sum1(4 downto 1)));

-- select outputs
with fun select
	cout <=	sum1(5)		when fun_adchex,
				over9			when fun_adcbcd,
				not sum1(5)	when fun_sbchex,
				not over9	when fun_sbcbcd,
				cin			when others;

with fun select
	y <= "0000" 					when fun_zero,
			s							when fun_s,
			r							when fun_r,
			(s xor r)				when fun_xor,
			sum1(4 downto 1)		when fun_adchex,
			sum2(3 downto 0)		when fun_adcbcd,
			sum1(4 downto 1)		when fun_sbchex,
			sum2(3 downto 0)		when fun_sbcbcd;

end Behavioral;

