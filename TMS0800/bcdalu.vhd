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

type lookup is array (0 to 31) of std_logic_vector(4 downto 0);
constant adcbcd: lookup :=
(
		'0' & X"0",   --0 
		'0' & X"1",   --1
		'0' & X"2",   --2
		'0' & X"3",   --3
		'0' & X"4",   --4
		'0' & X"5",   --5
		'0' & X"6",   --6
		'0' & X"7",   --7
		'0' & X"8",   --8
		'0' & X"9",   --9 
		'1' & X"0",   --A
		'1' & X"1",   --B
		'1' & X"2",   --C
		'1' & X"3",   --D
		'1' & X"4",   --E
		'1' & X"5",   --F 
		'1' & X"6",   --10 
		'1' & X"7",   --11
		'1' & X"8",   --12
		'1' & X"9",   --13 -- 9 + 9 + 1 = 19
		'1' & X"F",   --14 -- error from now on! 
		'1' & X"F",   --15
		'1' & X"F",   --16
		'1' & X"F",   --17 
		'1' & X"F",   --18
		'1' & X"F",   --19
		'1' & X"F",   --1A
		'1' & X"F",   --1B
		'1' & X"F",   --1C 
		'1' & X"F",   --1D
		'1' & X"F",   --1E
		'1' & X"F"	  --1F
);

constant sbcbcd: lookup :=
(
		'0' & X"0",   --0 
		'0' & X"1",   --1
		'0' & X"2",   --2
		'0' & X"3",   --3
		'0' & X"4",   --4
		'0' & X"5",   --5
		'0' & X"6",   --6
		'0' & X"7",   --7
		'0' & X"8",   --8
		'0' & X"9",   --9 
		'1' & X"F",   --A
		'1' & X"F",   --B
		'1' & X"F",   --C
		'1' & X"F",   --D
		'1' & X"F",   --E
		'1' & X"F",   --F 
		'1' & X"F",   --10 
		'1' & X"F",   --11
		'1' & X"F",   --12
		'1' & X"F",   --13 
		'1' & X"F",   --14 
		'1' & X"F",   --15
		'1' & X"0",   --16
		'1' & X"1",   --17 
		'1' & X"2",   --18
		'1' & X"3",   --19
		'1' & X"4",   --1A
		'1' & X"5",   --1B
		'1' & X"6",   --1C 
		'1' & X"7",   --1D
		'1' & X"8",   --1E
		'1' & X"9"	  --1F
);

-- internal inputs
signal rs: std_logic_vector(7 downto 0);
alias r: std_logic_vector(3 downto 0) is rs(7 downto 4);
alias s: std_logic_vector(3 downto 0) is rs(3 downto 0);
signal r_is_hex, s_is_hex, rs_is_hex: std_logic;

signal opr2, sum0, sum1, sum2, dif0, dif1, dif2: std_logic_vector(4 downto 0);

begin

-- input selections
with sel select
		rs <= a & b when src_ab,
				a & k when src_ak,
				c & k when src_ck,
				c & b when src_cb,
				X"00" when others;

-- if r or s were originally hex digits (for example, "E" means minus) then bypass the BCD correction
-- this kicks in when doing AAKC ALL for example if A is a negative number: otherwise 0EXX in A becomes 14XX in C! 
r_is_hex <= (r(3) and (not r(2)) and r(1)) or (r(3) and r(2));
s_is_hex <= (s(3) and (not s(2)) and s(1)) or (s(3) and s(2));
rs_is_hex <= s_is_hex or r_is_hex;

opr2 <= '0' & s when (cin = '0') else std_logic_vector(unsigned('0' & s) + 1);
-- add path
sum0 <= std_logic_vector(unsigned('0' & r) + unsigned(opr2));
sum2 <= sum0 when (rs_is_hex = '1') else adcbcd(to_integer(unsigned(sum0)));
-- sub path
dif0 <= std_logic_vector(unsigned('0' & r) - unsigned(opr2));
dif2 <= dif0 when (rs_is_hex = '1') else sbcbcd(to_integer(unsigned(dif0)));

-- select outputs
with fun select
	cout <=	sum0(4)		when fun_adchex,
				sum2(4)		when fun_adcbcd,
				dif0(4)		when fun_sbchex,
				dif2(4)		when fun_sbcbcd,
				cin			when others;

with fun select
	y <= 	s							when fun_s,
			r							when fun_r,
			(s xor r)				when fun_xor,
			sum0(3 downto 0)		when fun_adchex,
			sum2(3 downto 0)		when fun_adcbcd,
			dif0(3 downto 0)		when fun_sbchex,
			dif2(3 downto 0)		when fun_sbcbcd,
			"0000" 					when others;

end Behavioral;

