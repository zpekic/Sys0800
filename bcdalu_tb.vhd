--------------------------------------------------------------------------------
-- Company: 
-- Engineer:
--
-- Create Date:   19:50:22 06/08/2019
-- Design Name:   
-- Module Name:   C:/Users/zoltanp/Documents/HexCalc/Sys0800/bcdalu_tb.vhd
-- Project Name:  sys0800
-- Target Device:  
-- Tool versions:  
-- Description:   
-- 
-- VHDL Test Bench Created by ISE for module: bcdalu
-- 
-- Dependencies:
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
--
-- Notes: 
-- This testbench has been automatically generated using types std_logic and
-- std_logic_vector for the ports of the unit under test.  Xilinx recommends
-- that these types always be used for the top-level I/O of a design in order
-- to guarantee that the testbench will bind correctly to the post-implementation 
-- simulation model.
--------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
 
-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
USE ieee.numeric_std.ALL;
use work.tms0800_package.all;
 
ENTITY bcdalu_tb IS
END bcdalu_tb;
 
ARCHITECTURE behavior OF bcdalu_tb IS 
 
    -- Component Declaration for the Unit Under Test (UUT)
 
    COMPONENT bcdalu
    PORT(
         fun : IN  std_logic_vector(2 downto 0);
         sel : IN  std_logic_vector(2 downto 0);
         cin : IN  std_logic;
         a : IN  std_logic_vector(3 downto 0);
         b : IN  std_logic_vector(3 downto 0);
         c : IN  std_logic_vector(3 downto 0);
         k : IN  std_logic_vector(3 downto 0);
         y : OUT  std_logic_vector(3 downto 0);
         cout : OUT  std_logic
        );
    END COMPONENT;

   --Inputs
   signal fun : std_logic_vector(2 downto 0) := (others => '0');
   signal sel : std_logic_vector(2 downto 0) := src_ak;
   signal cin : std_logic := '0';
   signal a : std_logic_vector(3 downto 0) := (others => '0');
   signal b : std_logic_vector(3 downto 0) := (others => '0');
   signal c : std_logic_vector(3 downto 0) := (others => '0');
   signal k : std_logic_vector(3 downto 0) := (others => '0');

 	--Outputs
   signal y : std_logic_vector(3 downto 0);
   signal cout : std_logic;
 
   constant clock_period : time := 20 ns;
 
	type test_record is record
		i: std_logic_vector(11 downto 0);
		o: std_logic_vector(4 downto 0);
	end record;
	
	type test_table is array (1 to 4 * 4 * 2 + 4) of test_record;


	constant test_case: test_table := (
		(fun_adchex & X"0" & X"0" & '0', '0' & X"0"),	--ADC HEX
		(fun_adchex & X"0" & X"0" & '1', '0' & X"1"),
		(fun_adchex & X"0" & X"9" & '0', '0' & X"9"),
		(fun_adchex & X"0" & X"9" & '1', '0' & X"A"),
		(fun_adchex & X"9" & X"0" & '0', '0' & X"9"),
		(fun_adchex & X"9" & X"0" & '1', '0' & X"A"),
		(fun_adchex & X"9" & X"9" & '0', '1' & X"2"),
		(fun_adchex & X"9" & X"9" & '1', '1' & X"3"),
		(fun_adcbcd & X"0" & X"0" & '0', '0' & X"0"),	--ADC BCD
		(fun_adcbcd & X"0" & X"0" & '1', '0' & X"1"),
		(fun_adcbcd & X"0" & X"9" & '0', '0' & X"9"),
		(fun_adcbcd & X"0" & X"9" & '1', '1' & X"0"),
		(fun_adcbcd & X"9" & X"0" & '0', '0' & X"9"),
		(fun_adcbcd & X"9" & X"0" & '1', '1' & X"0"),
		(fun_adcbcd & X"9" & X"9" & '0', '1' & X"8"),
		(fun_adcbcd & X"9" & X"9" & '1', '1' & X"9"),
		(fun_sbchex & X"0" & X"0" & '0', '0' & X"0"),	--SBC HEX
		(fun_sbchex & X"0" & X"0" & '1', '1' & X"F"),
		(fun_sbchex & X"0" & X"9" & '0', '1' & X"7"),
		(fun_sbchex & X"0" & X"9" & '1', '1' & X"6"),
		(fun_sbchex & X"9" & X"0" & '0', '0' & X"9"),
		(fun_sbchex & X"9" & X"0" & '1', '0' & X"8"),
		(fun_sbchex & X"9" & X"9" & '0', '0' & X"0"),
		(fun_sbchex & X"9" & X"9" & '1', '1' & X"F"),
		(fun_sbcbcd & X"0" & X"0" & '0', '0' & X"0"),	--SBC BCD
		(fun_sbcbcd & X"0" & X"0" & '1', '1' & X"9"),
		(fun_sbcbcd & X"0" & X"9" & '0', '1' & X"1"),
		(fun_sbcbcd & X"0" & X"9" & '1', '1' & X"0"),
		(fun_sbcbcd & X"9" & X"0" & '0', '0' & X"9"),
		(fun_sbcbcd & X"9" & X"0" & '1', '0' & X"8"),
		(fun_sbcbcd & X"9" & X"9" & '0', '0' & X"0"),
		(fun_sbcbcd & X"9" & X"9" & '1', '1' & X"9"),
-- additional
		(fun_sbcbcd & X"0" & X"1" & '0', '1' & X"9"),
		(fun_sbcbcd & X"0" & X"1" & '1', '1' & X"8"),
		(fun_sbcbcd & X"5" & X"5" & '0', '0' & X"0"),
		(fun_sbcbcd & X"5" & X"5" & '1', '1' & X"9")

	);
BEGIN
 
	-- Instantiate the Unit Under Test (UUT)
   uut: bcdalu PORT MAP (
          fun => fun,
          sel => sel,
          cin => cin,
          a => a,
          b => b,
          c => c,
          k => k,
          y => y,
          cout => cout
        );

   -- Clock process definitions
--   <clock>_process :process
--   begin
--		<clock> <= '0';
--		wait for <clock>_period/2;
--		<clock> <= '1';
--		wait for <clock>_period/2;
--   end process;
 

   -- Stimulus process
   stim_proc: process
   begin		
      -- hold reset state for 100 ns.
      wait for 100 ns;	
		
		for i in test_table'range loop
			cin <= test_case(i).i(0);
			k <= test_case(i).i(4 downto 1);
			a <= test_case(i).i(8 downto 5);
			fun <= test_case(i).i(11 downto 9);

			wait for clock_period*10;

			assert false
				report 	"TEST CASE #" & integer'image(i) & 
							decode8(test_case(i).i(11 downto 9), " ? ", " ? ", " ? ", " ? ", " ADCHEX ", " ADCBCD ", " SBCHEX ", " SBCBCD ") &
							" a= " & integer'image(to_integer(unsigned(test_case(i).i(8 downto 5)))) & 
							" b= " & integer'image(to_integer(unsigned(test_case(i).i(4 downto 1)))) & 
							" cin= " & std_logic'image(test_case(i).i(0))
				severity note;
			
			assert test_case(i).o(3 downto 0) = y
				report "ERROR: alu_y is " & integer'image(to_integer(unsigned(y))) & " instead of " & integer'image(to_integer(unsigned(test_case(i).o(3 downto 0))))
				severity failure;

			assert test_case(i).o(4) = cout
				report "ERROR: alu_cout is " & std_logic'image(cout) & " instead of " & std_logic'image(test_case(i).o(4))
				severity failure;
		end loop;

      assert false
			report "OK: all alu test cases passed."
			severity note;

   end process;

END;
