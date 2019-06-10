----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    20:03:37 03/24/2019 
-- Design Name: 
-- Module Name:    controlunit - Behavioral 
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
--use ieee.std_logic_textio.all;
--use STD.textio.all;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;
use work.tms0800_package.all;

entity controlunit is
    Port	(	
				clk : in  STD_LOGIC;
				reset : in  STD_LOGIC;
				instruction : in  STD_LOGIC_VECTOR (6 downto 0);
				condition : in  STD_LOGIC_VECTOR (15 downto 0);
				u_code : out  STD_LOGIC_VECTOR (31 downto 0);
				-- for debug only
				u_addr: out STD_LOGIC_VECTOR(7 downto 0));
end controlunit;

architecture Behavioral of controlunit is

component microcode is
	Generic (
			lst_filename: string
	);
	port (	
			a: in STD_LOGIC_VECTOR(7 downto 0);
			d: out STD_LOGIC_VECTOR(51 downto 0)
	);
end component;

signal u_pc, u_ra, u_next: std_logic_vector(7 downto 0);
signal u_condition: std_logic;

--signal u_instruction: std_logic_vector(51 downto 0);
-- driving internal sequencer signals
signal u_if	 :std_logic_vector(3 downto 0);
signal u_then :std_logic_vector(7 downto 0);
signal u_else :std_logic_vector(7 downto 0);

begin

--u_instruction <= microcode(to_integer(unsigned(u_pc)));
-- driving external signals
--u_code <= u_instruction(31 downto 0);
u_addr <= u_pc;

mc: microcode
	generic map
	(
		lst_filename => "./tms0800/output/microcode"
	)
 	port map
	(
		a => u_pc,
		d(51 downto 48) => u_if,	-- sequencer condition select
		d(47 downto 40) => u_then,	-- sequencer destination/command if condition is true (1)
		d(39 downto 32) => u_else,	-- sequencer destination/command if condition is false (0)
		d(31 downto 0) => u_code	-- exposed to outside
	);

-- select condition code
with u_if select
	u_condition <= condition(cond_true)  			when "0000",
						condition(cond_breakpoint)  	when "0001",
						condition(cond_2)  				when "0010",
						condition(cond_3)  				when "0011",
						condition(cond_dk)  				when "0100",
						condition(cond_5)  				when "0101",
						condition(cond_digit10)  		when "0110",
						condition(cond_keystrobe)  	when "0111",
						condition(cond_kn)  				when "1000",
						condition(cond_ko)  				when "1001",
						condition(cond_kp) 				when "1010",
						condition(cond_e11) 				when "1011",
						condition(cond_cflag) 			when "1100",
						condition(cond_enabletrace) 	when "1101",
						condition(cond_charsent) 		when "1110",
						condition(cond_false) 			when "1111";

-- select then or else part
u_next <= u_then when (u_condition = '1') else u_else;

-- update microcode program counter
update_upc: process(clk, reset, u_next)
begin
	if (reset = '1') then
		-- start execution at location 0, microinstructions 0 - 127 can be shared by any instruction
		u_pc <= X"00";
		u_ra <= X"00";
	else
		if (rising_edge(clk)) then
			case u_next is
				-- if condition(0) = '1' then X"00000" (default) will cause simple u_pc advance
				when upc_next =>
					u_pc <= std_logic_vector(unsigned(u_pc) + 1);
				-- used to repeat same microinstruction until condition turns true
				when upc_repeat =>
					u_pc <= u_pc;
				-- start executing macroinstruction routine, which are mapped to 128 - 255
				when upc_fork => 
					u_pc <= '1' & instruction;
				-- return from "1 level subroutine"
				when upc_return => 
					u_pc <= u_ra;
				-- any other value is a jump to that microinstruction location, save return address for "1 level stack"
				when others =>
					u_pc <= u_next;
					u_ra <= std_logic_vector(unsigned(u_pc) + 1);
			end case;
		end if;
	end if;
end process;  

end Behavioral;

