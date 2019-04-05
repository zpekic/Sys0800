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

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;
use work.tms0800_package.all;

entity controlunit is
    Port ( clk : in  STD_LOGIC;
           reset : in  STD_LOGIC;
           instruction : in  STD_LOGIC_VECTOR (6 downto 0);
           condition : in  STD_LOGIC_VECTOR (15 downto 0);
           u_code : out  STD_LOGIC_VECTOR (31 downto 0);
			  -- for debug only
			  u_addr: out STD_LOGIC_VECTOR(7 downto 0));
end controlunit;

architecture Behavioral of controlunit is

-- define and initialize microcode
type rom256x52 is array(0 to 255) of std_logic_vector(51 downto 0);

-- 16 BITS 51..36 
impure function uc_if(cond: in integer; goto_then: in std_logic_vector(7 downto 0); goto_else: in std_logic_vector(7 downto 0)) return std_logic_vector is
begin
	return std_logic_vector(to_unsigned(cond, 4)) & goto_then & goto_else & X"00000000";
end uc_if;

-- 2 BITS 35..34
impure function uc_pc(pc_sel: in std_logic_vector(1 downto 0)) return std_logic_vector is
begin
	return X"00000" & pc_sel & "00" & X"0000000";
end uc_pc;

-- 2 BITS 33..32
impure function uc_e(e_sel: in std_logic_vector(1 downto 0)) return std_logic_vector is
begin
	return X"00000" & "00" & e_sel & X"0000000";
end uc_e;

-- 8 BITS 7..0
impure function uc_setchar(char: in std_logic_vector(7 downto 0)) return std_logic_vector is
begin
	return X"00000000000" & char;
end uc_setchar;

-- helper functions
impure function uc_label(destination: integer) return std_logic_vector is
begin
	return std_logic_vector(to_unsigned(destination, 8));
end uc_label;

impure function uc_tracechar(char: character) return std_logic_vector is
begin
	return uc_setchar(std_logic_vector(to_unsigned(character'pos(char), 8))) or
			 uc_if(cond_charsent, uc_label(50), upc_repeat);
end uc_tracechar;

impure function uc_tracedata(sel: std_logic_vector(3 downto 0)) return std_logic_vector is
begin
	return uc_setchar("1000" & sel) or
			 uc_if(cond_charsent, uc_label(50), upc_repeat);
end uc_tracedata;


impure function uc_goto(dest: in std_logic_vector(7 downto 0)) return std_logic_vector is
begin
	return uc_if(cond_true, dest, dest);
end uc_goto;
--

impure function init_microcode(dump_file_name: in string) return rom256x52 is
    variable temp_mem: rom256x52 := 
	 (
		0 =>	-- RESTART
			uc_pc(pc_clear),
		1 =>  -- nop, used here because return instruction prevents this location as jump target
			uc_pc(pc_clear),
		2 => 	-- FORK
			uc_e(e_init) or 
			uc_goto(upc_fork),
		3 =>  -- NEXT INSTRUCTION, NO TRACE
			uc_pc(pc_next) or 
			uc_goto(uc_label(2)),
	
		10 =>  -- NEXT INSTRUCTION, BUT TRACE FIRST
			uc_tracechar('P'),
		11 =>
			uc_tracechar('C'),
		12 =>
			uc_tracechar('='),
		13 =>
			uc_tracedata(t_pc2),
		14 =>
			uc_tracedata(t_pc1),
		15 =>
			uc_tracedata(t_pc0),

		16 =>
			uc_tracechar(' '),
		17 =>
			uc_tracechar('I'),
		18 =>
			uc_tracechar('='),
		19 =>
			uc_tracedata(t_instr2),
		20 =>
			uc_tracedata(t_instr1),
		21 =>
			uc_tracedata(t_instr0),

		22 =>
			uc_tracechar(' '),
		23 =>
			uc_tracechar('A'),
		24 =>
			uc_tracechar('F'),
		25 =>
			uc_tracechar('='),
--		26 =>
--			uc_goto(uc_label(44)), -- display t register
--		27 =>
--
--		28 =>
--			uc_e(e_all) or -- load all at once into t
--			uc_trace(std_logic_vector(to_unsigned(character'pos(' '), 8))),
--		29 =>
--			uc_t(digit_in2) or -- t <= bflags
--			uc_trace(std_logic_vector(to_unsigned(character'pos('B'), 8))),
--		30 =>
--			uc_trace(std_logic_vector(to_unsigned(character'pos('F'), 8))),
--		31 =>
--			uc_trace(std_logic_vector(to_unsigned(character'pos('='), 8))),
--		32 =>
--			uc_goto(uc_label(44)), -- display t register
--			
--		
		26 =>
			uc_setchar(char_CR) or
			uc_if(cond_charsent, uc_label(50), upc_repeat),
		27 =>
			uc_setchar(char_LF) or
			uc_if(cond_charsent, uc_label(50), upc_repeat),
		28 =>
			uc_goto(uc_label(3)),
		50 => -- reset txd to make it ready for next character
			uc_setchar(char_NULL) or 
			uc_goto(upc_return),

		-- jump taken
		126 =>
			uc_pc(pc_load) or
			--uc_cond(cond_zero) or
			uc_if(cond_enabletrace, uc_label(10), uc_label(3)),
		-- jump not taken
		127 =>
			uc_if(cond_enabletrace, uc_label(10), uc_label(3)),

		-- jump if condition reset (0)
		128|129|130|131|132|133|134|135|136|137|138|139|140|141|142|143|144|145|146|147|148|149|150|151|152|153|154|155|156|157|158|159 =>
			uc_if(cond_cond, uc_label(127), uc_label(126)),
		-- jump if condition set (1)
		160|161|162|163|164|165|166|167|168|169|170|171|172|173|174|175|176|177|178|179|180|181|182|183|184|185|186|187|188|189|190|191 =>
			uc_if(cond_cond, uc_label(126), uc_label(127)),
			
		others => -- DEFAULT - just execute next instruction
			uc_if(cond_enabletrace, uc_label(10), uc_label(3))
	 );

begin
	return temp_mem;
end init_microcode;

signal u_pc, u_ra, u_next: std_logic_vector(7 downto 0);
signal u_condition: std_logic;
signal u_instruction: std_logic_vector(51 downto 0);
-- driving internal sequencer signals
alias u_if	 :std_logic_vector(3 downto 0) is u_instruction(51 downto 48);
alias u_then :std_logic_vector(7 downto 0) is u_instruction(47 downto 40);
alias u_else :std_logic_vector(7 downto 0) is u_instruction(39 downto 32);

constant microcode: rom256x52 := init_microcode("./microcode.lst");

begin

u_instruction <= microcode(to_integer(unsigned(u_pc)));
-- driving external signals
u_code <= u_instruction(31 downto 0);
u_addr <= u_pc;

-- select condition code
with u_if select
	u_condition <= condition(0)  when "0000",
						condition(1)  when "0001",
						condition(2)  when "0010",
						condition(3)  when "0011",
						condition(4)  when "0100",
						condition(5)  when "0101",
						condition(6)  when "0110",
						condition(7)  when "0111",
						condition(8)  when "1000",
						condition(9)  when "1001",
						condition(10) when "1010",
						condition(11) when "1011",
						condition(12) when "1100",
						condition(13) when "1101",
						condition(14) when "1110",
						condition(15) when "1111";

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

