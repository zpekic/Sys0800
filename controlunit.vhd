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
use ieee.std_logic_textio.all;
use STD.textio.all;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;
use work.tms0800_package.all;

entity controlunit is
	 Generic (
				lst_filename: string);
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

-- define and initialize microcode
type rom256x52 is array(0 to 255) of std_logic_vector(51 downto 0);

-- driving external signals

-- 20 BITS 51..32 
-- alias u_if	 :std_logic_vector(3 downto 0) is u_instruction(51 downto 48);
-- alias u_then :std_logic_vector(7 downto 0) is u_instruction(47 downto 40);
-- alias u_else :std_logic_vector(7 downto 0) is u_instruction(39 downto 32);
impure function uc_if(cond: in integer; goto_then: in std_logic_vector(7 downto 0); goto_else: in std_logic_vector(7 downto 0)) return std_logic_vector is
begin
	return std_logic_vector(to_unsigned(cond, 4)) & goto_then & goto_else & X"00000000";
end uc_if;

-- 2 BITS 31..30
-- alias pc_verb: std_logic_vector(1 downto 0) is u_instruction(31 downto 30);
impure function uc_pc(pc_sel: in std_logic_vector(1 downto 0)) return std_logic_vector is
begin
	return X"00000" & pc_sel & "00" & X"0000000";
end uc_pc;

-- 2 BITS 29..28
-- alias e_verb: std_logic_vector(1 downto 0) is u_instruction(29 downto 28);
impure function uc_e(e_sel: in std_logic_vector(1 downto 0)) return std_logic_vector is
begin
	return X"00000" & "00" & e_sel & "0000000000000000000000000000";
end uc_e;

-- 2 BITS 27..26
--alias reg_verb: 		std_logic_vector(1 downto 0) is u_code(27 downto 26);
impure function uc_reg(reg_sel: in std_logic_vector(1 downto 0)) return std_logic_vector is
begin
	return X"00000" & "0000" & reg_sel & "00000000000000000000000000";
end uc_reg;

-- 2 BITS 25..24
-- alias flag_verb:		std_logic_vector(1 downto 0) is u_code(25 downto 24);
impure function uc_flag(flag_sel: in std_logic_vector(1 downto 0)) return std_logic_vector is
begin
	return X"00000" & "0000000000" & flag_sel & "00000000000000000000";
end uc_flag;

-- 1 BIT 23..23 
-- alias ss_disable:		std_logic is u_code(23);
impure function uc_ss(ss_switch: in std_logic) return std_logic_vector is
begin
	return X"00000" & "0000000000" & ss_switch & "00000000000000000000";
end uc_ss;

-- alias reserved1:		std_logic is u_code(22);

-- 3 BITS 21..19
--alias dst_sel: 		std_logic_vector(2 downto 0) is u_code(21 downto 19);
impure function uc_dst(dst_sel: in std_logic_vector(2 downto 0)) return std_logic_vector is
begin
	return X"00000" & "00000000000000" & dst_sel & "000000000000000";
end uc_dst;

-- 3 BITS 18..16
-- alias cond_verb: std_logic_vector(2 downto 0) is u_instruction(18 downto 16);
impure function uc_cond(cond_sel: in std_logic_vector(2 downto 0)) return std_logic_vector is
begin
	return X"00000" & "00000000000000" & cond_sel & "000000000000000";
end uc_cond;

-- alias reserved2:		std_logic_vector(1 downto 0) is u_code(15 downto 14);

-- 4 BITS 13..8
-- alias alu_fun: 		std_logic_vector(2 downto 0) is u_code(13 downto 11);
-- alias alu_sel: 		std_logic_vector(2 downto 0) is u_code(10 downto 8);
impure function uc_alu(alu_fun: in std_logic_vector(2 downto 0); alu_sel: in std_logic_vector(2 downto 0)) return std_logic_vector is
begin
	return X"00000" & "00000000000000000" & alu_fun & alu_sel & "000000000";
end uc_alu;

-- 8 BITS 7..0
-- alias tu_char: std_logic_vector(7 downto 0) is u_instruction(7 downto 0);
impure function uc_setchar(char: in std_logic_vector(7 downto 0)) return std_logic_vector is
begin
	return X"00000" & "000000000000000000000000" & char;
end uc_setchar;

-- some frequently used microcode destinations  --
constant RESTART: integer := 0;
constant FORK: integer := 2;
constant NEXTI: integer := 3;
constant TRACE: integer := 10;
constant CLEARTXD: integer := 63;
constant JUMPCC: integer := 126;
constant CONTINUE: integer := 127;
--------------------------------------------------

-- helper functions ------------------------------
impure function uc_label(destination: integer) return std_logic_vector is
begin
	return std_logic_vector(to_unsigned(destination, 8));
end uc_label;

impure function uc_tracechar(char: character) return std_logic_vector is
begin
	return uc_setchar(std_logic_vector(to_unsigned(character'pos(char), 8))) or
			 uc_ss(ss_off) or
			 uc_if(cond_charsent, uc_label(CLEARTXD), upc_repeat);
end uc_tracechar;

impure function uc_tracedata(sel: std_logic_vector(3 downto 0)) return std_logic_vector is
begin
	return uc_setchar("1000" & sel) or
			 uc_ss(ss_off) or
			 uc_if(cond_charsent, uc_label(CLEARTXD), upc_repeat);
end uc_tracedata;

impure function uc_goto(dest: in std_logic_vector(7 downto 0)) return std_logic_vector is
begin
	return uc_if(cond_true, dest, dest);
end uc_goto;

impure function uc_default return std_logic_vector is
begin
	return uc_goto(uc_label(CONTINUE));
end uc_default;

impure function decode_then_or_else(then_or_else: std_logic_vector(7 downto 0)) return string is
begin
	case then_or_else is
		when upc_next =>
			return "next";
		when upc_return =>
			return "return";
		when upc_repeat =>
			return "repeat";
		when upc_fork =>
			return "fork";
		when others =>
			return "goto " & get_string(to_integer(unsigned(then_or_else)), 2, 16);
	end case;
end decode_then_or_else;

impure function decode_sequence(if_sequence: std_logic_vector(19 downto 0)) return string is
	alias if_cond: std_logic_vector(3 downto 0) is if_sequence(19 downto 16);
	alias if_then: std_logic_vector(7 downto 0) is if_sequence(15 downto 8);
	alias if_else: std_logic_vector(7 downto 0) is if_sequence(7 downto 0);

begin
	if (if_then = if_else) then
		return decode_then_or_else(if_then) & ";";
	else
		case to_integer(unsigned(if_cond)) is
			when cond_false =>
				return decode_then_or_else(if_else) & ";";
			when cond_charsent =>
				return "if(charsent) then " & decode_then_or_else(if_then) & " else " & decode_then_or_else(if_else) & ";";
			when cond_enabletrace =>
				return "if(enabletrace) then " & decode_then_or_else(if_then) & " else " & decode_then_or_else(if_else) & ";";
			when cond_cflag =>
				return "if(cflag) then " & decode_then_or_else(if_then) & " else " & decode_then_or_else(if_else) & ";";
			when cond_e11 =>
				return "if(e11) then " & decode_then_or_else(if_then) & " else " & decode_then_or_else(if_else) & ";";
			when cond_ko =>
				return "if(ko) then " & decode_then_or_else(if_then) & " else " & decode_then_or_else(if_else) & ";";
			when cond_kp =>
				return "if(kp) then " & decode_then_or_else(if_then) & " else " & decode_then_or_else(if_else) & ";";
			when cond_8 =>
				return "if(c8) then " & decode_then_or_else(if_then) & " else " & decode_then_or_else(if_else) & ";";
			when cond_7 =>
				return "if(c7) then " & decode_then_or_else(if_then) & " else " & decode_then_or_else(if_else) & ";";
			when cond_6 =>
				return "if(c6) then " & decode_then_or_else(if_then) & " else " & decode_then_or_else(if_else) & ";";
			when cond_5 =>
				return "if(c5) then " & decode_then_or_else(if_then) & " else " & decode_then_or_else(if_else) & ";";
			when cond_4 =>
				return "if(c4) then " & decode_then_or_else(if_then) & " else " & decode_then_or_else(if_else) & ";";
			when cond_3 =>
				return "if(c3) then " & decode_then_or_else(if_then) & " else " & decode_then_or_else(if_else) & ";";
			when cond_2 =>
				return "if(c2) then " & decode_then_or_else(if_then) & " else " & decode_then_or_else(if_else) & ";";
			when cond_1 =>
				return "if(c1) then " & decode_then_or_else(if_then) & " else " & decode_then_or_else(if_else) & ";";
			when cond_true =>
				return decode_then_or_else(if_then) & ";";
			when others =>
				return "???;";
		end case;
	end if;
end decode_sequence;

impure function decode_tu(val: std_logic_vector(7 downto 0)) return string is
begin
	if (val(7) = '0') then
		case val is
			when X"00" =>
				return "NUL; ";
			when X"0A" =>
				return "LF; ";
			when X"0D" =>
				return "CR; ";
			when others =>
				return "'" & character'val(to_integer(unsigned(val))) & "'; ";
		end case;	
	else
		return decode16(val(3 downto 0), "aflag[e]; ", "bflag[e]; ", "cflag; ", "a[e]; ", "b[e]; ", "c[e]; ", "instr0; ", "instr1; ", "instr2; ", "pc0; ", "pc1; ", "pc2; ", "???", "???", "???", "???");
	end if;
end decode_tu;

procedure dump_microcode(out_file_name: in string; temp_mem: in rom256x52; depth: integer; base: integer) is
    file out_file : text; -- open write_mode is out_file_name;
    variable out_line : line;

begin
	-- dump memory content in <address> <word> format for verification
	 file_open(out_file, out_file_name, write_mode);
    for i in 0 to (depth - 1) loop

		  -- header for easier readibility
		  if ((i mod 32 = 0) and (base = 2)) then
				write(out_line, string'("---------------------------------------------------------------------------------"));writeline(out_file, out_line);
				write(out_line, string'("--ADDR IF     THEN    ELSE PC E REG FLAG SS RES1 DST COND RES2 ALUFUN ALUSEL CHAR  "));writeline(out_file, out_line);
				write(out_line, string'("---------------------------------------------------------------------------------"));writeline(out_file, out_line);
		  end if;
			
		  -- indication of entry points	
		  if (i < 128) then	-- addresses 0 .. 127 are regular microcode
				if (i = 0) then
					write(out_line, "-- RESET");
				else
					write(out_line, "-- ");
				end if;
		  else					-- addresses 128 .. 255 map to calculator instruction entry points
				write(out_line, "--" & unassemble(std_logic_vector(to_unsigned((i - 128) * 16, 12)), '0'));
		  end if;
		  writeline(out_file, out_line);
		  
		  -- write address
        write(out_line, get_string(i, 2, 16));
        write(out_line, string'(": "));

		  -- write content
		  if (temp_mem(i) = uc_default) then
				write(out_line, string'("-- uninitialized")); 
		  else
			  case base is
					when 2 =>
							-- u_if, u_then, u_else
						 write(out_line, temp_mem(i)(51 downto 48));write(out_line, string'(" "));
						 write(out_line, temp_mem(i)(47 downto 40));write(out_line, string'(" "));
						 write(out_line, temp_mem(i)(39 downto 32));write(out_line, string'(" "));
							-- pc_verb
						 write(out_line, temp_mem(i)(31 downto 30));write(out_line, string'(" "));
							-- e_verb
						 write(out_line, temp_mem(i)(29 downto 28));write(out_line, string'(" "));
							-- reg_verb
						 write(out_line, temp_mem(i)(27 downto 26));write(out_line, string'(" "));
							-- flag_verb
						 write(out_line, temp_mem(i)(25 downto 24));write(out_line, string'(" "));
							-- ss_disable
						 write(out_line, temp_mem(i)(23));write(out_line, string'(" "));
							-- reserved1
						 write(out_line, temp_mem(i)(22));write(out_line, string'(" "));
							-- dst_verb
						 write(out_line, temp_mem(i)(21 downto 19));write(out_line, string'(" "));
							-- cflag_verb
						 write(out_line, temp_mem(i)(18 downto 16));write(out_line, string'(" "));
							-- reserved2
						 write(out_line, temp_mem(i)(15 downto 14));write(out_line, string'(" "));
							-- alu_fun
						 write(out_line, temp_mem(i)(13 downto 11));write(out_line, string'(" "));
							-- alu_inp
						 write(out_line, temp_mem(i)(10 downto 8));write(out_line, string'(" "));
							-- tu_char
						 write(out_line, temp_mem(i)(7 downto 0));write(out_line, string'(" "));
					when 8 =>
						 owrite(out_line, temp_mem(i));
					when 16 =>
						 hwrite(out_line, temp_mem(i));
					when others => -- any other value will dump microcode "mnemonics"
							-- pc_verb
						 write(out_line, decode4(temp_mem(i)(31 downto 30), "", "pc <= 0, ", "pc <= pc + 1, ", "pc <= @(instruction - 1), "));
							-- e_verb
						 write(out_line, decode4(temp_mem(i)(29 downto 28), "", "e <= 011111111111, ", "e <= e * 2, ", "e <= e / 2, "));
							-- reg_verb
						 write(out_line, decode4(temp_mem(i)(27 downto 26), "", "reg[e, dst] <= reg / 2, ", "reg[e, dst] <= reg[e, dst] * 2, ", "reg[e, dst] <= alu, "));
							-- flag_verb
						 write(out_line, decode4(temp_mem(i)(25 downto 24), "", "flag[e, dst] <= 0; ", "flag[e, dst] <= 1; ", "flag[e, dst] <= !flag[e, dst]; "));
							-- ss_disable
						 write(out_line, decode2(temp_mem(i)(23), "", "ss = off; "));
							-- reserved1
						 --write(out_line, temp_mem(i)(22));
							-- dst_verb
						 write(out_line, decode8(temp_mem(i)(21 downto 19), "dst <= dst; ", "???", "dst <= nul; ", "dst <= bflag; ", "dst <= aflag; ", "dst <= c; ", "dst <= b; ", "dst <= a; "));
							-- cflag_verb
						 write(out_line, decode8(temp_mem(i)(18 downto 16), "", "cflag <= 0; ", "cflag <= 1; ", "cflag <= alu_cout; ", "???", "???", "???", "???"));
							-- reserved2
						 --write(out_line, temp_mem(i)(15 downto 14));
							-- alu_fun
						 write(out_line, decode8(temp_mem(i)(13 downto 11), "alu_y = zero(", "alu_y = s(", "alu_y = r(", "alu_y = xor(", "alu_y = adchex(", "alu_y = adcbcd(", "alu_y = sbchex(", "alu_y = sbcbcd("));
							-- alu_inp
						 write(out_line, decode8(temp_mem(i)(10 downto 8), "a, b); ", "a, k); ", "c, k); ", "c, b); ", "???, ???); ", "???, ???); ", "???, ???); ", "???, ???); "));
							-- tu_char
						 write(out_line, "txd = " & decode_tu(temp_mem(i)(7 downto 0)));
							-- write sequence at the end as it is most readable that way
						 write(out_line, decode_sequence(temp_mem(i)(51 downto 32)));
			  end case;
		  end if;	 
        writeline(out_file, out_line);
    end loop;
    file_close(out_file);
end dump_microcode;

impure function init_microcode(dump_file_name: in string) return rom256x52 is
    variable temp_mem: rom256x52 := 
	 (
		RESTART =>	-- RESTART
			uc_pc(pc_clear),
		1 =>  -- nop, used here because return instruction prevents this location as jump target
			uc_pc(pc_clear),
		FORK => 	-- FORK
			uc_e(e_init) or 		-- select no digit
			uc_dst(dst_none) or	-- select no register
			uc_goto(upc_fork),
		NEXTI =>  -- NEXT INSTRUCTION
			uc_pc(pc_next) or 
			uc_goto(uc_label(FORK)),

----- BEGIN TRACER ROUTINE ------------------	
		TRACE => 
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
			uc_e(e_init) or
			uc_tracechar('F'),
		25 =>
			uc_e(e_ror) or 
			uc_tracechar('='),
		26 =>
			uc_if(cond_e11, upc_next, uc_label(29)),
		27 =>
			uc_tracedata(t_af), -- display aflag register
		28 =>
			uc_e(e_ror) or 
			uc_goto(uc_label(26)),

		29 =>
			uc_tracechar(' '),
		30 =>
			uc_tracechar('B'),
		31 =>
			uc_e(e_init) or
			uc_tracechar('F'),
		32 =>
			uc_e(e_ror) or 
			uc_tracechar('='),
		33 =>
			uc_if(cond_e11, upc_next, uc_label(36)),
		34 =>
			uc_tracedata(t_bf), -- display bflag register
		35 =>
			uc_e(e_ror) or 
			uc_goto(uc_label(33)),

		36 =>
			uc_tracechar(' '),
		37 =>
			uc_tracechar('C'),
		38 =>
			uc_tracechar('F'),
		39 =>
			uc_tracechar('='),
		40 =>
			uc_tracedata(t_cf), -- display cond register

		41 =>
			uc_tracechar(' '),
		42 =>
			uc_e(e_init) or
			uc_tracechar('A'),
		43 =>
			uc_e(e_ror) or 
			uc_tracechar('='),
		44 =>
			uc_if(cond_e11, upc_next, uc_label(47)),
		45 =>
			uc_tracedata(t_a), -- display a register
		46 =>
			uc_e(e_ror) or 
			uc_goto(uc_label(44)),

		47 =>
			uc_tracechar(' '),
		48 =>
			uc_e(e_init) or
			uc_tracechar('B'),
		49 =>
			uc_e(e_ror) or 
			uc_tracechar('='),
		50 =>
			uc_if(cond_e11, upc_next, uc_label(53)),
		51 =>
			uc_tracedata(t_b), -- display b register
		52 =>
			uc_e(e_ror) or 
			uc_goto(uc_label(50)),

		53 =>
			uc_tracechar(' '),
		54 =>
			uc_e(e_init) or
			uc_tracechar('C'),
		55 =>
			uc_e(e_ror) or 
			uc_tracechar('='),
		56 =>
			uc_if(cond_e11, upc_next, uc_label(59)),
		57 =>
			uc_tracedata(t_c), -- display c register
		58 =>
			uc_e(e_ror) or 
			uc_goto(uc_label(56)),

		59 =>
			uc_goto(upc_next),
		60 =>
			uc_setchar(char_CR) or
			uc_if(cond_charsent, uc_label(CLEARTXD), upc_repeat),
		61 =>
			uc_setchar(char_LF) or
			uc_if(cond_charsent, uc_label(CLEARTXD), upc_repeat),
		62 =>
			uc_goto(uc_label(NEXTI)),
		CLEARTXD => -- subroutine to reset txd to make it ready for next character
			uc_setchar(char_NULL) or 
			uc_goto(upc_return),
----- END TRACER ROUTINE -------------

		64 => -- ZFB, ZFA
			uc_if(cond_e11, upc_next, uc_label(CONTINUE)),
		65 =>
			uc_flag(bit_zero),
		66 =>
			uc_e(e_ror) or
			uc_goto(uc_label(64)),

		67 => -- AKA, AKB, AKC
			uc_if(cond_e11, upc_next, uc_label(CONTINUE)),
		68 =>
			uc_alu(fun_s, inp_ak) or
			uc_reg(bcd_fromalu),
		69 =>
			uc_e(e_ror) or
			uc_goto(uc_label(67)),

		-- jump taken, clear cond flag too
		JUMPCC =>
			uc_pc(pc_load) or	-- note that pc <= (dest - 1) because pc will be incremented after!
			uc_cond(cf_zero),
		-- jump not taken
		CONTINUE =>
			uc_if(cond_enabletrace, uc_label(TRACE), uc_label(NEXTI)),

		-- jump if condition reset (0)
		128|129|130|131|132|133|134|135|136|137|138|139|140|141|142|143|144|145|146|147|148|149|150|151|152|153|154|155|156|157|158|159 =>
			uc_if(cond_cflag, uc_label(CONTINUE), uc_label(JUMPCC)),
		-- jump if condition set (1)
		160|161|162|163|164|165|166|167|168|169|170|171|172|173|174|175|176|177|178|179|180|181|182|183|184|185|186|187|188|189|190|191 =>
			uc_if(cond_cflag, uc_label(JUMPCC), uc_label(CONTINUE)),
		
		-- jump on KO
		192|193|194|195|196|197|198|199 =>
			uc_if(cond_ko, uc_label(JUMPCC), uc_label(CONTINUE)),

		-- jump on KP
		200|201|202|203|204|205|206|207 =>
			uc_if(cond_kp, uc_label(JUMPCC), uc_label(CONTINUE)),
			
		-- FLAG INSTRUCTIONS
		208 => -- NOP16
			uc_goto(uc_label(CONTINUE)),

		215 => -- ZFB
			uc_e(e_ror) or
			uc_dst(dst_bf) or
			uc_goto(uc_label(64)),

		216 => -- ZFA
			uc_e(e_ror) or
			uc_dst(dst_af) or
			uc_goto(uc_label(64)),
			
		222 => -- NOP30
			uc_goto(uc_label(CONTINUE)),
			
		-- REGISTER INSTRUCTIONS
		240 => -- AKA
			uc_e(e_ror) or
			uc_dst(dst_a) or
			uc_goto(uc_label(67)),

		241 => -- AKB
			uc_e(e_ror) or
			uc_dst(dst_b) or
			uc_goto(uc_label(67)),

		242 => -- AKC
			uc_e(e_ror) or
			uc_dst(dst_c) or
			uc_goto(uc_label(67)),
			
		others => -- DEFAULT - just execute next instruction (with or without tracing)
			uc_default
	 );

begin
	-- write into file for inspection
	dump_microcode(dump_file_name & "_bin.lst", temp_mem, 256, 2);
	--dump_microcode(dump_file_name & "_oct.lst", temp_mem, 256, 8);
	--dump_microcode(dump_file_name & "_hex.lst", temp_mem, 256, 16);
	dump_microcode(dump_file_name & "_sym.lst", temp_mem, 256, 10);
	-- return for runtime
	return temp_mem;
end init_microcode;

signal u_pc, u_ra, u_next: std_logic_vector(7 downto 0);
signal u_condition: std_logic;

signal u_instruction: std_logic_vector(51 downto 0);
-- driving internal sequencer signals
alias u_if	 :std_logic_vector(3 downto 0) is u_instruction(51 downto 48);
alias u_then :std_logic_vector(7 downto 0) is u_instruction(47 downto 40);
alias u_else :std_logic_vector(7 downto 0) is u_instruction(39 downto 32);

constant microcode: rom256x52 := init_microcode(lst_filename);

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

