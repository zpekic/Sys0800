----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    19:34:07 04/13/2019 
-- Design Name: 
-- Module Name:    microcode - Behavioral 
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

entity microcode is
	 Generic (
			lst_filename: string);
    Port ( a : in  STD_LOGIC_VECTOR (7 downto 0);
           d : out  STD_LOGIC_VECTOR (51 downto 0));
end microcode;

architecture Behavioral of microcode is

-- some frequently used microcode destinations  --
constant RESTART: integer := 0;
constant CLEAR: integer := 1;
constant FORK: integer := 2;
constant NEXTI: integer := 3;
constant CONTINUECS: integer := 4;
constant CONTINUECC: integer := 5;
constant JUMPCS: integer := 6;
constant JUMPCC: integer := 7;
constant JUMP: integer := 8;
constant CONTINUE: integer := 9;
constant TRACE: integer := 10;
constant CLEARTXD: integer := 63; --123
constant COPYS: integer := 67;
constant MUL10: integer := 73;
constant DIV10: integer := 76;
constant ADCBCD: integer := 82;
constant SBCBCD: integer := 109;
constant ADCHEX: integer := 112;
constant SBCHEX: integer := 115;
-- Sinclair instruction implementations ----------
constant ACBB: integer := 129; -- B <= C + B
constant SCBA: integer := 130; -- A <= C - B
constant SCKB: integer := 131; -- B <= C - K
constant AABC: integer := 132; -- C <= A + B			
constant ACBC: integer := 133; -- C <= C + B
--------------------------------------------------

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
	return X"00000" & "000000" & flag_sel & "000000000000000000000000";
end uc_flag;

-- 1 BIT 23..23 
-- alias ss_disable:		std_logic is u_code(23);
impure function uc_ss(ss_switch: in std_logic) return std_logic_vector is
begin
	return X"00000" & "00000000" & ss_switch & "00000000000000000000000";
end uc_ss;

-- 1 BIT 22..22 
-- alias uc_writesam:		std_logic is u_code(22);
impure function uc_sam(update: in std_logic) return std_logic_vector is
begin
	return X"00000" & "000000000" & update &   "0000000000000000000000";
end uc_sam;

-- 3 BITS 21..19
--alias dst_sel: 		std_logic_vector(2 downto 0) is u_code(21 downto 19);
impure function uc_dst(dst_sel: in std_logic_vector(2 downto 0)) return std_logic_vector is
begin
	return X"00000" & "0000000000" & dst_sel & "0000000000000000000";
end uc_dst;

-- 3 BITS 18..16
-- alias cond_verb: std_logic_vector(2 downto 0) is u_instruction(18 downto 16);
impure function uc_cond(cond_sel: in std_logic_vector(2 downto 0)) return std_logic_vector is
begin
	return X"00000" & "0000000000000" & cond_sel & "0000000000000000";
end uc_cond;

-- 2 BITS 15..14
-- alias sync_verb:		std_logic_vector(1 downto 0) is u_code(15 downto 14);
impure function uc_sync(sync_verb: in std_logic_vector(1 downto 0)) return std_logic_vector is
begin
	return X"00000" & "0000000000000000" & sync_verb & "00000000000000";
end uc_sync;

-- 3 BITS 13..11
-- alias alu_fun: 		std_logic_vector(2 downto 0) is u_code(13 downto 11);
impure function uc_alu(alu_fun: in std_logic_vector(2 downto 0)) return std_logic_vector is
begin
	return X"00000" & "000000000000000000" & alu_fun & "00000000000";
end uc_alu;

-- 3 BITS 10..8
-- alias alu_sel: 		std_logic_vector(2 downto 0) is u_code(10 downto 8);
impure function uc_src(alu_sel: in std_logic_vector(2 downto 0)) return std_logic_vector is
begin
	return X"00000" & "000000000000000000000" & alu_sel & "00000000";
end uc_src;

-- 8 BITS 7..0
-- alias tu_char: std_logic_vector(7 downto 0) is u_instruction(7 downto 0);
impure function uc_setchar(char: in std_logic_vector(7 downto 0)) return std_logic_vector is
begin
	return X"00000" & "000000000000000000000000" & char;
end uc_setchar;

impure function uc_display(flag: std_logic) return std_logic_vector is
begin
	return X"00000" & "00000000000000000000000000000000"; -- TODO: this does nothing
end uc_display;

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

impure function uc_halt return std_logic_vector is
begin
	return uc_goto(upc_repeat);
end uc_halt;

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
			when cond_kp =>
				return "if(kp) then " & decode_then_or_else(if_then) & " else " & decode_then_or_else(if_else) & ";";
			when cond_ko =>
				return "if(ko) then " & decode_then_or_else(if_then) & " else " & decode_then_or_else(if_else) & ";";
			when cond_kn =>
				return "if(kn) then " & decode_then_or_else(if_then) & " else " & decode_then_or_else(if_else) & ";";
			when cond_keystrobe =>
				return "if(keystrobe) then " & decode_then_or_else(if_then) & " else " & decode_then_or_else(if_else) & ";";
			when cond_digit10 =>
				return "if(digit10) then " & decode_then_or_else(if_then) & " else " & decode_then_or_else(if_else) & ";";
			when cond_sinclair =>
				return "if(sinclair) then " & decode_then_or_else(if_then) & " else " & decode_then_or_else(if_else) & ";";
			when cond_dk =>
				return "if(dk) then " & decode_then_or_else(if_then) & " else " & decode_then_or_else(if_else) & ";";
			when cond_3 =>
				return "if(c3) then " & decode_then_or_else(if_then) & " else " & decode_then_or_else(if_else) & ";";
			when cond_2 =>
				return "if(c2) then " & decode_then_or_else(if_then) & " else " & decode_then_or_else(if_else) & ";";
			when cond_breakpoint =>
				return "if(breakpoint) then " & decode_then_or_else(if_then) & " else " & decode_then_or_else(if_else) & ";";
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
			when char_NULL =>
				return "NUL; ";
			when char_CLEAR =>
				return "CLEAR; ";
			when char_LF =>
				return "LF; ";
			when char_CR =>
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
				write(out_line, "--" & unassemble(std_logic_vector(to_unsigned((i - 128) * 16, 12)), '0', false));
		  end if;
		  writeline(out_file, out_line);
		  
		  -- write address
        write(out_line, get_string(i, 2, 16));
        write(out_line, string'(": "));

		  -- write content
		  if (temp_mem(i) = uc_halt) then
				write(out_line, string'("-- halt")); 
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
							-- write_sam
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
						 write(out_line, decode4(temp_mem(i)(31 downto 30), "", "pc <= 0, ", "pc <= pc + 1, ", "pc <= @(instruction), "));
							-- e_verb
						 write(out_line, decode4(temp_mem(i)(29 downto 28), "", "e <= 011111111111, ", "e <= e * 2, ", "e <= e / 2, "));
							-- reg_verb
						 write(out_line, decode4(temp_mem(i)(27 downto 26), "", "reg[e, dst] <= reg / 2, ", "reg[e, dst] <= reg[e, dst] * 2, ", "reg[e, dst] <= alu_y, "));
							-- flag_verb
						 write(out_line, decode4(temp_mem(i)(25 downto 24), "", "flag[e, dst] <= 0; ", "flag[e, dst] <= flag[e, af] ^ flag[e, bf]; ", "flag[e, dst] <= !flag[e, dst]; "));
							-- ss_disable
						 write(out_line, decode2(temp_mem(i)(23), "", "ss = off; "));
							-- update_sam
						 write(out_line, decode2(temp_mem(i)(22), "", "update; "));
							-- dst_verb
						 write(out_line, decode8(temp_mem(i)(21 downto 19), "dst <= dst; ", "???", "dst <= nul; ", "dst <= bflag; ", "dst <= aflag; ", "dst <= c; ", "dst <= b; ", "dst <= a; "));
							-- cflag_verb
						 write(out_line, decode8(temp_mem(i)(18 downto 16), "", "cflag <= 0; ", "cflag <= 1; ", "cflag <= alu_cout; ", "cflag <= cflag | (af & mask); ", "cflag <= cflag | (bf & mask); ", "cflag <= cflag | (af ^ bf); ", "???"));
							-- sync_verb
						 write(out_line, decode4(temp_mem(i)(15 downto 14), "", "sync = pulse; ", "sync = ???; ", "sync = ???; "));
						 	-- alu_fun
						 write(out_line, decode8(temp_mem(i)(13 downto 11), "alu_y = 0; ", "alu_y = s(alu_sel); ", "alu_y = r(alu_sel); ", "alu_y = xor(alu_sel); ", "alu_y = adchex(alu_sel); ", "alu_y = adcbcd(alu_sel); ", "alu_y = sbchex(alu_sel); ", "alu_y = sbcbcd(alu_sel); "));						 
							-- alu_inp
						 write(out_line, decode8(temp_mem(i)(10 downto 8), "alu_sel <= alu_sel; ", "alu_sel <= ???;  ", "alu_sel <= ???;  ", "alu_sel <= ???; ", "alu_sel <= ab; ", "alu_sel <= ak; ", "alu_sel <= cb; ", "alu_sel <= ck; "));

							-- tu_char
						 if (temp_mem(i)(7 downto 0) /= X"00") then
							write(out_line, "txd = " & decode_tu(temp_mem(i)(7 downto 0)));
						 end if;
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
		RESTART =>	-- start microcode execution (note: this location can't be used as jump target as reserved for "next")
			uc_pc(pc_clear),
			
		CLEAR =>  -- output clear character (note: this location can't be used as jump target as reserved for "return")
			uc_setchar(char_CLEAR) or
			uc_if(cond_charsent, uc_label(CLEARTXD), upc_repeat) or
			uc_pc(pc_clear),
			
		FORK => 	-- FORK
			uc_sync(pulse) or
			uc_e(e_init) or 		-- select no digit
			uc_dst(dst_nul) or	-- select no register
			--uc_goto(upc_fork),
			uc_if(cond_breakpoint, upc_repeat, upc_fork), -- stay here until breakpoint cleared
			
		NEXTI =>  -- NEXT INSTRUCTION
			uc_pc(pc_next) or 
			uc_goto(uc_label(FORK)),
			--uc_goto(uc_label(5)),

		-- continue with cond flag set
		CONTINUECS =>
			uc_cond(cf_one) or
			uc_dst(dst_nul) or
			uc_if(cond_enabletrace, uc_label(TRACE), uc_label(NEXTI)),

		-- continue with cond flag cleared
		CONTINUECC =>
			uc_cond(cf_zero) or
			uc_dst(dst_nul) or
			uc_if(cond_enabletrace, uc_label(TRACE), uc_label(NEXTI)),
			
		-- jump taken, set cond flag
		JUMPCS =>
			uc_dst(dst_nul) or
			uc_cond(cf_one) or
			uc_pc(pc_load) or 
			uc_goto(uc_label(FORK)),

		-- jump taken, clear cond flag
		JUMPCC =>
			uc_dst(dst_nul) or
			uc_cond(cf_zero) or
			uc_pc(pc_load) or 
			uc_goto(uc_label(FORK)),

		-- jump taken, do not change cond flag
		JUMP =>
			uc_dst(dst_nul) or
			uc_pc(pc_load) or 
			uc_goto(uc_label(FORK)),

		-- jump not taken
		CONTINUE =>
			uc_dst(dst_nul) or
			uc_if(cond_enabletrace, uc_label(TRACE), uc_label(NEXTI)),

----- BEGIN TRACER ROUTINE ------------------	
		TRACE =>
			uc_tracechar('P'),
		11 =>
			uc_tracechar('C'),
		12 =>
			uc_tracechar('='),
		13 =>
			uc_tracedata(t_pc2),	-- program counter
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
			uc_tracedata(t_instr2), -- current instruction
		20 =>
			uc_tracedata(t_instr1),
		21 =>
			uc_tracedata(t_instr0),

		22 =>
			uc_e(e_init) or
			uc_tracechar(' '),
		23 =>
			uc_e(e_ror) or 
			uc_tracechar('A'),
		24 =>
			uc_tracechar('='),
		25 =>
			uc_if(cond_e11, upc_next, uc_label(28)),
		26 =>
			uc_tracedata(t_a), -- display a register
		27 =>
			uc_e(e_ror) or 
			uc_goto(uc_label(25)),

		28 =>
			uc_e(e_init) or
			uc_tracechar(' '),
		29 =>
			uc_e(e_ror) or 
			uc_tracechar('B'),
		30 =>
			uc_tracechar('='),
		31 =>
			uc_if(cond_e11, upc_next, uc_label(34)),
		32 =>
			uc_tracedata(t_b), -- display b register
		33 =>
			uc_e(e_ror) or 
			uc_goto(uc_label(31)),

		34 =>
			uc_e(e_init) or
			uc_tracechar(' '),
		35 =>
			uc_e(e_ror) or 
			uc_tracechar('C'),
		36 =>
			uc_tracechar('='),
		37 =>
			uc_if(cond_e11, upc_next, uc_label(40)),
		38 =>
			uc_tracedata(t_c), -- display c register
		39 =>
			uc_e(e_ror) or 
			uc_goto(uc_label(37)),

		40 =>
			uc_e(e_init) or
			uc_tracechar(' '),
		41 =>
			uc_e(e_ror) or 
			uc_tracechar('A'),
		42 =>
			uc_tracechar('F'),
		43 =>
			uc_tracechar('='),
		44 =>
			uc_if(cond_e11, upc_next, uc_label(47)),
		45 =>
			uc_tracedata(t_af), -- display aflag register
		46 =>
			uc_e(e_ror) or 
			uc_goto(uc_label(44)),

		47 =>
			uc_e(e_init) or
			uc_tracechar(' '),
		48 =>
			uc_e(e_ror) or 
			uc_tracechar('B'),
		49 =>
			uc_tracechar('F'),
		50 =>
			uc_tracechar('='),
		51 =>
			uc_if(cond_e11, upc_next, uc_label(54)),
		52 =>
			uc_tracedata(t_bf), -- display bflag register
		53 =>
			uc_e(e_ror) or 
			uc_goto(uc_label(51)),

		54 =>
			uc_tracechar(' '),
		55 =>
			uc_tracechar('C'),
		56 =>
			uc_tracechar('F'),
		57 =>
			uc_tracechar('='),
		58 =>
			uc_tracedata(t_cf), -- display cond register

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
			uc_setchar(char_NULL) or 	-- reset output character
			uc_goto(upc_return),			-- return to caller
----- END TRACER ROUTINE -------------

		64 => -- ZFB, ZFA
			uc_ss(ss_off) or
			uc_if(cond_e11, upc_next, uc_label(CONTINUE)),
		65 =>
			uc_ss(ss_off) or
			uc_sam(sam_update) or
			uc_flag(bit_zero),
		66 =>
			uc_ss(ss_off) or
			uc_e(e_rol) or
			uc_goto(uc_label(64)),

		COPYS => -- AKA, AKB, AKC, ABOA, ABOC
			uc_ss(ss_off) or
			uc_alu(fun_s) or
			uc_if(cond_e11, upc_next, uc_label(CONTINUE)),
		68 =>
			uc_ss(ss_off) or
			uc_sam(sam_update) or
			uc_alu(fun_s) or
			uc_reg(bcd_fromalu),
		69 =>
			uc_ss(ss_off) or
			uc_e(e_rol) or
			uc_goto(uc_label(COPYS)),

		70 => -- WAITDK
			uc_ss(ss_off) or
			uc_if(cond_dk, uc_label(JUMP), uc_label(CONTINUE)), 

		71 => -- WAITNO
			uc_ss(ss_off) or
			uc_if(cond_keystrobe, uc_label(JUMP), uc_label(CONTINUE)), 
			
		MUL10 => -- SLLA, SLLB, SLLC
			uc_ss(ss_off) or
			uc_if(cond_e11, upc_next, uc_label(CONTINUE)),
		74 =>
			uc_ss(ss_off) or
			uc_sam(sam_update) or
			uc_reg(bcd_fromright),
		75 =>
			uc_ss(ss_off) or
			uc_e(e_ror) or				
			uc_goto(uc_label(73)),

		DIV10 => -- SRLA, SRLB, SRLC
			uc_ss(ss_off) or
			uc_if(cond_e11, upc_next, uc_label(CONTINUE)),
		77 =>
			uc_ss(ss_off) or
			uc_sam(sam_update) or
			uc_reg(bcd_fromleft),
		78 =>
			uc_ss(ss_off) or
			uc_e(e_rol) or				
			uc_goto(uc_label(76)),

		79 => -- FFB, FFA
			uc_ss(ss_off) or
			uc_if(cond_e11, upc_next, uc_label(CONTINUE)),
		80 =>
			uc_ss(ss_off) or
			uc_sam(sam_update) or
			uc_flag(bit_invert),
		81 =>
			uc_ss(ss_off) or
			uc_e(e_rol) or
			uc_goto(uc_label(79)),
		
		ADCBCD => -- AABA, AAKA, AAKC, ACKA, ACKB 
			uc_ss(ss_off) or
			uc_reg(bcd_fromalu) or
			uc_if(cond_e11, upc_next, uc_label(CONTINUE)),
		83 =>
			uc_ss(ss_off) or
			uc_sam(sam_update) or
			uc_alu(fun_adcbcd) or
			uc_reg(bcd_fromalu) or
			uc_cond(cf_cout),
		84 =>
			uc_ss(ss_off) or
			uc_e(e_rol) or
			uc_goto(uc_label(ADCBCD)),
		
		85 => -- EXAB
			uc_ss(ss_off) or
			uc_dst(dst_nul) or	
			uc_src(src_ab) or 
			uc_dst(dst_a) or
			uc_reg(bcd_fromalu) or
			uc_if(cond_e11, upc_next, uc_label(CONTINUE)),
		86 =>
			uc_ss(ss_off) or
			uc_sam(sam_update) or
			uc_alu(fun_xor) or
			uc_reg(bcd_fromalu) or
			uc_dst(dst_b),
		87 =>
			uc_ss(ss_off) or
			uc_sam(sam_update) or
			uc_alu(fun_xor) or
			uc_reg(bcd_fromalu) or
			uc_dst(dst_a),
		88 =>
			uc_ss(ss_off) or
			uc_sam(sam_update) or
			uc_alu(fun_xor) or
			uc_reg(bcd_fromalu),
		89 =>
			uc_ss(ss_off) or
			uc_dst(dst_nul) or
			uc_e(e_rol) or
			uc_goto(uc_label(85)),

		90 => -- TFB
			uc_ss(ss_off) or
			uc_if(cond_e11, upc_next, uc_label(CONTINUE)),
		91 =>
			uc_ss(ss_off) or
			uc_cond(cf_or_bf),
		92 =>
			uc_ss(ss_off) or
			uc_e(e_rol) or
			uc_if(cond_cflag, uc_label(CONTINUE), uc_label(90)), -- bail if set, otherwise loop

		93 => -- TFA
			uc_ss(ss_off) or
			uc_if(cond_e11, upc_next, uc_label(CONTINUE)),
		94 =>
			uc_ss(ss_off) or
			uc_cond(cf_or_af),
		95 =>
			uc_ss(ss_off) or
			uc_e(e_rol) or
			uc_if(cond_cflag, uc_label(CONTINUE), uc_label(93)), -- bail if set, otherwise loop

		96 => -- CF
			uc_ss(ss_off) or
			uc_if(cond_e11, upc_next, uc_label(CONTINUE)),
		97 =>
			uc_ss(ss_off) or
			uc_cond(cf_or_af_xor_bf),
		98 =>
			uc_ss(ss_off) or
			uc_e(e_rol) or
			uc_if(cond_cflag, uc_label(CONTINUE), uc_label(96)), -- bail if set, otherwise loop

		99 => -- EXF
			uc_ss(ss_off) or
			uc_dst(dst_af) or		-- set af destination 1 clock ahead
			uc_if(cond_e11, upc_next, uc_label(CONTINUE)),
		100 =>
			uc_ss(ss_off) or
			uc_sam(sam_update) or
			uc_flag(bit_load) or	-- af <= af xor bf;
			uc_dst(dst_bf),		-- set bf destination 1 clock ahead
		101 =>
			uc_ss(ss_off) or
			uc_sam(sam_update) or
			uc_flag(bit_load) or -- bf <= af xor bf;
			uc_dst(dst_af),		-- set af destination 1 clock ahead
		102 =>
			uc_ss(ss_off) or
			uc_sam(sam_update) or
			uc_flag(bit_load),   -- af <= af xor bf;
		103 =>
			uc_ss(ss_off) or
			uc_dst(dst_nul) or
			uc_e(e_rol) or
			uc_goto(uc_label(99)),
			
		104 => -- used in hex add/sub to reset cflag
			uc_ss(ss_off) or
			uc_cond(cf_zero) or
			uc_goto(uc_label(CONTINUE)),
			
		105 => -- SFB, SFA
			uc_ss(ss_off) or
			uc_if(cond_e11, upc_next, uc_label(CONTINUE)),
		106 =>
			uc_ss(ss_off) or
			uc_sam(sam_update) or
			uc_flag(bit_zero),
		107 =>
			uc_ss(ss_off) or
			uc_sam(sam_update) or
			uc_flag(bit_invert),
		108 =>
			uc_ss(ss_off) or
			uc_e(e_rol) or
			uc_goto(uc_label(105)),

		SBCBCD => -- SABA, SABC, SAKA, SCBC, SCKC, CAB, CAK, CCB, CCK
			uc_ss(ss_off) or
			uc_reg(bcd_fromalu) or
			uc_if(cond_e11, upc_next, uc_label(CONTINUE)),
		110 =>
			uc_ss(ss_off) or
			uc_sam(sam_update) or
			uc_alu(fun_sbcbcd) or
			uc_reg(bcd_fromalu) or
			uc_cond(cf_cout),
		111 =>
			uc_ss(ss_off) or
			uc_e(e_rol) or
			uc_goto(uc_label(SBCBCD)),

		ADCHEX => -- AAKAH
			uc_ss(ss_off) or
			uc_reg(bcd_fromalu) or
			uc_if(cond_e11, upc_next, uc_label(104)), -- clear cflag first
		113 =>
			uc_ss(ss_off) or
			uc_sam(sam_update) or
			uc_alu(fun_adchex) or
			uc_reg(bcd_fromalu) or
			uc_cond(cf_cout),
		114 =>
			uc_ss(ss_off) or
			uc_e(e_rol) or
			uc_goto(uc_label(ADCHEX)),

		SBCHEX => -- SAKAH
			uc_ss(ss_off) or
			uc_reg(bcd_fromalu) or
			uc_if(cond_e11, upc_next, uc_label(104)), -- clear cflag first
		116 =>
			uc_ss(ss_off) or
			uc_sam(sam_update) or
			uc_alu(fun_sbchex) or
			uc_reg(bcd_fromalu) or
			uc_cond(cf_cout),
		117 =>
			uc_ss(ss_off) or
			uc_e(e_rol) or
			uc_goto(uc_label(SBCHEX)),

		118 => -- AKCN 
			uc_ss(ss_off) or
			uc_reg(bcd_fromalu) or
			uc_if(cond_e11, upc_next, uc_label(121)),
		119 =>
			uc_ss(ss_off) or
			uc_sam(sam_update) or
			uc_alu(fun_adcbcd) or
			uc_reg(bcd_fromalu) or
			uc_cond(cf_cout),
		120 =>
			uc_ss(ss_off) or
			uc_e(e_rol) or
			uc_goto(uc_label(118)),
		121 => -- if kn was down, means we have a correct count in last mantissa, so bail, otherwise continue
			uc_ss(ss_off) or
			uc_if(cond_kn, upc_next, uc_label(CONTINUE)),
		122 => -- if scanned all, bail with CF = 1 to indicate no key
			uc_ss(ss_off) or
			uc_if(cond_digit10, uc_label(CONTINUECS), uc_label(FORK)), 
						
		127 => -- SCANNO
			uc_ss(ss_off) or
			uc_cond(cf_zero) or
			uc_if(cond_digit10, uc_label(CONTINUE), uc_label(FORK)), 

		-- jump if condition reset (0)
		128 =>
			uc_if(cond_cflag, uc_label(CONTINUECC), uc_label(JUMPCC)),

		-- Sinclair instruction implementations ---
		ACBB => -- B <= C + B
			uc_cond(cf_zero) or
			uc_src(src_cb) or
			uc_dst(dst_b) or
			uc_goto(uc_label(ADCBCD)),  
			
		SCBA => -- A <= C - B
			uc_cond(cf_zero) or
			uc_src(src_cb) or
			uc_dst(dst_a) or
			uc_goto(uc_label(SBCBCD)),  
		
		SCKB => -- B <= C - K
			uc_cond(cf_zero) or
			uc_src(src_ck) or
			uc_dst(dst_b) or
			uc_goto(uc_label(SBCBCD)),  
		
		AABC => -- C <= A + B
			uc_cond(cf_zero) or
			uc_src(src_ab) or
			uc_dst(dst_c) or
			uc_goto(uc_label(ADCBCD)),  
			
		ACBC => -- C <= C + B
			uc_cond(cf_zero) or
			uc_src(src_cb) or
			uc_dst(dst_c) or
			uc_goto(uc_label(ADCBCD)),  
		-- end of Sinclair instructions
		 
		-- jump if condition set (1)
		160|161|162|163|164|165|166|167|168|169|170|171|172|173|174|175|176|177|178|179|180|181|182|183|184|185|186|187|188|189|190 => 
			uc_if(cond_cflag, uc_label(JUMPCC), uc_label(CONTINUECC)),
		
		-- jump on KO
		192|193|194|195|196|197|198|199 =>
			uc_if(cond_ko, uc_label(CONTINUECC), uc_label(JUMPCC)), -- KO is low active

		-- jump on KP
		200|201|202|203|204|205|206|207 =>
			uc_if(cond_kp, uc_label(CONTINUECC), uc_label(JUMPCC)), -- KP is low active
			
		-- FLAG INSTRUCTIONS --
		208 => -- NOP16
			uc_goto(uc_label(CONTINUE)),

		209 => -- WAITDK
			uc_display(zero) or
			uc_if(cond_dk, uc_label(JUMP), uc_label(FORK)), 
			--uc_goto(uc_label(CONTINUE)),

		210 => -- WAITNO
			uc_if(cond_keystrobe, uc_label(JUMP), uc_label(FORK)), 

		211 => -- SFB
			uc_e(e_rol) or
			uc_dst(dst_bf) or
			uc_goto(uc_label(105)),  

		212 => -- SFA
			uc_e(e_rol) or
			uc_dst(dst_af) or
			uc_goto(uc_label(105)),  

		213 => -- SYNC(H)
			uc_cond(cf_zero) or
			uc_if(cond_digit10, uc_label(CONTINUE), uc_label(FORK)), 

		214 => -- SCAN(NO)
			uc_if(cond_keystrobe, uc_label(CONTINUECS), uc_label(127)), -- bail with cf=1 if any key pressed, otherwise scan

		215 => -- ZFB
			uc_e(e_rol) or
			uc_dst(dst_bf) or
			uc_goto(uc_label(64)),

		216 => -- ZFA
			uc_e(e_rol) or
			uc_dst(dst_af) or
			uc_goto(uc_label(64)),

		217 => -- TFB
			uc_e(e_rol) or
			uc_cond(cf_zero) or
			uc_goto(uc_label(90)), 

		218 => -- TFA
			uc_e(e_rol) or
			uc_cond(cf_zero) or
			uc_goto(uc_label(93)),  

		219 => -- FFB
			uc_e(e_rol) or
			uc_dst(dst_bf) or
			uc_goto(uc_label(79)),  

		220 => -- FFA
			uc_e(e_rol) or
			uc_dst(dst_af) or
			uc_goto(uc_label(79)), 

		221 => -- CF
			uc_e(e_rol) or
			uc_cond(cf_zero) or
			uc_goto(uc_label(96)),  
			
		222 => -- NOP30
			uc_goto(uc_label(CONTINUE)),

		223 => -- EXF
			uc_e(e_rol) or
			uc_goto(uc_label(99)),  
		
		-- REGISTER INSTRUCTIONS --
		224 => -- AABA
			uc_e(e_rol) or
			uc_cond(cf_zero) or
			uc_src(src_ab) or
			uc_dst(dst_a) or
			uc_goto(uc_label(ADCBCD)),  

		225 => -- AAKA
			uc_e(e_rol) or
			uc_cond(cf_zero) or
			uc_src(src_ak) or
			uc_dst(dst_a) or
			uc_goto(uc_label(ADCBCD)),  
			
		226 => -- AAKC
			uc_e(e_rol) or
			uc_cond(cf_zero) or
			uc_src(src_ak) or
			uc_dst(dst_c) or
			uc_goto(uc_label(ADCBCD)),  
			
		227 => -- ABOA / ACBB
			uc_e(e_rol) or
			uc_src(src_ab) or
			uc_dst(dst_a) or
			uc_if(cond_sinclair, uc_label(ACBB), uc_label(COPYS)),
			
		228 => -- ABOC
			uc_e(e_rol) or
			uc_src(src_ab) or
			uc_dst(dst_c) or
			uc_goto(uc_label(COPYS)),
			
		229 => -- ACKA
			uc_e(e_rol) or
			uc_cond(cf_zero) or
			uc_src(src_ck) or
			uc_dst(dst_a) or
			uc_goto(uc_label(ADCBCD)),  
			
		230 => -- ACKB
			uc_e(e_rol) or
			uc_cond(cf_zero) or
			uc_src(src_ck) or
			uc_dst(dst_b) or
			uc_goto(uc_label(ADCBCD)),  
			 
		231 => -- SABA
			uc_e(e_rol) or
			uc_cond(cf_zero) or
			uc_src(src_ab) or
			uc_dst(dst_a) or
			uc_goto(uc_label(SBCBCD)),  
			
		232 => -- SABC 
			uc_e(e_rol) or
			uc_cond(cf_zero) or
			uc_src(src_ab) or
			uc_dst(dst_c) or
			uc_goto(uc_label(SBCBCD)),  
			
		233 => -- SAKA
			uc_e(e_rol) or
			uc_cond(cf_zero) or
			uc_src(src_ak) or
			uc_dst(dst_a) or
			uc_goto(uc_label(SBCBCD)),  
			
		234 => -- SCBC
			uc_e(e_rol) or
			uc_cond(cf_zero) or
			uc_src(src_cb) or
			uc_dst(dst_c) or
			uc_goto(uc_label(SBCBCD)),  
			
		235 => -- SCKC
			uc_e(e_rol) or
			uc_cond(cf_zero) or
			uc_src(src_ck) or
			uc_dst(dst_c) or
			uc_goto(uc_label(SBCBCD)),  
			
		236 => -- CAB
			uc_e(e_rol) or
			uc_cond(cf_zero) or
			uc_src(src_ab) or
			uc_dst(dst_nul) or
			uc_goto(uc_label(SBCBCD)),  
			
		237 => -- CAK
			uc_e(e_rol) or
			uc_cond(cf_zero) or
			uc_src(src_ak) or
			uc_dst(dst_nul) or
			uc_goto(uc_label(SBCBCD)),  
			
		238 => -- CCB
			uc_e(e_rol) or
			uc_cond(cf_zero) or
			uc_src(src_cb) or
			uc_dst(dst_nul) or
			uc_goto(uc_label(SBCBCD)),  
			
		239 => -- CCK
			uc_e(e_rol) or
			uc_cond(cf_zero) or
			uc_src(src_ck) or
			uc_dst(dst_nul) or
			uc_goto(uc_label(SBCBCD)),  
		
		240 => -- AKA
			uc_e(e_rol) or
			uc_src(src_ck) or
			uc_dst(dst_a) or
			uc_goto(uc_label(COPYS)),

		241 => -- AKB
			uc_e(e_rol) or
			uc_src(src_ck) or
			uc_dst(dst_b) or
			uc_goto(uc_label(COPYS)),

		242 => -- AKC
			uc_e(e_rol) or
			uc_src(src_ck) or
			uc_dst(dst_c) or
			uc_goto(uc_label(COPYS)), 

		243 => -- EXAB
			uc_e(e_rol) or
			uc_goto(uc_label(85)),

		244 => -- SLLA
			uc_e(e_ror) or		-- going from 1 to 10, so first digit changed is #10
			uc_dst(dst_a) or
			uc_goto(uc_label(MUL10)),

		245 => -- SLLB
			uc_e(e_ror) or		-- shifting from 1 to 10, so first digit changed is #10
			uc_dst(dst_b) or
			uc_goto(uc_label(MUL10)),

		246 => -- SLLC
			uc_e(e_ror) or		-- going from 1 to 10, so first digit changed is #10
			uc_dst(dst_c) or
			uc_goto(uc_label(MUL10)),

		247 => -- SRLA
			uc_e(e_rol) or		-- going from 10 to 1, so first digit changed is #1
			uc_dst(dst_a) or
			uc_goto(uc_label(DIV10)),

		248 => -- SRLB
			uc_e(e_rol) or		-- going from 10 to 1, so first digit changed is #1
			uc_dst(dst_b) or
			uc_goto(uc_label(DIV10)),

		249 => -- SRLC
			uc_e(e_rol) or		-- going from 10 to 1, so first digit changed is #1
			uc_dst(dst_c) or
			uc_goto(uc_label(DIV10)),

		250 => -- AKCN
			uc_e(e_rol) or
			uc_cond(cf_zero) or
			uc_src(src_ak) or
			uc_dst(dst_a) or
			uc_goto(uc_label(118)),  

		251 => -- AAKAH / SCBA
			uc_e(e_rol) or
			uc_cond(cf_zero) or
			uc_src(src_ak) or
			uc_dst(dst_a) or
			uc_if(cond_sinclair, uc_label(SCBA), uc_label(ADCHEX)),
			
		252 => -- SAKAH / SCKB
			uc_e(e_rol) or
			uc_cond(cf_zero) or
			uc_src(src_ak) or
			uc_dst(dst_a) or
			uc_if(cond_sinclair, uc_label(SCKB), uc_label(SBCHEX)),

		253 => -- ACKC
			uc_e(e_rol) or
			uc_cond(cf_zero) or
			uc_src(src_ck) or
			uc_dst(dst_c) or
			uc_goto(uc_label(ADCBCD)),  
			
		254 => -- NOP / AABC (note: this location can't be used as jump target because code is reserved for "fork")
			uc_e(e_rol) or
			uc_if(cond_sinclair, uc_label(AABC), uc_label(CONTINUE)),

		255 => -- NOP / free (note: this location can't be used as jump target because code is reserved for "repeat")
			uc_e(e_rol) or
			uc_if(cond_sinclair, uc_label(ACBC), uc_label(CONTINUE)),
			
		others => -- stop microcode (this should never happen!)
			uc_halt
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

constant mc: rom256x52 := init_microcode(lst_filename);
--attribute rom_style : string;
--attribute rom_style of mc : signal is "block";

begin

d <= mc(to_integer(unsigned(a)));

end Behavioral;

