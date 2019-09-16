--	Package File Template
--
--	Purpose: This package defines supplemental types, subtypes, 
--		 constants, and functions 

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.numeric_std.all;

package tms0800_package is

subtype t_7seg is std_logic_vector(7 downto 0);
type bytemask is array (0 to 15) of std_logic_vector(7 downto 0);

constant pattern0: t_7seg := "00111111";   --0
constant pattern1: t_7seg  := "00000110";   --1
constant pattern2: t_7seg  := "01011011";   --2
constant pattern3: t_7seg  := "01001111";   --3
constant pattern4: t_7seg  := "01100110";   --4
constant pattern5: t_7seg  := "01101101";   --5
constant pattern6: t_7seg  := "01111101";   --6
constant pattern7: t_7seg  := "00000111";   --7
constant pattern8: t_7seg  := "01111111";   --8
constant pattern9: t_7seg  := "01101111";   --9
constant pattern_minus: t_7seg  := "01000000";   --(show minus sign)
constant pattern_blank: t_7seg  := "00000000";

constant char_NULL: std_logic_vector(7 downto 0) := X"00";
constant char_CLEAR: std_logic_vector(7 downto 0) := X"01";
constant char_HOME: std_logic_vector(7 downto 0) := X"02";
constant char_CR: std_logic_vector(7 downto 0) := X"0D";
constant char_LF: std_logic_vector(7 downto 0) := X"0A";

constant tab: character := character'val(9);

constant decode4to8: bytemask :=(
"00000000", -- 0 disabled
"00000000",
"00000000",
"00000000",
"00000000",
"00000000",
"00000000",
"00000000",	-- 7 disabled
"00000001", -- 0 enabled
"00000010",
"00000100",
"00001000",
"00010000",
"00100000",
"01000000",
"10000000"	-- 7 enabled
);

-- selections for tracer unit
constant t_af : 		std_logic_vector(3 downto 0) := X"0";
constant t_bf : 		std_logic_vector(3 downto 0) := X"1";
constant t_cf : 		std_logic_vector(3 downto 0) := X"2";
constant t_a : 		std_logic_vector(3 downto 0) := X"3";
constant t_b : 		std_logic_vector(3 downto 0) := X"4";
constant t_c : 		std_logic_vector(3 downto 0) := X"5";
constant t_instr0 : 	std_logic_vector(3 downto 0) := X"6";
constant t_instr1 : 	std_logic_vector(3 downto 0) := X"7";
constant t_instr2 : 	std_logic_vector(3 downto 0) := X"8";
constant t_pc0: 		std_logic_vector(3 downto 0) := X"9";
constant t_pc1: 		std_logic_vector(3 downto 0) := X"A";
constant t_pc2: 		std_logic_vector(3 downto 0) := X"B";
constant t_dbgin0: 	std_logic_vector(3 downto 0) := X"C";
constant t_dbgin1: 	std_logic_vector(3 downto 0) := X"D";
constant t_dbgin2: 	std_logic_vector(3 downto 0) := X"E";
constant t_dbgin3: 	std_logic_vector(3 downto 0) := X"F";

-- special microcode "goto" codes (all others will be jump to that location)
constant upc_next:   std_logic_vector(7 downto 0) := X"00"; -- means we can't jump to location 0!
constant upc_return: std_logic_vector(7 downto 0) := X"01"; -- means we can't jump to location 1!
constant upc_repeat: std_logic_vector(7 downto 0) := X"FF"; -- means we can't jump to location 255!
constant upc_fork:   std_logic_vector(7 downto 0) := X"FE"; -- means we can't jump to location 254!

-- 1 big microinstruction fields
-- single step
constant ss_on : std_logic := '0';
constant ss_off : std_logic := '1';
-- update SAM
constant sam_nop : std_logic := '0';
constant sam_update : std_logic := '1';
-- display
constant zero: std_logic := '0';	-- TODO: does nothing, beside being a lame name
constant one: std_logic := '1';	-- TODO: does nothing, beside being a lame name

-- basic colors (BBGGGRRR)
constant color8_black : std_logic_vector(7 downto 0) := "00000000"; 
constant color8_red	 : std_logic_vector(7 downto 0) := "00000111"; 
constant color8_green : std_logic_vector(7 downto 0) := "00111000"; 
constant color8_yellow: std_logic_vector(7 downto 0) := "00111111"; 
constant color8_blue	 : std_logic_vector(7 downto 0) := "11000000"; 
constant color8_purple: std_logic_vector(7 downto 0) := "11000111"; 
constant color8_cyan	 : std_logic_vector(7 downto 0) := "11111000"; 
constant color8_white : std_logic_vector(7 downto 0) := "11111111"; 

-- 2 bit microinstruction fields
-- program counter
constant pc_nop: 	std_logic_vector(1 downto 0) 		:= "00";
constant pc_clear:std_logic_vector(1 downto 0) 		:= "01";
constant pc_next: std_logic_vector(1 downto 0) 		:= "10";
constant pc_load: std_logic_vector(1 downto 0) 		:= "11";
-- E reg (ring shift register to enable single digit at a time)
constant e_nop: 	std_logic_vector(1 downto 0) 		:= "00";
constant e_init:	std_logic_vector(1 downto 0) 		:= "01";
constant e_rol: 	std_logic_vector(1 downto 0) 		:= "10";
constant e_ror: 	std_logic_vector(1 downto 0) 		:= "11";
-- A, B, C bcd regs
constant bcd_nop: 		std_logic_vector(1 downto 0) 		:= "00";
constant bcd_fromleft:	std_logic_vector(1 downto 0) 		:= "01";
constant bcd_fromright: std_logic_vector(1 downto 0) 		:= "10";
constant bcd_fromalu: 	std_logic_vector(1 downto 0) 		:= "11";
-- AF, BF bit regs
constant bit_nop: 	std_logic_vector(1 downto 0) 		:= "00";
constant bit_zero:	std_logic_vector(1 downto 0) 		:= "01";
constant bit_load: 	std_logic_vector(1 downto 0) 		:= "10";
constant bit_invert: std_logic_vector(1 downto 0) 		:= "11";
-- display / kbd sync
constant nop: 		std_logic_vector(1 downto 0) 		:= "00";
constant pulse:	std_logic_vector(1 downto 0) 		:= "01";
--constant turnoff: std_logic_vector(1 downto 0) 		:= "10";
--constant turnon: 	std_logic_vector(1 downto 0) 		:= "11";

-- 3 bit microinstruction fields
-- destination selections
constant dst_nop : 	std_logic_vector(2 downto 0) := "000"; -- dst reg remains the same, meaning same SAM reg will be changed
constant dst_nop1 : 	std_logic_vector(2 downto 0) := "001";
constant dst_nul : 	std_logic_vector(2 downto 0) := "010";	-- dst reg is cleared, meaning no SAM reg will be changed
constant dst_bf : 	std_logic_vector(2 downto 0) := "011";
constant dst_af : 	std_logic_vector(2 downto 0) := "100";
constant dst_c : 		std_logic_vector(2 downto 0) := "101";
constant dst_b : 		std_logic_vector(2 downto 0) := "110";
constant dst_a : 		std_logic_vector(2 downto 0) := "111";
-- ALU selections
constant src_nop : 	std_logic_vector(2 downto 0) := "000";
constant src_1 : 		std_logic_vector(2 downto 0) := "001";
constant src_2 : 		std_logic_vector(2 downto 0) := "010";
constant src_3 : 		std_logic_vector(2 downto 0) := "011";
constant src_ab : 	std_logic_vector(2 downto 0) := "100";
constant src_ak : 	std_logic_vector(2 downto 0) := "101";
constant src_cb : 	std_logic_vector(2 downto 0) := "110";
constant src_ck : 	std_logic_vector(2 downto 0) := "111";
-- ALU functions
constant fun_zero : 	std_logic_vector(2 downto 0) := "000";
constant fun_s : 		std_logic_vector(2 downto 0) := "001";
constant fun_r : 		std_logic_vector(2 downto 0) := "010";
constant fun_xor : 	std_logic_vector(2 downto 0) := "011";
constant fun_adchex :std_logic_vector(2 downto 0) := "100";
constant fun_adcbcd :std_logic_vector(2 downto 0) := "101";
constant fun_sbchex :std_logic_vector(2 downto 0) := "110";
constant fun_sbcbcd :std_logic_vector(2 downto 0) := "111";
-- Cond flag reg functions
constant cf_nop : 	std_logic_vector(2 downto 0) := O"0";
constant cf_zero : 	std_logic_vector(2 downto 0) := O"1";
constant cf_one : 	std_logic_vector(2 downto 0) := O"2";
constant cf_cout : 	std_logic_vector(2 downto 0) := O"3";
constant cf_or_af :	std_logic_vector(2 downto 0) := O"4";
constant cf_or_bf :	std_logic_vector(2 downto 0) := O"5";
constant cf_or_af_xor_bf :		std_logic_vector(2 downto 0) := O"6";
constant cf_7 :		std_logic_vector(2 downto 0) := O"7";

-- 16 control unit branch conditions
constant cond_false: 		integer := 15; 
constant cond_charsent: 	integer := 14;
constant cond_enabletrace: integer := 13;
constant cond_cflag: 		integer := 12;
constant cond_e11: 			integer := 11;
constant cond_kp: 			integer := 10;
constant cond_ko: 			integer := 9;
constant cond_kn: 			integer := 8;
constant cond_keystrobe: 	integer := 7;
constant cond_digit10: 		integer := 6;
constant cond_sinclair:		integer := 5;
constant cond_dk: 			integer := 4;
constant cond_3: 				integer := 3;
constant cond_digit0: 		integer := 2;
constant cond_breakpoint: 	integer := 1;
constant cond_true: 			integer := 0;

-- generic helpers
impure function char2logic(char: in character) return std_logic;
impure function char2hex(char: in character) return integer;
impure function get_string(value: in integer; len: in integer; base: in integer) return string;
impure function parseBinary8(bin_str: in string) return std_logic_vector;
impure function parseBinary16(bin_str: in string) return std_logic_vector;
impure function decode2(val: std_logic; s0: string; s1: string) return string;
impure function decode4(val: std_logic_vector(1 downto 0); 	s0: string; s1: string; s2: string; s3: string) return string;
impure function decode8(val: std_logic_vector(2 downto 0);  s0: string; s1: string; s2: string; s3: string;  s4: string; s5: string; s6: string; s7: string) return string;
impure function decode16(val: std_logic_vector(3 downto 0); s0: string; s1: string; s2: string; s3: string;  s4: string; s5: string; s6: string; s7: string; s8: string; s9: string; s10: string; s11: string; s12: string; s13: string; s14: string; s15: string) return string;

-- TODO move these elsewhere
impure function get_mask(mask: in std_logic_vector(3 downto 0); sinclair_mode: in boolean) return string;
impure function unassemble(instruction: in std_logic_vector(11 downto 0); full: in std_logic; sinclair_mode: in boolean) return string;

end tms0800_package;

package body tms0800_package is

impure function char2logic(char: in character) return std_logic is
begin
	case char is
		when '0' =>
			return '0';
		when '1' =>
			return '1';
		when others =>
			assert false report "char2logic(): unexpected character '" & char & "'" severity failure;
	end case;
	return 'X';
end char2logic;

impure function char2hex(char: in character) return integer is
begin
	case char is
		when '0' to '9' =>
			return character'pos(char) - character'pos('0');
		when 'a' to 'f' =>
			return character'pos(char) - character'pos('a') + 10;
		when 'A' to 'F' =>
			return character'pos(char) - character'pos('A') + 10;
		when others =>
			assert false report "char2hex(): unexpected character '" & char & "'" severity failure;
	end case;
	return 0;
end char2hex;

impure function get_string(value: in integer; len: in integer; base: in integer) return string is
	variable str: string(1 to 8) := "????????"; 
	variable m, d: integer;
	
begin
	d := value;
	
	for i in 0 to len - 1 loop
		m := d mod base;
		d := d / base;
		case m is
			when 0 => str(8 - i) := '0';
			when 1 => str(8 - i) := '1';
			when 2 => str(8 - i) := '2';
			when 3 => str(8 - i) := '3';
			when 4 => str(8 - i) := '4';
			when 5 => str(8 - i) := '5';
			when 6 => str(8 - i) := '6';
			when 7 => str(8 - i) := '7';
			when 8 => str(8 - i) := '8';
			when 9 => str(8 - i) := '9';
			when 10 => str(8 - i) := 'A';
			when 11 => str(8 - i) := 'B';
			when 12 => str(8 - i) := 'C';
			when 13 => str(8 - i) := 'D';
			when 14 => str(8 - i) := 'E';
			when 15 => str(8 - i) := 'F';
			when others =>
				assert false report "get_string() reached unexpected case m =" & integer'image(m) severity failure; 
		end case;
	end loop;
	
	return str(8 - len + 1 to 8);
	
end get_string;

impure function parseBinary8(bin_str: in string) return std_logic_vector is
	variable val: std_logic_vector(7 downto 0) := "00000000";
begin
	--report "parseBinary8(" & bin_str & ")" severity note;
	for i in bin_str'left to bin_str'right loop
		case bin_str(i) is
			when '0' =>
				val := val(6 downto 0) & "0";
			when '1'|'X' => -- interpret X as '1' due to bus signal being low active - this way is undefined microinstruction is executed, bus won't short!
				val := val(6 downto 0) & "1";
			when others =>
				assert false report "parseBinary8(): unexpected character '" & bin_str(i) & "'" severity failure;
		end case;
	end loop;

	return val;
end parseBinary8;

impure function parseBinary16(bin_str: in string) return std_logic_vector is
begin
	--report "parseBinary16(" & bin_str & ")" severity note;
	return parseBinary8(bin_str(1 to 8)) & parseBinary8(bin_str(9 to 16));
end parseBinary16;

impure function parseHex16(hex_str: in string) return std_logic_vector is
	variable intVal: integer := 0;
begin
	--report "parseHex16(" & hex_str & ")" severity note;
	
	for i in hex_str'left to hex_str'right loop
		intVal := 16 * intVal + char2hex(hex_str(i));
	end loop;
	return std_logic_vector(to_unsigned(intVal, 16));
end parseHex16;

impure function decode2(val: std_logic; s0: string; s1: string) return string is
begin
	if (val = '0') then
		return s0;
	else
		return s1;
	end if;
end decode2;

impure function decode4(val: std_logic_vector(1 downto 0); s0: string; s1: string; s2: string; s3: string) return string is
begin
	if (val(1) = '0') then
		return decode2(val(0), s0, s1);
	else
		return decode2(val(0), s2, s3);
	end if;
end decode4;

impure function decode8(val: std_logic_vector(2 downto 0);  s0: string; s1: string; s2: string; s3: string;  s4: string; s5: string; s6: string; s7: string) return string is
begin
	if (val(2) = '0') then
		return decode4(val(1 downto 0), s0, s1, s2, s3);
	else
		return decode4(val(1 downto 0), s4, s5, s6, s7);
	end if;
end decode8;

impure function decode16(val: std_logic_vector(3 downto 0);  s0: string; s1: string; s2: string; s3: string;  s4: string; s5: string; s6: string; s7: string; s8: string; s9: string; s10: string; s11: string; s12: string; s13: string; s14: string; s15: string) return string is
begin
	if (val(3) = '0') then
		return decode8(val(2 downto 0), s0, s1, s2, s3, s4, s5, s6, s7);
	else
		return decode8(val(2 downto 0), s8, s9, s10, s11, s12, s13, s14, s15);
	end if;
end decode16;

impure function get_mask(mask: in std_logic_vector(3 downto 0); sinclair_mode: in boolean) return string is
begin
	if (sinclair_mode) then
		-- see here: https://github.com/shirriff/TICalculatorJSSimulator/blob/master/masks_sinclair.js
		return decode16(mask,
				 "00000000000",
				 "5..........",
				 "..00.......",
				 "....1......",
				 "....0000000",
				 "..........1",
				 "..01.......",
				 ".5.........",
				 "000000.....",
				 "0001.......",
				 "....0000001",
				 ".....1.....",
				 "....00005..",
				 "....00001..",
				 "....4......",
				 "....0......"
			);
	else
		-- see here: 
		return decode16(mask, 
				"F0/DPT7 ..........7",
				"F1/EXPD .........4.",
				"F2/LSD  ........1..",
				"F3      .......0...",
				"F4      ......0....",
				"F5      .....0.....",
				"F6      ....0......",
				"F9      .0.........",
				"F10/OV1 1..........",
				"OPFGS   ........000",
				"MSD1    01.........",
				"MANT1   000000001..",
				"MANT    000000000..",
				"EXP1    .........01",
				"EXP     .........00",
				"ALL     00000000000"
			);
	end if;
end get_mask;

-- simple instruction disassemble
impure function unassemble(instruction: in std_logic_vector(11 downto 0); full: in std_logic; sinclair_mode: in boolean) return string is
alias mask: std_logic_vector(3 downto 0) is instruction(3 downto 0);
alias target: std_logic_vector(8 downto 0) is instruction(8 downto 0);

begin
	case instruction(10 downto 7) is
		when "0000" | "0001" | "0010" | "0011" =>
			return "BC0 " & decode2(full, "", get_string(to_integer(unsigned(target)), 3, 16));
			-- jump if condition reset
		when "0100" | "0101" | "0110" | "0111" =>
			-- jump if condition set
			return "BC1 " & decode2(full, "", get_string(to_integer(unsigned(target)), 3, 16));
		when "1000" =>
			-- address if key down on KO (0 to 127)
			return "BKO " & decode2(full, "", get_string(to_integer(unsigned(target)), 3, 16));
		when "1001" =>
			-- address if key down on KP (128 to 255)
			return "BKP " & decode2(full, "", get_string(to_integer(unsigned(target)), 3, 16));
		when "1010" | "1011" => 
			-- flag instructions
			case instruction(7 downto 4) is
				when "0000" =>
					return "NOP16 " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "0001" =>
					return "WAITDK " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "0010" =>
					return "WAITNO " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "0011" =>
					return "SFB " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "0100" =>
					return "SFA " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "0101" =>
					return "SYNCH " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "0110" =>
					return "SCANNO " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "0111" =>
					return "ZFB " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "1000" =>
					return "ZFA " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "1001" =>
					return "TFB " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "1010" =>
					return "TFA " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "1011" =>
					return "FFB " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "1100" =>
					return "FFA " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "1101" =>
					return "CF " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "1110" =>
					return "NOP30 " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "1111" =>
					return "EXF " & decode2(full, "", get_mask(mask, sinclair_mode));
				when others =>
					assert false report "unassemble(): unrecognized instruction '" & get_string(to_integer(unsigned(instruction)), 3, 16) & "'" severity failure;
			end case;
		when "1100" | "1101" =>
			-- register instructions
			case instruction(7 downto 4) is
				when "0000" =>
					return "AABA " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "0001" =>
					return "AAKA " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "0010" =>
					return "AAKC " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "0011" =>
					if (sinclair_mode) then
						return "ACBB " & decode2(full, "", get_mask(mask, sinclair_mode));
					else
						return "ABOA " & decode2(full, "", get_mask(mask, sinclair_mode));
					end if;
				when "0100" =>
					return "ABOC " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "0101" =>
					return "ACKA " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "0110" =>
					return "ACKB " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "0111" =>
					return "SABA " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "1000" =>
					return "SABC " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "1001" =>
					return "SAKA " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "1010" =>
					return "SCBC " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "1011" =>
					return "SCKC " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "1100" =>
					return "CAB " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "1101" =>
					return "CAK " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "1110" =>
					return "CCB " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "1111" =>
					return "CCK " & decode2(full, "", get_mask(mask, sinclair_mode));
				when others =>
					assert false report "unassemble(): unrecognized instruction '" & get_string(to_integer(unsigned(instruction)), 3, 16) & "'" severity failure;
			end case;
		when "1110" | "1111" =>
			-- register instructions
			case instruction(7 downto 4) is
				when "0000" =>
					return "AKA " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "0001" =>
					return "AKB " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "0010" =>
					return "AKC " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "0011" =>
					return "EXAB " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "0100" =>
					return "SLLA " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "0101" =>
					return "SLLB " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "0110" =>
					return "SLLC " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "0111" =>
					return "SRLA " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "1000" =>
					return "SRLB " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "1001" =>
					return "SRLC " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "1010" =>
					return "AKCN " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "1011" =>
					if (sinclair_mode) then
						return "SCBA " & decode2(full, "", get_mask(mask, sinclair_mode));
					else
						return "AAKAH " & decode2(full, "", get_mask(mask, sinclair_mode));
					end if;
				when "1100" =>
					if (sinclair_mode) then
						return "SCKB " & decode2(full, "", get_mask(mask, sinclair_mode));
					else
						return "SAKAH " & decode2(full, "", get_mask(mask, sinclair_mode));
					end if;
				when "1101" =>
					return "ACKC " & decode2(full, "", get_mask(mask, sinclair_mode));
				when "1110" =>
					if (sinclair_mode) then
						return "AABC " & decode2(full, "", get_mask(mask, sinclair_mode));
					else 
						return "NOP " & decode2(full, "", get_mask(mask, sinclair_mode));
					end if;
				when "1111" =>
					if (sinclair_mode) then
						return "ACBC " & decode2(full, "", get_mask(mask, sinclair_mode));
					else
						return "NOP " & decode2(full, "", get_mask(mask, sinclair_mode));
					end if;
				when others =>
					assert false report "unassemble(): unrecognized instruction '" & get_string(to_integer(unsigned(instruction)), 3, 16) & "'" severity failure;
			end case;
			when others =>
				assert false report "unassemble(): unrecognized instruction '" & get_string(to_integer(unsigned(instruction)), 3, 16) & "'" severity failure;
	end case;

	return "???";

end unassemble;


end tms0800_package;
