--	Package File Template
--
--	Purpose: This package defines supplemental types, subtypes, 
--		 constants, and functions 

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.numeric_std.all;

package tms0800_package is

--type t_sevenseg is std_logic_vector(7 downto 0);

constant pattern0: std_logic_vector(7 downto 0) := "00111111";   --0
constant pattern1: std_logic_vector(7 downto 0) := "00000110";   --1
constant pattern2: std_logic_vector(7 downto 0) := "01011011";   --2
constant pattern3: std_logic_vector(7 downto 0) := "01001111";   --3
constant pattern4: std_logic_vector(7 downto 0) := "01100110";   --4
constant pattern5: std_logic_vector(7 downto 0) := "01101101";   --5
constant pattern6: std_logic_vector(7 downto 0) := "01111101";   --6
constant pattern7: std_logic_vector(7 downto 0) := "00000111";   --7
constant pattern8: std_logic_vector(7 downto 0) := "01111111";   --8
constant pattern9: std_logic_vector(7 downto 0) := "01101111";   --9
constant pattern_minus: std_logic_vector(7 downto 0) := "01000000";   --(show minus sign)
constant pattern_blank: std_logic_vector(7 downto 0) := "00000000";

constant char_NULL: std_logic_vector(7 downto 0) := X"00";
constant char_CR: std_logic_vector(7 downto 0) := X"0D";
constant char_LF: std_logic_vector(7 downto 0) := X"0A";

-- selections for tracer unit
constant t_af : 		std_logic_vector(3 downto 0) := "0000";
constant t_bf : 		std_logic_vector(3 downto 0) := "0001";
constant t_cf : 		std_logic_vector(3 downto 0) := "0010";
constant t_a : 		std_logic_vector(3 downto 0) := "0011";
constant t_b : 		std_logic_vector(3 downto 0) := "0100";
constant t_c : 		std_logic_vector(3 downto 0) := "0101";
constant t_instr0 : 	std_logic_vector(3 downto 0) := "0110";
constant t_instr1 : 	std_logic_vector(3 downto 0) := "0111";
constant t_instr2 : 	std_logic_vector(3 downto 0) := "1000";
constant t_pc0: 		std_logic_vector(3 downto 0) := "1001";
constant t_pc1: 		std_logic_vector(3 downto 0) := "1010";
constant t_pc2: 		std_logic_vector(3 downto 0) := "1011";

-- special microcode "goto" codes (all others will be jump to that location)
constant upc_next:   std_logic_vector(7 downto 0) := X"00"; -- means we can't jump to location 0!
constant upc_return: std_logic_vector(7 downto 0) := X"01"; -- means we can't jump to location 1!
constant upc_repeat: std_logic_vector(7 downto 0) := X"FF"; -- means we can't jump to location 255!
constant upc_fork:   std_logic_vector(7 downto 0) := X"FE"; -- means we can't jump to location 254!

-- 2 bit microinstruction fields
constant pc_nop: 	std_logic_vector(1 downto 0) 		:= "00";
constant pc_clear:std_logic_vector(1 downto 0) 		:= "01";
constant pc_next: std_logic_vector(1 downto 0) 		:= "10";
constant pc_load: std_logic_vector(1 downto 0) 		:= "11";

constant e_nop,	digit_nop: 	std_logic_vector(1 downto 0) 		:= "00";
constant e_init,	digit_in1:	std_logic_vector(1 downto 0) 		:= "01";
constant e_shl,	digit_in2: 	std_logic_vector(1 downto 0) 		:= "10";
constant e_shr,	digit_in3: 	std_logic_vector(1 downto 0) 		:= "11";

-- 3 bit microinstruction fields

-- 16 control unit branch conditions
constant cond_false: integer := 15; 
constant cond_charsent: integer := 14;
constant cond_enabletrace: integer := 13;
constant cond_cond: integer := 12;
constant cond_11: integer := 11;
constant cond_10: integer := 10;
constant cond_9: integer := 9;
constant cond_8: integer := 8;
constant cond_7: integer := 7;
constant cond_6: integer := 6;
constant cond_5: integer := 5;
constant cond_4: integer := 4;
constant cond_3: integer := 3;
constant cond_2: integer := 2;
constant cond_1: integer := 1;
constant cond_true: integer := 0;


impure function char2logic(char: in character) return std_logic;
impure function char2hex(char: in character) return integer;
impure function get_string(value: in integer; len: in integer; base: in integer) return string;
impure function parseBinary8(bin_str: in string) return std_logic_vector;
impure function parseBinary16(bin_str: in string) return std_logic_vector;
impure function get_mask(mask: in std_logic_vector(3 downto 0)) return string;
impure function unassemble(instruction: in std_logic_vector(10 downto 0)) return string;

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

impure function get_mask(mask: in std_logic_vector(3 downto 0)) return string is
begin
	case mask is
		when "0000" =>
			return "F0/DPT7";
		when "0001" =>
			return "F1/EXPD";
		when "0010" =>
			return "F2/LSD";
		when "0011" =>
			return "F3";
		when "0100" =>
			return "F4";
		when "0101" =>
			return "F5";
		when "0110" =>
			return "F6";
		when "0111" =>
			return "F9";
		when "1000" =>
			return "F10/OV1";
		when "1001" =>
			return "OPFGS ";
		when "1010" =>
			return "MSD1 ";
		when "1011" =>
			return "MANT1 ";
		when "1100" =>
			return "MANT ";
		when "1101" =>
			return "EXP1 ";
		when "1110" =>
			return "EXP ";
		when "1111" =>
			return "ALL ";
		when others =>
			return "???";
	end case;
end get_mask;

impure function unassemble(instruction: in std_logic_vector(11 downto 0)) return string is
begin
	case instruction(10 downto 7) is
		when "0000" | "0001" | "0010" | "0011" =>
			return "BC0 " & get_string(to_integer(unsigned(instruction(8 downto 0))), 3, 16);
			-- jump if condition reset
		when "0100" | "0101" | "0110" | "0111" =>
			-- jump if condition set
			return "BC1 " & get_string(to_integer(unsigned(instruction(8 downto 0))), 3, 16);
		when "1000" =>
			-- address if key down on KO (0 to 127)
			return "BKO " & get_string(to_integer(unsigned(instruction(7 downto 0))), 3, 16);
		when "1001" =>
			-- address if key down on KP (128 to 255)
			return "BKP " & get_string(to_integer(unsigned(instruction(7 downto 0))), 3, 16);
		when "1010" | "1011" => 
			-- flag instructions
			case instruction(7 downto 4) is
				when "0000" =>
					return "NOP16 " & get_mask(instruction(3 downto 0));
				when "0001" =>
					return "WAITDK " & get_mask(instruction(3 downto 0));
				when "0010" =>
					return "WAITNO " & get_mask(instruction(3 downto 0));
				when "0011" =>
					return "SFB " & get_mask(instruction(3 downto 0));
				when "0100" =>
					return "SFA " & get_mask(instruction(3 downto 0));
				when "0101" =>
					return "SYNCH " & get_mask(instruction(3 downto 0));
				when "0110" =>
					return "SCANNO " & get_mask(instruction(3 downto 0));
				when "0111" =>
					return "ZFB " & get_mask(instruction(3 downto 0));
				when "1000" =>
					return "ZFA " & get_mask(instruction(3 downto 0));
				when "1001" =>
					return "TFB " & get_mask(instruction(3 downto 0));
				when "1010" =>
					return "TFA " & get_mask(instruction(3 downto 0));
				when "1011" =>
					return "FFB " & get_mask(instruction(3 downto 0));
				when "1100" =>
					return "FFA " & get_mask(instruction(3 downto 0));
				when "1101" =>
					return "CF " & get_mask(instruction(3 downto 0));
				when "1110" =>
					return "NOP30 " & get_mask(instruction(3 downto 0));
				when "1111" =>
					return "EXF " & get_mask(instruction(3 downto 0));
				when others =>
					return "???";
			end case;
		when "1100" | "1101" =>
			-- register instructions
			case instruction(7 downto 4) is
				when "0000" =>
					return "AABA " & get_mask(instruction(3 downto 0));
				when "0001" =>
					return "AAKA " & get_mask(instruction(3 downto 0));
				when "0010" =>
					return "AAKC " & get_mask(instruction(3 downto 0));
				when "0011" =>
					return "ABOA " & get_mask(instruction(3 downto 0));
				when "0100" =>
					return "ABOC " & get_mask(instruction(3 downto 0));
				when "0101" =>
					return "ACKA " & get_mask(instruction(3 downto 0));
				when "0110" =>
					return "ACKB " & get_mask(instruction(3 downto 0));
				when "0111" =>
					return "SABA " & get_mask(instruction(3 downto 0));
				when "1000" =>
					return "SABC " & get_mask(instruction(3 downto 0));
				when "1001" =>
					return "SAKA " & get_mask(instruction(3 downto 0));
				when "1010" =>
					return "SCBC " & get_mask(instruction(3 downto 0));
				when "1011" =>
					return "SCKC " & get_mask(instruction(3 downto 0));
				when "1100" =>
					return "CAB " & get_mask(instruction(3 downto 0));
				when "1101" =>
					return "CAK " & get_mask(instruction(3 downto 0));
				when "1110" =>
					return "CCB " & get_mask(instruction(3 downto 0));
				when "1111" =>
					return "CCK " & get_mask(instruction(3 downto 0));
				when others =>
					return "???";
			end case;
		when "1110" | "1111" =>
			-- register instructions
			case instruction(7 downto 4) is
				when "0000" =>
					return "AKA " & get_mask(instruction(3 downto 0));
				when "0001" =>
					return "AKB " & get_mask(instruction(3 downto 0));
				when "0010" =>
					return "AKC " & get_mask(instruction(3 downto 0));
				when "0011" =>
					return "EXAB " & get_mask(instruction(3 downto 0));
				when "0100" =>
					return "SLLA " & get_mask(instruction(3 downto 0));
				when "0101" =>
					return "SLLB " & get_mask(instruction(3 downto 0));
				when "0110" =>
					return "SLLC " & get_mask(instruction(3 downto 0));
				when "0111" =>
					return "SRLA " & get_mask(instruction(3 downto 0));
				when "1000" =>
					return "SRLB " & get_mask(instruction(3 downto 0));
				when "1001" =>
					return "SRLC " & get_mask(instruction(3 downto 0));
				when "1010" =>
					return "AKCN " & get_mask(instruction(3 downto 0));
				when "1011" =>
					return "AAKAH " & get_mask(instruction(3 downto 0));
				when "1100" =>
					return "SAKAH " & get_mask(instruction(3 downto 0));
				when "1101" =>
					return "ACKC " & get_mask(instruction(3 downto 0));
				when "1110" =>
					return "??? " & get_mask(instruction(3 downto 0));
				when "1111" =>
					return "??? " & get_mask(instruction(3 downto 0));
				when others =>
					return "???";
			end case;
			when others =>
				return "???";
	end case;
	return "???";
end unassemble;


end tms0800_package;
