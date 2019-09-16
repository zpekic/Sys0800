----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    16:51:58 03/15/2019 
-- Design Name: 
-- Module Name:    tinyrom - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: http://files.righto.com/calculator/TI_calculator_simulator.html
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use STD.textio.all;
--use ieee.std_logic_textio.all;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

use work.tms0800_package.all;

entity rom512x12 is
	 Generic (
			fill_value: std_logic_vector(11 downto 0);
			sinclair_mode: boolean;
			asm_filename: string;
			lst_filename: string);
    Port ( 
			address : in  STD_LOGIC_VECTOR (8 downto 0);
         data : out  STD_LOGIC_VECTOR (11 downto 0));
end rom512x12;

architecture Behavioral of rom512x12 is

alias a9: std_logic_vector(8 downto 0) is address(8 downto 0);

type rom_array is array(0 to 511) of std_logic_vector(11 downto 0);

impure function load_mem(mif_file_name : in string; depth: in integer; default_value: std_logic_vector(11 downto 0)) return rom_array is
    variable temp_mem : rom_array;-- := (others => (others => default));
	 -- mif file variables
    file mif_file : text; -- open read_mode is mif_file_name;
    variable mif_line : line;
	 variable char: character;
	 variable line_cnt: integer := 1;
	 variable isOk: boolean;
	 variable word_binaddress, word_hexaddress: std_logic_vector(8 downto 0);
	 variable word_binvalue: std_logic_vector(11 downto 0);
	 variable hex_cnt: integer;
	 variable continue: boolean := true;

begin
	 -- fill with default value
	 for i in 0 to depth - 1 loop	
		temp_mem(i) := default_value;
	 end loop;
	 report "init_bytememory(): initialized " & integer'image(depth) & " bytes of memory to " & integer'image(to_integer(unsigned(default_value))) severity note;
	 -- parse the file for the data
	 report "init_bytememory(): loading memory from file " & mif_file_name severity note;
	 file_open(mif_file, mif_file_name, read_mode);
	 while continue and (not endfile(mif_file)) loop--till the end of file is reached continue.
      readline (mif_file, mif_line);
		--next when mif_line'length = 0;  -- Skip empty lines
		--report "init_mem(): line " & integer'image(line_cnt) & " read";
		isOk := true;
		hex_cnt := 0;
		while isOk loop
			read(mif_line, char, isOk);
			if (isOk) then
				--assert false report "init_wordmemory(): line=" & integer'image(line_cnt) & " char='" & char & "' hex_cnt=" & integer'image(hex_cnt) severity note; 
				case char is
					when '.' =>
						if (hex_cnt = 0) then
							report mif_file_name & " has been processed." severity NOTE;
							continue := false;
							file_close(mif_file);
							return temp_mem;
						end if;

					when ' '|tab =>
						null;
						--if (hex_cnt > 32) then
						--      isOk := false;
						--		report "init_wordmemory(): space detected, enough data parsed.";
						--end if;

					when '/' =>
						report "init_wordmemory(" & integer'image(hex_cnt) & "): comment detected, rest of line is ignored";
						exit;

					when '0'|'1' =>
						case hex_cnt is
							when 0 =>
								word_binaddress := "00000000" & char2logic(char);
							when 1 to 10 =>
								word_binaddress := word_binaddress(7 downto 0) & char2logic(char);
							when 12 =>
								word_hexaddress := "00000" & std_logic_vector(to_unsigned(char2hex(char), 4));
							when 13 to 14 =>
								word_hexaddress := word_hexaddress(4 downto 0) & std_logic_vector(to_unsigned(char2hex(char), 4));
							when 15 =>
								word_hexaddress := word_hexaddress(4 downto 0) & std_logic_vector(to_unsigned(char2hex(char), 4));
								if (word_binaddress /= word_hexaddress) then
									assert false report "init_wordmemory(): addresses not matching in line " & integer'image(line_cnt) severity failure; 
								else 
									report "init_wordmemory(" & integer'image(hex_cnt) & "): addresses " & get_string(to_integer(unsigned(word_hexaddress)), 3, 16) & " matching in line " & integer'image(line_cnt) severity note; 
								end if;
							when 17 =>
								word_binvalue := "00000000000" & char2logic(char);
								--report "init_wordmemory(" & integer'image(hex_cnt) & "): binvalue=" & get_string(to_integer(unsigned(word_binvalue)), 3, 16) & " in line " & integer'image(line_cnt) severity note; 
							when 18 to 28 =>
								word_binvalue := word_binvalue(10 downto 0) & char2logic(char);
								--report "init_wordmemory(" & integer'image(hex_cnt) & "): binvalue=" & get_string(to_integer(unsigned(word_binvalue)), 3, 16) & " in line " & integer'image(line_cnt) severity note; 
							when 29 =>
								word_binvalue := word_binvalue(10 downto 0) & char2logic(char);
								--report "init_wordmemory(" & integer'image(hex_cnt) & "): binvalue=" & get_string(to_integer(unsigned(word_binvalue)), 3, 16) & " in line " & integer'image(line_cnt) severity note; 
								temp_mem(to_integer(unsigned(word_binaddress))) := word_binvalue;
								report "init_wordmemory(" & integer'image(hex_cnt) & "): temp_mem(" & integer'image(to_integer(unsigned(word_binaddress))) & ") := " & integer'image(to_integer(unsigned(word_binvalue))) severity note;
							when others =>
								null;
						end case;

					when '*' => -- indicate breakpoint if at the right column, otherwise ignore
						case hex_cnt is
							when 0 to 29 =>								
								assert false report "init_wordmemory(" & integer'image(hex_cnt) & "): unexpected char '" & char & "' in line " & integer'image(line_cnt) severity note; 
							when 30 =>
								temp_mem(to_integer(unsigned(word_binaddress))) := X"800" or word_binvalue;
								report "init_wordmemory(" & integer'image(hex_cnt) & "): breakpoint set at " & integer'image(to_integer(unsigned(word_binaddress))) severity note;
								exit; -- no need to parse this line further
							when others =>
								null;
						end case;

					when '2' to '9'|'a' to 'f'|'A' to 'F' =>
						case hex_cnt is
							when 12 =>
								word_hexaddress := "00000" & std_logic_vector(to_unsigned(char2hex(char), 4));
							when 13 to 14 =>
								word_hexaddress := word_hexaddress(4 downto 0) & std_logic_vector(to_unsigned(char2hex(char), 4));
							when 15 =>
								word_hexaddress := word_hexaddress(4 downto 0) & std_logic_vector(to_unsigned(char2hex(char), 4));
								if (word_binaddress /= word_hexaddress) then
									assert false report "init_wordmemory(" & integer'image(hex_cnt) & "): addresses not matching in line " & integer'image(line_cnt) severity failure; 
								end if;
							when others =>
								assert false report "init_wordmemory(" & integer'image(hex_cnt) & "): unexpected char '" & char & "' in line " & integer'image(line_cnt) severity note; 
								exit;
						end case;
					
					when others =>
						assert false report "init_wordmemory(" & integer'image(hex_cnt) & "): unexpected char '" & char & "' in line " & integer'image(line_cnt) severity note; 
						exit;
				end case;
			
				hex_cnt := hex_cnt + 1;
				
			else
				report "init_bytememory(" & integer'image(hex_cnt) & "): end of line " & integer'image(line_cnt) & " reached";
			end if;
		end loop;
		
		line_cnt := line_cnt + 1;
	end loop; -- next line in file
 
	file_close(mif_file);
   return temp_mem;
	
end load_mem;

impure function dump_mem(hex_file_name: in string; depth: in integer; temp_mem: in rom_array) return boolean is
	 -- hex file variables
	 file hex_file : text; -- open write_mode is hex_file_name;
	 variable hex_line : line;
	 variable checksum: integer;
	 variable status: FILE_OPEN_STATUS;
	
begin
	-- dump memory content to Intel hex-format like file
	file_open(status, hex_file, hex_file_name, write_mode);
	report "FILE_OPEN_STATUS of " & hex_file_name & " is " & FILE_OPEN_STATUS'IMAGE(status) severity note;
	
	write(hex_line, string'("type rom is array(0 to " & integer'image(depth - 1) & ") of std_logic_vector(11 downto 0);"));
	writeline(hex_file, hex_line);
	write(hex_line, string'("constant program: rom := ("));
	writeline(hex_file, hex_line);
	
	for i in 0 to depth - 1 loop
		--report integer'image(i) severity note;
		write(hex_line, string'("X"""));
		write(hex_line, get_string(i, 3, 16));
		write(hex_line, string'(""" => X"""));
		write(hex_line, get_string(to_integer(unsigned(temp_mem(i))), 3, 16));
		write(hex_line, string'(""" --"));
		write(hex_line, unassemble(temp_mem(i), '1', sinclair_mode));
		writeline(hex_file, hex_line);
   end loop;

   write(hex_line, string'(");")); -- write last line
	writeline(hex_file, hex_line);

   file_close(hex_file);
	return true;
end dump_mem;
		
impure function init_wordmemory(mif_file_name : in string; hex_file_name: in string; depth: in integer; default_value: std_logic_vector(11 downto 0); sinclair_mode: in boolean) return rom_array is
    variable temp_mem : rom_array;-- := (others => (others => default));
	 variable dummy: boolean;

begin
	temp_mem := load_mem(mif_file_name, depth, default_value);
	dummy := dump_mem(hex_file_name, depth, temp_mem);
	return temp_mem;
end init_wordmemory;
	
signal data_from_file: rom_array := init_wordmemory(asm_filename, lst_filename, 512, fill_value, sinclair_mode);
attribute rom_style : string;
attribute rom_style of data_from_file : signal is "block";

begin
	data <= data_from_file(to_integer(unsigned(a9)));

end Behavioral;

