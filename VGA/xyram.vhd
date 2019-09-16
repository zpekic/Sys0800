----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    21:43:00 06/17/2019 
-- Design Name: 
-- Module Name:    xyram - Behavioral 
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

entity xyram is
	 generic (maxram: integer;
				 maxrow: integer;
				 maxcol: integer);
    Port ( clk : in  STD_LOGIC;
           we : in  STD_LOGIC;
           x : in  STD_LOGIC_VECTOR (7 downto 0);
           y : in  STD_LOGIC_VECTOR (7 downto 0);
			  mode: in STD_LOGIC_VECTOR (7 downto 0);
           nDigit : in  STD_LOGIC_VECTOR (8 downto 0);
           segment : in  STD_LOGIC_VECTOR(7 downto 0);
           din : in  STD_LOGIC_VECTOR (7 downto 0);
			  field: buffer STD_LOGIC_VECTOR(1 downto 0);
           dout : out  STD_LOGIC_VECTOR (7 downto 0));
end xyram;

architecture Behavioral of xyram is

COMPONENT ram4k8
  PORT (
    clka : IN STD_LOGIC;
    wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addra : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
    dina : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
  );
END COMPONENT;

component mux11x4 is
    Port ( e : in  STD_LOGIC_VECTOR (10 downto 0);
           x : in  STD_LOGIC_VECTOR (43 downto 0);
           y : out  STD_LOGIC_VECTOR (3 downto 0));
end component;

--type ram4k is array(0 to (maxram - 1)) of std_logic_vector(7 downto 0);
--signal vram: ram4k := (others => X"2E"); -- dot
--attribute ram_style: string;
--attribute ram_style of vram : signal is "block";

type rom256 is array(0 to 255) of std_logic_vector(7 downto 0);

constant vrom_ti: rom256 := (
X"20", X"20", X"20", X"20", X"20", X"54", X"4d", X"53", X"30", X"38", X"30", X"30", X"2d", X"62", X"61", X"73", X"65", X"64", X"20", X"63", X"61", X"6c", X"63", X"75", X"6c", X"61", X"74", X"6f", X"72", X"20", X"6f", X"6e", X"20", X"46", X"50", X"47", X"41", X"20", X"2d", X"20", X"68", X"74", X"74", X"70", X"73", X"3a", X"2f", X"2f", X"67", X"69", X"74", X"68", X"75", X"62", X"2e", X"63", X"6f", X"6d", X"2f", X"7a", X"70", X"65", X"6b", X"69", X"63", X"2f", X"53", X"79", X"73", X"30", X"38", X"30", X"30", X"2f", X"20", X"20", X"20", X"20", X"20", X"20",
X"2a", X"20", X"20", X"20", X"20", X"54", X"49", X"20", X"44", X"61", X"74", X"61", X"6d", X"61", X"74", X"68", X"20", X"6d", X"6f", X"64", X"65", X"20", X"28", X"53", X"57", X"37", X"3d", X"6f", X"6e", X"20", X"61", X"6e", X"64", X"20", X"72", X"65", X"73", X"65", X"74", X"20", X"66", X"6f", X"72", X"20", X"53", X"69", X"6e", X"63", X"6c", X"61", X"69", X"72", X"2c", X"20", X"70", X"72", X"65", X"73", X"73", X"20", X"42", X"54", X"4e", X"33", X"20", X"74", X"6f", X"20", X"73", X"74", X"61", X"72", X"74", X"20", X"29", X"20", X"20", X"20", X"20", X"2a", 
X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"28", X"69", X"6e", X"73", X"70", X"69", X"72", X"65", X"64", X"20", X"62", X"79", X"20", X"68", X"74", X"74", X"70", X"3a", X"2f", X"2f", X"72", X"69", X"67", X"68", X"74", X"6f", X"2e", X"63", X"6f", X"6d", X"2f", X"74", X"69", X"20", X"29", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", 
X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20"
); 

constant vrom_sinclair: rom256 := (
X"20", X"20", X"20", X"20", X"20", X"54", X"4d", X"53", X"30", X"38", X"30", X"30", X"2d", X"62", X"61", X"73", X"65", X"64", X"20", X"63", X"61", X"6c", X"63", X"75", X"6c", X"61", X"74", X"6f", X"72", X"20", X"6f", X"6e", X"20", X"46", X"50", X"47", X"41", X"20", X"2d", X"20", X"68", X"74", X"74", X"70", X"73", X"3a", X"2f", X"2f", X"67", X"69", X"74", X"68", X"75", X"62", X"2e", X"63", X"6f", X"6d", X"2f", X"7a", X"70", X"65", X"6b", X"69", X"63", X"2f", X"53", X"79", X"73", X"30", X"38", X"30", X"30", X"2f", X"20", X"20", X"20", X"20", X"20", X"20",
X"2a", X"20", X"53", X"69", X"6e", X"63", X"6c", X"61", X"69", X"72", X"20", X"53", X"63", X"69", X"65", X"6e", X"74", X"69", X"66", X"69", X"63", X"20", X"6d", X"6f", X"64", X"65", X"20", X"28", X"53", X"57", X"37", X"3d", X"6f", X"66", X"66", X"20", X"61", X"6e", X"64", X"20", X"72", X"65", X"73", X"65", X"74", X"20", X"66", X"6f", X"72", X"20", X"54", X"49", X"2c", X"20", X"70", X"72", X"65", X"73", X"73", X"20", X"42", X"54", X"4e", X"33", X"20", X"74", X"6f", X"20", X"73", X"74", X"61", X"72", X"74", X"29", X"20", X"20", X"20", X"20", X"20", X"2a",
X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"28", X"69", X"6e", X"73", X"70", X"69", X"72", X"65", X"64", X"20", X"62", X"79", X"20", X"68", X"74", X"74", X"70", X"3a", X"2f", X"2f", X"72", X"69", X"67", X"68", X"74", X"6f", X"2e", X"63", X"6f", X"6d", X"2f", X"73", X"69", X"6e", X"63", X"6c", X"61", X"69", X"72", X"20", X"29", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", 
X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20", X"20"
); 

type rom64 is array(0 to 63) of std_logic_vector(7 downto 0);
constant sevensegmask: rom64 := (
X"00", X"00", X"00", X"00", X"00", X"00", X"00", X"00",
X"20", X"01", X"01", X"01", X"02", X"00", X"00", X"00",
X"20", X"00", X"00", X"00", X"02", X"00", X"00", X"00",
X"20", X"00", X"00", X"00", X"02", X"00", X"00", X"00",
X"00", X"40", X"40", X"40", X"00", X"00", X"00", X"00",
X"10", X"00", X"00", X"00", X"04", X"00", X"00", X"00",
X"10", X"00", X"00", X"00", X"04", X"00", X"00", X"00",
X"10", X"08", X"08", X"08", X"04", X"00", X"80", X"00"
);

type ram16 is array(0 to 15) of std_logic_vector(7 downto 0);
signal seg_mem: ram16;

--attribute ram_style of vrom : signal is "block";
--attribute ram_init_file: string;
--attribute ram_init_file of vrom : signal is "logo.mif";

signal x_ok, y_ram, y_rom: std_logic;
signal vram_addr, vrom_addr, y64, y16, x1: std_logic_vector(11 downto 0);
--signal field: std_logic_vector(1 downto 0);
signal douta, vrom: std_logic_vector(7 downto 0);

signal fontmask, sevensegchar: std_logic_vector(7 downto 0);
signal maskaddress: std_logic_vector(5 downto 0);
signal write_address: std_logic_vector(3 downto 0);
--signal nDig, nSeg: std_logic;

begin

x_ok <= '1' when (unsigned(x) < maxcol) else '0';
y_ram <= x_ok when (unsigned(y) < maxrow) else '0';
y_rom <= x_ok when (unsigned(y) > 56) else '0';

x1 <= "0000" & x;
y16 <= y & "0000";
y64 <= y(5 downto 0) & "000000";
vram_addr <= std_logic_vector(unsigned(y64) + unsigned(y16) + unsigned(x1));	-- a = 80 * y + x
vrom_addr <= std_logic_vector(unsigned(vram_addr) - (57 * 80));					-- map to row 57..59
vrom <= vrom_sinclair(to_integer(unsigned(vrom_addr(7 downto 0)))) when (mode(7) = '1') else vrom_ti(to_integer(unsigned(vrom_addr(7 downto 0))));

--update_vram: process(we, y_ram, clk)
--begin
--	if (rising_edge(clk) and we = '1' and y_ram = '1') then
--		vram(to_integer(unsigned(vram_addr))) <= din;
--	end if;
--end process;

vram: ram4k8 PORT map 
(
    clka => clk, 
    wea => "" & we and y_ram & "",
    addra => vram_addr,
    dina => din,
    douta => douta
);

field <= y_rom & y_ram;

-- "paint" the 7seg state on VGA
addrmux: mux11x4 port map 
	(
		e => '1' & nDigit & '1',
		x => X"01234567890",
		y => write_address
	);

writeseg: process(clk, segment, write_address)
begin
	if (rising_edge(clk)) then
		if (write_address = X"0") then
			seg_mem(0) <= X"00";
		else
			seg_mem(to_integer(unsigned(write_address))) <= segment;
		end if;
	end if;
end process;

maskaddress <= y(2 downto 0) & x(2 downto 0);
fontmask <= sevensegmask(to_integer(unsigned(maskaddress))) and seg_mem(to_integer(unsigned(x(6 downto 3))));
sevensegchar <= X"20" when (fontmask = X"00") else X"A0";
--nSeg <= '1' when (fontmask = X"00") else '0';
--
--with x(6 downto 3) select
--	nDig <= 	nDigit(8) when X"1",
--				nDigit(7) when X"2",
--				nDigit(6) when X"3",
--				nDigit(5) when X"4",
--				nDigit(4) when X"5",
--				nDigit(3) when X"6",
--				nDigit(2) when X"7",
--				nDigit(1) when X"8",
--				nDigit(0) when X"9",
--				'1' when others;
--				
--sevensegchar <= X"A0" when ((nDig or nSeg) = '0') else X"20";
--	
with field select
	dout <= 	douta 			when "01",		-- x < 80, y < 50
				vrom				when "10",		-- x < 80, y > 56
				sevensegchar	when others;	-- calculator result display

end Behavioral;

