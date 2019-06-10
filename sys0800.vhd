----------------------------------------------------------------------------------
-- Company: @Home
-- Engineer: zpekic@hotmail.com
-- 
-- Create Date: 03/10/2019 11:13:02 PM
-- Design Name: Calculator based on TMS0800
-- Module Name: sys0800 - Behavioral
-- Project Name: 
-- Target Devices: https://www.micro-nova.com/mercury/ + Baseboard
-- Input devices: 
-- 	https://store.digilentinc.com/pmod-kypd-16-button-keypad/ (use when SW(0) is off)
-- 	https://www.parallax.com/product/28024 (use when SW(0) is on, RX = PMOD(0), TX = PMOD(4), RST = N/C, GND = PMOD_GND)
-- Tool Versions: ISE 14.7 (nt)
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.99 - Kinda works...
-- Additional Comments:
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;
use work.tms0800_package.all;

entity sys0800 is
    Port ( 
				-- 50MHz on the Mercury board
				CLK: in std_logic;
				-- Master reset button on Mercury board
				USR_BTN: in std_logic; 
				-- Switches on baseboard
				-- SW(0) -- enable tracing after each instruction on TXD
				-- SW(1) -- show program counters instead of regular calculator output  
				-- SW(2) -- 
				-- SW(3) -- 
				-- SW(4) -- 
				-- SW(6 downto 5) -- system clock speed 
				--   0   0	1 Hz	
				--   0   1	1024 Hz 
				--   1   0  2048 Hz
				--   1   1  1.5625 MHz
				-- SW(7)
				--   0   single step mode off (BTN3 should be pressed once to start the system)
				--   1   single step mode on (use with BTN3)
				SW: in std_logic_vector(7 downto 0); 
				-- Push buttons on baseboard
				-- BTN0 - show upper 4 digits 
				-- BTN1 - 
				-- BTN2 - 
				-- BTN3 - single step clock cycle forward if in SS mode (NOTE: single press on this button is needed after reset to unlock SS circuit)
				BTN: in std_logic_vector(3 downto 0); 
				-- Stereo audio output on baseboard
				--AUDIO_OUT_L, AUDIO_OUT_R: out std_logic;
				-- 7seg LED on baseboard 
				A_TO_G: out std_logic_vector(6 downto 0); 
				AN: out std_logic_vector(3 downto 0); 
				DOT: out std_logic; 
				-- 4 LEDs on Mercury board
				--LED: out std_logic_vector(3 downto 0);
				-- ADC interface
				-- channel	input
				-- 0			Audio Left
				-- 1 			Audio Right
				-- 2			Temperature
				-- 3			Light	
				-- 4			Pot
				-- 5			Channel 5 (free)
				-- 6			Channel 6 (free)
				-- 7			Channel 7 (free)
				--ADC_MISO: in std_logic;
				--ADC_MOSI: out std_logic;
				--ADC_SCK: out std_logic;
				--ADC_CSN: out std_logic;
				--PS2_DATA: in std_logic;
				--PS2_CLOCK: in std_logic;
				--VGA interface
				HSYNC: out std_logic;
				VSYNC: out std_logic;
				RED: out std_logic_vector(2 downto 0);
				GRN: out std_logic_vector(2 downto 0);
				BLU: out std_logic_vector(1 downto 0);
				--PMOD interface
				PMOD: inout std_logic_vector(7 downto 0)
          );
end sys0800;

architecture Structural of sys0800 is

component mwvga is
    Port ( reset : in  STD_LOGIC;
           clk : in  STD_LOGIC;
           rgbBorder : in  STD_LOGIC_VECTOR (7 downto 0);
           rgb : out  STD_LOGIC_VECTOR (7 downto 0);
           hsync : out  STD_LOGIC;
           vsync : out  STD_LOGIC;
           hactive : out  STD_LOGIC;
           vactive : out  STD_LOGIC;
           x : out  STD_LOGIC_VECTOR (10 downto 0);
           y : out  STD_LOGIC_VECTOR (10 downto 0));
end component;

-------
-- From: https://www.digikey.com/eewiki/pages/viewpage.action?pageId=28278929#PS/2KeyboardInterface(VHDL)-CodeDownloads
-------
component ps2_keyboard IS
  GENERIC(
    clk_freq              : INTEGER := 50_000_000; --system clock frequency in Hz
    debounce_counter_size : INTEGER := 8);         --set such that (2^size)/clk_freq = 5us (size = 8 for 50MHz)
  PORT(
    clk          : IN  STD_LOGIC;                     --system clock
    ps2_clk      : IN  STD_LOGIC;                     --clock signal from PS/2 keyboard
    ps2_data     : IN  STD_LOGIC;                     --data signal from PS/2 keyboard
    ps2_code_new : OUT STD_LOGIC;                     --flag that new PS/2 code is available on ps2_code bus
    ps2_code     : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)); --code received from PS/2
END component;

-------------------------------------------
-- From : https://opencores.org/websvn/filedetails?repname=ps2core&path=%2Fps2core%2Ftrunk%2Frtl%2Fvhdl%2Fps2.vhd
-------------------------------------------
--component ps2 is
--        port (
--                clk_i : in std_logic;   -- Global clk
--                rst_i : in std_logic;   -- GLobal Asinchronous reset
-- 
--                data_o    : out std_logic_vector(7 downto 0);  -- Data in
--                data_i    : in  std_logic_vector(7 downto 0);  -- Data out
--                ibf_clr_i : in  std_logic;  -- Ifb flag clear input
--                obf_set_i : in  std_logic;  -- Obf flag set input
--                ibf_o     : out std_logic;  -- Received data available
--                obf_o     : out std_logic;  -- Data ready to sent
-- 
--                frame_err_o  : out std_logic;  -- Error receiving data
--                parity_err_o : out std_logic;  -- Error in received data parity
--                busy_o       : out std_logic;  -- uart busy
--                err_clr_i : in std_logic;  -- Clear error flags
-- 
--                wdt_o : out std_logic;  -- Watchdog timer out every 400uS
-- 
--                ps2_clk_io  : inout std_logic;   -- PS2 Clock line
--                ps2_data_io : inout std_logic);  -- PS2 Data line
--end component;

component mux11x4 is
    Port ( e : in  STD_LOGIC_VECTOR (10 downto 0);
           x : in  STD_LOGIC_VECTOR (43 downto 0);
           y : out  STD_LOGIC_VECTOR (3 downto 0));
end component;

component clock_divider is
	 generic (CLK_FREQ: integer);
    Port ( reset : in  STD_LOGIC;
           clock : in  STD_LOGIC;
           slow : out  STD_LOGIC_VECTOR (11 downto 0);
			  baud : out STD_LOGIC_VECTOR(7 downto 0);
           fast : out  STD_LOGIC_VECTOR (4 downto 0)
			 );
end component;

component clocksinglestepper is
    Port ( reset : in STD_LOGIC;
           clock0_in : in STD_LOGIC;
           clock1_in : in STD_LOGIC;
           clock2_in : in STD_LOGIC;
           clock3_in : in STD_LOGIC;
           clocksel : in STD_LOGIC_VECTOR(1 downto 0);
           modesel : in STD_LOGIC;
           singlestep : in STD_LOGIC;
           clock_out : out STD_LOGIC);
end component;

component freqmux is
    Port ( reset : in  STD_LOGIC;
           f0in : in  STD_LOGIC;
           f1in : in  STD_LOGIC;
           sel : in  STD_LOGIC;
           fout : out  STD_LOGIC);
end component;

component debouncer8channel is
    Port ( clock : in  STD_LOGIC;
           reset : in  STD_LOGIC;
           signal_raw : in  STD_LOGIC_VECTOR(7 downto 0);
           signal_debounced : out  STD_LOGIC_VECTOR(7 downto 0));
end component;

component tms0800 is
    Port ( reset : in  STD_LOGIC;
           clk_cpu : in  STD_LOGIC;
           clk_txd : in  STD_LOGIC;
           enable_trace : in  STD_LOGIC;
           enable_breakpoint : in  STD_LOGIC;
           show_debug : in  STD_LOGIC;
           nDigit : out  STD_LOGIC_VECTOR (8 downto 0);
           segment : out  STD_LOGIC_VECTOR(7 downto 0);
			  ka : in  STD_LOGIC;
           kb : in  STD_LOGIC;
           kc : in  STD_LOGIC;
           kd : in  STD_LOGIC;
			  trace_txd: out STD_LOGIC;
			  trace_rxd: in STD_LOGIC;
			  disableSingleStep: out STD_LOGIC;
			  ps2: in STD_LOGIC_VECTOR(15 downto 0);
			  dbg_select: in STD_LOGIC_VECTOR(2 downto 0);
			  dbg_state: out STD_LOGIC_VECTOR(3 downto 0));
end component;

-- various frequencies

signal slow: std_logic_vector(11 downto 0);
signal baud: std_logic_vector(7 downto 0);
signal fast: std_logic_vector(4 downto 0);
--        slow(11) => freq1, -- 1Hz
alias freq2: std_logic is slow(10); -- 2Hz
--        slow(9) => freq4, -- 4Hz
--        slow(8) => freq8, -- 8Hz
--        slow(7) => freq16,  -- 16Hz
--        slow(6) => freq32,  -- 32Hz
--        slow(5) => freq64,  -- 64Hz
--        slow(4) => freq128,  -- 128Hz
--        slow(3) => freq256,  -- 256Hz
alias freq512: std_logic is slow(2);  	-- 512Hz
alias freq1k: std_logic is slow(1);  	-- 1024Hz
alias freq2k: std_logic is slow(0);  	-- 2048Hz
--		  baud(7) => freq300,
--		  baud(6) => freq600,		  
--		  baud(5) => freq1200,
--		  baud(4) => freq2400,
--		  baud(3) => freq4800,
--		  baud(2) => freq9600,
--		  baud(1) => freq19200,
alias freq38400: std_logic is baud(0);		-- 38400
alias freq1M5625: std_logic is fast(4); 	-- 1.5625MHz
--		  fast(3) => freq3M125,
--		  fast(2) => freq6M25,
--		  fast(1) => freq12M5,
alias freq25M: std_logic is fast(0);		-- 25MHz (suitable for 640x480 VGA)

signal reset, nReset: std_logic;
-- debounced inputs
signal switch: std_logic_vector(7 downto 0);
alias enable_trace: std_logic is switch(0);
alias show_debug: std_logic is switch(1);

signal button: std_logic_vector(3 downto 0);
alias show_upper_digits: std_logic is button(0);

alias clk_du: std_logic is freq2k;
alias clk_txd: std_logic is freq38400;

-- keyboard
signal kbd_row, kbd_col, kbd_cnt: std_logic_vector(3 downto 0);
signal ka, kb, kc, kd: std_logic;
------------------------------------------------------------------------------------------------------------------
--		 D8			D7				D6			D5			D4				D3				D2				D1			D0	------------------				
signal k_1, 		k_2, 			k_3, 		k_4, 		k_5, 			k_6, 			k_7, 			k_8, 		k_9: std_logic; -- KC
signal k_clear, 	k_equals, 	k_plus, 	k_minus, k_multiply, k_divide, 	k_cerror, 	k_dot, 	k_0: std_logic; -- KB
------------------------------------------------------------------------------------------------------------------

signal nDigit: std_logic_vector(8 downto 0);
signal segment: std_logic_vector(7 downto 0);
signal led_debug, led_digit8: std_logic_vector(3 downto 0);
signal debugAn: std_logic_vector(3 downto 0) := "1110";
signal tmsAn: std_logic_vector(3 downto 0);

-- PS/2 interface ----------------
signal ps2_pulse: std_logic;
signal ps2_code: std_logic_vector(7 downto 0);
signal ps2_scan: std_logic_vector(15 downto 0);

-- other
signal clk_cpu, clk_ss: std_logic;
signal trace_txd, disableSs: std_logic;

begin
   
	reset <= USR_BTN;
	nReset <= not reset;

    -- FREQUENCY GENERATOR
    one_sec: clock_divider
	 generic map (CLK_FREQ => 50e6)	 
	 port map 
    (
        clock => CLK,
        reset => Reset,
        slow => slow,
		  baud => baud,
		  fast => fast
    );

	-- Single step by each clock cycle, slow or fast
	ss: clocksinglestepper port map (
        reset => Reset,
        clock0_in => freq2,
        clock1_in => freq1k,
        clock2_in => freq38400,
        clock3_in => freq1M5625,
        clocksel => switch(6 downto 5),
        modesel => switch(7),
        singlestep => button(3),
        clock_out => clk_ss
    );

	-- DEBOUNCE the 8 switches, 4 buttons and 4 return lines from PMOD keyboard
    debouncer_sw: debouncer8channel port map (
        clock => freq512,
        reset => Reset,
        signal_raw => SW,
        signal_debounced => switch
    );

    debouncer_kbd_btn: debouncer8channel port map (
        clock => freq512,
        reset => Reset,
        signal_raw(7 downto 4) => PMOD(7 downto 4),
        signal_raw(3 downto 0) => BTN(3 downto 0),
        --signal_debounced(7) => kbd_row(0),
        --signal_debounced(6) => kbd_row(1),
        --signal_debounced(5) => kbd_row(2),
        --signal_debounced(4) => kbd_row(3),
		  signal_debounced(7 downto 4) => kbd_row,
		  signal_debounced(3 downto 0) => button(3 downto 0)
    );

	vga: mwvga Port map ( 
		reset => reset,
		clk => freq25M, 
		rgbBorder => switch,
		rgb(2 downto 0) => RED,
		rgb(5 downto 3) => GRN,
		rgb(7 downto 6) => BLU,
		hsync => HSYNC,
		vsync => VSYNC,
		hactive => open,
		vactive => open,
		x => open,
		y => open
	);

	calculator: tms0800 Port map ( 
		reset => reset,
		clk_cpu => clk_cpu,
		clk_txd => clk_txd,
		enable_trace => enable_trace,
		enable_breakpoint => not switch(7),
		show_debug => show_debug,
		nDigit => nDigit,
		segment => segment,
		ka => ka,
		kb => kb,
		kc => kc,
		kd => kd,
		trace_txd => PMOD(3), 
		trace_rxd => PMOD(2),
		disableSingleStep => disableSs,
		ps2 => X"FFFF" xor ("111" & nDigit & kd & kc & kb & ka), --ps2_scan,
		dbg_select(2) => show_upper_digits,
		dbg_select(1 downto 0) => slow(4 downto 3), 
		dbg_state => led_debug
	);

--clk_cpu <= clk_ss when disableSs = '0' else freq256;
ss_mux: freqmux Port map ( 
				reset => reset,
				f0in => clk_ss,
				f1in => freq2k,
				sel => disableSs,
				fout => clk_cpu
			 );

-- Adapt TMS0800 9 digit common cathode display to 4 digit + 4 leds display
A_TO_G(6) <= not segment(0);
A_TO_G(5) <= not segment(1);
A_TO_G(4) <= not segment(2);
A_TO_G(3) <= not segment(3);
A_TO_G(2) <= not segment(4);
A_TO_G(1) <= not segment(5);
A_TO_G(0) <= not segment(6);
DOT <= not segment(7);

AN <= debugAn when (show_debug = '1') else tmsAn;
--LED <= led_debug when (show_debug = '1') else led_digit8;

tmsAn <= nDigit(7 downto 4) when (show_upper_digits = '1') else nDigit(3 downto 0);

with slow(4 downto 3) select
	debugAn <= 	"1110" when "00",
					"1101" when "01",
					"1011" when "10",
					"0111" when "11";

led_digit8 <= "1111" when (nDigit(8) = '0' and segment = pattern_minus) else "0000";

--kbd: ps2_keyboard generic map
--	(
--		clk_freq => 50_000_000, --system clock frequency in Hz
--		debounce_counter_size => 8         --set such that (2^size)/clk_freq = 5us (size = 8 for 50MHz)
--	)
--	port map
--	(
--		 clk          => CLK,          --system clock
--		 ps2_clk      => PS2_CLOCK,    --clock signal from PS/2 keyboard
--		 ps2_data     => PS2_DATA,     --data signal from PS/2 keyboard
--		 ps2_code_new => ps2_pulse,    --flag that new PS/2 code is available on ps2_code bus
--		 ps2_code     => ps2_code 		 --code received from PS/2
--	);

--ps2kbd: ps2 port map 
--		(
--		 clk_i => CLK,   	-- Global clk
--		 rst_i => nReset,  -- GLobal Asinchronous reset
--
--		 data_o    => ps2_code,		-- Data in
--		 data_i    => X"00",  		-- Data out
--		 ibf_clr_i => button(1),  	-- Ifb flag clear input
--		 obf_set_i => '0',  			-- Obf flag set input
--		 ibf_o     => ps2_pulse,  	-- Received data available
--		 obf_o     => open,  		-- Data ready to sent
--
--		 frame_err_o  => open,  -- Error receiving data
--		 parity_err_o => open,  -- Error in received data parity
--		 busy_o       => open,  -- uart busy
--		 err_clr_i 	  => '0',  	-- Clear error flags
--
--		 wdt_o => open,  -- Watchdog timer out every 400uS
--
--		 ps2_clk_io  => PS2_CLOCK,   -- PS2 Clock line
--		 ps2_data_io => PS2_DATA	  -- PS2 Data line
--		);

ps2_key: process(reset, ps2_pulse, ps2_code)
begin
	--if (reset = '1') then
	--	ps2_scan <= X"BEEF";
	--else
		if (rising_edge(ps2_pulse)) then
			ps2_scan <= std_logic_vector(unsigned(ps2_scan) + 1);
			--ps2_scan(15 downto 8) <= ps2_scan(7 downto 0);
			--ps2_scan(7 downto 0) <= ps2_code;
		end if;
	--end if;
end process;

-- Adapt TMS0800 scan keyboard

k_1 <= not button(1) when (switch(4 downto 2) = "000") else '1';
k_2 <= not button(1) when (switch(4 downto 2) = "001") else '1';
k_3 <= not button(1) when (switch(4 downto 2) = "010") else '1';
k_4 <= not button(1) when (switch(4 downto 2) = "011") else '1';
k_5 <= not button(1) when (switch(4 downto 2) = "100") else '1';
k_6 <= not button(1) when (switch(4 downto 2) = "101") else '1';
k_7 <= not button(1) when (switch(4 downto 2) = "110") else '1';
k_8 <= not button(1) when (switch(4 downto 2) = "111") else '1';
k_9 <= '1'; -- TODO!

k_clear 		<= not button(2) when (switch(4 downto 2) = "000") else '1';
k_equals 	<= not button(2) when (switch(4 downto 2) = "001") else '1';
k_plus 		<= not button(2) when (switch(4 downto 2) = "010") else '1';
k_minus 		<= not button(2) when (switch(4 downto 2) = "011") else '1';
k_multiply 	<= not button(2) when (switch(4 downto 2) = "100") else '1';
k_divide 	<= not button(2) when (switch(4 downto 2) = "101") else '1';
k_cerror 	<= not button(2) when (switch(4 downto 2) = "110") else '1';
k_dot 		<= not button(2) when (switch(4 downto 2) = "111") else '1';
k_0			<= '1'; -- TODO!

-- intersect digit scans with the keys to drive kx lines
kbdmux: mux11x4 port map 
	(
		e => "11" & nDigit,
		x(43 downto 40) => "1111",
		x(39 downto 36) => "1111",
		x(35 downto 32) => '1' & k_1 & k_clear 	& '1',
		x(31 downto 28) => '1' & k_2 & k_equals 	& '1',
		x(27 downto 24) => '1' & k_3 & k_plus 		& '1',
		x(23 downto 20) => '1' & k_4 & k_minus 	& '1',
		x(19 downto 16) => '1' & k_5 & k_multiply & '1',
		x(15 downto 12) => '1' & k_6 & k_divide 	& '1',
		x(11 downto 8)  => '1' & k_7 & k_cerror 	& '1',
		x(7 downto 4)   => '1' & k_8 & k_dot 		& '1',
		x(3 downto 0)   => '1' & k_9 & k_0 			& '1',
		y(3) => kd,	-- not used
		y(2) => kc, -- == kn
		y(1) => kb,	-- == ko
		y(0) => ka	-- == kp, not used
	);

-- scanning rows and cols
--drive_kbd: process(freq64, kbd_row)
--begin
--	if (rising_edge(freq64)) then
--		kbd_cnt <= std_logic_vector(unsigned(kbd_cnt) + 1);
--		case kbd_cnt is
--			when X"0" =>
--				kbd_col <= "0111"; --
--				k_0 <= kbd_row(0);
--			when X"1" =>
--				kbd_col <= "0111"; --
--				k_1 <= kbd_row(3);
--			when X"2" =>
--				kbd_col <= "1011"; --
--				k_2 <= kbd_row(3);
--			when X"3" =>
--				kbd_col <= "1101"; 
--				k_3 <= kbd_row(3);
--			when X"4" =>
--				kbd_col <= "0111"; --
--				k_4 <= kbd_row(2);
--			when X"5" =>
--				kbd_col <= "1011"; --
--				k_5 <= kbd_row(2);
--			when X"6" =>
--				kbd_col <= "1101";
--				k_6 <= kbd_row(2);
--			when X"7" =>
--				kbd_col <= "0111"; --
--				k_7 <= kbd_row(1);
--			when X"8" =>
--				kbd_col <= "1011"; --
--				k_8 <= kbd_row(1);
--			when X"9" =>
--				kbd_col <= "1101";
--				k_9 <= kbd_row(1);
--			when X"A" =>
--				kbd_col <= "1110";
--				k_plus <= kbd_row(3);
--			when X"B" =>
--				kbd_col <= "1110";
--				k_minus <= kbd_row(2);
--			when X"C" =>
--				kbd_col <= "1110";
--				k_multiply <= kbd_row(1);
--			when X"D" =>
--				kbd_col <= "1110";
--				k_divide <= kbd_row(0);
--			when X"E" =>
--				kbd_col <= "1101";
--				k_equals <= kbd_row(0);
--			when X"F" =>
--				kbd_col <= "1011"; --
--				k_dot <= kbd_row(0);
--			when others =>
--				null;
--		end case;
--	end if;
--end process;

--drive_col: process(freq32, kbd_col, reset)
--begin
--	if (reset = '1') then
--		kbd_col <= "1110";
--	else
--		if (rising_edge(freq32)) then
--			kbd_col <= kbd_col(2 downto 0) & kbd_col(3);
--		end if;
--	end if;
--end process;
--
--sense_row3: process(kbd_row(3), kbd_col)
--begin
--	if (falling_edge(kbd_row(3))) then
--		k_0 		<= kbd_col(0); ---
--		k_dot 	<= kbd_col(1); ---
--		k_equals <= kbd_col(2);
--		k_divide <= kbd_col(3);
--	end if;
--end process;
--
--sense_row2: process(kbd_row(2), kbd_col)
--begin
--	if (falling_edge(kbd_row(2))) then
--		k_7 			<= kbd_col(0); ---
--		k_8 			<= kbd_col(1);
--		k_9 			<= kbd_col(2);
--		k_multiply 	<= kbd_col(3);
--	end if;
--end process;
--
--sense_row1: process(kbd_row(1), kbd_col)
--begin
--	if (falling_edge(kbd_row(1))) then
--		k_4 		<= kbd_col(0); ---
--		k_5 		<= kbd_col(1);
--		k_6 		<= kbd_col(2);
--		k_minus 	<= kbd_col(3);
--	end if;
--end process;
--
--sense_row0: process(kbd_row(0), kbd_col)
--begin
--	if (falling_edge(kbd_row(0))) then
--		k_1 		<= kbd_col(0);
--		k_2 		<= kbd_col(1);
--		k_3 		<= kbd_col(2);
--		k_plus 	<= kbd_col(3);
--	end if;
--end process;

end;
