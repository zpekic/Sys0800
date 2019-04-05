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
				LED: out std_logic_vector(3 downto 0);
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
				--PMOD interface
				PMOD: inout std_logic_vector(7 downto 0)--;
				--PS2_DATA: in std_logic;
				--PS2_CLOCK: out std_logic

          );
end sys0800;

architecture Structural of sys0800 is

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

component debouncer8channel is
    Port ( clock : in  STD_LOGIC;
           reset : in  STD_LOGIC;
           signal_raw : in  STD_LOGIC_VECTOR(7 downto 0);
           signal_debounced : out  STD_LOGIC_VECTOR(7 downto 0));
end component;

component tms0800 is
    Port ( reset : in  STD_LOGIC;
           clk_calc : in  STD_LOGIC;
           clk_scan : in  STD_LOGIC;
           clk_txd : in  STD_LOGIC;
           enable_trace : in  STD_LOGIC;
           show_debug : in  STD_LOGIC;
           nDigit : out  STD_LOGIC_VECTOR (8 downto 0);
           segment : out  STD_LOGIC_VECTOR(7 downto 0);
			  ka : in  STD_LOGIC;
           kb : in  STD_LOGIC;
           kc : in  STD_LOGIC;
           kd : in  STD_LOGIC;
			  trace_txd: out STD_LOGIC;
			  trace_rxd: in STD_LOGIC;
			  dbg_state: out STD_LOGIC_VECTOR(3 downto 0));
end component;

signal freq2k, freq1k, freq512, freq256, freq128, freq64, freq32, freq16, freq8, freq4, freq2, freq1: std_logic;
signal freq38400, freq19200, freq9600, freq4800, freq2400, freq1200, freq600, freq300: std_logic;
signal freq25M, freq12M5, freq6M25, freq3M125, freq1M5625: std_logic;

signal reset: std_logic;
-- debounced inputs
signal switch: std_logic_vector(7 downto 0);
alias enable_trace: std_logic is switch(0);
alias show_debug: std_logic is switch(1);

signal button: std_logic_vector(3 downto 0);
alias show_upper_digits: std_logic is button(0);

alias clk_scan: std_logic is freq2k;
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

-- other
signal clk_calc: std_logic;
signal trace_txd: std_logic;

begin
   
	reset <= USR_BTN;

    -- FREQUENCY GENERATOR
    one_sec: clock_divider
	 generic map (CLK_FREQ => 50e6)	 
	 port map 
    (
        clock => CLK,
        reset => Reset,
        slow(11) => freq1, -- 1Hz
        slow(10) => freq2, -- 2Hz
        slow(9) => freq4, -- 4Hz
        slow(8) => freq8, -- 8Hz
        slow(7) => freq16,  -- 16Hz
        slow(6) => freq32,  -- 32Hz
        slow(5) => freq64,  -- 64Hz
        slow(4) => freq128,  -- 128Hz
        slow(3) => freq256,  -- 256Hz
        slow(2) => freq512,  -- 512Hz
        slow(1) => freq1k,  -- 1024Hz
        slow(0) => freq2k,  -- 2048Hz
		  baud(7) => freq300,
		  baud(6) => freq600,		  
		  baud(5) => freq1200,
		  baud(4) => freq2400,
		  baud(3) => freq4800,
		  baud(2) => freq9600,
		  baud(1) => freq19200,
		  baud(0) => freq38400,
		  fast(4) => freq1M5625,
		  fast(3) => freq3M125,
		  fast(2) => freq6M25,
		  fast(1) => freq12M5,
		  fast(0) => freq25M
    );

	-- Single step by each clock cycle, slow or fast
	ss: clocksinglestepper port map (
        reset => Reset,
        clock0_in => freq2,
        clock1_in => freq1k,
        clock2_in => freq2k,
        clock3_in => freq1M5625,
        clocksel => switch(6 downto 5),
        modesel => switch(7),
        singlestep => button(3),
        clock_out => clk_calc
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

	calculator: tms0800 Port map ( 
		reset => reset,
		clk_calc => clk_calc,
		clk_scan => clk_scan,
		clk_txd => clk_txd,
		enable_trace => enable_trace,
		show_debug => show_debug,
		nDigit => nDigit,
		segment => segment,
		ka => ka,
		kb => kb,
		kc => kc,
		kd => kd,
		trace_txd => PMOD(3), 
		trace_rxd => PMOD(2),
		dbg_state => led_debug
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
AN <= nDigit(7 downto 4) when (show_upper_digits = '1') else nDigit(3 downto 0);
LED <= led_debug;-- when (show_debug = '1') else led_digit8;

--led_debug <= kbd_row;--k_0 & k_8 & k_6 & k_plus;--kd & kc & kb & ka;--trace_txd & clk_calc & display_debug & enable_trace;

digit8: process(clk_scan, nDigit(8), segment)
begin
	if (falling_edge(clk_scan)) then
		if (segment = pattern_minus and nDigit(8) = '0') then
			led_digit8 <= "1111";
		else
			led_digit8 <= "0000";
		end if;
	end if;
end process;

-- Adapt TMS0800 scan keyboard
--PMOD(3 downto 0) <= kbd_col;

-- scanning rows and cols
drive_kbd: process(freq64, kbd_row)
begin
	if (rising_edge(freq64)) then
		kbd_cnt <= std_logic_vector(unsigned(kbd_cnt) + 1);
		case kbd_cnt is
			when X"0" =>
				kbd_col <= "0111"; --
				k_0 <= kbd_row(0);
			when X"1" =>
				kbd_col <= "0111"; --
				k_1 <= kbd_row(3);
			when X"2" =>
				kbd_col <= "1011"; --
				k_2 <= kbd_row(3);
			when X"3" =>
				kbd_col <= "1101"; 
				k_3 <= kbd_row(3);
			when X"4" =>
				kbd_col <= "0111"; --
				k_4 <= kbd_row(2);
			when X"5" =>
				kbd_col <= "1011"; --
				k_5 <= kbd_row(2);
			when X"6" =>
				kbd_col <= "1101";
				k_6 <= kbd_row(2);
			when X"7" =>
				kbd_col <= "0111"; --
				k_7 <= kbd_row(1);
			when X"8" =>
				kbd_col <= "1011"; --
				k_8 <= kbd_row(1);
			when X"9" =>
				kbd_col <= "1101";
				k_9 <= kbd_row(1);
			when X"A" =>
				kbd_col <= "1110";
				k_plus <= kbd_row(3);
			when X"B" =>
				kbd_col <= "1110";
				k_minus <= kbd_row(2);
			when X"C" =>
				kbd_col <= "1110";
				k_multiply <= kbd_row(1);
			when X"D" =>
				kbd_col <= "1110";
				k_divide <= kbd_row(0);
			when X"E" =>
				kbd_col <= "1101";
				k_equals <= kbd_row(0);
			when X"F" =>
				kbd_col <= "1011"; --
				k_dot <= kbd_row(0);
			when others =>
				null;
		end case;
	end if;
end process;


-- clear and clear error keys do not fit on the https://store.digilentinc.com/pmod-kypd-16-button-keypad/
k_clear <= not button(1);
k_cerror <= not button(2);
-- intersect digit scans with the keys to drive kx lines
kd <= '1';
kc <= (nDigit(8) or k_1) 		and (nDigit(7) or k_2)      and (nDigit(6) or k_3)    and (nDigit(5) or k_4)     and (nDigit(4) or k_5)        and (nDigit(3) or k_6)      and (nDigit(2) or k_7)      and (nDigit(1) or k_8)   and (nDigit(0) or k_9);
kb <= (nDigit(8) or k_clear)  and (nDigit(7) or k_equals) and (nDigit(6) or k_plus) and (nDigit(5) or k_minus) and (nDigit(4) or k_multiply) and (nDigit(3) or k_divide) and (nDigit(2) or k_cerror) and (nDigit(1) or k_dot) and (nDigit(0) or k_0);
ka <= '1';

end;


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

