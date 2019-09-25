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
				-- SW(0) -- show trace on VGA
				-- SW(1) -- show debug data (program and microcode program counter) on 7seg instead of calculator data
				-- SW(2) -- enable microcode single stepping (use with BTN(3))
				-- SW(3) -- enable calculator program breakpoints
				-- SW(4) -- (not used)
				-- SW(6 downto 5) -- system clock speed 
				--   0   0	1024 Hz 
				--   0   1	57600 Hz (close to original calculator frequency) 
				--   1   0  6.25 MHz
				--   1   1  12.5 MHz
				-- SW(7)
				--   0   TI Datamath
				--   1   Sinclair Scientific
				SW: in std_logic_vector(7 downto 0); 

				-- Push buttons on baseboard
				-- BTN0 - show upper 4 digits on 7seg LEDs
				-- BTN1 - CE[NTRY] key for TI and UP key for Sinclair
				-- BTN2 - C[LEAR] key for both TI and Sinclair (this is also "reset" for Sinclair)
				-- BTN3 - single step clock cycle forward if in SS mode (NOTE: single press on this button is needed after reset to unlock SS circuit)
				BTN: in std_logic_vector(3 downto 0); 

				-- Stereo audio output on baseboard
				--AUDIO_OUT_L, AUDIO_OUT_R: out std_logic;

				-- 7seg LED on baseboard 
				A_TO_G: out std_logic_vector(6 downto 0); 
				AN: out std_logic_vector(3 downto 0); 
				DOT: out std_logic; 
				-- 4 LEDs on Mercury board (3 and 2 are used by VGA VSYNC and HSYNC)
				LED: out std_logic_vector(1 downto 0);

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
				--register state is traced to VGA after each instruction if SW0 = on
				--640*480 50Hz mode is used, which give 80*60 character display
				--but to save memory, only 80*50 are used which fits into 4k video RAM
				HSYNC: out std_logic;
				VSYNC: out std_logic;
				RED: out std_logic_vector(2 downto 0);
				GRN: out std_logic_vector(2 downto 0);
				BLU: out std_logic_vector(1 downto 0);
				
				--PMOD interface
				--connection to https://store.digilentinc.com/pmod-kypd-16-button-keypad/
				PMOD: inout std_logic_vector(7 downto 0)
          );
end sys0800;

architecture Structural of sys0800 is

component mwvga is
    Port ( reset : in  STD_LOGIC;
           clk : in  STD_LOGIC;
           rgbBorder : in  STD_LOGIC_VECTOR (7 downto 0);
			  field: in STD_LOGIC_VECTOR(1 downto 0);
			  din: in STD_LOGIC_VECTOR (7 downto 0);
           hactive : buffer  STD_LOGIC;
           vactive : buffer  STD_LOGIC;
           x : out  STD_LOGIC_VECTOR (7 downto 0);
           y : out  STD_LOGIC_VECTOR (7 downto 0);
			  -- VGA connections
			  rgb : out  STD_LOGIC_VECTOR (7 downto 0);
           hsync : out  STD_LOGIC;
           vsync : out  STD_LOGIC);
end component;

component xyram is
	 generic (maxram: integer;
				 maxrow: integer;
				 maxcol: integer);
    Port ( clk : in  STD_LOGIC;
           rw_we : in  STD_LOGIC;
           rw_x : in  STD_LOGIC_VECTOR (7 downto 0);
           rw_y : in  STD_LOGIC_VECTOR (7 downto 0);
           rw_din : in  STD_LOGIC_VECTOR (7 downto 0);
           rw_dout : out  STD_LOGIC_VECTOR (7 downto 0);
			  mode: in STD_LOGIC_VECTOR (7 downto 0);
           nDigit : in  STD_LOGIC_VECTOR (8 downto 0);
           segment : in  STD_LOGIC_VECTOR(7 downto 0);
			  field: buffer STD_LOGIC_VECTOR(1 downto 0);
           ro_x : in  STD_LOGIC_VECTOR (7 downto 0);
           ro_y : in  STD_LOGIC_VECTOR (7 downto 0);
           ro_dout : out  STD_LOGIC_VECTOR (7 downto 0));
end component;
--
component vio0800_microcode is
	 generic (maxrow: integer;
				 maxcol: integer);
    Port ( reset : in  STD_LOGIC;
           clk : in  STD_LOGIC;
           char : in  STD_LOGIC_VECTOR (7 downto 0);
           char_sent : out STD_LOGIC;
			  busy_in: in STD_LOGIC;
			  busy_out: out STD_LOGIC;
			  we : out STD_LOGIC;
			  din: in STD_LOGIC_VECTOR(7 downto 0);
			  dout: buffer STD_LOGIC_VECTOR(7 downto 0);
			  x: out STD_LOGIC_VECTOR(7 downto 0);
			  y: out STD_LOGIC_VECTOR(7 downto 0));
end component;

-------
-- From: https://www.digikey.com/eewiki/pages/viewpage.action?pageId=28278929#PS/2KeyboardInterface(VHDL)-CodeDownloads
-------
--component ps2_keyboard IS
--  GENERIC(
--    clk_freq              : INTEGER := 50_000_000; --system clock frequency in Hz
--    debounce_counter_size : INTEGER := 8);         --set such that (2^size)/clk_freq = 5us (size = 8 for 50MHz)
--  PORT(
--    clk          : IN  STD_LOGIC;                     --system clock
--    ps2_clk      : IN  STD_LOGIC;                     --clock signal from PS/2 keyboard
--    ps2_data     : IN  STD_LOGIC;                     --data signal from PS/2 keyboard
--    ps2_code_new : OUT STD_LOGIC;                     --flag that new PS/2 code is available on ps2_code bus
--    ps2_code     : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)); --code received from PS/2
--END component;

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

--component freqmux is
--    Port ( reset : in  STD_LOGIC;
--           f0in : in  STD_LOGIC;
--           f1in : in  STD_LOGIC;
--           sel : in  STD_LOGIC;
--           fout : out  STD_LOGIC);
--end component;

component debouncer8channel is
    Port ( clock : in  STD_LOGIC;
           reset : in  STD_LOGIC;
           signal_raw : in  STD_LOGIC_VECTOR(7 downto 0);
           signal_debounced : out  STD_LOGIC_VECTOR(7 downto 0));
end component;

component sio0800 is
    Port ( reset : in  STD_LOGIC;
           clk : in  STD_LOGIC;
           txChar : in  STD_LOGIC_VECTOR (7 downto 0);
           txChar_sent : buffer STD_LOGIC;
           txd : out  STD_LOGIC);
end component;

component tms0800 is
    Port ( -- present in original
			  reset : in  STD_LOGIC;
           clk_cpu : in  STD_LOGIC;
           nDigit : out  STD_LOGIC_VECTOR (8 downto 0);
           segment : out  STD_LOGIC_VECTOR(7 downto 0);
           ka : in  STD_LOGIC;
           kb : in  STD_LOGIC;
           kc : in  STD_LOGIC;
           kd : in  STD_LOGIC;
			  -- mode
			  sinclair: in STD_LOGIC;
			  -- debug, trace additions
           trace_enable : in  STD_LOGIC;
			  trace_ascii: out STD_LOGIC_VECTOR(7 downto 0);
			  trace_ready: in STD_LOGIC;
			  breakpoint_req: out STD_LOGIC;
			  breakpoint_ack: in STD_LOGIC;
			  singlestep_disable: out STD_LOGIC;
			  dbg_mpc: out STD_LOGIC_VECTOR(8 downto 0); -- 9 bit macro-program counter
			  dbg_upc: out STD_LOGIC_VECTOR(7 downto 0)  -- 8 bit micro-program counter
			);
end component;

-- various frequencies

signal slow: std_logic_vector(11 downto 0);
signal baud: std_logic_vector(7 downto 0);
signal fast: std_logic_vector(4 downto 0);
alias freq1: std_logic is slow(11); -- 1Hz
alias freq2: std_logic is slow(10); -- 2Hz
alias freq4: std_logic is slow(9); -- 4Hz
alias freq8: std_logic is slow(8); -- 8Hz
alias freq16: std_logic is slow(7);  -- 16Hz
alias freq32: std_logic is slow(6);  -- 32Hz
alias freq64: std_logic is slow(5);  -- 64Hz
alias freq128: std_logic is slow(4);  -- 128Hz
alias freq256: std_logic is slow(3);  -- 256Hz
alias freq512: std_logic is slow(2);  	-- 512Hz
alias freq1k: std_logic is slow(1);  	-- 1024Hz
alias freq2k: std_logic is slow(0);  	-- 2048Hz
alias freq57600: std_logic is baud(0);		-- 57600
alias freq1M5625: std_logic is fast(4); 	-- 1.5625MHz
alias freq3M125: std_logic is fast(3);
alias freq6M25: std_logic is fast(2);
alias freq12M5: std_logic is fast(1);
alias freq25M: std_logic is fast(0);		-- 25MHz (suitable for 640x480 VGA)

-- debounced inputs
--signal switch: std_logic_vector(7 downto 0);
--signal button: std_logic_vector(3 downto 0);

signal reset, nReset, reset_0800: std_logic;

alias sw_sinclair: std_logic is SW(7);
alias sw_freqsel: std_logic_vector(1 downto 0) is SW(6 downto 5);
alias sw_enable_breakpoints: std_logic is SW(3);
alias sw_enable_singlestep: std_logic is SW(2);
alias sw_show_leddebug: std_logic is SW(1);
alias sw_show_vgatrace: std_logic is SW(0);

alias btn_step: std_logic is BTN(3);
alias btn_clear: std_logic is BTN(2);
alias btn_centry: std_logic is BTN(1);
alias btn_up: std_logic is BTN(1);
alias btn_show_upper_digits: std_logic is BTN(0);

-- tracing bus
signal trace_ascii: std_logic_vector(7 downto 0);
signal trace_done, s_tracedone, v_tracedone: std_logic;

-- keyboard
signal kbd_row, kbd_col: std_logic_vector(3 downto 0);
signal kb, kc, ko_ti, ko_sinclair: std_logic;
signal kbd_key: std_logic_vector(15 downto 0);
signal kbd_state: std_logic;
signal pmod_in: std_logic_vector(3 downto 0);

-- display
signal nDigit, tmsDigit, dbgDigit: std_logic_vector(8 downto 0);
signal segment, tmsSegment, dbgSegment, a2g: std_logic_vector(7 downto 0);
signal led_digit8: std_logic_vector(1 downto 0);
--signal led_debug, led_digit8: std_logic_vector(3 downto 0);
--signal tmsAn, debugAn: std_logic_vector(3 downto 0);
signal led_flash: std_logic;

-- debug display
signal tms_mpc: std_logic_vector(8 downto 0);
signal tms_upc: std_logic_vector(7 downto 0);
signal dbgHex: std_logic_vector(3 downto 0);

-- other
signal clk_cpu, clk_ss_on, clk_ss_off: std_logic;
signal singlestep_disable, singlestep_enable: std_logic;
signal brk_ack, brk_req: std_logic;

-- VGA
signal tracer_out, tracer_in, controller_in: std_logic_vector(7 downto 0);
signal controller_x, tracer_x: std_logic_vector(7 downto 0);
signal controller_y, tracer_y: std_logic_vector(7 downto 0);
signal tracer_we, ram_we: std_logic;
signal colorband: std_logic_vector(1 downto 0);

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
ss_on: clocksinglestepper port map (
	  reset => Reset,
	  clock3_in => freq1k,
	  clock2_in => freq57600,
	  clock1_in => freq6M25,
	  clock0_in => freq12M5,
	  clocksel => sw_freqsel,
	  modesel => singlestep_enable,
	  singlestep => btn_step,
	  clock_out => clk_cpu
 );

singlestep_enable <= sw_enable_singlestep and (not singlestep_disable);

--debounced: block is
--begin
--	-- DEBOUNCE the 8 switches, 4 buttons and 4 return lines from PMOD keyboard
--    debouncer_sw: debouncer8channel port map (
--        clock => freq2k,
--        reset => Reset,
--        signal_raw => SW,
--        signal_debounced => switch
--    );
--
--   debouncer_kbd_btn: debouncer8channel port map (
--        clock => freq2k,
--        reset => Reset,
--        signal_raw(7 downto 4) => pmod_in,
--        signal_raw(3 downto 0) => BTN(3 downto 0),
--		  signal_debounced(7) => kbd_row(0),
--		  signal_debounced(6) => kbd_row(1),
--		  signal_debounced(5) => kbd_row(2),
--		  signal_debounced(4) => kbd_row(3),
--		  signal_debounced(3 downto 0) => button(3 downto 0)
--    );
--end block;

-- temporarily bypass debouncers!
bounce: block is
begin
	--switch <= SW;
	--button <= BTN;
	kbd_row(3) <= pmod_in(0);
	kbd_row(2) <= pmod_in(1);
	kbd_row(1) <= pmod_in(2);
	kbd_row(0) <= pmod_in(3);
end block;

-- VGA debug tracing ----------------------
	v_controller: mwvga 
	port map ( 
		reset => reset,
		clk => freq25M, 
		rgbBorder => SW,
		field => colorband,
		din => controller_in,
		hactive => open, --controller_h,
		vactive => open, --controller_v, --controller_busy,
		x => controller_x,
		y => controller_y,
		-- VGA connections
		rgb(2 downto 0) => RED,
		rgb(5 downto 3) => GRN,
		rgb(7 downto 6) => BLU,
		hsync => HSYNC,
		vsync => VSYNC
	);

	v_ram: xyram 
	generic map (
		maxram => 2048, -- must be <= than maxrow * maxcol
		maxrow => 48,
		maxcol => 80	 
	)
	port map (
		clk => CLK, --freq25M,
		rw_we => tracer_we,
		rw_x => tracer_x,
		rw_y => tracer_y,
		rw_din => tracer_out,
		rw_dout => tracer_in,
		mode => SW, -- TODO: display on VGA current switch settings
		nDigit => nDigit,
		segment => segment,
		field => colorband,
		ro_x => controller_x,
		ro_y => controller_y,
		ro_dout => controller_in
	);

---- vga debug tracer
	v_tracer: vio0800_microcode 
	generic map (
		maxrow => 48,
		maxcol => 80)	 
	port map ( 
		reset => reset,
		clk => CLK, --freq25M,
		char => trace_ascii,
		char_sent => v_tracedone,
		busy_in => '0', --controller_busy,
		busy_out => open, --tracer_busy,
		we => tracer_we,
		din => tracer_in,
		dout => tracer_out,
		x => tracer_x,
		y => tracer_y
	);

trace_done <= v_tracedone; -- and s_tracedone when (enable_serialtrace = '1') else v_tracedone;

-- Dual mode calculator (TI Datamath or Sinclair Scientific) -----
	-- in Sinclair mode, C[lear] key is tied to reset
	reset_0800 <= reset or (btn_clear and sw_sinclair);
	-- Debug sequence - build time:
	-- (1) Run GetSourceCode.cmd to download TI and Sinclair sources and generate .asm files for both
	-- (2) Add asterisk (*) after last hex character of instruction in .asm file
	-- (3) build .bin file which sets bit 11 of instructions into TI/Sinclair ROM where * is found
	-- Debug sequence - run time:
	-- (1) set SW(3) to high, press BTN(3) to start calculator
	-- (2) when breakpoint is encountered, TMS0800 will stop and display will flash, set SW(2) to on
	-- (3) press BTN(3) to single step
	-- (4) disable breakpoint by setting SW(3) to low, disable single step by setting SW(2) to low
	brk_ack <= brk_req and sw_enable_breakpoints;
	led_flash <= brk_ack and freq2;
	
	calculator: tms0800 Port map ( 
		-- present on original chip
		reset => reset_0800,
		clk_cpu => clk_cpu, 
		nDigit => tmsDigit,
		segment => tmsSegment,
		ka => '1', -- not used
		kb => kb,
		kc => kc,
		kd => '1', -- not used
		-- mode
		sinclair => sw_sinclair,
		-- debug and control
		trace_enable => sw_show_vgatrace,
		trace_ascii => trace_ascii, 
		trace_ready => trace_done,
		breakpoint_req => brk_req,
		breakpoint_ack => brk_ack,
		singlestep_disable => singlestep_disable,
		dbg_mpc => tms_mpc,
		dbg_upc => tms_upc
	);

-- Data and debug display paths
driveDbgDisplay: process(reset, freq1k)
begin
	if (reset = '1') then
		dbgDigit <= "011111111";
	else
		if (rising_edge(freq1k)) then
			dbgDigit <= dbgDigit(0) & dbgDigit(8 downto 1); -- shift right 
		end if;
	end if;
end process;

nDigit <= dbgDigit when (sw_show_leddebug = '1') else tmsDigit;
segment <= dbgSegment when (sw_show_leddebug = '1') else tmsSegment;

dbgmux: mux11x4 port map 
	(
		e => "11" & dbgDigit,
		x(43 downto 40) => X"F", -- not displayed
		x(39 downto 36) => X"F", -- not displayed
		x(35 downto 32) => X"F" xor ("11" & kc & kb), -- keyboard sense lines
		x(31 downto 28) => X"F" xor ("111" & tmsDigit(8)),
		x(27 downto 24) => X"F" xor tmsDigit(7 downto 4),
		x(23 downto 20) => X"F" xor tmsDigit(3 downto 0),
		x(19 downto 16) => "000" & tms_mpc(8),		-- value of macroprogram counter
		x(15 downto 12) => tms_mpc(7 downto 4),
		x(11 downto 8)  => tms_mpc(3 downto 0),
		x(7 downto 4)   => tms_upc(7 downto 4),	-- value of microprogram counter
		x(3 downto 0)   => tms_upc(3 downto 0),
		-----------------------------------------
		y => dbgHex
	);

dbgSegment <= hexfont(to_integer(unsigned(dbgHex)));

-- Adapt TMS0800 9 digit common cathode display to 4 digit + 4 leds display
a2g <= X"FF" when (led_flash = '1') else (X"FF" xor segment); 
A_TO_G(0) <= a2g(6);
A_TO_G(1) <= a2g(5);
A_TO_G(2) <= a2g(4);
A_TO_G(3) <= a2g(3);
A_TO_G(4) <= a2g(2);
A_TO_G(5) <= a2g(1);
A_TO_G(6) <= a2g(0);
DOT <= a2g(7);

AN <= nDigit(7 downto 4) when (btn_show_upper_digits = '1') else nDigit(3 downto 0);
LED <= (not kc) & (not kb) when (sw_show_leddebug = '1') else led_digit8;
led_digit8 <= "11" when (tmsDigit(8) = '0' and tmsSegment = pattern_minus) else "00";

-- Adapt TMS0800 scan keyboard
-- PMOD --- PmodKYPD -- 
-- 0 (out)	COL4		
-- 1 (out)	COL3						 
-- 2 (out)	COL2		
-- 3 (out)	COL1		
-- 4 (in)	ROW4		
-- 5 (in)	ROW3		
-- 6 (in)	ROW2		
-- 7 (in)	ROW1		
-----------------------
-- hook up PmodKYPD as the keyboard
PMOD(3 downto 0) <= kbd_col(0) & kbd_col(1) & kbd_col(2) & kbd_col(3); -- when (enable_serialtrace = '0') else txd & "111";
pmod_in <= PMOD(7 downto 4); 

with slow(8 downto 7) select
	kbd_state <= 	kbd_row(0) when "00",
						kbd_row(1) when "01",
						kbd_row(2) when "10",
						kbd_row(3) when others;

with slow(6 downto 5) select
	kbd_col <= 	"1110" when "00",
					"1101" when "01",
					"1011" when "10",
					"0111" when others;
				
get_pmodkey: process(reset, slow(4))
begin
	if (rising_edge(slow(4))) then
		kbd_key(to_integer(unsigned(slow(8 downto 5)))) <= kbd_state;
	end if;
end process;

-- intersect digit scans with the keys to drive kx lines
kbdmux: mux11x4 port map 
	(
		e => "11" & tmsDigit,
		x(43 downto 40) => "1111",
		x(39 downto 36) => "1111",
		--------------------------------------------------------------------
		--								 BOTH  			TI					  SINCLAIR
		--------------------------------------------------------------------
		x(35 downto 32) => '1' & kbd_key(0)  & not btn_clear 	& not btn_clear,
		x(31 downto 28) => '1' & kbd_key(1)  & kbd_key(14) 	& kbd_key(13),
		x(27 downto 24) => '1' & kbd_key(2)  & kbd_key(3) 		& kbd_key(11),
		x(23 downto 20) => '1' & kbd_key(4)  & kbd_key(7) 		& kbd_key(15),
		x(19 downto 16) => '1' & kbd_key(5)  & kbd_key(11) 	& kbd_key(7),
		x(15 downto 12) => '1' & kbd_key(6)  & kbd_key(15) 	& kbd_key(3),
		x(11 downto 8)  => '1' & kbd_key(8)  & not btn_centry & not btn_up,
		x(7 downto 4)   => '1' & kbd_key(9)  & kbd_key(13) 	& kbd_key(14),
		x(3 downto 0)   => '1' & kbd_key(10) & kbd_key(12) 	& kbd_key(12),
		--------------------------------------------------------------------
		y(3) => open,			-- not used
		y(2) => kc, 			-- kn, common for both
		y(1) => ko_ti,			-- ko for TI
		y(0) => ko_sinclair	-- ko for Sinclair
	);

	kb <= ko_sinclair when (sw_sinclair = '1') else ko_ti;
	
end;
