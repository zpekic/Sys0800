# Sys0800
VHDL implementation of vintage TMS0800 calculator chip

This is the reverse engineering of the US patent #3934233 which can be found here:
https://patents.google.com/patent/US3934233

The project has been inspired by the cool JScript / HTML simulation by Ken Shirriff: http://files.righto.com/calculator/TI_calculator_simulator.html

The implementation is not replicating 100% the internals of the original chip, as random MOS logic does not translate too well into FPGA RTL. Instead, a classic micro-coded approach was used, with a 256 * 48 control store. The upper part of this store (0x80 - 0xFF) maps directly into TMS0800 instructions (e.g. entry point for ZFA (0x58X) is at 0xD8), and the lower part is used to implement the instructions. Also included in the "TMS0800" is a tracer circuit which allows observation of internal state after each instruction (by connecting to any 38400bps 8N1 terminal). This is also driven by microcode routine, locations 0x0A to 0x3F. Here is the startup sequence:

PC=000 I=58F A=9999999999 B=9999999999 C=9999999999 AF=0000000000 BF=1111111111 CF=0
PC=001 I=57F A=9999999999 B=9999999999 C=9999999999 AF=0000000000 BF=0000000000 CF=0
PC=002 I=70F A=0000000000 B=9999999999 C=9999999999 AF=0000000000 BF=0000000000 CF=0
PC=003 I=72F A=0000000000 B=9999999999 C=0000000000 AF=0000000000 BF=0000000000 CF=0
PC=005 I=6DB A=0000000000 B=9999999999 C=0000000000 AF=0000000000 BF=0000000000 CF=1
PC=013 I=70F A=0000000000 B=9999999999 C=0000000000 AF=0000000000 BF=0000000000 CF=0
PC=014 I=586 A=0000000000 B=9999999999 C=0000000000 AF=0000000000 BF=0000000000 CF=0
PC=015 I=73F A=9999999999 B=0000000000 C=0000000000 AF=0000000000 BF=0000000000 CF=0
PC=016 I=70B A=0000000199 B=0000000000 C=0000000000 AF=0000000000 BF=0000000000 CF=0
PC=017 I=61B A=0000000299 B=0000000000 C=0000000000 AF=0000000000 BF=0000000000 CF=0
PC=018 I=73C A=0000000099 B=0000000200 C=0000000000 AF=0000000000 BF=0000000000 CF=0
PC=019 I=63E A=0000000000 B=0000000200 C=0000000000 AF=0000000000 BF=0000000000 CF=0
PC=01A I=69D A=0000000099 B=0000000200 C=0000000000 AF=0000000000 BF=0000000000 CF=1
PC=01E I=63E A=0000000000 B=0000000200 C=0000000000 AF=0000000000 BF=0000000000 CF=0
PC=01F I=598 A=0000000000 B=0000000200 C=0000000000 AF=0000000000 BF=0000000000 CF=0
PC=020 I=229 A=0000000000 B=0000000200 C=0000000000 AF=0000000000 BF=0000000000 CF=0
PC=021 I=550 A=0000000000 B=0000000200 C=0000000000 AF=0000000000 BF=0000000000 CF=0
PC=022 I=560 A=0000000000 B=0000000200 C=0000000000 AF=0000000000 BF=0000000000 CF=0
PC=023 I=221 A=0000000000 B=0000000200 C=0000000000 AF=0000000000 BF=0000000000 CF=0
PC=024 I=550 A=0000000000 B=0000000200 C=0000000000 AF=0000000000 BF=0000000000 CF=0
PC=025 I=560 A=0000000000 B=0000000200 C=0000000000 AF=0000000000 BF=0000000000 CF=0
PC=026 I=221 A=0000000000 B=0000000200 C=0000000000 AF=0000000000 BF=0000000000 CF=0
PC=027 I=66E A=0000000000 B=0000000200 C=0000000000 AF=0000000000 BF=0000000000 CF=0
PC=028 I=72E A=0000000000 B=0000000200 C=0000000000 AF=0000000000 BF=0000000000 CF=0
PC=029 I=578 A=0000000000 B=0000000200 C=0000000000 AF=0000000000 BF=0000000000 CF=0

at this point TMS0800 is executing the following instruction:
X"02A" => X"527" --WAITNO F9      .0.........
which means it is waiting in the loop for any key press.

Typical instructions takes at least 14 cycles (11 to go over digit plus 2 fork and next overhead), but the display and keyboard scan is driven at the fork microinstruction which happens only once per instruction execution, therefore from outside the basic timing relationship between execution and scan is preserved. This is visible in this sequence:

...
  '112          SYNC         ',
  '113          BKO    CLEAR  ; Clear key pressed?',
  '114          BKO    EQLKEY ; Equal key pressed?',
  '115          BKO    PLSKEY ; Plus key pressed?',
  '116          BKO    MINKEY ; Minus key pressed?',
  '117          BKO    MLTKEY ; Mult key pressed?',
  '118          BKO    DIVKEY ; Divide key pressed?',
  '119          BKO    CEKEY  ; CE key pressed?',
  '120          BKO    DPTKEY ; Decimal point key pressed?',
  '121          BKO    ZERKEY ; Zero key pressed?',
...  

SYNC is done when the keyboard scan is about to start a new round. After that each instruction advances the scan to next key, so a single sense line controlling the BKO instruction can branch to the right routine. 

