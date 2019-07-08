----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    11:37:53 05/19/2019 
-- Design Name: 
-- Module Name:    sio0800 - Behavioral 
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
use work.tms0800_package.all;

entity vio0800 is
    Port ( reset : in  STD_LOGIC;
           clk : in  STD_LOGIC;
           char : in  STD_LOGIC_VECTOR (7 downto 0);
           char_sent : out STD_LOGIC;
			  busy_in: in STD_LOGIC;
			  busy_out: out STD_LOGIC;
			  we : out STD_LOGIC;
			  din: in STD_LOGIC_VECTOR(7 downto 0);
			  dout: out STD_LOGIC_VECTOR(7 downto 0);
			  x: out STD_LOGIC_VECTOR(7 downto 0);
			  y: out STD_LOGIC_VECTOR(7 downto 0));
end vio0800;

architecture Behavioral of vio0800 is

constant bst_none:	std_logic_vector(1 downto 0) := "00";
constant bst_read:	std_logic_vector(1 downto 0) := "10";
constant bst_write:	std_logic_vector(1 downto 0) := "11";

---- internal registers
signal row, col: std_logic_vector(7 downto 0);
signal busstate: std_logic_vector(1 downto 0);
--signal u_pc, u_ra, u_next: std_logic_vector(7 downto 0);
--
---- conditions
--signal u_condition: std_logic;
--signal row0, row49: std_logic;
--signal col0, col79: std_logic;
--signal char0, charcontrol, charcr, charlf, charcls, charhome: std_logic;
--
type state is (st_reset, 			--0
					st_readytostart, 	--1
					st_memread0,			--2
					st_memread1,			--2
					st_memwrite0,		--3
					st_memwrite1,		--3
					st_print,			--4	
					st_print_incx,			--5
					st_print_incy,			--6
					st_next_row,			--7
					st_next_col,	--8
					st_cr,
					st_lf,--9
					st_done,		--A
					st_donehold				--B
					);
					
signal state_current, state_next, state_return: state;

begin

-- direct output signals
busy_out <= busstate(1);
we <= busstate(0);
x <= col;
y <= row when (busstate(0) = '0') else std_logic_vector(unsigned(row) + 1); -- always read from the row below

-- FSM -- 
drive: process(reset, clk, state_next)
begin
	if (reset = '1') then
		state_current <= st_reset;
	else
		if (rising_edge(clk)) then
			state_current <= state_next;
		end if;
	end if;
end process;

execute: process(clk, state_current)
begin
	if (rising_edge(clk)) then
		case state_current is
		
			when st_reset =>
				busstate <= bst_none;
				char_sent <= '1';

			when st_readytostart =>
				busstate <= bst_none;
				char_sent <= '1';
				
			when st_print =>
				busstate <= bst_none;
				char_sent <= '0'; -- indicates wait for the client
				dout <= char;		-- will write char to memory
				
			when st_next_row =>
				busstate <= bst_none;
				char_sent <= '0'; -- indicates wait for the client
				row <= std_logic_vector(unsigned(row) + 1);
				
			when st_next_col =>
				busstate <= bst_none;
				char_sent <= '0'; -- indicates wait for the client
				col <= std_logic_vector(unsigned(col) + 1);
				row <= X"00";

			when st_cr =>
				busstate <= bst_none;
				char_sent <= '0'; -- indicates wait for the client
				row <= X"00";

			when st_lf =>
				busstate <= bst_none;
				char_sent <= '0'; -- indicates wait for the client
				col <= std_logic_vector(unsigned(col) + 1);
			
			when st_memread0 =>
				busstate <= bst_read;
				char_sent <= '0'; -- indicates wait for the client

			when st_memread1 =>
				busstate <= bst_read;
				char_sent <= '0'; -- indicates wait for the client
				dout <= din;

			when st_memwrite0 =>
				busstate <= bst_write;
				char_sent <= '0'; -- indicates wait for the client
				dout <= din;

			when st_memwrite1 =>
				busstate <= bst_write;
				char_sent <= '0'; -- indicates wait for the client
				
			when others =>
				busstate <= bst_none;
				char_sent <= '1';	-- client can continue

		end case;
	end if;
end process;

sequence: process(state_current, char, row, col) 
begin
	case state_current is

		when st_reset =>
			state_next <= st_readytostart;

		when st_readytostart =>
			if (char(7 downto 5) = "000") then
				case char is
					when char_NULL =>
						state_next <= st_print;
					when char_CLEAR =>
						state_next <= st_print; -- TODO: implement clear screen
					when char_HOME =>
						state_next <= st_print;	-- TODO: implement home
					when char_CR =>
						state_next <= st_cr;
					when char_LF =>
						state_next <= st_lf;
					when others =>
						state_next <= st_print;
				end case;
			else
				state_next <= st_print;
			end if;
			
		when st_print =>
			if (busy_in = '1') then
				state_next <= st_print; -- hold in loop until bus clears
			else
				state_next <= st_memwrite0;
			end if;
			
		when st_next_row =>
			if (unsigned(row) >= 79) then
				state_next <= st_print_incy;
			else
				state_next <= st_done;
			end if;
			
		when st_next_col =>
			if (unsigned(col) >= 50) then
				state_next <= st_reset; -- TODO!!
			else
				state_next <= st_done;
			end if;
			
		when st_cr =>
			state_next <= st_done;

		when st_lf =>
			if (unsigned(col) >= 50) then
				state_next <= st_reset; -- TODO!!
			else
				state_next <= st_done;
			end if;
			
		when st_memread0 =>
			state_next <= st_memread1;

		when st_memread1 =>
			state_next <= st_done; -- TODO

		when st_memwrite0 =>
			state_next <= st_memwrite1;

		when st_memwrite1 =>
			state_next <= st_done; -- TODO?

		when st_done =>
			state_next <= st_donehold;
			
		when st_donehold =>
			if (char = char_NULL) then
				state_next <= st_readytostart;
			else
				state_next <= st_donehold;
			end if;
		
		when others =>
			state_next <= st_done;
			
	end case;
end process;

--
--u_instruction <= u_code(to_integer(unsigned(u_pc(5 downto 0))));
--
---- various conditions
--row0 <= '1' when (unsigned(row) = 0) else '0';
--row49 <= '1' when (unsigned(row) = 49) else '0';
--col0 <= '1' when (unsigned(col) = 0) else '0';
--col79 <= '1' when (unsigned(col) = 79) else '0';
--char0 <= '1' when (char = char_NULL) else '0';
--charcontrol <= '1' when (char(7 downto 5) = "000") else '0';
--charcr <= '1' when (char = char_CR) else '0';
--charlf <= '1' when (char = char_LF) else '0';
--charcls <= '1' when (char = char_CLEAR) else '0';
--charhome <= '1' when (char = char_HOME) else '0';
--
---- select condition code
--with u_if select
--	u_condition <= '1' 			when cond_true,			-- 0
--						char0			when cond_char0,
--						charcontrol when cond_charcontrol,
--						busy_in 		when cond_busyin,
--						row0  		when cond_row0,
--						row49  		when cond_row49,
--						col0  		when cond_col0,
--						col79  		when cond_col79,
--						charcr 		when cond_charcr,		
--						charlf 		when cond_charlf,			
--						charhome 	when cond_charhome,		
--						charcls 		when cond_charcls,		
--						'0' 			when cond_12,				-- not used
--						'0' 			when cond_13,				-- not used
--						'0' 			when cond_14,				-- not used
--						'0' 			when cond_false;			-- 15
--
---- select then or else part
--u_next <= u_then when (u_condition = '1') else u_else;
--
---- update microcode program counter
--update_upc: process(clk, reset, u_next)
--begin
--	if (reset = '1') then
--		-- start execution at location 0
--		u_pc <= X"00";
--		u_ra <= X"00";
--	else
--		if (rising_edge(clk)) then
--			case u_next is
--				-- if condition(0) = '1' then X"00000" (default) will cause simple u_pc advance
--				when upc_next =>
--					u_pc <= std_logic_vector(unsigned(u_pc) + 1);
--				-- used to repeat same microinstruction until condition turns true
--				when upc_repeat =>
--					u_pc <= u_pc;
--				-- not used
--				--when upc_fork => 
--				--	u_pc <= '1' & instruction;
--				-- return from "1 level subroutine"
--				when upc_return => 
--					u_pc <= u_ra;
--				-- any other value is a jump to that microinstruction location, save return address for "1 level stack"
--				when others =>
--					u_pc <= u_next;
--					u_ra <= std_logic_vector(unsigned(u_pc) + 1);
--			end case;
--		end if;
--	end if;
--end process;  
--
---- update dout
--update_dout: process(clk)
--begin
--	if (rising_edge(clk)) then
--		case u_dout is
--			when dout_constant =>
--				dout <= u_constant;
--			when dout_din =>
--				dout <= din;
--			when dout_char =>
--				dout <= char;
--			when others =>
--				null;
--		end case;
--	end if;
--end process;
--
---- update row
--update_row: process(clk)
--begin
--	if (rising_edge(clk)) then
--		case u_row is
--			when row_constant =>
--				row <= u_constant;
--			when row_inc =>
--				row <= std_logic_vector(unsigned(row) + 1);
--			when row_dec =>
--				row <= std_logic_vector(unsigned(row) + 1);
--			when others =>
--				null;
--		end case;
--	end if;
--end process;
--
---- update row
--update_col: process(clk)
--begin
--	if (rising_edge(clk)) then
--		case u_row is
--			when row_constant =>
--				col <= u_constant;
--			when row_inc =>
--				col <= std_logic_vector(unsigned(col) + 1);
--			when row_dec =>
--				col <= std_logic_vector(unsigned(col) + 1);
--			when others =>
--				null;
--		end case;
--	end if;
--end process;


end Behavioral;

