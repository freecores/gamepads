-------------------------------------------------------------------------------
--
-- GCpad controller core
--
-- $Id: gcpad_ctrl.vhd,v 1.1 2004-10-07 21:23:10 arniml Exp $
--
-- Copyright (c) 2004, Arnim Laeuger (arniml@opencores.org)
--
-- All rights reserved
--
-- Redistribution and use in source and synthezised forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- Redistributions of source code must retain the above copyright notice,
-- this list of conditions and the following disclaimer.
--
-- Redistributions in synthesized form must reproduce the above copyright
-- notice, this list of conditions and the following disclaimer in the
-- documentation and/or other materials provided with the distribution.
--
-- Neither the name of the author nor the names of other contributors may
-- be used to endorse or promote products derived from this software without
-- specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
-- THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
-- PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.
--
-- Please report bugs to the author, but before you do so, please
-- make sure that this is not a derivative work and that
-- you have the latest version of this file.
--
-- The latest version of this file can be found at:
--      http://www.opencores.org/cvsweb.shtml/gamepads/
--
-- The project homepage is located at:
--      http://www.opencores.org/projects.cgi/web/gamepads/overview
--
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity gcpad_ctrl is

  generic (
    reset_level_g :     integer := 0
  );
  port (
    -- System Interface -------------------------------------------------------
    clk_i         : in  std_logic;
    reset_i       : in  std_logic;
    pad_request_i : in  std_logic;
    pad_avail_o   : out std_logic;
    -- Control Interface ------------------------------------------------------
    tx_start_o    : out boolean;
    tx_finished_i : in  boolean;
    rx_en_o       : out boolean;
    rx_done_i     : in  boolean
  );

end gcpad_ctrl;


use work.gcpad_pack.all;

architecture rtl of gcpad_ctrl is

  type state_t is (IDLE,
                   TX,
                   RX_START,
                   RX_WAIT,
                   DEL1,
                   DEL1_WAIT,
                   DEL2,
                   DEL2_WAIT,
                   DEL3,
                   DEL3_WAIT);
  signal state_s,
         state_q  : state_t;

begin

  -----------------------------------------------------------------------------
  -- Process seq
  --
  -- Purpose:
  --   Implements the sequential elements.
  --
  seq: process (reset_i, clk_i)
  begin
    if reset_i = reset_level_g then
      state_q   <= IDLE;

    elsif clk_i'event and clk_i = '1' then
      state_q <= state_s;

    end if;

  end process seq;
  --
  -----------------------------------------------------------------------------


  -----------------------------------------------------------------------------
  -- Process fsm
  --
  -- Purpose:
  --   Models the controlling state machine.
  --
  fsm: process (state_q,
                tx_finished_i,
                rx_done_i,
                pad_request_i)
  begin
    rx_en_o          <= false;
    state_s          <= IDLE;
    tx_start_o       <= false;
    pad_avail_o      <= '0';

    case state_q is
      when IDLE =>
        if pad_request_i = '1' then
          state_s      <= TX;
          tx_start_o   <= true;
        else
          state_s      <= IDLE;
        end if;

      when TX =>
        if not tx_finished_i then
          state_s      <= TX;
        else
          state_s      <= RX_START;
        end if;

      when RX_START =>
        rx_en_o <= true;
        state_s <= RX_WAIT;

      when RX_WAIT =>
        if rx_done_i then
          state_s     <= DEL1;
        else
          state_s     <= RX_WAIT;
        end if;

      when DEL1 =>
        -- start receiver and wait for its initial timeout
        rx_en_o <= true;
        state_s <= DEL1_WAIT;

      when DEL1_WAIT =>
        if rx_done_i then
          state_s <= DEL2;
        else
          state_s <= DEL1_WAIT;
        end if;

      when DEL2 =>
        -- start receiver and wait for its initial timeout
        rx_en_o <= true;
        state_s <= DEL2_WAIT;

      when DEL2_WAIT =>
        if rx_done_i then
          state_s <= DEL3;
        else
          state_s <= DEL2_WAIT;
        end if;

      when DEL3 =>
        -- start receiver and wait for its initial timeout
        rx_en_o <= true;
        state_s <= DEL3_WAIT;

      when DEL3_WAIT =>
        if rx_done_i then
          pad_avail_o <= '1';
          state_s     <= IDLE;
        else
          state_s     <= DEL3_WAIT;
        end if;

      when others =>
        null;

    end case;

  end process fsm;
  --
  -----------------------------------------------------------------------------

end rtl;


-------------------------------------------------------------------------------
-- File History:
--
-- $Log: not supported by cvs2svn $
-------------------------------------------------------------------------------
