-------------------------------------------------------------------------------
--
-- GCpad controller core
--
-- $Id: gcpad_rx.vhd,v 1.2 2004-10-08 20:51:59 arniml Exp $
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
use work.gcpad_pack.analog_axis_t;

entity gcpad_rx is

  generic (
    reset_level_g    :     integer := 0;
    clocks_per_1us_g :     integer := 2
  );
  port (
    -- System Interface -------------------------------------------------------
    clk_i            : in  std_logic;
    reset_i          : in  std_logic;
    -- Control Interface ------------------------------------------------------
    rx_en_i          : in  boolean;
    rx_done_o        : out boolean;
    rx_size_i        : in  std_logic_vector(3 downto 0);
    -- Gamepad Interface ------------------------------------------------------
    pad_data_i       : in  std_logic;
    -- Buttons Interface ------------------------------------------------------
    but_a_o          : out std_logic;
    but_b_o          : out std_logic;
    but_x_o          : out std_logic;
    but_y_o          : out std_logic;
    but_z_o          : out std_logic;
    but_start_o      : out std_logic;
    but_tl_o         : out std_logic;
    but_tr_o         : out std_logic;
    but_left_o       : out std_logic;
    but_right_o      : out std_logic;
    but_up_o         : out std_logic;
    but_down_o       : out std_logic;
    ana_joy_x_o      : out analog_axis_t;
    ana_joy_y_o      : out analog_axis_t;
    ana_c_x_o        : out analog_axis_t;
    ana_c_y_o        : out analog_axis_t;
    ana_l_o          : out analog_axis_t;
    ana_r_o          : out analog_axis_t
  );

end gcpad_rx;


library ieee;
use ieee.numeric_std.all;
use work.gcpad_pack.all;

architecture rtl of gcpad_rx is

  type state_t is (IDLE,
                   DETECT_TIMEOUT,
                   WAIT_FOR_1,
                   WAIT_FOR_0,
                   FINISHED);
  signal state_s,
         state_q  : state_t;

  signal buttons_q,
         shift_buttons_q : buttons_t;
  signal save_buttons_s  : boolean;
  signal shift_buttons_s : boolean;

  constant cnt_sample_high_c  : natural := clocks_per_1us_g * 4 - 1;
  subtype  cnt_sample_t       is natural range 0 to cnt_sample_high_c;
  signal   cnt_zeros_q        : cnt_sample_t;
  signal   cnt_ones_q         : cnt_sample_t;
  signal   sampled_s          : boolean;
  signal   sync_sample_s      : boolean;
  signal   wrap_sample_s      : boolean;
  signal   sample_underflow_q : boolean;

  signal   more_ones_q    : boolean;

  -- timeout counter counts three sample undeflows
  constant cnt_timeout_high_c : natural := 3;
  subtype  cnt_timeout_t      is natural range 0 to cnt_timeout_high_c;
  signal   cnt_timeout_q      : cnt_timeout_t;
  signal   timeout_q          : boolean;
  signal   sync_timeout_s     : boolean;


  subtype num_buttons_read_t  is unsigned(6 downto 0);
  signal  num_buttons_read_q  : num_buttons_read_t;
  signal  all_buttons_read_s  : boolean;
  signal  reset_num_buttons_s : boolean;

  signal pad_data_sync_q : std_logic_vector(1 downto 0);
  signal pad_data_s      : std_logic;

  signal rx_done_q,
         set_rx_done_s : boolean;

begin

  -----------------------------------------------------------------------------
  -- Process seq
  --
  -- Purpose:
  --   Implements the sequential elements of this module.
  --
  seq: process (reset_i, clk_i)
    variable dec_timeout_v : boolean;
    variable size_v        : std_logic_vector(num_buttons_read_t'range);
  begin
    if reset_i = reset_level_g then
      buttons_q       <= (others => '0');
      shift_buttons_q <= (others => '0');

      state_q              <= IDLE;

      cnt_zeros_q          <= cnt_sample_high_c;
      cnt_ones_q           <= cnt_sample_high_c;
      more_ones_q          <= false;
      sample_underflow_q   <= false;

      cnt_timeout_q        <= cnt_timeout_high_c;

      timeout_q            <= false;

      num_buttons_read_q   <= (others => '0');
      rx_done_q            <= false;

      pad_data_sync_q      <= (others => '1');


    elsif clk_i'event and clk_i = '1' then
      -- synchronizer for pad data
      pad_data_sync_q(0) <= pad_data_i;
      pad_data_sync_q(1) <= pad_data_sync_q(0);


      state_q <= state_s;


      dec_timeout_v := false;
      -- sample counter
      if sync_sample_s then
        -- explicit preload
        cnt_zeros_q <= cnt_sample_high_c;
        cnt_ones_q  <= cnt_sample_high_c;
      else
        if cnt_zeros_q = 0 then
          if wrap_sample_s then
            cnt_zeros_q   <= cnt_sample_high_c;
          end if;
          dec_timeout_v := true;
        elsif pad_data_s = '0' then
          cnt_zeros_q   <= cnt_zeros_q - 1;
        end if;

        if cnt_ones_q = 0 then
          if wrap_sample_s then
            cnt_ones_q    <= cnt_sample_high_c;
          end if;
          dec_timeout_v := true;
        elsif pad_data_s /= '0' then
          cnt_ones_q  <= cnt_ones_q - 1;
        end if;
      end if;

      if cnt_ones_q < cnt_zeros_q then
        more_ones_q <= true;
      else
        more_ones_q <= false;
      end if;

      -- detect sample underflow
      sample_underflow_q <= dec_timeout_v;

      -- timeout counter
      if sync_timeout_s then
        -- explicit preload
        cnt_timeout_q <= cnt_timeout_high_c;
        timeout_q     <= false;
      elsif cnt_timeout_q = 0 then
        -- wrap-around
        cnt_timeout_q <= cnt_timeout_high_c;
        timeout_q     <= true;
      elsif dec_timeout_v then
        -- decrement counter when sampler wraps around
        cnt_timeout_q <= cnt_timeout_q - 1;
      end if;


      -- count remaining number of buttons to read
      if shift_buttons_s then
        shift_buttons_q(buttons_t'high downto 1) <= shift_buttons_q(buttons_t'high-1 downto 0);

        if more_ones_q then
          shift_buttons_q(0) <= '1';
        else
          shift_buttons_q(0) <= '0';
        end if;

      end if;

      if reset_num_buttons_s then
        -- explicit preload
        size_v(num_buttons_read_t'high downto 3) := rx_size_i;
        size_v(2 downto 0)                       := (others => '0');
        num_buttons_read_q   <= unsigned(size_v);
      elsif shift_buttons_s then
        -- decrement counter when a button bit has been read
        if not all_buttons_read_s then
          num_buttons_read_q <= num_buttons_read_q - 1;
        end if;
      end if;


      -- the buttons
      if save_buttons_s then
        buttons_q <= shift_buttons_q;
      end if;

      if set_rx_done_s then
        rx_done_q <= true;
      else
        rx_done_q <= false;
      end if;

    end if;

  end process seq;
  --
  -----------------------------------------------------------------------------

  pad_data_s         <= pad_data_sync_q(1);

  -- indicates that all buttons have been read
  all_buttons_read_s <= num_buttons_read_q = 0;


  -----------------------------------------------------------------------------
  -- Process fsm
  --
  -- Purpose:
  --   Models the controlling state machine.
  --
  fsm: process (state_q,
                rx_en_i,
                pad_data_s,
                all_buttons_read_s,
                sampled_s,
                sample_underflow_q,
                timeout_q)
  begin
    sync_sample_s       <= false;
    sync_timeout_s      <= false;
    state_s             <= IDLE;
    shift_buttons_s     <= false;
    save_buttons_s      <= false;
    set_rx_done_s       <= false;
    reset_num_buttons_s <= false;
    wrap_sample_s       <= false;

    case state_q is
      -- IDLE -----------------------------------------------------------------
      -- The idle state.
      when IDLE =>
        if rx_en_i then
          state_s             <= DETECT_TIMEOUT;

        else
          -- keep counters synchronized when no reception is running
          sync_sample_s       <= true;
          sync_timeout_s      <= true;
          reset_num_buttons_s <= true;
          state_s             <= IDLE;

        end if;

      when DETECT_TIMEOUT =>
        state_s             <= DETECT_TIMEOUT;

        if pad_data_s = '0' then
          sync_sample_s     <= true;
          state_s           <= WAIT_FOR_1;

        else
          -- wait for timeout
          wrap_sample_s     <= true;
          if timeout_q then
            set_rx_done_s   <= true;
            state_s         <= IDLE;
          end if;

        end if;


      -- WAIT_FOR_1 -----------------------------------------------------------
      -- Sample counter has expired and a 0 bit has been detected.
      -- We must now wait for pad_data_s to become 1.
      -- Or abort upon timeout.
      when WAIT_FOR_1 =>
        if pad_data_s = '0' then
          if not sample_underflow_q then
            state_s       <= WAIT_FOR_1;
          else
            -- timeout while reading buttons!
            set_rx_done_s <= true;
            state_s       <= IDLE;
          end if;

        else
          state_s         <= WAIT_FOR_0;
        end if;

      -- WAIT_FOR_0 -----------------------------------------------------------
      -- pad_data_s is at 1 level now and no timeout occured so far.
      -- We wait for the next 0 level on pad_data_s or abort upon timeout.
      when WAIT_FOR_0 =>
        -- wait for falling edge of pad data
        if pad_data_s = '0' then
          sync_sample_s       <= true;

          if not all_buttons_read_s then
            -- loop another time if there are still some buttons to read
            shift_buttons_s   <= true;
            state_s           <= WAIT_FOR_1;
          else
            state_s           <= WAIT_FOR_0;
          end if;

        else
          if sample_underflow_q then
            if all_buttons_read_s then
              -- last button was read
              -- so it's ok to timeout
              state_s         <= FINISHED;
            else
              -- timeout while reading buttons!
              set_rx_done_s   <= true;
              state_s         <= IDLE;
            end if;

          else
            state_s           <= WAIT_FOR_0;

          end if;

        end if;

      when FINISHED =>
        -- finally save buttons
        save_buttons_s <= true;
        set_rx_done_s  <= true;

      when others =>
        null;

    end case;

  end process fsm;
  --
  -----------------------------------------------------------------------------


  -----------------------------------------------------------------------------
  -- Output Mapping
  -----------------------------------------------------------------------------
  rx_done_o   <= rx_done_q;
  but_a_o     <= buttons_q(pos_a_c);
  but_b_o     <= buttons_q(pos_b_c);
  but_x_o     <= buttons_q(pos_x_c);
  but_y_o     <= buttons_q(pos_y_c);
  but_z_o     <= buttons_q(pos_z_c);
  but_start_o <= buttons_q(pos_start_c);
  but_tl_o    <= buttons_q(pos_tl_c);
  but_tr_o    <= buttons_q(pos_tr_c);
  but_left_o  <= buttons_q(pos_left_c);
  but_right_o <= buttons_q(pos_right_c);
  but_up_o    <= buttons_q(pos_up_c);
  but_down_o  <= buttons_q(pos_down_c);
  ana_joy_x_o <= buttons_q(joy_x_high_c downto joy_x_low_c);
  ana_joy_y_o <= buttons_q(joy_y_high_c downto joy_y_low_c);
  ana_c_x_o   <= buttons_q(c_x_high_c   downto c_x_low_c);
  ana_c_y_o   <= buttons_q(c_y_high_c   downto c_y_low_c);
  ana_l_o     <= buttons_q(l_high_c     downto l_low_c);
  ana_r_o     <= buttons_q(r_high_c     downto r_low_c);


end rtl;


-------------------------------------------------------------------------------
-- File History:
--
-- $Log: not supported by cvs2svn $
-- Revision 1.1  2004/10/07 21:23:10  arniml
-- initial check-in
--
-------------------------------------------------------------------------------
