-------------------------------------------------------------------------------
--
-- Testbench for the
-- GCpad controller core
--
-- $Id: tb.vhd,v 1.3 2004-10-10 17:27:44 arniml Exp $
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

entity tb is

end tb;


use work.gcpad_pack.all;
use work.gcpad_comp.gcpad_basic;
use work.gcpad_comp.gcpad_full;

architecture behav of tb is

  component gcpad_mod
    generic (
      clocks_per_1us_g :       natural := 2
    );
    port (
      clk_i            : in    std_logic;
      pad_data_io      : inout std_logic;
      rx_data_i        : in    std_logic_vector(63 downto 0)
    );
  end component;

  constant period_c         : time    := 100 ns;
  constant reset_level_c    : natural := 0;
  constant clocks_per_1us_c : natural := 10;

  signal clk_s   : std_logic;
  signal reset_s : std_logic;

  -- signals for basic gcpad
  signal stimuli_1_end_s : boolean;
  signal pad_data_1_s    : std_logic;
  signal buttons_1_s     : std_logic_vector(64 downto 0);
  signal pad_request_1_s : std_logic;
  signal pad_avail_1_s   : std_logic;
  signal rx_data_1_s     : std_logic_vector(63 downto 0);

  -- signals for full gcpad
  signal stimuli_2_end_s : boolean;
  signal pad_data_2_s    : std_logic;
  signal pad_request_2_s : std_logic;
  signal pad_avail_2_s   : std_logic;
  signal pad_timeout_2_s : std_logic;
  signal tx_size_2_s     : std_logic_vector( 1 downto 0);
  signal tx_command_2_s  : std_logic_vector(23 downto 0);
  signal rx_size_2_s     : std_logic_vector( 3 downto 0);
  signal rx_data_2_s     : std_logic_vector(63 downto 0);

begin

  basic_b : gcpad_basic
    generic map (
      reset_level_g    => reset_level_c,
      clocks_per_1us_g => clocks_per_1us_c
    )
    port map (
      clk_i          => clk_s,
      reset_i        => reset_s,
      pad_request_i  => pad_request_1_s,
      pad_avail_o    => pad_avail_1_s,
      pad_data_io    => pad_data_1_s,
      but_a_o        => buttons_1_s(56),
      but_b_o        => buttons_1_s(57),
      but_x_o        => buttons_1_s(58),
      but_y_o        => buttons_1_s(59),
      but_z_o        => buttons_1_s(52),
      but_start_o    => buttons_1_s(60),
      but_tl_o       => buttons_1_s(54),
      but_tr_o       => buttons_1_s(53),
      but_left_o     => buttons_1_s(48),
      but_right_o    => buttons_1_s(49),
      but_up_o       => buttons_1_s(51),
      but_down_o     => buttons_1_s(50),
      ana_joy_x_o    => buttons_1_s(47 downto 40),
      ana_joy_y_o    => buttons_1_s(39 downto 32),
      ana_c_x_o      => buttons_1_s(31 downto 24),
      ana_c_y_o      => buttons_1_s(23 downto 16),
      ana_l_o        => buttons_1_s(15 downto  8),
      ana_r_o        => buttons_1_s( 7 downto  0)
    );

  buttons_1_s(64)           <= '0';
  buttons_1_s(63 downto 61) <= (others => '0');
  buttons_1_s(55)           <= '1';


  full_b: gcpad_full
    generic map (
      reset_level_g    => reset_level_c,
      clocks_per_1us_g => clocks_per_1us_c
    )
    port map (
      clk_i            => clk_s,
      reset_i          => reset_s,
      pad_request_i    => pad_request_2_s,
      pad_avail_o      => pad_avail_2_s,
      pad_timeout_o    => pad_timeout_2_s,
      tx_size_i        => tx_size_2_s,
      tx_command_i     => tx_command_2_s,
      rx_size_i        => rx_size_2_s,
      rx_data_o        => rx_data_2_s,
      pad_data_io      => pad_data_2_s
    );


  pad_1 : gcpad_mod
    generic map (
      clocks_per_1us_g => clocks_per_1us_c
    )
    port map (
      clk_i            => clk_s,
      pad_data_io      => pad_data_1_s,
      rx_data_i        => rx_data_1_s
    );

  -----------------------------------------------------------------------------
  -- Process stimuli_pad_1
  --
  -- Executes test stimuli with Pad 1, the gcpad_basic flavour.
  --
  stimuli_pad_1: process


    procedure send_packet(packet : in std_logic_vector(64 downto 0)) is
    begin
      wait until clk_s'event and clk_s = '1';
      wait for 1 ns;

      rx_data_1_s <= packet(64 downto 1);

      -- send request;
      pad_request_1_s <= '1';
      wait for 1 * period_c;
      pad_request_1_s <= '0';

      wait for 10 * 40 * period_c;

      wait until pad_avail_1_s = '1';
      wait for 10 * period_c;

      -- check result
      for i in 1 to packet'high loop
        assert packet(i) = buttons_1_s(i-1)
          report "Button mismatch!"
          severity error;
      end loop;

    end send_packet;


    procedure timeout_gcpad is
    begin
      -- send request;
      pad_request_1_s <= '1';
      wait for 1 * period_c;
      pad_request_1_s <= '0';

      wait for 2 * period_c;

      -- disturb communication
      pad_data_1_s <= 'X';

      wait until pad_avail_1_s = '1';
      wait for 10 * period_c;
      pad_data_1_s <= 'H';
      wait for 10 * period_c;

    end timeout_gcpad;

  begin
    stimuli_1_end_s <= false;

    pad_data_1_s    <= 'H';
    pad_request_1_s <= '0';
    rx_data_1_s     <= (others => '0');

    wait until reset_s = '1';
    wait for period_c * 4;

    timeout_gcpad;
    send_packet(packet => "00000000100000000000000000000000000000000000000000000000000000001");
    wait for clocks_per_1us_c * 100 * period_c;
    send_packet(packet => "00011111111111111111111111111111111111111111111111111111111111111");
    send_packet(packet => "00010000100000000000000000000000000000000000000000000000000000001");
    send_packet(packet => "00001000100000000000000000000000000000000000000000000000000000001");
    send_packet(packet => "00000100100000000000000000000000000000000000000000000000000000001");
    send_packet(packet => "00000010100000000000000000000000000000000000000000000000000000001");
    send_packet(packet => "00000001100000000000000000000000000000000000000000000000000000001");
    send_packet(packet => "00001010101010101010101010101010101010101010101010101010101010101");
    send_packet(packet => "00010101110101010101010101010101010101010101010101010101010101011");
    send_packet(packet => "00000000100000000000000000000000000000000000000000000000000000001");
    send_packet(packet => "00000000110000000000000000000000000000000000000000000000000000001");
    send_packet(packet => "00000000101000000000000000000000000000000000000000000000000000001");
    send_packet(packet => "00000000100100000000000000000000000000000000000000000000000000001");
    send_packet(packet => "00000000100010000000000000000000000000000000000000000000000000001");
    send_packet(packet => "00000000100001000000000000000000000000000000000000000000000000001");
    send_packet(packet => "00000000100000100000000000000000000000000000000000000000000000001");
    send_packet(packet => "00000000100000010000000000000000000000000000000000000000000000001");
    send_packet(packet => "00000000100000001000000000000000000000000000000000000000000000001");
    send_packet(packet => "00000000100000000100000000000000000000000000000000000000000000001");
    send_packet(packet => "00000000100000000010000000000000000000000000000000000000000000001");
    send_packet(packet => "00000000100000000001000000000000000000000000000000000000000000001");
    send_packet(packet => "00000000100000000000100000000000000000000000000000000000000000001");
    send_packet(packet => "00000000100000000000010000000000000000000000000000000000000000001");
    send_packet(packet => "00000000100000000000001000000000000000000000000000000000000000001");
    send_packet(packet => "00000000100000000000000100000000000000000000000000000000000000001");
    send_packet(packet => "00000000100000000000000010000000000000000000000000000000000000001");
    send_packet(packet => "00000000100000000000000001000000000000000000000000000000000000001");
    send_packet(packet => "00000000100000000000000000100000000000000000000000000000000000001");
    send_packet(packet => "00000000100000000000000000010000000000000000000000000000000000001");
    send_packet(packet => "00000000100000000000000000001000000000000000000000000000000000001");
    send_packet(packet => "00000000100000000000000000000100000000000000000000000000000000001");
    send_packet(packet => "00000000100000000000000000000010000000000000000000000000000000001");
    send_packet(packet => "00000000100000000000000000000001000000000000000000000000000000001");
    send_packet(packet => "00000000100000000000000000000000100000000000000000000000000000001");
    send_packet(packet => "00000000100000000000000000000000010000000000000000000000000000001");
    send_packet(packet => "00000000100000000000000000000000001000000000000000000000000000001");
    send_packet(packet => "00000000100000000000000000000000000100000000000000000000000000001");
    send_packet(packet => "00000000100000000000000000000000000010000000000000000000000000001");
    send_packet(packet => "00000000100000000000000000000000000001000000000000000000000000001");
    send_packet(packet => "00000000100000000000000000000000000000100000000000000000000000001");
    send_packet(packet => "00000000100000000000000000000000000000010000000000000000000000001");
    send_packet(packet => "00000000100000000000000000000000000000001000000000000000000000001");
    send_packet(packet => "00000000100000000000000000000000000000000100000000000000000000001");
    send_packet(packet => "00000000100000000000000000000000000000000010000000000000000000001");
    send_packet(packet => "00000000100000000000000000000000000000000001000000000000000000001");
    send_packet(packet => "00000000100000000000000000000000000000000000100000000000000000001");
    send_packet(packet => "00000000100000000000000000000000000000000000010000000000000000001");
    send_packet(packet => "00000000100000000000000000000000000000000000001000000000000000001");
    send_packet(packet => "00000000100000000000000000000000000000000000000100000000000000001");
    send_packet(packet => "00000000100000000000000000000000000000000000000010000000000000001");
    send_packet(packet => "00000000100000000000000000000000000000000000000001000000000000001");
    send_packet(packet => "00000000100000000000000000000000000000000000000000100000000000001");
    send_packet(packet => "00000000100000000000000000000000000000000000000000010000000000001");
    send_packet(packet => "00000000100000000000000000000000000000000000000000001000000000001");
    send_packet(packet => "00000000100000000000000000000000000000000000000000000100000000001");
    send_packet(packet => "00000000100000000000000000000000000000000000000000000010000000001");
    send_packet(packet => "00000000100000000000000000000000000000000000000000000001000000001");
    send_packet(packet => "00000000100000000000000000000000000000000000000000000000100000001");
    send_packet(packet => "00000000100000000000000000000000000000000000000000000000010000001");
    send_packet(packet => "00000000100000000000000000000000000000000000000000000000001000001");
    send_packet(packet => "00000000100000000000000000000000000000000000000000000000000100001");
    send_packet(packet => "00000000100000000000000000000000000000000000000000000000000010001");
    send_packet(packet => "00000000100000000000000000000000000000000000000000000000000001001");
    send_packet(packet => "00000000100000000000000000000000000000000000000000000000000000101");
    send_packet(packet => "00000000100000000000000000000000000000000000000000000000000000011");


    wait for period_c * 2*40;
    stimuli_1_end_s <= true;
    wait;

  end process stimuli_pad_1;
  --
  -----------------------------------------------------------------------------


  pad_2 : gcpad_mod
    generic map (
      clocks_per_1us_g => clocks_per_1us_c
    )
    port map (
      clk_i            => clk_s,
      pad_data_io      => pad_data_2_s,
      rx_data_i        => rx_data_2_s
    );

  -----------------------------------------------------------------------------
  -- Process stimuli_pad_2
  --
  -- Executes test stimuli with Pad 2, the gcpad_full flavour.
  --
  stimuli_pad_2: process

    procedure issue_command(cmd  : in std_logic_vector(23 downto 0);
                            size : in std_logic_vector( 1 downto 0)) is
    begin
      wait until clk_s'event and clk_s = '1';
      wait for 1 ns;

      tx_command_2_s  <= cmd;
      tx_size_2_s     <= size;
      -- send request;
      pad_request_2_s <= '1';
      wait for 1 * period_c;
      pad_request_2_s <= '0';


    end issue_command;

  begin
    stimuli_2_end_s <= false;

    pad_data_2_s    <= 'H';
    pad_request_2_s <= '0';
    tx_size_2_s     <= (others => '0');
    tx_command_2_s  <= (others => '0');
    rx_size_2_s     <= (others => '0');

    wait until reset_s = '1';
    wait for period_c * 4;


    issue_command(cmd  => "010000000000001100000010",
                  size => "11");


    wait for period_c * 2*40;
    stimuli_2_end_s <= true;
    wait;

  end process stimuli_pad_2;
  --
  -----------------------------------------------------------------------------


  -----------------------------------------------------------------------------
  -- Clock Generator
  -----------------------------------------------------------------------------
  clk: process
  begin
    clk_s <= '0';
    wait for period_c / 2;
    clk_s <= '1';
    wait for period_c / 2;
  end process clk;


  -----------------------------------------------------------------------------
  -- Reset Generator
  -----------------------------------------------------------------------------
  reset: process
  begin
    if reset_level_c = 0 then
      reset_s <= '0';
    else
      reset_s <= '1';
    end if;

    wait for period_c * 4 + 10 ns;

    reset_s <= not reset_s;

    wait;
  end process reset;


  -----------------------------------------------------------------------------
  -- End of simulation detection
  -----------------------------------------------------------------------------
  eos: process (stimuli_1_end_s, stimuli_2_end_s)
  begin
    if stimuli_1_end_s and stimuli_2_end_s then
      assert false
        report "End of simulation reached."
        severity failure;
    end if;
  end process eos;

end behav;


-------------------------------------------------------------------------------
-- File History:
--
-- $Log: not supported by cvs2svn $
-- Revision 1.2  2004/10/09 17:05:59  arniml
-- delay assertion of request signal by real time (instead of delta cycles)
--
-- Revision 1.1  2004/10/07 21:24:06  arniml
-- initial check-in
--
-------------------------------------------------------------------------------
