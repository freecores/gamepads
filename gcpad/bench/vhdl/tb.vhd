-------------------------------------------------------------------------------
--
-- Testbench for the
-- GCpad controller core
--
-- $Id: tb.vhd,v 1.2 2004-10-09 17:05:59 arniml Exp $
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

architecture behav of tb is

  constant period_c         : time    := 100 ns;
  constant reset_level_c    : natural := 0;
  constant clocks_per_1us_c : natural := 10;

  component gcpad_basic
    generic (
      reset_level_g    :       integer := 0;
      clocks_per_1us_g :       integer := 2
    );
    port (
      clk_i            : in    std_logic;
      reset_i          : in    std_logic;
      pad_request_i    : in    std_logic;
      pad_avail_o      : out   std_logic;
      pad_data_io      : inout std_logic;
      but_a_o          : out   std_logic;
      but_b_o          : out   std_logic;
      but_x_o          : out   std_logic;
      but_y_o          : out   std_logic;
      but_z_o          : out   std_logic;
      but_start_o      : out   std_logic;
      but_tl_o         : out   std_logic;
      but_tr_o         : out   std_logic;
      but_left_o       : out   std_logic;
      but_right_o      : out   std_logic;
      but_up_o         : out   std_logic;
      but_down_o       : out   std_logic;
      ana_joy_x_o      : out   std_logic_vector(7 downto 0);
      ana_joy_y_o      : out   std_logic_vector(7 downto 0);
      ana_c_x_o        : out   std_logic_vector(7 downto 0);
      ana_c_y_o        : out   std_logic_vector(7 downto 0);
      ana_l_o          : out   std_logic_vector(7 downto 0);
      ana_r_o          : out   std_logic_vector(7 downto 0)
    );
  end component;

  signal clk_s   : std_logic;
  signal reset_s : std_logic;

  signal pad_data_s : std_logic;

  signal buttons_s : std_logic_vector(64 downto 0);

  signal pad_request_s : std_logic;
  signal pad_avail_s   : std_logic;

begin

  basic_b : gcpad_basic
    generic map (
      reset_level_g    => reset_level_c,
      clocks_per_1us_g => clocks_per_1us_c
    )
    port map (
      clk_i          => clk_s,
      reset_i        => reset_s,
      pad_request_i  => pad_request_s,
      pad_avail_o    => pad_avail_s,
      pad_data_io    => pad_data_s,
      but_a_o        => buttons_s(56),
      but_b_o        => buttons_s(57),
      but_x_o        => buttons_s(58),
      but_y_o        => buttons_s(59),
      but_z_o        => buttons_s(52),
      but_start_o    => buttons_s(60),
      but_tl_o       => buttons_s(54),
      but_tr_o       => buttons_s(53),
      but_left_o     => buttons_s(48),
      but_right_o    => buttons_s(49),
      but_up_o       => buttons_s(51),
      but_down_o     => buttons_s(50),
      ana_joy_x_o    => buttons_s(47 downto 40),
      ana_joy_y_o    => buttons_s(39 downto 32),
      ana_c_x_o      => buttons_s(31 downto 24),
      ana_c_y_o      => buttons_s(23 downto 16),
      ana_l_o        => buttons_s(15 downto  8),
      ana_r_o        => buttons_s( 7 downto  0)
    );

  buttons_s(64)           <= '0';
  buttons_s(63 downto 61) <= (others => '0');
  buttons_s(55)           <= '1';


  -- pullup on pad_data
--  pad_data_s <= 'H';


  stimuli: process

    procedure check_request is
      constant request_c : std_logic_vector(24 downto 0) := "H0H000000HH000000000000H0";
    begin

      -- read 25 bits from pad_data_s
      for i in 0 to 24 loop
        wait until pad_data_s = '0';

        wait for 2 * clocks_per_1us_c * period_c;
        if pad_data_s /= request_c(i) then
          assert false
            report "Found wrong level on pad_data_s while checking request packet!"
            severity error;
        end if;

        wait for 2 * clocks_per_1us_c * period_c;

      end loop;


    end check_request;

    procedure send_packet(packet : in std_logic_vector(64 downto 0)) is
      variable time_low_v, time_high_v : time;
    begin
      wait until clk_s'event and clk_s = '1';
      wait for 1 ns;
      -- send request;
      pad_request_s <= '1';
      wait for 1 * period_c;
      pad_request_s <= '0';

      check_request;

      wait for 10 * 40 * period_c;

      for i in packet'high downto 0 loop
        if packet(i) = '0' then
          time_low_v  := 3 us;
          time_high_v := 1 us;
        else
          time_low_v  := 1 us;
          time_high_v := 3 us;
        end if;

        pad_data_s <= '0';
        wait for time_low_v;

        pad_data_s <= 'H';
        wait for time_high_v;

      end loop;

      wait until pad_avail_s = '1';
      wait for 10 * period_c;

      -- check result
      for i in 1 to packet'high loop
        assert packet(i) = buttons_s(i-1)
          report "Button mismatch!"
          severity error;
      end loop;

    end send_packet;


    procedure timeout_gcpad is
    begin
      -- send request;
      pad_request_s <= '1';
      wait for 1 * period_c;
      pad_request_s <= '0';

      check_request;

      wait until pad_avail_s = '1';
      wait for 10 * period_c;

    end timeout_gcpad;

  begin
    pad_data_s    <= 'H';
    pad_request_s <= '0';

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
    assert false
      report "End of simulation reached."
      severity failure;

  end process stimuli;



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

end behav;


-------------------------------------------------------------------------------
-- File History:
--
-- $Log: not supported by cvs2svn $
-- Revision 1.1  2004/10/07 21:24:06  arniml
-- initial check-in
--
-------------------------------------------------------------------------------
