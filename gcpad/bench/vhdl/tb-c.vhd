-------------------------------------------------------------------------------
--
-- Testbench for the
-- GCpad controller core
--
-- Copyright (c) 2004, Arnim Laeuger (arniml@opencores.org)
--
-- $Id: tb-c.vhd,v 1.1 2004-10-07 21:24:06 arniml Exp $
--
-------------------------------------------------------------------------------

configuration tb_behav_c0 of tb is

  for behav
    for basic_b : gcpad_basic
      use configuration work.gcpad_basic_struct_c0;
    end for;
  end for;

end tb_behav_c0;
