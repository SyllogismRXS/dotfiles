-------------------------------------------------------------------------------
--
-- DISTRIBUTION STATEMENT C
--   Distribution authorized to U.S. Government agencies and their contractors;
--   Critical Technology; 4 MAR 2003. Other requests shall be referred to
--   WR-ALC/LSEER.
--
-- EXPORT CONTROL WARNING
--   Warning - This document contains technical data whose export is restricted
--   by the Arms Export Control Act (Title 22, U.S.C., Sec 2751, et. seq.) or
--   the Export Administration Act of 1979, as amended, Title 50, U.S.C.,
--   App. 2401 et. seq. Violations of these export laws are subject to severe
--   criminal penalties. Disseminate in accordance with provisions of
--   DOD Directive 5230.25.
--
-- DESTRUCTION NOTICE
--   Destroy by any method that will prevent disclosure of contents or
--   reconstruction of the document.
--
-- Copyright 2011
-- Georgia Tech Research Corporation
-- All rights reserved
--
-- This material may be reproduced by or for the U.S. Government pursuant
-- to the copyright license under FAR 52.227-14.
--
-------------------------------------------------------------------------------
-- Title      : (>>>FILE_SANS<<<)
-- Project    : 
-------------------------------------------------------------------------------
-- File       : (>>>FILE<<<)
-- Author     : Kevin DeMarco
-- Company    : GTRI
-- Created    : (>>>DATE<<<)
-- Last update: 2013-01-02
-- Platform   : 
-- Standard   : VHDL'93
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Revisions  :
-- $Log: (>>>FILE<<<) $
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity (>>>FILE_SANS<<<) is
  generic(MY_GENERIC : integer);
  port (Reset  : in std_logic;
        SysClk : in std_logic;
        (>>>POINT<<<)
        );

end (>>>FILE_SANS<<<);

architecture Behavioral of (>>>FILE_SANS<<<) is

  signal blank0 : std_logic;
  signal blank1 : std_logic;
  
begin

-------------------------------------------------------------------------------
-- Process Description
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------

end Behavioral;
