{-# LANGUAGE OverloadedStrings #-}
module LinuxSignal where

--import qualified Data.Map             as M
import qualified Data.HashMap.Strict       as M
import qualified Data.Text            as T
import           Foreign.C.Types
import           System.Posix.Signals

signalMap :: M.HashMap T.Text CInt
signalMap = M.fromList [
  ("EXIT",     0 ),
  ("HUP",      1 ),
  ("INT",      2 ),
  ("QUIT",     3 ),
  ("ILL",      4 ),
  ("TRAP",     5 ),
  ("ABRT",     6 ),
  ("BUS",      7 ),
  ("FPE",      8 ),
--  ("KILL",     9 ),
  ("USR1",     10),
  ("SEGV",     11),
  ("USR2",     12),
  ("PIPE",     13),
  ("ALRM",     14),
  ("TERM",     15),
  ("STKFLT",   16),
  ("CHLD",     17),
  ("CONT",     18),
--  ("STOP",     19),
  ("TSTP",     20),
  ("TTIN",     21),
  ("TTOU",     22),
  ("URG",      23),
  ("XCPU",     24),
  ("XFSZ",     25),
  ("VTALRM",   26),
  ("PROF",     27),
--  ("WINCH",    28),
  ("IO",       29),
  ("PWR",      30),
  ("SYS",      31),
  ("RTMIN",    34),
  ("RTMIN+1",  35),
  ("RTMIN+2",  36),
  ("RTMIN+3",  37),
  ("RTMIN+4",  38),
  ("RTMIN+5",  39),
  ("RTMIN+6",  40),
  ("RTMIN+7",  41),
  ("RTMIN+8",  42),
  ("RTMIN+9",  43),
  ("RTMIN+10", 44),
  ("RTMIN+11", 45),
  ("RTMIN+12", 46),
  ("RTMIN+13", 47),
  ("RTMIN+14", 48),
  ("RTMIN+15", 49),
  ("RTMAX-14", 50),
  ("RTMAX-13", 51),
  ("RTMAX-12", 52),
  ("RTMAX-11", 53),
  ("RTMAX-10", 54),
  ("RTMAX-9" , 55),
  ("RTMAX-8" , 56),
  ("RTMAX-7" , 57),
  ("RTMAX-6" , 58),
  ("RTMAX-5" , 59),
  ("RTMAX-4" , 60),
  ("RTMAX-3" , 61),
  ("RTMAX-2" , 62),
  ("RTMAX-1" , 63),
  ("RTMAX"   , 64)
  ]

defaultSignal = [sigHUP, sigINT, sigQUIT, sigTERM]
