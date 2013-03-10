import System.Posix.Env (getEnv)
import Data.Maybe (maybe)

import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

main = do
     xmproc  <- spawnPipe "/usr/bin/xmobar" 
     session <- getEnv "DESKTOP_SESSION"
     -- one day we will have locale support in uxterm
     xmonad $ (maybe desktopConfig desktop session) { terminal = "urxvt"
                                                    , logHook = dynamicLogWithPP sjanssenPP 
                                                        { ppOutput = hPutStrLn xmproc
                                                        , ppTitle = xmobarColor "black" "" . shorten 50
                                                        }
                                                    , focusedBorderColor = "#FF0000" 
                                                    }

desktop _ = desktopConfig
