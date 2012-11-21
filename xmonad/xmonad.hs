-- default desktop configuration for Fedora

import System.Posix.Env (getEnv)
import Data.Maybe (maybe)

import XMonad

import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Config.Kde
import XMonad.Config.Xfce
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

main = do
     xmproc  <- spawnPipe "/usr/bin/xmobar" 
     session <- getEnv "DESKTOP_SESSION"
     -- one day we will have locale support in uxterm
     xmonad $ (maybe desktopConfig desktop session) { terminal = "gnome-terminal --hide-menubar"
                                                    , logHook = dynamicLogWithPP xmobarPP
                                                        { ppOutput = hPutStrLn xmproc
                                                        , ppTitle = xmobarColor "green" "" . shorten 50
                                                        }
                                                    , focusedBorderColor = "#000000" 
                                                    }

desktop "gnome" = gnomeConfig
desktop "kde" = kde4Config
desktop "xfce" = xfceConfig
desktop "xmonad-gnome" = gnomeConfig
desktop _ = desktopConfig
