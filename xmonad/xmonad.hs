import System.Posix.Env (getEnv)
import Data.Maybe (maybe)

import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Actions.NoBorders
import qualified Data.Map as M

-- import XMonad.Layout.NoBorders

import System.IO

mykeys (XConfig {modMask = modm}) = M.fromList $ 
  [ ((modm , xK_x), sendMessage ToggleStruts) 
  , ((modm,  xK_g ), withFocused toggleBorder) 
  ]

main = do
     xmproc  <- spawnPipe "/usr/bin/xmobar" 
     session <- getEnv "DESKTOP_SESSION"
     xmonad $ (maybe desktopConfig desktop session) { terminal = "urxvt"
                                                    , logHook = dynamicLogWithPP sjanssenPP 
                                                        { ppOutput = hPutStrLn xmproc
                                                        , ppTitle = xmobarColor "black" "" . shorten 50
                                                        }
                                                     , layoutHook=avoidStruts $ layoutHook defaultConfig
                                                     , manageHook=manageHook defaultConfig <+> manageDocks
                                                     , keys = \c -> mykeys c `M.union` keys defaultConfig c
                                                    , focusedBorderColor = "#FF0000" 
                                                    }
desktop _ = desktopConfig
