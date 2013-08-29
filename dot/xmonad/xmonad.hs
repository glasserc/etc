import XMonad
import XMonad.Config.Kde
import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks
import XMonad.Actions.WindowNavigation
import XMonad.Layout.WindowNavigation
import XMonad.Util.EZConfig
import XMonad.Util.WindowProperties (getProp32s)
import qualified XMonad.StackSet as W

myLayout = avoidStruts (tiled ||| (Mirror tiled) ||| Full)
         where tiled = Tall nmaster delta ratio
               nmaster = 1
               ratio = 1/2
               delta = 3/100

kdeOverride :: Query Bool
kdeOverride = ask >>= \w -> liftX $ do
  override <- getAtom "_KDE_NET_WM_WINDOW_TYPE_OVERRIDE"
  wt <- getProp32s "_NET_WM_WINDOW_TYPE" w
  return $ maybe False (elem $ fromIntegral override) wt

myManageHook = composeAll
   [
--   io (appendFile "/tmp/xwtf" "Hi\n") >> idHook,
--   className >>= io . appendFile "/home/ethan/xmonad_debug" >> idHook,
   className =? "plasma-desktop" --> (doFloat <+> doF W.focusDown),
--   className =? "Plasma-desktop" --> (doF W.focusUp),
   manageDocks
   ]

main = do
     config <- withWindowNavigation (xK_Up, xK_Left, xK_Down, xK_Right)
               $ desktopConfig
         { modMask = mod4Mask
--       , terminal = "konsole"
         , manageHook = ((className =? "krunner" <||> className =? "Plasma-desktop") >>= return . not --> manageHook kde4Config) <+> (kdeOverride --> doFloat) <+>
           myManageHook <+> manageHook defaultConfig
--         , manageHook = manageHook defaultConfig
         } `additionalKeys` myKeys
     xmonad config

dmenuBinding = spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\""
defaultModP = "exe=`dmenu_path | dmenu` && eval \"exec $exe\""
myKeys =
       [ ((mod4Mask, xK_apostrophe), dmenuBinding)
--       , ("M-<F2>", spawn "dmrun")
       ]
