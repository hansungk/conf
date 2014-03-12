import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

main = do
	xmproc <- spawnPipe "/usr/bin/xmobar /home/stephen/.xmobarrc"
	xmonad $ defaultConfig
		{ manageHook = manageDocks <+> manageHook defaultConfig
		, layoutHook = avoidStruts  $  layoutHook defaultConfig
		, logHook = dynamicLogWithPP xmobarPP
						{ ppOutput = hPutStrLn xmproc
						, ppTitle = xmobarColor "green" "" . shorten 100
						}
		, modMask = mod4Mask
		, terminal = "urxvt"
		, borderWidth = myBorderWidth
		, normalBorderColor = myNormalBorderColor
		, focusedBorderColor = myFocusedBorderColor
		} `additionalKeys`
		[ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
		, ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
		, ((0, xK_Print), spawn "scrot")
		]

myBorderWidth = 1
myNormalBorderColor = "#0f0f0f"
myFocusedBorderColor = "#00B7EB"
