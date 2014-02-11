-- Compiler flags --
{-# LANGUAGE NoMonomorphismRestriction #-}

-- Imports --
-- stuff
import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Renamed
import XMonad.Layout.Tabbed
import XMonad.Layout.Spacing

import XMonad.Util.Run (spawnPipe, hPutStrLn)

-- stock
import qualified Data.Map as M

------------------------------------------------------------------------------------------
-- Main Configuration
main :: IO()
--main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig
main = do
	h <- spawnPipe myBar
	xmonad myConfig
	where
		myConfig = defaultConfig { workspaces = ["code", "web", "3", "4", "5", "6", "7", "8", "9"]
								 , modMask = mod4Mask
								 , terminal = "urxvt"
								 , borderWidth = 2
								 , focusedBorderColor = (colLook Cyan 0)
								 , normalBorderColor = (colLook Black 0)
								 , layoutHook = myLayoutHook
								 , logHook = myLogHook h
								 }


------------------------------------------------------------------------------------------
-- Constants
myBar = "xmobar"


------------------------------------------------------------------------------------------
-- Custom logHook
myLogHook h = dynamicLogWithPP myPP
	where
		myPP = defaultPP	{ ppCurrent	= xmobarColor (colLook White 1) (colLook Green 0) . wrap " " " "
							, ppVisible	= xmobarColor (colLook White 1) "" . wrap " " " "
							, ppHidden	= xmobarColor (colLook Green 1) "" . wrap " " " "
							, ppHiddenNoWindows = xmobarColor (colLook Black 0) "" . wrap " " " "
							, ppUrgent	= xmobarColor (colLook White 1) "" . wrap "[" "]"
							, ppLayout	= xmobarColor (colLook Green 1) "" -- . (\x -> "<icon=/home/stephen/.xmonad/icons/stlarch/tile.xbm/>")
							, ppTitle	= xmobarColor (colLook White 1) "" . shorten 80
							, ppSep		= xmobarColor (colLook Grey 0) "" " | "
							, ppWsSep	= xmobarColor "" "" "" -- Eliminates the gap
							, ppOutput	= hPutStrLn h
							}


------------------------------------------------------------------------------------------
-- Custom layoutHook
myLayoutHook = spacing 7 $ Tall nmaster delta ratio
	where 
		-- The default number of windows in the master pane
		nmaster = 1
		-- Percentage of screen to increment by when resizing panes
		delta = 5/100
		-- Default proportion of screen occupied by master pane
		ratio = 1/2


------------------------------------------------------------------------------------------
-- Key Bindings
-- Bar
toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)


------------------------------------------------------------------------------------------
-- Color Definitions
type Hex = String
type ColorCode = (Hex, Hex)
type ColorMap = M.Map Colors ColorCode

data Colors = Black | Grey | Red | Green | Yellow | Blue | Magenta | Cyan | White | BG
	deriving (Ord, Show, Eq)

colLook :: Colors -> Int -> Hex
colLook color n =
	case M.lookup color colors of
		Nothing -> "#000000"
		Just (c1,c2) -> if n == 0
						then c1
						else c2

colors :: ColorMap
colors = M.fromList
	[ (Black 	, (	"#555555",
					"#121212"))
	, (Grey 	, (	"#aaaaaa",
					"#121212"))
	, (Red	 	, (	"#c90c25",
					"#F21835"))
	, (Green 	, (	"#308888",
					"#53A6A6"))
	, (Yellow 	, (	"#54777d",
					"#415D62"))
	, (Blue 	, (	"#5c5dad",
					"#5063ab"))
	, (Magenta 	, (	"#6f4484",
					"#915eaa"))
	, (Cyan 	, (	"#2B7694",
					"#47959E"))
	, (White 	, (	"#D6D6D6",
					"#EEEEEE"))
	, (BG	 	, (	"#000000",
					"#444444"))
	]
