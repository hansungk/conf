-- Compiler flags --
{-# LANGUAGE NoMonomorphismRestriction #-}

-- Imports --
import XMonad

import XMonad.Actions.CycleWS

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.NoBorders as NB
import XMonad.Layout.Spacing
-- import XMonad.Layout.ResizableTile
import XMonad.Layout.Renamed
import XMonad.Layout.Fullscreen
import XMonad.Layout.Tabbed

import XMonad.Util.Run (spawnPipe, hPutStrLn)

import qualified Data.Map as M
import Control.Monad (when, join)
import Data.Maybe (maybeToList)

-- Notes.
--
-- Fullscreen:
-- - see https://github.com/xmonad/xmonad-contrib/issues/183#issuecomment-307407822
--
------------------------------------------------------------------------------------------
-- Main Configuration
main :: IO()
--main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig
main = do
        h <- spawnPipe myBar
        xmonad $ ewmh $ fullscreenSupport $ docks $ myConfig h

myConfig p = def
        { manageHook = myManageHook <+> manageHook defaultConfig -- merge with default
        , handleEventHook = XMonad.Layout.Fullscreen.fullscreenEventHook -- FIXME
        , layoutHook = myLayoutHook
        , logHook = myLogHook p
        , startupHook = addEWMHFullscreen
        , modMask = mod4Mask
        , terminal = "st"
        , keys = \c -> mykeys c `M.union` keys def c
        , borderWidth = 3
        , focusedBorderColor = (colLook Red 0)
        , normalBorderColor = (colLook Black 0)
        }


------------------------------------------------------------------------------------------
-- Keybindings
mykeys (XConfig {modMask = modm}) = M.fromList $
        [ ((modm, xK_Tab),    toggleWS)
        , ((modm , xK_p),     spawn "rofi -modi run,drun -show run")
        , ((modm, xK_b     ), sendMessage ToggleStruts)]

------------------------------------------------------------------------------------------
-- Constants
myBar = "xmobar /home/hansung/.xmobarrc"


------------------------------------------------------------------------------------------
-- Manage hook
myManageHook :: ManageHook
myManageHook =
        manageDocks
    <+> XMonad.Layout.Fullscreen.fullscreenManageHook
    <+> composeAll
            [ className =? "kakaotalk.exe" --> doFloat
            ]


------------------------------------------------------------------------------------------
-- Handle event hook
myHandleEventHook = handleEventHook def
                <+> XMonad.Layout.Fullscreen.fullscreenEventHook


------------------------------------------------------------------------------------------
-- Custom logHook
-- configure xmobar looks

-- Pretty with colored background
myLogHook h = dynamicLogWithPP $ def
        { ppCurrent	= xmobarColor (colLook White 1) (colLook Green 0) . wrap " " " "
        , ppVisible	= xmobarColor (colLook Green 1) "" . wrap " " " "
        , ppHidden	= xmobarColor (colLook White 1) "" . wrap " " " "
        , ppHiddenNoWindows = xmobarColor (colLook Black 0) "" . wrap " " " "
        , ppUrgent	= xmobarColor (colLook White 1) "" . wrap "[" "]"
        , ppLayout	= xmobarColor (colLook Green 1) "" .
        (\x -> case x of
         "Spacing 7 Tall" -> "[]="
         "Tall"           -> "[]="
         "Full"	          -> "[F]"
         _                -> x
        )
        , ppTitle	= xmobarColor (colLook White 1) "" . shorten 80
        , ppSep		= xmobarColor (colLook Grey 0) "" " | "
        , ppWsSep	= xmobarColor "" "" "" -- Eliminates the gap
        , ppOutput	= hPutStrLn h
        }

-- Vanilla config
-- myLogHook h = dynamicLogWithPP xmobarPP
--         { ppOutput = hPutStrLn h
--         , ppTitle = xmobarColor "green" "" . shorten 80
--         }


------------------------------------------------------------------------------------------
-- Custom layoutHook
-- configure layouts

-- Useless gaps
-- gaplessLayoutHook = avoidStruts $
--	tiled ||| Full ||| fullTiled
--	where 
--		tiled		= spacing 7 $ Tall nmaster delta ratio
--		fullTiled	= Tall nmaster delta ratio
--
--		-- The default number of windows in the master pane
--		nmaster	= 1
--		-- Percentage of screen to increment by when resizing panes
--		delta	= 5/100
--		-- Default proportion of screen occupied by master pane
--		ratio	= 1/2

myLayoutHook =
        NB.lessBorders NB.OnlyScreenFloat $
        avoidStruts $
        spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True $
        layoutHook defaultConfig

------------------------------------------------------------------------------------------
-- Key Bindings
-- Bar
-- FIXME
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


------------------------------------------------------------------------------------------
-- Fullscreen fix
--
-- See: https://github.com/xmonad/xmonad-contrib/issues/183#issuecomment-307407822
addNETSupported :: Atom -> X ()
addNETSupported x   = withDisplay $ \dpy -> do
    r               <- asks theRoot
    a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
    a               <- getAtom "ATOM"
    liftIO $ do
       sup <- (join . maybeToList) <$> getWindowProperty32 dpy a_NET_SUPPORTED r
       when (fromIntegral x `notElem` sup) $
         changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen   = do
    wms <- getAtom "_NET_WM_STATE"
    wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    mapM_ addNETSupported [wms, wfs]
