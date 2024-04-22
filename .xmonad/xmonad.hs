---------------------------------------------------------------------------
-- My XMonad Config - Kelvin Davis                                       --
---------------------------------------------------------------------------

------------------------------------------------------------------------}}}
-- TODO                                                                 {{{
---------------------------------------------------------------------------

{-|
  * Notifications
  * Binding / prompt for network connections
  * Documentation lookup with surf
  * bind w,e to change screen and move mouse to that screen
  * script to fuzzy search pdfs by metadata
 -}

------------------------------------------------------------------------}}}
-- Modules                                                              {{{
---------------------------------------------------------------------------

-- Core
import XMonad
import XMonad.ManageHook
import XMonad.Prompt
-- Contrib
import XMonad.Actions.DynamicProjects
import XMonad.Actions.ShowText
import XMonad.Actions.Search
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Spacing
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Prompt.Pass
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Man
import XMonad.Prompt.Shell
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.XMonad
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run

import System.Directory
import Control.Monad
import qualified Data.Map as M

import Colors

------------------------------------------------------------------------}}}
-- Main                                                                 {{{
---------------------------------------------------------------------------

main :: IO ()
main = xmonad
     . dynamicProjects projects
     . fullscreenSupport =<< xmobar myConfig

------------------------------------------------------------------------}}}
-- Hooks                                                                {{{
---------------------------------------------------------------------------

-- use xprop to find these properties
myManageHook :: ManageHook
myManageHook = fullscreenManageHook 
           <+> composeAll [ resource =? "mendeleydesktop" --> idHook
                          , className =? "Tor Browser"    --> doFloat
                          , className =? "Spotify"        --> doShift "music"
                          ]

myLayoutHook = spacing 24 myTall
           {-||| smartBorders  myTall-}
           ||| noBorders Full where
             myTall = Tall 1 (5/200) (5/8)

------------------------------------------------------------------------}}}
-- Bindings                                                             {{{
---------------------------------------------------------------------------

{-
-- Below are the extra keybindings
-- These are in the Emacs style as parsed by additionalKeysP
-- For more go to https://hackage.haskell.org/package/xmonad-contrib-0.13/docs/XMonad-Util-EZConfig.html
-}

myKeys = [ ("M-C-q",                   spawn "xmonad --recompile && xmonad --restart")
         , ("M-S-d",                   flashText def 1 "hello")
         , ("<XF86MonBrightnessUp>",   spawn "xbacklight -inc 5")
         , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 5")
         -- volume
         , ("<XF86AudioMute>",         spawn "~/bash_scripts/volumeControl.sh mute")
         , ("<XF86AudioRaiseVolume>",  spawn "~/bash_scripts/volumeControl.sh up")
         , ("<XF86AudioLowerVolume>",  spawn "~/bash_scripts/volumeControl.sh down")
         --, ("<XF86AudioMute>",         spawn "amixer -qD pulse set Master 1+ toggle")
         --, ("<XF86AudioRaiseVolume>",  spawn "amixer -q set Master 2%+")
         --, ("<XF86AudioLowerVolume>",  spawn "amixer -q set Master 2%-")
         , ("M-<XF86AudioMute>",       spawn "amixer set Capture toggle")
         , ("C-S-<XF86AudioMute>",     spawn "~/bash_scripts/muffin-button")
         -- spotify
         , ("<XF86AudioPlay>",         spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")
         , ("<XF86AudioPrev>",         spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")
         , ("<XF86AudioNext>",         spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")
         , ("C-<XF86AudioLowerVolume>",spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")
         , ("C-<XF86AudioRaiseVolume>",spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")
         , ("M-<XF86AudioRaiseVolume>",spawn "~/bash_scripts/spotify-vol-up")
         , ("M-<XF86AudioLowerVolume>",spawn "~/bash_scripts/spotify-vol-down")
         -- screen
         , ("<F8>",                    spawn "~/bash_scripts/screen-resolution")
         , ("C-<Print>",                 spawn "~/bash_scripts/screenshot-drag")
         , ("C-S-<Print>",               spawn "maim -s -u | xclip -selection clipboard -t image/png -i")
         , ("M-C-l",                   spawn "slock")
         -- apps
         , ("M-S-f",                   spawn $ runInMyTerminal "ranger")
         , ("M-f",                     spawn myBrowser)
         , ("M-g",                     spawn "chromium")
         , ("M-p",                     spawn myLauncher)
         --, ("M-d",                     myMenu ["no", "yes"] >> return ())
         --, ("M-d",                     xmonadDocOpts >>= myXMenu)
         --, ("M-S-1",                   namedScratchpadAction scratchpads "plan")
         -- prompt
         , ("M-o",                     switchProjectPrompt myXPConfig)
         , ("M-S-m",                   manPrompt myXPConfig)
         , ("M-S-p",                   passPrompt myXPConfig)
         , ("M-C-r",                   shellPrompt myXPConfig)
         , ("M-x",                     xmonadPrompt myXPConfig)
         ] ++ [("M-s " ++ c, promptSearchBrowser myXPConfig "surf" f) | (c, f) <- mySearchEngines]

------------------------------------------------------------------------}}}
-- Scratchpads                                                          {{{
---------------------------------------------------------------------------

planNS = NS { name  = "plan"
            , cmd   = runInMyTerminal "calcurse"
            , query = title =? "calcurse"
            , hook  = defaultFloating
            }

scratchpads = [ NS { name  = "plan"
                   , cmd   = runInMyTerminal "calcurse"
                   , query = title =? "calcurse"
                   , hook  = defaultFloating
                   }
              ]

------------------------------------------------------------------------}}}
-- Projects                                                             {{{
---------------------------------------------------------------------------

myWorkspaces = fmap show [1..9]
--myWorkspaces = [ "gen"
               --, "plan"
               --] ++ fmap show [3..9]

projects :: [Project]
projects =
  [ Project { projectName      = "xmonad"
            , projectDirectory = "~/.xmonad"
            , projectStartHook = Just $ do
                spawn $ runInMyTerminal "stack ghci xmonad.hs"
                -- spawn $ unwords [ myBrowserMinimal
                --                 , "http://hackage.haskell.org/package/xmonad-contrib-0.13"
                --                 , "http://hackage.haskell.org/package/xmonad"
                --                 ]
                spawn $ runInMyTerminal "nvim xmonad.hs"
            }

  , Project { projectName      = "music"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do 
                spawn "spotify"
                spawn $ runInMyTerminal "pulsemixer"
                --spawn $ runInMyTerminal "cava"
            }

  , Project { projectName      = "9"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do
                spawn $ runInMyTerminal "top"
                spawn $ runInMyTerminal "bluetoothctl"
            }
  ]

------------------------------------------------------------------------}}}
-- Misc                                                                 {{{
---------------------------------------------------------------------------

(+/+) :: FilePath -> FilePath -> FilePath
s1 +/+ s2 = s1 ++ "/" ++ s2

xmonadDocOpts :: X (M.Map String (X ()))
xmonadDocOpts = do
  cacheDir <- asks (cacheDir . directories)
  let docPath = cacheDir +/+ ".stack-work/install/x86_64-linux/lts-11.10/8.2.2/doc"
      hasIndex s = elem "index.html" <$> getDirectoryContents (docPath +/+ s)
      options = getDirectoryContents docPath
        >>= filterM (doesDirectoryExist . (docPath +/+))
        >>= filterM (return . (/='.') . head)
        >>= filterM hasIndex
      optPairs = (fmap . fmap) (\s ->
        (s, spawn $ unwords [ myBrowserMinimal
                            , docPath +/+ s +/+ "index.html"
                            ])) options
   in M.fromList <$> liftIO optPairs

maybeVoid :: (Monad m) => (a -> m ()) -> m (Maybe a) -> m ()
maybeVoid k m = do
  a <- m
  case a of
    Nothing -> return ()
    Just x  -> k x

mySearchEngines :: [(String, SearchEngine)]
mySearchEngines = [ ("g", google)
                  , ("s", scholar)
                  , ("m", maps)
                  , ("w", wikipedia)
                  , ("d", duckduckgo)
                  , ("v", vocabulary)
                  , ("i", imdb)
                  , ("a", searchEngine "archwiki" "https://wiki.archlinux.org/index.php?search=")
                  , ("e", searchEngine "ebay" "https://www.ebay.com/sch/i.html?_nkw=")
                  ]

------------------------------------------------------------------------}}}
-- Applications                                                         {{{
---------------------------------------------------------------------------

--myStatusBar = "xmobar"
myStatusBar = "xmobar -x0 /home/kelvin/.xmobarrc"

myTerminals = [ "kitty"
              , "alacritty"
              , "st"
              , "urxvt"
              , "gnome-terminal --hide-menubar"
              ]

myTerminal = head myTerminals
runInMyTerminal s = unwords [ myTerminal, "-e" , s]

myBrowser  = "firefox"
myBrowserMinimal = "surf"
myLauncher = "dmenu_run"

myMenu :: [String] -> X String
myMenu = menuArgs "rofi" [ "-matching", "fuzzy"
                         , "-dmenu"
                         ] -- TODO: add -mesg

myMenuMap ::  M.Map String a -> X (Maybe a)
myMenuMap = menuMapArgs "rofi" [ "-matching", "fuzzy"
                               , "-dmenu"
                               ] -- TODO: add -mesg

myXMenu ::  M.Map String (X ()) -> X ()
myXMenu = maybeVoid id . myMenuMap 

------------------------------------------------------------------------}}}
-- Configs(/Theme?)                                                     {{{
---------------------------------------------------------------------------

myXPConfig = def
  { font        = "xft:Source Code Pro:size=9"
  , bgColor     = background
  , fgColor     = foreground
  , fgHLight    = background
  , bgHLight    = color1
  , borderColor = color1
  , searchPredicate      = fuzzyMatch
  , sorter      = fuzzySort
  }

myConfig = def
  { terminal           = myTerminal
  , normalBorderColor  = background
  , focusedBorderColor = color1
  , borderWidth        = 3
  , layoutHook         = myLayoutHook
  , manageHook         = myManageHook
  , handleEventHook    = fullscreenEventHook <+> handleTimerEvent
  , workspaces         = myWorkspaces
  } `additionalKeysP` myKeys

-- vim: ft=haskell:foldmethod=marker:expandtab:ts=2:shiftwidth=2
