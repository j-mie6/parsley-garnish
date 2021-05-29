module Parsley.LiftPlugin.Plugin (plugin) where

import Plugins    (Plugin (..), defaultPlugin, purePlugin)

import Parsley.LiftPlugin.LiftReplace (replaceLiftDicts)
import Parsley.LiftPlugin.LiftFind (findLiftPlugin)

-- Plugin definitions
plugin :: Plugin
plugin = defaultPlugin {
    tcPlugin = const (Just findLiftPlugin),
    typeCheckResultAction = replaceLiftDicts,
    pluginRecompile = purePlugin
  }