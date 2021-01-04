{-# Language TemplateHaskell #-}

module Kabigon.Savefile where

import Kabigon.Prelude

import Data.Map.Strict (Map)

data Item = Item
  { _itemCount :: Word8
  , _itemIndex :: Word8
  } deriving (Show, Eq, Ord, Generic)
makeLenses ''Item

data Pokemon = Pokemon
  { _pokemonOriginalTrainerName :: Text
  , _pokemonName :: Text
  , _pokemonIndex :: Word8
  , _pokemonHeldItemIndex :: Word8
  , _pokemonMove1Index :: Word8
  , _pokemonMove2Index :: Word8
  , _pokemonMove3Index :: Word8
  , _pokemonMove4Index :: Word8
  , _pokemonOriginalTrainerID :: Word16
  , _pokemonExperience :: Word32
  , _pokemonHPEV :: Word16
  , _pokemonAttackEV :: Word16
  , _pokemonDefenseEV :: Word16
  , _pokemonSpeedEV :: Word16
  , _pokemonSpecialEV :: Word16
  , _pokemonIV :: Word16
  , _pokemonMove1PP :: Word8
  , _pokemonMove2PP :: Word8
  , _pokemonMove3PP :: Word8
  , _pokemonMove4PP :: Word8
  , _pokemonFriendship :: Word8
  , _pokemonPokerus :: Word8
  , _pokemonCaughtData :: Word16
  , _pokemonLevel :: Word8
  } deriving (Show, Eq, Ord, Generic)
makeLenses ''Pokemon

data ActivePokemon = ActivePokemon
  { _activePokemon :: Pokemon
  , _activePokemonStatus :: Word8
  , _activePokemonCurrentHP :: Word16
  , _activePokemonMaximumHP :: Word16
  , _activePokemonAttack :: Word16
  , _activePokemonDefense :: Word16
  , _activePokemonSpeed :: Word16
  , _activePokemonSpecialAttack :: Word16
  , _activePokemonSpecialDefense :: Word16
  } deriving (Show, Eq, Ord, Generic)
makeLenses ''ActivePokemon

data Savefile = Savefile
  { _saveOptions :: Word64
  , _saveTrainerID :: Word16
  , _savePlayerName :: Text
  , _saveRivalName :: Text
  , _saveDST :: Word8
  , _savePlaytime :: Word32
  , _savePlayerPalette :: Word8
  , _saveMoney :: Word32
  , _saveJohtoBadges :: Word8
  , _saveTMs :: Map Word8 Word8
  , _saveItems :: [Item]
  , _saveKeyItems :: [Item]
  , _saveBalls :: [Item]
  , _savePCItems :: [Item]
  , _saveBoxNumber :: Word8
  , _saveBoxNames :: [Text]
  , _saveWeeklyNumber :: Word16
  , _saveTeamPokemon :: [ActivePokemon]
  , _savePokedexOwned :: Map Word8 Bool
  , _savePokedexSeen :: Map Word8 Bool
  , _saveCurrentBoxPokemon :: [Pokemon]
  , _savePlayerGender :: Word8
  , _saveBoxPokemon :: Map Word8 [Pokemon]
  , _saveChecksum1 :: Word16
  , _saveChecksum2 :: Word16
  } deriving (Show, Eq, Ord, Generic)
makeLenses ''Savefile
