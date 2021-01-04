{-# Language RecordWildCards #-}
{-# Language ParallelListComp #-}

module Kabigon.Parser where

import Kabigon.Prelude

import Control.Monad (replicateM)

import Data.Char (ord, chr)
import Data.Bits
  ( (.|.), shift
  )
import Data.Binary.Get
  ( Get
  , runGetOrFail
  , skip
  , getByteString
  , getWord8, getWord16be, getWord32be, getWord64be
  )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS.Lazy
import qualified Data.Map.Strict as Map

import Kabigon.Savefile

parseSavefile ::
  (MonadIO m, MonadThrow m) =>
  FilePath ->
  m Savefile
parseSavefile p =
  fmap third
  . throwLeft (KabigonException . pack . third)
  . runGetOrFail getSavefile
  =<< liftIO (BS.Lazy.readFile p)
  where
    third (_, _, x) = x

decodeString :: ByteString -> Text
decodeString = pack . mapMaybe decodeWord . BS.unpack 
  where
    decodeWord :: Word8 -> Maybe Char
    decodeWord w
      | w >= 0x80 && w <= 0x99 = Just . chr $ ord 'A' + fromIntegral w - 0x80
      | w >= 0xa0 && w <= 0xb9 = Just . chr $ ord 'a' + fromIntegral w - 0xa0
      | w >= 0xf6 && w <= 0xff = Just . chr $ ord '0' + fromIntegral w - 0xf6
      | otherwise = case w of
          0x9a -> Just '('
          0x9b -> Just ')'
          0x9c -> Just ':'
          0x9d -> Just ';'
          0x9e -> Just '['
          0x9f -> Just ']'
          0xe0 -> Just '\''
          0xe3 -> Just '-'
          0xe6 -> Just '?'
          0xe7 -> Just '!'
          0xe8 -> Just '.'
          0xe9 -> Just '&'
          _ -> Nothing

getWord24be :: Get Word32
getWord24be = do
  b1 <- fromIntegral <$> getWord8
  b2 <- fromIntegral <$> getWord8
  b3 <- fromIntegral <$> getWord8
  pure $ shift 16 b1 .|. shift 8 b2 .|. b3

getList :: Int -> Get (Maybe a) -> Get [a]
getList n f = do
  count <- getWord8
  ret <- catMaybes . take (fromIntegral count) <$> replicateM n f
  skip 1
  pure ret

getItem :: Get Item
getItem = Item <$> getWord8 <*> getWord8

getPokemon :: Get (Text -> Text -> Pokemon)
getPokemon = do
  _pokemonIndex <- getWord8
  _pokemonHeldItemIndex <- getWord8
  _pokemonMove1Index <- getWord8
  _pokemonMove2Index <- getWord8
  _pokemonMove3Index <- getWord8
  _pokemonMove4Index <- getWord8
  _pokemonOriginalTrainerID <- getWord16be
  _pokemonExperience <- getWord24be
  _pokemonHPEV <- getWord16be
  _pokemonAttackEV <- getWord16be
  _pokemonDefenseEV <- getWord16be
  _pokemonSpeedEV <- getWord16be
  _pokemonSpecialEV <- getWord16be
  _pokemonIV <- getWord16be
  _pokemonMove1PP <- getWord8
  _pokemonMove2PP <- getWord8
  _pokemonMove3PP <- getWord8
  _pokemonMove4PP <- getWord8
  _pokemonFriendship <- getWord8
  _pokemonPokerus <- getWord8
  _pokemonCaughtData <- getWord16be
  _pokemonLevel <- getWord8
  pure $ \_pokemonOriginalTrainerName _pokemonName -> Pokemon{..}

getActivePokemon :: Get (Text -> Text -> ActivePokemon)
getActivePokemon = do
  pkmn <- getPokemon
  _activePokemonStatus <- getWord8
  skip 1
  _activePokemonCurrentHP <- getWord16be
  _activePokemonMaximumHP <- getWord16be
  _activePokemonAttack <- getWord16be
  _activePokemonDefense <- getWord16be
  _activePokemonSpeed <- getWord16be
  _activePokemonSpecialAttack <- getWord16be
  _activePokemonSpecialDefense <- getWord16be
  pure $ \ot nm -> let _activePokemon = pkmn ot nm in ActivePokemon{..}

getPokemonListHelper :: Int -> Get (Text -> Text -> a) -> Get [a]
getPokemonListHelper n g = do
  count <- fromIntegral <$> getWord8
  skip $ n + 1
  pkmns <- replicateM n g
  ots <- replicateM n $ decodeString <$> getByteString 11
  nms <- replicateM n $ decodeString <$> getByteString 11
  pure $ take count
    [ pkmn ot nm
    | pkmn <- pkmns
    | ot <- ots
    | nm <- nms
    ]

getPokemonList :: Int -> Get [Pokemon]
getPokemonList n = getPokemonListHelper n getPokemon

getActivePokemonList :: Int -> Get [ActivePokemon]
getActivePokemonList n = getPokemonListHelper n getActivePokemon

getSavefile :: Get Savefile
getSavefile = do
  skip 0x2000

  -- 0x2000
  _saveOptions <- getWord64be
  skip 1

  -- 0x2009
  _saveTrainerID <- getWord16be

  -- 0x200b
  _savePlayerName <- decodeString <$> getByteString 11

  -- 0x2013
  skip 11 -- unused - mom's name

  -- 0x2021
  _saveRivalName <- decodeString <$> getByteString 11

  -- 0x202c
  skip 11 -- unused - red's name

  -- 0x2037
  _saveDST <- getWord8
  skip 28
  
  -- 0x2054
  _savePlaytime <- getWord32be
  skip 18
  
  -- 0x206a
  _savePlayerPalette <- getWord8
  skip 881
  
  -- 0x23dc
  _saveMoney <- getWord24be
  skip 6
  
  -- 0x23e5
  _saveJohtoBadges <- getWord8
  skip 1

  -- 0x23e7
  skip 57 -- TMs TODO
  let _saveTMs = Map.empty

  -- 0x2420
  _saveItems <- getList 20 $ fmap Just getItem

  -- 0x244a
  _saveKeyItems <- getList 12 $ fmap Just getItem
  skip 1

  -- 0x2465
  _saveBalls <- getList 12 $ fmap Just getItem

  -- 0x247f
  _savePCItems <- getList 50 $ fmap Just getItem
  skip 539

  -- 0x2700
  _saveBoxNumber <- getWord8
  skip 2

  -- 0x2703
  skip 126 -- PC box names TODO
  let _saveBoxNames = []

  -- _saveWeeklyNumber <- getWord16be
  let _saveWeeklyNumber = 0
  skip 228

  -- 0x2865
  _saveTeamPokemon <- getActivePokemonList 6
  skip 22

  -- 0x2a27
  skip 32 -- pokedex owned TODO
  let _savePokedexOwned = Map.empty

  -- 0x2a47
  skip 32 -- pokedex seen TODO
  let _savePokedexSeen = Map.empty
  skip 681

  -- 0x2d10
  _saveCurrentBoxPokemon <- getPokemonList 20
  skip 3295

  -- 0x3e3d
  _savePlayerGender <- getWord8
  skip 450

  -- 0x4000
  box1 <- getPokemonList 20
  skip 2

  -- 0x4450
  box2 <- getPokemonList 20
  skip 2

  -- 0x48a0
  box3 <- getPokemonList 20
  skip 2

  -- 0x4cf0
  box4 <- getPokemonList 20
  skip 2

  -- 0x5140
  box5 <- getPokemonList 20
  skip 2

  -- 0x5590
  box6 <- getPokemonList 20
  skip 2

  -- 0x59e0
  box7 <- getPokemonList 20
  skip 466

  -- 0x6000
  box8 <- getPokemonList 20
  skip 2

  -- 0x6450
  box9 <- getPokemonList 20
  skip 2

  -- 0x68a0
  box10 <- getPokemonList 20
  skip 2

  -- 0x6cf0
  box11 <- getPokemonList 20
  skip 2

  -- 0x7140
  box12 <- getPokemonList 20
  skip 2

  -- 0x7590
  box13 <- getPokemonList 20
  skip 2

  -- 0x79e0
  box14 <- getPokemonList 20
  skip 2

  let _saveBoxPokemon = Map.fromList $ zip [0..]
        [ box1, box2, box3, box4, box5, box6, box7
        , box8, box9, box10, box11, box12, box13, box14
        ]

  let _saveChecksum1 = 0
  let _saveChecksum2 = 0

  pure Savefile{..}
