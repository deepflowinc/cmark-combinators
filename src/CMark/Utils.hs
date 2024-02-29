{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE FieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module CMark.Utils (
  para,
  text,
  code,
  document,
  codeBlock,
  render,
  emph,
  link,
  Render (..),
  Inline,
  Block,
  Doc,
  renders,
  heading,
  ul,
  ol,
  htmlBlock,
  details,

  -- * Re-exports
  module CMark,
) where

import CMark
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import NeatInterpolation (trimming)

instance IsString Inline where
  fromString = text . fromString

instance IsString Block where
  fromString = para . pure . fromString

instance IsString Doc where
  fromString = document . pure . fromString

newtype Inline = Inline {getInline :: Node}
  deriving (Show, Eq, Ord, Generic)

newtype Block = Block {getBlock :: Node}
  deriving (Show, Eq, Ord, Generic)

newtype Doc = Doc {getDoc :: Node}
  deriving (Show, Eq, Ord, Generic)

link :: Url -> [Inline] -> Inline
link url inls = Inline $ Node Nothing (LINK url "") $ coerce inls

code :: Text -> Inline
code = mkInline . CODE

para :: [Inline] -> Block
para = Block . Node Nothing PARAGRAPH . coerce

text :: Text -> Inline
text = Inline . flip (Node Nothing) [] . TEXT

mkInline :: NodeType -> Inline
mkInline = Inline . flip (Node Nothing) []

document :: [Block] -> Doc
document = Doc . Node Nothing DOCUMENT . coerce

codeBlock :: Maybe Text -> Text -> Block
codeBlock mlang = mkBlock . CODE_BLOCK (fromMaybe "" mlang)

htmlBlock :: Text -> Block
{-# INLINE htmlBlock #-}
htmlBlock src = Block $ Node Nothing (HTML_BLOCK src) []

details :: Text -> Text -> [Block]
details title body =
  [ htmlBlock $
      [trimming|
        <details>
        <summary>${title}</summary>
      |]
        <> "\n\n"
  , codeBlock Nothing body
  , htmlBlock "\n\n</details>\n"
  ]

mkBlock :: NodeType -> Block
mkBlock = Block . flip (Node Nothing) []

class Render a where
  toDoc :: a -> Doc

instance Render Doc where
  toDoc = id

instance Render Block where
  toDoc = document . pure

instance Render Inline where
  toDoc = document . pure . para . pure

instance Render [Block] where
  toDoc = document

instance Render [Inline] where
  toDoc = document . pure . para

render :: (Render a) => a -> Text
{-# INLINE render #-}
render = nodeToCommonmark [] Nothing . getDoc . toDoc

instance Render Text where
  toDoc = toDoc . text

renders :: [Block] -> Text
renders = render . document

heading :: Int -> [Inline] -> Block
heading i = Block . Node Nothing (HEADING i) . coerce

emph :: [Inline] -> Inline
emph = Inline . Node Nothing EMPH . coerce

ul :: [Block] -> Block
ul =
  Block
    . Node
      Nothing
      ( LIST
          ListAttributes
            { listType = BULLET_LIST
            , listTight = True
            , listStart = 1
            , listDelim = PERIOD_DELIM
            }
      )
    . map (Node Nothing ITEM . pure . coerce)

ol :: [Block] -> Block
ol =
  Block
    . Node
      Nothing
      ( LIST
          ListAttributes
            { listType = ORDERED_LIST
            , listTight = True
            , listStart = 1
            , listDelim = PERIOD_DELIM
            }
      )
    . map (Node Nothing ITEM . pure . coerce)
