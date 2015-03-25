{-# LANGUAGE OverloadedStrings, FlexibleInstances  #-}
module Redmine.Reporter where
import Redmine.Types
import qualified Text.PrettyPrint as PP
import Data.Maybe (isJust, fromJust)
import qualified Data.Text as T (Text, drop, length, lines, pack, replace, take, unpack)
import Data.Time.Clock

import Redmine.Reporter.Config (IdMap, OrderMap)
import qualified Data.Map as Map (fromList, lookup)


import Debug.Trace (trace)
data Marker = Marker{mark::T.Text, space:: T.Text, width::Int}
              deriving(Show, Eq)

reportMark    = Marker "--" ""   2 :: Marker -- This mark appears in a journal(=  history) data.
updatedMarker = Marker "* " "  " 2 :: Marker
l1Marker      = Marker "-"  " "  1 :: Marker
l2Marker      = Marker "+"  " "  1 :: Marker

l2Mark = "+" : repeat " "

indent = 2

text = PP.text . T.unpack
newtype Group a = Group{group :: a} deriving(Show, Eq)
class ToDoc a where
  toDocWith :: (Bool -> PP.Doc -> PP.Doc) -> (Maybe UTCTime) -> (Maybe UTCTime) ->  a -> PP.Doc

toDoc from to a = toDocWith (const (\_ -> PP.empty)) from to a
instance ToDoc (Group [Issue]) where
  toDocWith _  from to  (Group is@(i:_)) = (text . name_ObjRef . tracker_Issue $ i) PP.$$
                                    (toDocWith f from to is)
    where
      f = \b -> (PP.<>) (text (if b then mark updatedMarker else space updatedMarker))

instance ToDoc Issue where
  toDocWith f from to i =
    (f b) ((text "-" PP.<>) . text . subject_Issue $ i) PP.<+> PP.parens (text . name_ObjRef . status_Issue $ i) PP.$$
    (toDocWith g from to (fmap (filter needReport) $ journals_Issue i))
    where
      b = isUpdatedBetween from to i
      g = \b -> (PP.<>) (text (if b then mark updatedMarker else space updatedMarker) PP.<> text "  ")

instance ToDoc Journal where

  toDocWith f from to j = PP.vcat . map (\(d, m) -> (f b) (text m PP.<> text d)) $ zip ls l2Mark
    where
      b = isUpdatedBetween from to j
      ls = T.lines . T.drop (T.length (mark reportMark)) . T.replace "\r" "" . notes_Journal $ j

instance ToDoc a => ToDoc (Maybe a) where
  toDocWith _ _ _ Nothing      = PP.empty
  toDocWith f from to (Just a) = toDocWith f from to a
instance ToDoc a =>ToDoc [a] where
  toDocWith f from to = PP.vcat . map (toDocWith f from to) 
class Updated a where
  --                  from            to
  isUpdatedBetween :: Maybe UTCTime -> Maybe UTCTime -> a -> Bool
instance Updated Journal where
  isUpdatedBetween _          _          Journal{notes_Journal = ""}           = False
  isUpdatedBetween (Just from) (Just to) Journal{createdOn_Journal = Just utc} = from <= utc && utc <= to
  isUpdatedBetween (Just from) _         Journal{createdOn_Journal = Just utc} = from <= utc
  isUpdatedBetween _           (Just to) Journal{createdOn_Journal = Just utc} = utc <= to
  isUpdatedBetween _           _         Journal{createdOn_Journal = Just utc} = True
  isUpdatedBetween _           _         _                                     = False


instance Updated Issue where
  isUpdatedBetween f t Issue{journals_Issue = Just js} = or . map (isUpdatedBetween f t) $ js
  isUpdatedBetween f t Issue{journals_Issue = _} =  False

needReport:: Journal -> Bool
needReport (Journal{notes_Journal = n}) = T.take (T.length . mark $ reportMark) n == mark reportMark
nestWithMark:: Int -> T.Text -> PP.Doc -> PP.Doc
nestWithMark n t = undefined

modifyIssue:: (Integral a) => IdMap a String -> Issue -> Issue
modifyIssue m i@Issue{tracker_Issue = ti}  = i{tracker_Issue = ti'}
  where
    ti' = maybe ti (\x -> ObjRef (fromIntegral . id_ObjRef $ ti) (T.pack x)) (Map.lookup (fromIntegral . id_ObjRef $ ti) m)

sortIssue::[Issue] -> [Issue]
sortIssue = undefined
