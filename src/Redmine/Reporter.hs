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

reportMark    = Marker "--" ""   2 :: Marker -- This mark specifies the journal to be appeared in the report
updatedMarker = Marker "* " "  " 2 :: Marker -- This marker is added for updated information during a specified period.
--l1Marker      = Marker "-"  " "  1 :: Marker -- This marker is a mark for the first level indent.
--l2Marker      = Marker "+"  " "  1 :: Marker -- This marker is a mark for the second level indent.

l2Mark = "+" : repeat " "

indent = 2

text = PP.text . T.unpack
newtype Group a = Group{group :: a} deriving(Show, Eq)

-- | Top level report generator.
toDoc :: ToDoc a => (Maybe UTCTime) -> (Maybe UTCTime) -> a -> PP.Doc
toDoc from to = toDocWith (const (\_ -> PP.empty)) isBetween
  where
    isBetween utc = case (from, to) of
      (Just f, Just t) -> f <= utc && utc <= t
      (Just f, _)      -> f <= utc
      (_,      Just t) -> utc <= t
      (_, _)           -> True      


-- | Type class for generating report.
class ToDoc a where
--  toDocWith :: (Bool -> PP.Doc -> PP.Doc) -> (Maybe UTCTime) -> (Maybe UTCTime) ->  a -> PP.Doc
  toDocWith :: (Bool -> PP.Doc -> PP.Doc) -> (UTCTime -> Bool) ->  a -> PP.Doc

instance ToDoc (Group [Issue]) where
  toDocWith _ p (Group is@(i:_)) = (text . name_ObjRef . tracker_Issue $ i) PP.$$
                                    (toDocWith f p is)
    where
      f = \b -> (PP.<>) (text (if b then mark updatedMarker else space updatedMarker))


instance ToDoc Issue where
  toDocWith f p i =
    (f b) ((text "-" PP.<>) . text . subject_Issue $ i) PP.<+> PP.parens (text . name_ObjRef . status_Issue $ i) PP.$$
    (toDocWith g p (fmap (filter needReport) $ journals_Issue i))
    where
      b = isUpdated p i
      g = \b -> (PP.<>) (text (if b then mark updatedMarker else space updatedMarker) PP.<> text "  ")

instance ToDoc Journal where
  toDocWith f p j = PP.vcat . map (\(d, m) -> (f b) (text m PP.<> text d)) $ zip ls l2Mark
    where
      b = isUpdated p j
      ls = T.lines . T.drop (T.length (mark reportMark)) . T.replace "\r" "" . notes_Journal $ j

instance ToDoc a => ToDoc (Maybe a) where
  toDocWith _ _ Nothing      = PP.empty
  toDocWith f p (Just a) = toDocWith f p a
instance ToDoc a =>ToDoc [a] where
  toDocWith f p = PP.vcat . map (toDocWith f p) 

class Updated a where
  --                  from            to
  isUpdated :: (UTCTime -> Bool)  -> a -> Bool
instance Updated Journal where
  isUpdated _  Journal{notes_Journal = ""}           = False
  isUpdated p  Journal{createdOn_Journal = Just utc} = p utc
  isUpdated _  _                                     = False


instance Updated Issue where
  isUpdated p Issue{journals_Issue = Just js} = or . map (isUpdated p ) $ js
  isUpdated p Issue{journals_Issue = _} =  False

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
