{-# LANGUAGE DeriveDataTypeable #-}

module Map
    ( Site(..)
    , River(..)
    , Map(..)
    , SiteId
    )
where

import Text.JSON.Generic


type SiteId = Int

data Site
    = Site
      { id :: SiteId
      } deriving (Show, Data, Typeable)

data River
    = River
      { source :: SiteId
      , target :: SiteId
      } deriving (Show, Data, Typeable)

instance Eq River where
    (River s1 t1) == (River s2 t2)
        = (s1 == s2 && t1 == t2) || (s1 == t2 && t1 == s2)

data Map
    = Map
      { sites  :: [Site]
      , rivers :: [River]
      , mines  :: [SiteId]
      } deriving (Show, Data, Typeable)

