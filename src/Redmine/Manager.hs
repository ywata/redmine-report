{-# LANGUAGE OverloadedStrings #-}

module Redmine.Manager where

import Data.ByteString

-- Ajt Gestionaire de connection
data RedmineMng

   -- |Constructor to create an anonymous connection.
   = RedmineMng {rmhost::ByteString}

   -- |Constructor to create a connection throught a proxy
   | RedmineMngWithProxy { rmhost::ByteString
                         , rmurlProxy::ByteString
                         , rmportProxy::Int }

   -- |Constructor to create a connection with authentification
   | RedmineMngWithAuth { rmhost::ByteString
                        , login::ByteString
                        , passwd::ByteString }

   -- |Constructor to create a connection throught a proxy with authentification
   | RedmineMngWithAuthAndProxy { rmhost::ByteString
                                , login::ByteString
                                , passwd::ByteString
                                , rmurlProxy::ByteString
                                , rmportProxy::Int }
