{-# LANGUAGE TemplateHaskell #-}

module GTF.Pages.Partials.Footer (footer) where

import CommonPrelude
import Lucid.Html5
import Lucid.Base (Html)
import GTF.Pages.Helpers (datetime, now, internal)

footer :: Html ()
footer = footer_ [class_ "site-footer"] $ do
  span_ [class_ "footer-item"] $
    "Site updated: " <> datetime $now
  span_ [class_ "footer-item"] $
    a_ [href_ "/gideon-farrell.pubkey.asc", title_ "Gideon's Public Key"] "Public PGP Key"
  span_ [class_ "footer-item"] $
    internal "colophon" "Colophon"
