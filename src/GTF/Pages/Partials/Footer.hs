{-# LANGUAGE TemplateHaskell #-}

module GTF.Pages.Partials.Footer (footer) where

import CommonPrelude
import GTF.Pages.Helpers (datetime, internal, now)
import Lucid.Base (Html)
import Lucid.Html5

footer :: Html ()
footer = footer_ [class_ "site-footer"] $ do
  span_ [class_ "footer-item"]
    $ "Site updated: "
    <> datetime $now
  span_ [class_ "footer-item"]
    $ a_ [href_ "/assets/gideon-farrell.pubkey.asc", title_ "Gideon's Public Key"] "Public PGP Key"
  span_ [class_ "footer-item"]
    $ internal "colophon" "Colophon"
