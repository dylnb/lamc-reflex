{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Repl hiding (main)
import Syntax
import Infer
import Parser
import Pretty
import Eval
import qualified Env
import DbExp hiding (Lam)

import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import Control.Monad.Identity
import Control.Monad.State.Strict
import Control.Comonad (extend)

import Data.List (isPrefixOf, foldl')

import System.Exit
import System.Environment
import System.Console.Repline

import Reflex.Dom
import Data.FileEmbed


{--
main :: IO ()
main = return ()
--}

{--}
main :: IO ()
main = mainWidgetWithCss $(embedFile "static/lamc.css") $
  divClass "page-wrap" $
    divClass "column-main" $ do
      activeFilter <- controls 
      let nodeCompute filter ex =
            case filter of
              Types -> 
                  let Right (_, _, _, tt_subs, _, _) = constraintsExpr Env.empty ex
                  in ppexpr' tt_subs
              Terms -> 
                  ppexpr' $ extend (nf . cf2db) ex
                  where
                    cf2db cf =
                      case unwrap cf of
                        Lit (LInt n) -> L (LI n)
                        Lit (LBool b) -> L (LB b)
                        Var x -> V x
                        App m n -> cf2db m :@ cf2db n
                        Lam x body -> x ! cf2db body
      newTerm <- termEntry
      dynTree <- holdDyn ident newTerm
      -- el "pre" $ display =<< mapDyn (extend (pretty . nf . cf2db)) dynTree
      el "pre" $ do
        labelFunc <- mapDyn nodeCompute activeFilter
        dynText =<< combineDyn id labelFunc dynTree
      return ()

data Filter = Types | Terms deriving (Show,Eq)

controls :: MonadWidget t m => m (Dynamic t Filter)
controls = divClass "controls" $ do
  elAttr "ul" ("class" =: "filters") $ do
    rec activeFilter <- holdDyn Terms setFilter
        typesButton <- el "li" $ do
          buttonAttrs <- mapDyn (\af -> "class" =: (if Types == af then "selected" else "")) activeFilter
          (e, _) <- elDynAttr' "a" buttonAttrs $ text $ show Types
          return $ fmap (const Types) (domEvent Click e)
        text " "
        termsButton <- el "li" $ do
          buttonAttrs <- mapDyn (\af -> "class" =: (if Terms == af then "selected" else "")) activeFilter
          (e, _) <- elDynAttr' "a" buttonAttrs $ text $ show Terms
          return $ fmap (const Terms) (domEvent Click e)
        let setFilter = leftmost [typesButton, termsButton]
    return activeFilter

termEntry :: MonadWidget t m => m (Event t (CfExpr ()))
termEntry = divClass "sh" $ do
  let termBoxAttrs = ["placeholder" =: "Enter a term", "name" =: "newTerm", "id" =: "query"]
  rec let newValueEntered = ffilter (==keycodeEnter) (_textInput_keypress termBox)
      termBox <- divClass "query" $
        textInput $ def & setValue .~ fmap (const "") newValueEntered
                        & attributes .~ constDyn (mconcat termBoxAttrs)
  let newValue = tag (current $ _textInput_value termBox) newValueEntered
      newTerm = fmapMaybe checkTerm newValue
  elAttr "pre" ("class" =: "curterm") $
    dynText =<< holdDyn "Showing: \\x. x" (fmap (("Showing: " ++) . ppexpr) newTerm)
  el "br" (return ())
  return newTerm
  where checkTerm t = lookup t demoTerms `mplus` either (const Nothing) Just (parseExpr (L.pack t))

comp = lam "f" (lam "g" (lam "x" (app (var "f") (app (var "g") (var "x")))))
k3 = lam "k" (lit $ LInt 3)
ident = lam "x" (var "x")
ex2 = app (app (app Main.comp k3) ident) (lit $ LInt 7)
ex3 = app (lam "f" (app (var "f") (var "f"))) (lam "x" (var "x"))
ex4 = app (var "x") (var "x")

demoTerms :: [(String, CfExpr ())]
demoTerms =
  [ ("comp", Main.comp)
  , ("k3", k3)
  , ("ident", ident)
  , ("ex2", ex2)
  , ("ex3", ex3)
  , ("ex4", ex4)
  ]

--}
