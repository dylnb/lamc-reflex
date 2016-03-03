{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}
{-# LANGUAGE DefaultSignatures #-}

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

import Data.JSString (JSString, pack)
import qualified JavaScript.JSON.Types.Internal as JS
import JavaScript.JSON.Types.Class

foreign import javascript unsafe "(new Date())['getTime']()"
  getTime :: IO Double

foreign import javascript unsafe
  "document.getElementById('tree').innerHTML=''; new Treant($1);"
  treant :: JS.Value -> IO ()

ov = JS.objectValue . JS.object
sv = JS.stringValue . pack
av = JS.arrayValue . JS.arrayValueList
dv = JS.doubleValue

testTree :: JS.Value
testTree =
  ov [ ("chart", ov [("container", sv "#tree")])
     , ("nodeStructure"
       , ov [ ("text", ov [("name", sv "Parent node")])
            , ("children"
              , av [ ov [("text", ov [("name", sv "First child")])]
                   , ov [("text", ov [("name", sv "Second child")])]
                   ]
              ) 
            ]
       )
     ]

dispTree :: Pretty a => CfExpr a -> JS.Value
dispTree cf =
  ov [ ("chart"
       , ov [ ("container", sv "#tree")
            , ("levelSeparation", dv 20)
            , ("siblingSeparation", dv 15)
            , ("subTreeSeparation", dv 15)
            , ("node", ov [("HTMLclass", sv "tree-draw")])
            , ("connectors"
              , ov [ ("type", sv "straight")
                   , ("style", ov [("stroke-width", dv 2), ("stroke", sv "#ccc")])
                   ]
              )
            ]
       )
     , ("nodeStructure", go cf)
     ]
  where go expr = case unwrap expr of
          App fun arg -> ov [ ("text", ov [("name", sv . pp $ extract expr)])
                            , ("children", av [go fun, go arg])
                            ]
          Lam v body  -> ov [ ("text", ov [("name", sv . pp $ extract expr)])
                            , ("children"
                              , av [ ov [("text", ov [("name", sv $ "\\" ++ v)])]
                                   , go body
                                   ]
                              )
                            ]
          Lit w       -> ov [("text"
                             , ov [("name"
                                   , sv $ pp w ++ ": " ++ pp (extract expr)
                                   )]
                             )]
          Var v       -> ov [("text"
                             , ov [("name"
                                   , sv $ v ++ ": " ++ pp (extract expr)
                                   )]
                             )]

{--
main :: IO ()
main = return ()
--}

stylesheet :: MonadWidget t m => String -> m ()
stylesheet s = elAttr "link" (Map.fromList [("rel", "stylesheet"), ("href", s)]) $ return ()

scriptSrc :: MonadWidget t m => String -> m ()
scriptSrc s = elAttr "script" (Map.fromList [("type", "javascript"), ("src", s)]) $ return ()

headSection = do 
  stylesheet "../static/lamc.css"
  stylesheet "../static/treant.css"

{--}
main :: IO ()
main = mainWidgetWithHead headSection $ do
  divClass "page-wrap" $ do
    divClass "column-main" $ do
      activeFilter <- controls 
      elAttr "div" ("id" =: "tree" <> "class" =: "chart") (return ())
      let nodeCompute filter ex =
            case filter of
              Types -> 
                  let Right (_, _, _, tt_subs, _, _) = constraintsExpr Env.empty ex
                  in dispTree tt_subs
              Terms -> 
                  dispTree $ extend (nf . cf2db) ex
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
      labelFunc <- mapDyn nodeCompute activeFilter
      t <- combineDyn id labelFunc dynTree
      performEvent_ $ fmap (liftIO . treant) (updated t)
      -- dynText t
      -- el "br" (return ())
      -- el "div" $ do
      --   e <- button "Time?" 
      --   t <- performEvent $ fmap (const $ liftIO getTime) e
      --   display =<< holdDyn 0 t
      -- el "br" (return ())
      -- elAttr "div" ("id" =: "tree" <> "class" =: "chart") (return ())
      -- button "Tree?" >>= performEvent_ . fmap (const $ liftIO $ treant testTree)

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
