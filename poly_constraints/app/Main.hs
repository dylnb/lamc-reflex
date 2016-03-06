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
import Control.Monad.Writer
import Control.Comonad (extend)
import Control.Concurrent (threadDelay, forkIO)

import Data.List (isPrefixOf, foldl')

import System.Exit
import System.Environment
import System.Console.Repline

import GHCJS.DOM.Document (getElementById)
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.Element (click)
import Reflex.Host.Class
import Reflex.Dom
import Reflex.Dom.Class
import Data.FileEmbed

import Data.JSString (JSString, pack)
import qualified JavaScript.JSON.Types.Internal as JS
import JavaScript.JSON.Types.Class

foreign import javascript unsafe
  "var t = document.getElementById('tree');\
   t.innerHTML = '';\
   new Treant($1);\
   t.style.height = t.scrollHeight + 50 + 'px';"
  treant :: JS.Value -> IO ()

ov = JS.objectValue . JS.object
sv = JS.stringValue . pack
av = JS.arrayValue . JS.arrayValueList
dv = JS.doubleValue

dispTree :: Pretty a => CfExpr a -> JS.Value
dispTree cf = ov [("chart", chartConfig), ("nodeStructure", fst $ buildTreeObj cf 0)]
  where connectConfig =
          ov [ ("type", sv "straight")
             , ("style", ov [("stroke-width", dv 2), ("stroke", sv "#ccc")])
             ]
        chartConfig =
          ov [ ("container", sv "#tree")
             , ("levelSeparation", dv 20)
             , ("siblingSeparation", dv 15)
             , ("subTreeSeparation", dv 15)
             , ("scrollBar", sv "None")
             , ("node", ov [("HTMLclass", sv "tree-draw")])
             , ("connectors", connectConfig)
             ]

buildTreeObj :: Pretty a => CfExpr a -> Int -> (JS.Value, Int)
buildTreeObj expr n = case unwrap expr of
  App fun arg ->
    let (funObj, f) = buildTreeObj fun n
        (argObj, a) = buildTreeObj arg (f+1)
        thisObj =
          ov [ ("text", ov [("name", sv . pp $ extract expr)])
             , ("HTMLid", sv $ "node-" ++ show f)
             , ("HTMLclass", sv "branch")
             , ("children", av [funObj, argObj])
             ]
    in (thisObj, a)
  Lam v body  ->
    let (bodyObj, b) = buildTreeObj body (n+2)
        thisObj =
          ov [ ("text", ov [("name", sv . pp $ extract expr)])
             , ("HTMLid", sv $ "node-" ++ show (n+1))
             , ("HTMLclass",sv "branch")
             , ("children"
               , av [ ov [ ("text", ov [("name", sv $ "\\" ++ v)])
                         , ("HTMLid", sv $ "node-" ++ show n)
                         , ("HTMLclass", sv "leaf")
                         ]
                    , bodyObj
                    ]
               )
             ]
    in (thisObj, b)
  Lit w ->
    let thisObj =
          ov [ ("text", ov [("name", sv $ pp w ++ ": " ++ pp (extract expr))])
             , ("HTMLid", sv $ "node-" ++ show n)
             , ("HTMLclass", sv "leaf")
             ]
    in (thisObj, n+1)
  Var v ->
    let thisObj =
          ov [ ("text", ov [("name", sv $ v ++ ": " ++ pp (extract expr))])
             , ("HTMLid", sv $ "node-" ++ show n)
             , ("HTMLclass", sv "leaf")
             ]
    in (thisObj, n+1)

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

fragmentSection = do
  el "h3" $ text "Fragment"
  el "br" $ return ()
  forM demoTerms $ \(name, term) -> el "pre" (text $ name ++ ": " ++ pp term)

{--}
main :: IO ()
main = mainWidgetWithHead headSection $ do
  divClass "page-wrap" $ do
    divClass "column-left" fragmentSection
    (newTerm, dynTree) <- divClass "column-main" $ do
      activeFilter <- controls 
      let nodeCompute filter ex =
            case filter of
              Types -> 
                  let Right (_, _, _, tt_subs, _, _) = constraintsExpr Env.empty ex
                  in dispTree tt_subs
              Terms -> 
                  dispTree $ extend (nf . cf2db) ex
      newTerm <- termEntry
      elAttr "div" ("id" =: "tree" <> "class" =: "chart") (return ())
      dynTree <- holdDyn ident newTerm
      labelFunc <- mapDyn nodeCompute activeFilter
      t <- combineDyn id labelFunc dynTree
      performEvent_ $ fmap (liftIO . treant) (updated t)
      return (newTerm, dynTree)
    divClass "column-right" $ do
      el "h3" $ text "Reductions"
      el "br" $ return ()
      lagTerm <- delay 1 newTerm
      let eme = fmap (mapClicks . numNodes) lagTerm
      de <- widgetHold (return never) eme 
      dynIndex <- holdDyn Nothing (switch $ current de)
      dynSteps <- combineDyn stepsAt dynIndex dynTree
      el "pre" (dynText =<< mapDyn (unlines . map pp) dynSteps)
  post <- getPostBuild >>= delay 1
  let idTree = dispTree $ extend (nf . cf2db) ident
  performEvent_ $ fmap (const $ liftIO $ treant $ idTree) post


stepsAt :: (Maybe Int) -> CfExpr () -> [Exp String]
stepsAt Nothing _ = []
stepsAt (Just n) cf = (\(x,w) -> w ++ [x]) $ runWriter $ steps
  where steps = case unwrap (extend cf2db target) of
          App f x -> nfTrace (\x -> tell [x]) $ nf (extract f) :@ nf (extract x)
          Lam v b -> nfTrace (\x -> tell [x]) $ v ! nf (extract b)
          _       -> nfTrace (\x -> tell [x]) $ cf2db target
        target = either id (error "gah") $ go cf 0
        go expr i = case unwrap expr of
          App fun arg ->
            go fun i >>= \i' -> if i' == n then Left expr else go arg (i'+1)
          Lam v body ->
            go (var v) i >>= \i' -> if i' == n then Left expr else go body (i'+1)
          _ -> 
            if i == n then Left expr else return (i+1)

mapClicks :: MonadWidget t m => Int -> m (Event t (Maybe Int))
mapClicks n = fmap leftmost es
  where es = forM [0..n-1] $ \i -> do
               doc <- askDocument
               ex <- getElementById doc $ "node-" ++ show i
               case ex of
                 Just e -> wrapDomEvent e (`on` click) (return $ Just i)
                 Nothing -> return never

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

cf2db cf = case unwrap cf of
  Lit (LInt n) -> L (LI n)
  Lit (LBool b) -> L (LB b)
  Var x -> V x
  App m n -> cf2db m :@ cf2db n
  Lam x body -> x ! cf2db body

--}
