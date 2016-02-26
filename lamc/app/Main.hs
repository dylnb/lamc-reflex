{-# LANGUAGE FlexibleContexts, RecursiveDo, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import CofreeTree as CF
import DbExp
import TypeCheck
import Eval
import Parsers
import Lexicon

import Control.Comonad
import Control.Comonad.Cofree
import Control.Monad.State hiding (sequence)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, fromJust)
import Control.Monad.Trans.Class

import Reflex.Dom
import Data.FileEmbed

cf2db :: CfTree a -> Exp String
cf2db cf =
  case unwrap cf of
    ANumber i -> N i
    AVar x -> V x
    ALex w -> fromLex lexicon (V w)
    AApply m n -> cf2db m :@ cf2db n
    ALambda x body -> x ! cf2db body

labelTypes :: CfTree () -> String
labelTypes t = maybe "" show $
  fmap (\ss -> fmap (subst ss . fst) r) maybeSubs
  where r = evalState (sequence $ labelConstraints t) (TypeState 0 M.empty)
        maybeSubs = solveConsts . constraints $ snd (extract r)

labelConstraints :: CfTree () -> CfTree (TypeCheck (CfTree ()))
labelConstraints = extend (memTC genConsts)

labelNormals :: CfTree () -> String
labelNormals = show . extend (pretty . nf . cf2db)

labelMeanings :: CfTree () -> String
labelMeanings = show . extend (eval defEnv . cf2db)
  where defEnv =
          [ ("id", VFun id)
          , ("plus1", VFun $ \(VInt i) -> VInt (i+1))
          , ("k1", VFun $ \(VFun g) -> g (VInt 1))
          , ("k2k5", VFun $ \(VFun g) ->
                        VFun $ \(VFun h) ->
                          let VInt i = g (VInt 2)
                              VInt j = h (VInt 5) in
                          VInt $ i + j)
          ]

t1 = app (var "k1") (var "plus1")
t2 = app (app (var "k2k5") (lam "x" (var "x"))) (lam "y" (var "z"))
add23 = app (app (CF.lex "add") (CF.lex "two")) (CF.lex "three")
iftt = app (app (CF.lex "if") (CF.lex "True")) (CF.lex "True")
comp = lam "f" (lam "g" (lam "x" (app (var "f") (app (var "g") (var "x")))))
k3 = lam "k" (num 3)
ident = lam "x" (var "x")
ex2 = app (app (app comp k3) ident) (num 7)
ex3 = app (lam "f" (app (var "f") (var "f"))) (lam "x" (var "x"))
ex4 = app (var "x") (var "x")
test = app (lam "b" (app (app (CF.lex "if") (var "b")) (CF.lex "true"))) (CF.lex "false")  

demoTerms :: [(String, CfTree ())]
demoTerms =
  [ ("t1", t1)
  , ("t2", t2)
  , ("test", test)
  , ("ex2", ex2)
  ]

-- acc :: (Int -> Maybe String) -> CfTree () -> IO ()
-- acc g cf = runInputT defaultSettings loop
--   where loop = do let opts = enumerate g cf
--                   outputStrLn $ show opts
--                   Just x <- getInputLine "What next? "
--                   let (a :@ b) = cf2db . fromJust $ nthNode (read x) opts
--                   outputStrLn $ pretty (nf a :@ nf b)
--                   tryIt (read x) (a :@ b)
--         tryIt ind gold = do
--           Just guess <- getInputLine "Reduce: "
--           case parseExp guess of
--             Left s -> outputStrLn (show s)
--             Right p -> do
--               let correct = p == nf gold
--               outputStrLn (show correct)
--               if correct
--                 then do Just s <- getInputLine "Continue? "
--                         let g' n = if n == ind then Just (pretty p) else g n
--                         unless (s == "no") $ liftIO (acc g' cf)
--                 else tryIt ind gold


enumerate :: (Int -> Maybe String) -> CfTree () -> CfTree (Either Int String)
enumerate g cf = evalState (sequence $ extend enum cf) 0
  where enum :: CfTree () -> State Int (Either Int String)
        enum (() :< ANumber i) = state $ \s -> (Right $ "n" ++ show i, s+1)
        enum (() :< AVar x) = state $ \s -> (Right x, s+1)
        enum (() :< ALex w) = state $ \s ->
          (Right $ pretty . nf $ fromLex lexicon (V w), s+1)
        enum (() :< ALambda x b) = state $ \s ->
          case g (s+1) of
            Nothing -> (Left s, s+1)
            Just t  -> (Right $ pretty . nf $ x ! cf2db b, s+1)
        enum (() :< AApply a b) = state $ \s ->
          case g s of                          
            Nothing -> (Left s, s+1)           
            Just t  -> (Right t, s+1)          

nthNode :: Int -> CfTree a -> Maybe (CfTree a)
nthNode i cf = case iter cf 0 of {Left a -> Just a; Right _ -> Nothing}
  where iter cf n
          | n == i    = Left cf
          | otherwise = case cf of
                          (_ :< AApply a b) -> iter a (n+1) >>= iter b
                          (_ :< ALambda v b) -> iter b (n+1)
                          _ -> return n

main :: IO ()
-- main = acc (const Nothing) test 
main = mainWidgetWithCss $(embedFile "static/lamc.css") $
  divClass "page-wrap" $
    divClass "column-main" $ do
      activeFilter <- controls 
      let nodeCompute filter = case filter of {Types -> labelTypes; Terms -> labelNormals}
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

termEntry :: MonadWidget t m => m (Event t (CfTree ()))
termEntry = divClass "sh" $ do
  let termBoxAttrs = ["placeholder" =: "Enter a term", "name" =: "newTerm", "id" =: "query"]
  rec let newValueEntered = ffilter (==keycodeEnter) (_textInput_keypress termBox)
      termBox <- divClass "query" $
        textInput $ def & setValue .~ fmap (const "") newValueEntered
                        & attributes .~ constDyn (mconcat termBoxAttrs)
  let newValue = tag (current $ _textInput_value termBox) newValueEntered
      newTerm = fmapMaybe checkTerm newValue
  elAttr "pre" ("class" =: "curterm") $
    dynText =<< holdDyn "Showing: \\x. x" (fmap (("Showing: " ++) . fringe) newTerm)
  el "br" (return ())
  return newTerm
  where checkTerm t = lookup t demoTerms `mplus` either (const Nothing) Just (parseCf t)
