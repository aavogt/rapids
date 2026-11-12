module SvgTreeAntiquote
  ( parsePathCommands,
    parsePathCommandsSupplement,
    parsePathCommandsSupplementCmd,
    renderCmdsWithDiff,
    renderCmdWithDiff,
    Supplement (..),
    WArgV (..),
    JArg (..),

    -- * optics for expression Cmd
    _Ca,
    _CA,
    _Ct,
    _CT,
    _Cl,
    _CL,
    _CQ,
    _Cq,
    _CS,
    _Cs,
    _CC,
    _Cc,
    _Ch,
    _CH,
    _Cv,
    _CV,
    _Cm,
    _CM,
    _Cw,
    _CW,
    _Cj,
    _CJ,
    _CZ,
    _COpen,
    _CClose,
    _COpenW,
    _CCloseW,
    -- * optics for PathCommand
    _MoveTo,
    _LineTo,
    _HorizontalTo,
    _VerticalTo,
    _CurveTo,
    _SmoothCurveTo,
    _QuadraticBezier,
    _SmoothQuadraticBezierCurveTo,
    _EllipticalArc,
    _EndPath,
  )
where

import Control.Lens
import Data.Char (isAlphaNum, isSpace)
import Data.Functor ((<&>))
import Graphics.Svg.Types (Origin (..), PathCommand (..))
import Language.Haskell.TH
  ( Exp,
    ExpQ,
    Q,
    appsE,
    dyn,
    listE,
    reportError,
    tupE,
  )
import Language.LBNF
  ( AlexInput,
    Array,
    BNFC_QQType,
    HappyStk (..),
    ParseMonad (Bad, Ok),
    Posn (..),
    alexGetByte,
    appEPAll,
    appEPAllL,
    bnfc,
    fromLit,
    fromToken,
    lbnf,
    listArray,
    (!),
  )
import Language.LBNF.Runtime (err, printTree)
import Linear (V2 (V2))
import Prelude hiding (exp)
import Language.Haskell.TH.Syntax ( lookupValueName, Lift(lift) )
import Data.Maybe ( catMaybes, fromMaybe )
import Data.List ( groupBy, stripPrefix )
import Data.Either (fromLeft)
import GHC.Generics
import Control.Monad

bnfc
  [lbnf|

entrypoints Cmd, Cmds;

comment "--";
separator Cmd "";
Cmds.Cmds ::= [Cmd];

Ca.Cmd ::= "a" [AArg];
CA.Cmd ::= "A" [AArg];
Ct.Cmd ::= "t" [MArg];
CT.Cmd ::= "T" [MArg];
Cl.Cmd ::= "l" [MArg];
CL.Cmd ::= "L" [MArg];
CQ.Cmd ::= "Q" [QArg];
Cq.Cmd ::= "q" [QArg];
CS.Cmd ::= "S" [QArg];
Cs.Cmd ::= "s" [QArg];
CC.Cmd ::= "C" [CArg];
Cc.Cmd ::= "c" [CArg];
Ch.Cmd ::= "h" [HArg];
CH.Cmd ::= "H" [HArg];
Cv.Cmd ::= "v" [HArg];
CV.Cmd ::= "V" [HArg];
Cm.Cmd ::= "m" [MArg];
CM.Cmd ::= "M" [MArg];
Cw.Cmd ::= "w" [WArg];
CW.Cmd ::= "W" [WArg];
Cj.Cmd ::= "j" [JArg];
CJ.Cmd ::= "J" [JArg];
CZ.Cmd ::= "z";
CZ.Cmd ::= "Z";
COpen.Cmd ::= "{";
CClose.Cmd ::= "}";
COpenW.Cmd ::= "[";
CCloseW.Cmd ::= "]";

W1.WArg ::= E;
W2.WArg ::= E "," E;
J1.JArg ::= Integer;
J2.JArg ::= Integer "," Integer;
AArg.AArg::= E "," E "," E "," B "," B "," MArg;
QArg.QArg ::= MArg "," MArg;
CArg.CArg ::= MArg "," MArg"," MArg;
MArg.MArg::= E "," E;
HArg.HArg ::= E;
separator AArg "";
separator MArg "";
separator QArg "";
separator CArg "";
separator HArg "";
separator JArg "";
separator WArg "";


EDouble.E4 ::= Double;
EInt. E4 ::= Integer;
EVar. E4 ::= Ident [E1];
EOp . E3 ::= E4 Op3 E3;
EOp . E2  ::= E2 Op2 E3;
EOp . E1  ::= E1 Op E2;
ENeg. E  ::= "-" E1;

BV.B ::= Ident;
BI.B ::= Integer;

separator E1 "";

EPlus. Op ::= "+";
EMinus. Op ::= "-";
ETimes. Op2 ::= "*";
EDiv. Op2 ::= "/";
EPow. Op3 ::= "**";
EPow. Op3 ::= "^";
EPow. Op3 ::= "^^";


_. E4 ::= "(" E ")" ;
_. E3 ::= E4 ;
_. E2 ::= E3 ;
_. E1 ::= E2 ;
_. E  ::= E1 ;

 |]

makePrisms ''PathCommand
makePrisms ''Cmd

deriving instance Generic AArg
deriving instance Generic HArg
deriving instance Generic MArg
deriving instance Generic QArg
deriving instance Generic CArg
deriving instance Generic WArg
deriving instance Generic JArg
instance Field1 HArg HArg E E
instance Field1 MArg MArg E E
instance Field2 MArg MArg E E
instance Field1 QArg QArg MArg MArg
instance Field2 QArg QArg MArg MArg
instance Field1 CArg CArg MArg MArg
instance Field2 CArg CArg MArg MArg
instance Field3 CArg CArg MArg MArg
instance Field1 AArg AArg E E
instance Field2 AArg AArg E E
instance Field3 AArg AArg E E
instance Field4 AArg AArg B B
instance Field5 AArg AArg B B
instance Field6 AArg AArg MArg MArg

deriving instance Lift AArg
deriving instance Lift B
deriving instance Lift CArg
deriving instance Lift Cmd
deriving instance Lift Cmds
deriving instance Lift E
deriving instance Lift HArg
deriving instance Lift Ident
deriving instance Lift MArg
deriving instance Lift Op
deriving instance Lift QArg
deriving instance Lift WArg

-- | Parse svg path commands into svg-tree's 'PathCommand',
-- supporting arithmetic @+ - * / ** ( )@ with haskell-like fixity
--
-- @
-- ghci> let x = 7; y = 3 in $(parsePathCommands "v -x**2 h y")
-- [VerticalTo OriginRelative [-49.0],HorizontalTo OriginRelative [3.0]]
-- @
--
-- Single-letter variables (@atlqschvmz@) are supported: when in-scope
-- ('lookupValueName' won't find declarations after) tokens after the first are
-- mangled and unmangled for the lbnfc parser. In the following example the
-- absolute @T@-command can be rewritten as a relative @t@-command, without
-- interfering with the @t@-variables
--
-- >T t,t
-- >T 0,0
-- >T
-- > t,t
--
-- @
-- ghci> let t = 2 in $(parsePathCommands "t t,t\nt 0,0\nt\n t,t")
-- >[SmoothQuadraticBezierCurveTo OriginRelative [V2 2.0 2.0]
-- >,SmoothQuadraticBezierCurveTo OriginRelative [V2 0.0 0.0]
-- >,SmoothQuadraticBezierCurveTo OriginRelative [V2 2.0 2.0]]
-- @
parsePathCommands :: String -- ^ https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/d#path_commands
  -> ExpQ -- ^ ['PathCommand']
parsePathCommands input = do
  (mangle,unmangle) <- mangleUnmangle
  let ?unmangle = unmangle
  case pCmds $ applyMangle mangle $ myLexer input of
    Ok (Cmds s) -> listE (map (either id (error "unexpected") . toPC) s)
    Bad msg -> reportError msg >> [|()|]

parsePathCommandsSupplement :: String -> ExpQ -- ^ [Either Supplement PathCommand]
parsePathCommandsSupplement input = do
  (mangle,unmangle) <- mangleUnmangle
  let ?unmangle = unmangle
  case pCmds $ applyMangle mangle $ myLexer input of
    Ok (Cmds s) -> listE (map (liftEither . toPC) s)
    Bad msg -> reportError msg >> [|()|]

-- | Parse supplement commands with both their syntax tree and evaluated value.
parsePathCommandsSupplementCmd :: String -> ExpQ -- ^ [(Cmd, Either Supplement PathCommand)]
parsePathCommandsSupplementCmd input = do
  (mangle, unmangle) <- mangleUnmangle
  let ?unmangle = unmangle
  case pCmds $ applyMangle mangle $ myLexer input of
    Ok (Cmds commands) ->
      listE $ map (\command ->
        let value = liftEither (toPC command)
        in [| (command, $value) |]) commands
    Bad msg -> reportError msg >> [|()|]

liftEither :: Either ExpQ ExpQ -> ExpQ
liftEither (Left x) = [| Left $x |]
liftEither (Right x) = [| Right $x |]

-- | hanging indent
applyMangle :: (Token -> Token) -> [Token] -> [Token]
applyMangle f ts = concatMap (mapTail f) $ groupBy sameLineOrIndented ts

sameLineOrIndented (PT (Pn _ x i) _) (PT (Pn _ y j) _) = x == y || j > i
sameLineOrIndented _ _ = False

mapTail f (x:xs) = x : map f xs
mapTail _ [] = []

-- | add a (hopefully) unique prefix
mangleUnmangle :: Q (Token -> Token, String -> String)
mangleUnmangle = do
  let prefix = "oOoOoO"
  ns <- catMaybes <$> sequence [ lookupValueName n <&> fmap (const n)
   | n <- ["a","t","l","q","s","c","h","v","m","z","w","j"]]
  return (\case
    PT p (TS n@[_] _) | n `elem` ns -> PT p (TV (prefix ++ n))
    x -> x
   , \mangled -> fromMaybe mangled $ stripPrefix prefix mangled)

toPC = \case
  Ca args -> Right [|EllipticalArc OriginRelative $(toAArgs args)|]
  CA args -> Right [|EllipticalArc OriginAbsolute $(toAArgs args)|]
  Ct args -> Right [|SmoothQuadraticBezierCurveTo OriginRelative $(toMArgs args)|]
  CT args -> Right [|SmoothQuadraticBezierCurveTo OriginAbsolute $(toMArgs args)|]
  Cl args -> Right [|LineTo OriginRelative $(toMArgs args)|]
  CL args -> Right [|LineTo OriginAbsolute $(toMArgs args)|]
  Cq args -> Right [|QuadraticBezier OriginRelative $(toQArgs args)|]
  CQ args -> Right [|QuadraticBezier OriginAbsolute $(toQArgs args)|]
  Cs args -> Right [|SmoothCurveTo OriginRelative $(toQArgs args)|]
  CS args -> Right [|SmoothCurveTo OriginAbsolute $(toQArgs args)|]
  Cc args -> Right [|CurveTo OriginRelative $(toCArgs args)|]
  CC args -> Right [|CurveTo OriginAbsolute $(toCArgs args)|]
  Ch args -> Right [|HorizontalTo OriginRelative $(toHArgs args)|]
  CH args -> Right [|HorizontalTo OriginAbsolute $(toHArgs args)|]
  Cv args -> Right [|VerticalTo OriginRelative $(toHArgs args)|]
  CV args -> Right [|VerticalTo OriginAbsolute $(toHArgs args)|]
  Cm args -> Right [|MoveTo OriginRelative $(toMArgs args)|]
  CM args -> Right [|MoveTo OriginAbsolute $(toMArgs args)|]
  CZ -> Right [|EndPath|]
  Cw args -> Left [| Swidth $(toWArgs args) |]
  CW args -> Left [| SWidth $(toWArgs args) |]
  Cj args -> Left [| SJoin $(toJArgs args) |]
  COpen -> Left [| SOpen |]
  CClose -> Left [| SClose |]
  COpenW -> Left [| SOpenW |]
  CCloseW -> Left [| SCloseW |]

-- | for svg-tree-offset:SvgTreeOffset
data Supplement = SWidth [WArgV] | Swidth [WArgV] | SJoin [JArg] | Sjoin [JArg] | SOpen | SClose | SOpenW | SCloseW

data WArgV = W1V Double | W2V Double Double

deriving instance Lift JArg

toWArgs = listE . map toWArg

toWArg = \case
  W1 e -> [| W1V $(toExp e) |]
  W2 e f -> [| W2V $(toExp e) $(toExp f) |]

toJArgs = listE . map lift

toHArgs = listE . map toCoord

toMArgs = listE . map toV2

toV2 (MArg x y) = [|V2 $(toExp x) $(toExp y)|]

toCoord (HArg x) = toExp x

toExp = \case
  EDouble d -> [|d|]
  EInt i -> [|fromIntegral i|]
  EVar (Ident a) b -> appsE (dyn (?unmangle a) : map toExp b)
  ENeg e -> [| - $(toExp e) |]
  EOp a op b -> liftOp op (toExp a) (toExp b)

liftOp :: Op -> ExpQ -> ExpQ -> ExpQ
liftOp op a b = case op of
  EPlus -> [| $a + $b |]
  EMinus -> [| $a - $b |]
  ETimes -> [| $a * $b |]
  EDiv -> [| $a / $b |]
  EPow -> [| $a ** $b |]

toQArgs = listE . map toQArg

toQArg (QArg a b) = tupE [toV2 a, toV2 b]

toCArgs = listE . map toCArg

toCArg (CArg a b c) = tupE [toV2 a, toV2 b, toV2 c]

toAArg (AArg rx ry rot large sweep xy) =
  tupE
    [ toExp rx,
      toExp ry,
      toExp rot,
      toExpB large,
      toExpB sweep,
      toV2 xy
    ]

toExpB = \case
  BV (Ident b) -> dyn (?unmangle b)
  BI n
    | n == 1 -> [| True |]
    | n == 0 -> [| False |]


toAArgs = listE . map toAArg

-- | update and pretty print
renderCmdsWithDiff :: [(Cmd, Either Supplement PathCommand)] -> [Either Supplement PathCommand] -> Maybe String
renderCmdsWithDiff orig new = unlines <$> zipWithM renderCmdWithDiff orig new

-- | Rebuild a parsed command, applying numeric changes to its original expressions.
renderCmdWithDiff :: (Cmd, Either Supplement PathCommand) -> Either Supplement PathCommand -> Maybe String
renderCmdWithDiff (syntax, original) updated =
  normalizePrinted . unmanglePrinted . printTree <$> adjustParsedCommand syntax original updated

adjustParsedCommand :: Cmd -> Either Supplement PathCommand -> Either Supplement PathCommand -> Maybe Cmd
adjustParsedCommand (CW args) (Left (SWidth old)) (Left (SWidth new)) = CW <$> adjustWArgs args old new
adjustParsedCommand (Cw args) (Left (Swidth old)) (Left (Swidth new)) = Cw <$> adjustWArgs args old new
adjustParsedCommand (CJ args) (Left (SJoin old)) (Left (SJoin new)) = CJ <$> adjustJArgs args old new
adjustParsedCommand CZ (Right EndPath) (Right EndPath) = Just CZ
adjustParsedCommand COpen (Left SOpen) (Left SOpen) = Just COpen
adjustParsedCommand CClose (Left SClose) (Left SClose) = Just CClose
adjustParsedCommand COpenW (Left SOpenW) (Left SOpenW) = Just COpenW
adjustParsedCommand CCloseW (Left SCloseW) (Left SCloseW) = Just CCloseW
adjustParsedCommand c (Right old) (Right new) = mergePC1 c old new
adjustParsedCommand _ _ _ = Nothing

mergePC1 :: Cmd -> PathCommand -> PathCommand -> Maybe Cmd
mergePC1 = \cases
  (CM args) (MoveTo OriginAbsolute old) (MoveTo OriginAbsolute new) -> CM <$> adjustMArgs args old new
  (Cm args) (MoveTo OriginRelative old) (MoveTo OriginRelative new) -> Cm <$> adjustMArgs args old new
  (CL args) (LineTo OriginAbsolute old) (LineTo OriginAbsolute new) -> CL <$> adjustMArgs args old new
  (Cl args) (LineTo OriginRelative old) (LineTo OriginRelative new) -> Cl <$> adjustMArgs args old new
  (CT args) (SmoothQuadraticBezierCurveTo OriginAbsolute old) (SmoothQuadraticBezierCurveTo OriginAbsolute new) -> CT <$> adjustMArgs args old new
  (Ct args) (SmoothQuadraticBezierCurveTo OriginRelative old) (SmoothQuadraticBezierCurveTo OriginRelative new) -> Ct <$> adjustMArgs args old new
  (CQ args) (QuadraticBezier OriginAbsolute old) (QuadraticBezier OriginAbsolute new) -> CQ <$> adjustQArgs args old new
  (Cq args) (QuadraticBezier OriginRelative old) (QuadraticBezier OriginRelative new) -> Cq <$> adjustQArgs args old new
  (CS args) (SmoothCurveTo OriginAbsolute old) (SmoothCurveTo OriginAbsolute new) -> CS <$> adjustQArgs args old new
  (Cs args) (SmoothCurveTo OriginRelative old) (SmoothCurveTo OriginRelative new) -> Cs <$> adjustQArgs args old new
  (CC args) (CurveTo OriginAbsolute old) (CurveTo OriginAbsolute new) -> CC <$> adjustCArgs args old new
  (Cc args) (CurveTo OriginRelative old) (CurveTo OriginRelative new) -> Cc <$> adjustCArgs args old new
  (CH args) (HorizontalTo OriginAbsolute old) (HorizontalTo OriginAbsolute new) -> CH <$> adjustHArgs args old new
  (Ch args) (HorizontalTo OriginRelative old) (HorizontalTo OriginRelative new) -> Ch <$> adjustHArgs args old new
  (CV args) (VerticalTo OriginAbsolute old) (VerticalTo OriginAbsolute new) -> CV <$> adjustHArgs args old new
  (Cv args) (VerticalTo OriginRelative old) (VerticalTo OriginRelative new) -> Cv <$> adjustHArgs args old new
  (CA args) (EllipticalArc OriginAbsolute old) (EllipticalArc OriginAbsolute new) -> CA <$> adjustAArgs args old new
  (Ca args) (EllipticalArc OriginRelative old) (EllipticalArc OriginRelative new) -> Ca <$> adjustAArgs args old new

-- | 'Just (zipWith3 f a b c) == zipWith3eq f a b c' if a,b,c have equal lengths
zipWith3eq :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> Maybe [d] 
zipWith3eq f (a:as) (b:bs) (c:cs) = (f a b c :) <$> zipWith3eq f as bs cs
zipWith3eq f [] [] [] = Just []
zipWith3eq _ _ _ _ = Nothing

-- | 'Just (zipWith f a b) == zipWith3eq f a b' if a,b have equal lengths
zipWith2eq :: (a -> b -> d) -> [a] -> [b] -> Maybe [d] 
zipWith2eq f (a:as) (b:bs) = (f a b :) <$> zipWith2eq f as bs
zipWith2eq f [] [] = Just []
zipWith2eq _ _ _ = Nothing

adjustMArgs :: [MArg] -> [V2 Double] -> [V2 Double] -> Maybe [MArg]
adjustMArgs args old new = zipWith3eq adjustMArg args old new

adjustMArg :: MArg -> V2 Double -> V2 Double -> MArg
adjustMArg (MArg x y) oldPoint newPoint =
  let V2 dx dy = newPoint - oldPoint
  in MArg (addDelta dx x) (addDelta dy y)

adjustHArgs :: [HArg] -> [Double] -> [Double] -> Maybe [HArg]
adjustHArgs args old new = zipWith3eq (\x y -> _1 %~ addDelta (y-x)) old new args

adjustQArgs :: [QArg] -> [(V2 Double, V2 Double)] -> [(V2 Double, V2 Double)] -> Maybe [QArg]
adjustQArgs args old new = zipWith3eq adjust args old new
  where
    adjust (QArg a b) (oldA, oldB) (newA, newB) =
      QArg (adjustMArg a oldA newA) (adjustMArg b oldB newB)

adjustCArgs :: [CArg] -> [(V2 Double, V2 Double, V2 Double)] -> [(V2 Double, V2 Double, V2 Double)] -> Maybe [CArg]
adjustCArgs args old new = zipWith3eq adjust args old new
  where
    adjust (CArg a b c) (oldA, oldB, oldC) (newA, newB, newC) =
      CArg (adjustMArg a oldA newA) (adjustMArg b oldB newB) (adjustMArg c oldC newC)

adjustAArgs :: [AArg] -> [(Double, Double, Double, Bool, Bool, V2 Double)] -> [(Double, Double, Double, Bool, Bool, V2 Double)] -> Maybe [AArg]
adjustAArgs args old new = zipWith3eq adjust args old new
  where
    adjust (AArg rx ry rotation large sweep end) (oldRx, oldRy, oldRotation, oldLarge, oldSweep, oldEnd) (newRx, newRy, newRotation, newLarge, newSweep, newEnd) =
      AArg
        (addDelta (newRx - oldRx) rx)
        (addDelta (newRy - oldRy) ry)
        (addDelta (newRotation - oldRotation) rotation)
        (adjustFlag large oldLarge newLarge)
        (adjustFlag sweep oldSweep newSweep)
        (adjustMArg end oldEnd newEnd)

adjustWArgs :: [WArg] -> [WArgV] -> [WArgV] -> Maybe [WArg]
adjustWArgs args old new = sequence =<< zipWith3eq adjust args old new
  where
    adjust (W1 e) (W1V x) (W1V y) = Just (W1 (addDelta (y - x) e))
    adjust (W2 e f) (W2V x y) (W2V x' y') =
      Just (W2 (addDelta (x' - x) e) (addDelta (y' - y) f))
    adjust _ _ _ = Nothing

adjustJArgs :: [JArg] -> [JArg] -> [JArg] -> Maybe [JArg]
adjustJArgs source old new = zipWith3eq (\syntax before after -> if before == after then syntax else after) source old new

adjustFlag :: B -> Bool -> Bool -> B
adjustFlag source old new
  | old == new = source
  | new = BI 1
  | otherwise = BI 0

addDelta :: Double -> E -> E
addDelta delta expression
  | delta == 0 = expression
addDelta delta (EOp expression EPlus rhs)
  | Just amount <- numericExpressionValue rhs = addDelta (delta + amount) expression
addDelta delta (EOp expression EMinus rhs)
  | Just amount <- numericExpressionValue rhs = addDelta (delta - amount) expression
addDelta delta expression
  | delta > 0 = EOp expression EPlus (numberExpression delta)
  | otherwise = EOp expression EMinus (numberExpression (abs delta))

numericExpressionValue :: E -> Maybe Double
numericExpressionValue (EInt value) = Just (fromInteger value)
numericExpressionValue (EDouble value) = Just value
numericExpressionValue _ = Nothing

numberExpression :: Double -> E
numberExpression value
  | not (isNaN value || isInfinite value) && fromInteger (round value) == value = EInt (round value)
  | otherwise = EDouble value

unmanglePrinted [] = []
unmanglePrinted input@(c:cs)
  | isAlphaNum c || c == '_' =
      let (name, rest) = span (\char -> isAlphaNum char || char == '_') input
      in unmangleName name ++ unmanglePrinted rest
  | otherwise = c : unmanglePrinted cs
  where
    unmangleName name = fromMaybe name (lookup name mangledNames)
    mangledNames = [("oOoOoO" ++ [letter], [letter]) | letter <- "atlqschvmzwj"]

normalizePrinted :: String -> String
normalizePrinted = stripRedundantLeftParens
  . replaceAll " , " ","
  . replaceAll ", " ","
  . replaceAll " (**) " "**"
  . replaceAll " (*) " "*"
  . replaceAll " (/) " "/"
  . replaceAll " (+) " "+"
  . replaceAll " (-) " "-"

stripRedundantLeftParens :: String -> String
stripRedundantLeftParens [] = []
stripRedundantLeftParens ('(':rest) = case takeGroup rest of
  Nothing -> '(' : stripRedundantLeftParens rest
  Just (inside, after) ->
    let inside' = stripRedundantLeftParens inside
        after' = stripRedundantLeftParens after
    in if canDropLeftParens inside' after
         then inside' ++ after'
         else '(' : inside' ++ ")" ++ after'
stripRedundantLeftParens (c:cs) = c : stripRedundantLeftParens cs

takeGroup :: String -> Maybe (String, String)
takeGroup = go 1 []
  where
    go _ _ [] = Nothing
    go depth reversed (c:cs)
      | c == '(' = go (depth + 1) (c : reversed) cs
      | c == ')' && depth == 1 = Just (reverse reversed, cs)
      | c == ')' = go (depth - 1) (c : reversed) cs
      | otherwise = go depth (c : reversed) cs

canDropLeftParens :: String -> String -> Bool
canDropLeftParens inside after = case outerOperator after of
  Nothing -> False
  Just (outerPrecedence, rightAssociative) ->
    let innerPrecedence = expressionPrecedence inside
    in innerPrecedence > outerPrecedence
         || (innerPrecedence == outerPrecedence && not rightAssociative)

outerOperator :: String -> Maybe (Int, Bool)
outerOperator ('*':'*':_) = Just (3, True)
outerOperator ('^':'^':_) = Just (3, True)
outerOperator ('^':_) = Just (3, True)
outerOperator ('*':_) = Just (2, False)
outerOperator ('/':_) = Just (2, False)
outerOperator ('+':_) = Just (1, False)
outerOperator ('-':_) = Just (1, False)
outerOperator _ = Nothing

expressionPrecedence :: String -> Int
expressionPrecedence expression = case topLevelPrecedences expression of
  [] -> 4
  precedences -> minimum precedences

topLevelPrecedences :: String -> [Int]
topLevelPrecedences = go 0 Nothing
  where
    go _ _ [] = []
    go depth previous input@(c:cs)
      | c == '(' = go (depth + 1) (Just c) cs
      | c == ')' = go (depth - 1) (Just c) cs
      | isSpace c = go depth previous cs
      | depth == 0 = case input of
          '*':'*':rest -> 3 : go depth (Just '*') rest
          '^':'^':rest -> 3 : go depth (Just '^') rest
          '^':rest -> 3 : go depth (Just '^') rest
          '*':rest -> 2 : go depth (Just '*') rest
          '/':rest -> 2 : go depth (Just '/') rest
          '+':rest
            | isOperandEnd previous -> 1 : go depth (Just '+') rest
          '-':rest
            | isOperandEnd previous -> 1 : go depth (Just '-') rest
          _ -> go depth (Just c) cs
      | otherwise = go depth (Just c) cs
    isOperandEnd (Just c) = isAlphaNum c || c == '_' || c == ')'
    isOperandEnd Nothing = False

replaceAll :: String -> String -> String -> String
replaceAll needle replacement = go
  where
    go [] = []
    go input@(c:cs) = case stripPrefix needle input of
      Just rest -> replacement ++ go rest
      Nothing -> c : go cs
