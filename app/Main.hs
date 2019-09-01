{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE StrictData                 #-}

module Main where

import qualified Data.Scientific as S
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.Expr
import           Foreign.C.Error
import           Codec.Binary.UTF8.String
import           Control.Concurrent
import qualified Control.Exception.Base    as E
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Data.IORef
import           Data.List
import qualified Data.List.Split           as S
--import qualified Data.Map.Strict           as M
import qualified Data.HashMap.Strict       as H
import qualified Data.HashSet              as HS
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import qualified Data.Text.Read            as TR
import           Data.Void
import           Debug.Trace
--import           LinuxEcode
import           LinuxSignal
import           System.Console.GetOpt
import           System.Console.Haskeline  hiding (Handler, throwTo)
import           System.Directory          hiding (isSymbolicLink)
import           System.Environment
import           System.Exit
import qualified System.FilePath.Glob      as G
import           System.FilePath.Posix
import           System.IO
import           System.Posix.Files
import qualified System.Posix.Process      as P
import           System.Posix.Signals
import           System.Posix.Types
import           System.Posix.User
import qualified System.Posix.IO           as PI
import           System.Process
import           System.Timeout
import           System.IO.Temp
import           Text.Megaparsec           hiding (State)
import           Text.Megaparsec.Char      hiding (space)
import           Text.Regex.Posix
import           Text.Printf
import qualified TextShow                  as TS
--import qualified TextShow.Data.Floating    as TF

data Env = Env { status :: Bool,
                 ret    :: [Val],
                 args   :: [Val],
                 vars   :: H.HashMap T.Text Val,
                 inn    :: Handle,
                 out    :: Handle,
                 err    :: Handle,
                 flags  :: Flags,
                 parenv :: ParseEnv,
                 funID  :: Int,
                 dir    :: String,
                 funcs  :: IORef (H.HashMap T.Text Val),
                 thread :: IORef ThreadInfo,
                 idSrc  :: IORef Int
                 }

sethandles denv senv = denv { out = out senv,
                              inn = inn senv,
                              err = err senv }

setRetEnv env renv = env { status = status renv,
                           ret    = ret renv }

defaultEnv :: FilePath -> IORef (H.HashMap T.Text Val) -> IORef ThreadInfo -> IORef Int -> Env
defaultEnv =
  Env True
      []
      []
      defaultVars
      stdin
      stdout
      stderr
      (Flags False False)
      defaultParseEnv
      0

incFunID :: Env -> Eval Env
incFunID env = do
  i <- liftIO $ (1+) <$> readIORef (idSrc env)
  liftIO $ writeIORef (idSrc env) i
  return env{funID=i}

data Flags = Flags { interactiveMode        :: Bool,
                     ignoreInterpreterError :: Bool }
  deriving Show

data ThreadInfo = ThreadInfo { tid      :: ThreadId,
                               exitMvar :: MVar Env,
                               cmdMvar  :: Maybe (MVar ()),
                               exitTrap :: Val
                               }

defaultVars :: H.HashMap T.Text Val
defaultVars = H.fromList [
  ("T", Bool True),
  ("F", Bool False)
  ]



defaultFuncs :: H.HashMap T.Text Val
defaultFuncs = H.fromList [
--  ("ret"   , Prim Normal return' ["[COMMAND]..."]),
  ("dict"  , Prim Purely dict    ["[KEY VALUE]..."]),
  ("+"     , Prim Purely plus''  ["[VALUE]..."]),
  ("true"  , Prim Purely true    ["[VALUE]..."]),
  (":"     , Prim Purely colon   ["VALUE..."]),
  ("false" , Prim Purely false   ["[VALUE]..."]),
  ("exit"  , Prim Normal exit'   ["[NUMBER]..."]),
  ("break" , Prim Normal break'  ["[VALUE]..."]),
--  ("breakT", Prim Normal breakT  ["[VALUE]..."]),
--  ("breakF", Prim Normal breakF  ["[VALUE]..."]),
  ("let"   , Prim Normal unavail ["NAME... VALUE"]),
  ("letr"  , Prim Normal unavail ["-r NAME... COMMAND"]),
  ("def"   , Prim Normal unavail ["NAME {COMMAND|VALUE}"]),
  ("trap"  , Prim Normal trap    ["COMMAND SIGNAL..."]),
  ("cd"    , Prim Normal cd      ["[DIR]"]),
  ("usage" , Prim Normal usage   ["NAME"]),
  ("load"  , Prim Normal unavail ["FILE"]),
  ("loop"  , Prim Normal loop    ["COMMAND [ARG]..."]),
  ("bool"  , Prim Purely bool    ["COMMAND [ARG]..."]),
  ("ubool" , Prim Purely ubool   ["COMMAND [ARG]..."]),
  ("read"  , Prim Normal read'   ["[-i] [-n COUNT]"]),
  ("echo"  , Prim Normal echo    ["[VALUE]..."]),
  ("print" , Prim Normal echo    ["[VALUE]..."]),
  ("show"  , Prim Normal show'   ["VALUE..."]),
  ("glob"  , Prim Normal glob    ["[-f] EXPRESSION..."]),
  ("sep"   , Prim Purely split   ["[SEPARATOR] STRING"]),
  ("sub"   , Prim Purely sub     ["REGEXP REPLACEMENT STRING"]),
  ("getenv", Prim Normal getenv  ["NAME"]),
  ("setenv", Prim Normal setenv  ["NAME STRING"]),
  ("map"   , Prim Normal map'    ["[-n NUMBER] COMMAND [ARG]... LIST"]),
  ("fold"  , Prim Normal fold'   ["COMMAND [ARG]... VALUE LIST"]),
  ("filter", Prim Normal filter' ["COMMAND [ARG]... VALUE LIST"]),
  ("len"   , Prim Purely len'    ["VALUE..."]),
  ("lenc"  , Prim Purely lenc    ["VALUE..."]),
--  ("idx"   , Prim Purely idx     ["NUMBER [NUMBER] LIST"]),
  ("int"   , Prim Purely int     ["{-r|-c|-f} NUMBER"]),
  ("timeo" , Prim Normal timeo   ["DURATION COMMAND [ARG]..."]),
  ("check" , Prim Normal check   ["[-bcdefiLnoprsSwxOG] FILE..."]),
--  ("list?" , Prim Purely isList  ["value..."]),
  ("type" , Prim Purely isList  ["value..."]),
  ("list"  , Prim Purely list    ["value..."]),
  ("shift" , Prim Normal shift   ["NUMBER"]),
  ("ulist" , Prim Purely ulist   ["value..."]),
  ("fork"  , Prim Normal fork    ["COMMAND [ARG]..."]),
  ("tmpf"  , Prim Normal tmpfile ["[-d DIR] [-p PREFIX] COMMAND"]),
  ("tmpd"  , Prim Normal tmpdir  ["[-d DIR] [-p PREFIX] COMMAND"]),
  ("keep"  , Prim Normal keep'   ["COMMAND [ARG]..."])
  ]

usagePrint name = Eval $ throwError ([], SomeError $ usageShow name)

unavail env xs = Eval $ throwError ([], SomeError $ "cannot call special functions pia variable")

dict env xs = do
  d <- dict' env H.empty xs
  return env{ret=[Dict d], status=True}
  where
    dict' :: Env -> (H.HashMap T.Text Val) -> [Val] -> Eval (H.HashMap T.Text Val)
    dict' env m (k:v:xs) = do
      t <- expand env k
      dict' env (H.insert t v m) xs
    dict' env m _ = return m

shift env [] = shift env [Float 1]
shift env [n] = do
  n <- getInt n
  let vs = args env in
    if greater n vs then return env{status=True, ret=take n $ args env, args=drop n $ args env}
                    else return env{status=False, ret=args env, args=[]}
shift env _ = usagePrint "shift"

int env [x] = do n <- getFloat x; return env{status=True, ret=[toFloat $ floor n]}
int env (Str _ "-r":[x]) = do n <- getFloat x; return env{status=True, ret=[toFloat $ round n]}
int env (Str _ "-c":[x]) = do n <- getFloat x; return env{status=True, ret=[toFloat $ ceiling n]}
int env (Str _ "-f":[x]) = do n <- getFloat x; return env{status=True, ret=[toFloat $ floor n]}
int env _ = usagePrint "int"

list env xs = return env{ret=[List xs]}
ulist env xs = return env{ret=ulist' xs}
  where ulist' [] = []
        ulist' (List x:xs) = x ++ ulist' xs
        ulist' (x:xs) = x:ulist' xs

bool env xs = return env{ret=Bool (status env):xs}
ubool env (Bool b:xs) = return env{status=b, ret=xs}

plus'' env xs = do
  ys <- mapM getFloat xs
  return env{ret=[Float $ foldl (+) 0 ys], status=True}

tmpdir = tmpdir' "" Nothing "snale"
tmpdir' t dir pref env xs =
  let (o, s, ys) = optHead xs t in
    case o of
      "d" -> do (x, ys) <- getSubOpt "tmpdir" s ys
                td <- expand env x
                tmpdir' s (Just $ T.unpack td) pref env ys
      "p" -> do (x, ys) <- getSubOpt "tmpdir" s ys
                p <- expand env x
                tmpdir' s dir (T.unpack p) env ys
      "" ->  do
        x <- liftIO $ maybe (withSystemTempDirectory pref) (\d->withTempDirectory d pref) dir $
               \d->runEval env $ eval (ys ++ [toStr $ T.pack d]) env
        case x of
          Right renv -> return renv
          Left  e    -> Eval $ throwError e
      _ -> usagePrint "tmpdir"

tmpfile = tmpfile' "" Nothing "snale"
tmpfile' :: T.Text -> Maybe String -> String -> Env -> [Val] -> Eval Env
tmpfile' t dir pref env xs =
  let (o, s, ys) = optHead xs t in
    case o of
      "d" -> do (x, ys) <- getSubOpt "tmpfile" s ys
                td <- expand env x
                tmpfile' s (Just $ T.unpack td) pref env ys
      "p" -> do (x, ys) <- getSubOpt "tmpfile" s ys
                p <- expand env x
                tmpfile' s dir (T.unpack p) env ys
      "" ->  do
        x <- liftIO $ maybe (withSystemTempFile pref) (\d->withTempFile d pref) dir $
               \d h->do hClose h
                        runEval env $ eval (ys ++ [toStr $ T.pack d]) env
        case x of
          Right renv -> return renv
          Left  e    -> Eval $ throwError e
      _ -> usagePrint "tmpfile"

isList env [] = return env{status=False, ret=[]}
isList env [x@List{}] = return env{status=True, ret=[x]}
isList env [x] = return env{status=False, ret=[x]}
isList env xs = return env{status=isList' xs, ret=xs}
  where
    isList' []          = True
    isList' (List{}:xs) = isList' xs
    isList' _           = False

check env (x:xs) = do
  paths <- map (mkPath env) <$> mapM (expand env) xs
  os <- getOptList . T.tail <$> expand env x
  if isJust $ find (\y->isNothing $ T.find (T.head y==) "erwxLsbcdfpSnoiOG") os then
    usagePrint "check"
  else do
    s <- liftIO $ (and <$> mapM (check' paths) os) `catchError` (errHandlerIO' False status env)
    return env{status=s, ret=xs}
  where
    check' :: [String] -> T.Text -> IO Bool
    check' files opt =
      case () of
        _ | opt == "e" ||opt == "r" || opt == "w" || opt == "x" -> do
              and <$> mapM (\x -> fileAccess x (opt == "r") (opt == "w") (opt == "x")) files
          | opt == "L" || opt == "s" -> do
              ss <- mapM getSymbolicLinkStatus files
              if opt == "L" then
                return $ all isSymbolicLink ss
              else
                return $ all ((/=0) . fileSize) ss
          | otherwise -> do
              rs <- mapM getFileStatus files
              case opt of
                "b" -> return $ all isBlockDevice rs
                "c" -> return $ all isCharacterDevice rs
                "d" -> return $ all isDirectory rs
                "f" -> return $ all isRegularFile rs
                "p" -> return $ all isNamedPipe rs
                "S" -> return $ all isSocket rs
                "n" -> return $ sortedBy (>=) $ map modificationTime rs
                "o" -> return $ sortedBy (<=) $ map modificationTime rs
                "i" -> return $ ((== 1) $ length $ group $ map deviceID rs) &&
                                ((== 1) $ length $ group $ map fileID rs)
                "O" -> do y <- getEffectiveUserID
                          return $ all (\x-> fileOwner x==y) rs
                "G" -> do y <- getEffectiveGroupID
                          return $ all (\x-> fileGroup x==y) rs
    sortedBy _ []       = True
    sortedBy _ [x]      = True
    sortedBy f (x:y:xs) = f x y && sortedBy f (y:xs)
    getOptList :: T.Text -> [T.Text]
    getOptList "" = []
    getOptList opt = let (o, t, _) = optHead [] opt in o:getOptList t

usage env [] = do
  liftIO $ hPutStrLn (out env) "usage command shows the usage of following built-in commands."
  liftIO $ mapM (TIO.hPutStrLn (out env)) $ sort $ H.keys defaultFuncs
  return env{status=True, ret=[]}
usage env [x] = do
  n <- expand env x
  liftIO $ hPutStrLn (out env) (usageShow n)
  return env{status=True, ret=[]}
usage env xs = Eval $ throwError ([fromEno eINVAL], NumArgs "1" $ length xs)

usageShow :: T.Text -> String
usageShow n =
  case H.lookup n defaultFuncs of
    Just (Prim _ _ usage) -> T.unpack $
      case usage of
        [] -> "sorry, no usage"
        _ -> formatUsage n usage
    _ -> T.unpack n ++ " is not a primitive function"

formatUsage :: T.Text -> [T.Text] -> T.Text
formatUsage n (x:xs) =
  T.intercalate "\n" $ T.unwords ["Usage:", n, x]:map (\x -> T.unwords ["      ", n, x]) xs

--fdToHandle :: Env -> Val -> Handle
--fdToHandle env (FD 1) = out env
--fdToHandle env (FD 2) = err env
--fdToHandle env (FD 0) = inn env

cd env x = do
  path <- if null x then liftIO $ getEnv "HOME"
                    else (normalise . mkPath env) <$> expand env (head x)
  e <- liftIO $ fileExist path
  if e then do
    f <- liftIO $ (isDirectory <$> getFileStatus path)
                  `catchError` \e -> hPrint (err env) e >> return False
    if f then liftIO $ (do
      setCurrentDirectory path
      return $ env {status=True, ret=[Float 0], dir=path}
      ) `catchError` errHandlerIO env
    else Eval $ throwError ([fromEno eNOENT], SomeError $ normalise path ++ " is not directory")
  else Eval $ throwError ([fromEno eNOENT], SomeError $ normalise path ++ ": no such file or directory")

mkPath :: Env -> T.Text -> String
mkPath env x =
  let path = T.unpack x in
    normalise $ if head path == '/' then path
                                    else dir env </> path

load env [] = return env
load env x@(y:xs) = do
  file <- expand env y
  let path = mkPath env file in do
    e <- liftIO $ fileExist path --todo
    if e then do
      code <- liftIO $ TIO.readFile (mkPath env file)
                       `catchError` \e -> hPrint (err env)  e >> return ""
      z <- liftIO $ runStateT (runParserT genEval "snale" code) (parenv env)
      case z of
        (Right val, rpenv) -> let nenv = env{parenv=rpenv} in do
          x <- liftIO $ runEval nenv $ val nenv
          case x of
            Right renv -> load renv xs
            Left e     -> Eval $ throwError e
        (Left e, s) -> do
          liftIO $ hPutStr (err env) (parseErrorTextPretty e)
          return env{status=False, ret=[fromEno eINVAL]}
    else
      Eval $ throwError ([fromEno eNOENT], SomeError $ path ++ ": no such file or directory")

return' env xs = do
  renv <- eval xs env
  Eval $ throwError (ret renv, Returned $ status renv)

colon env xs = return env{ret=xs}

true env xs = return env{ret=xs, status=True}

false env xs = return env{ret=xs, status=False}

exit' env [] = Eval $ throwError ([Float 0], Exited True)
exit' env (x:_) = do
  n <- getFloat x
  case n of
    0 -> Eval $ throwError ([Float 0], Exited True)
    _ | n > 0 -> Eval $ throwError ([Float n], Exited False)
      | otherwise -> Eval $ throwError ([fromEno eINVAL], SomeError $ show n ++ " is not natural number")

break' env xs  = Eval $ throwError (xs, Broken (status env))
breakT env xs  = Eval $ throwError (xs, Broken True)
breakF env xs  = Eval $ throwError (xs, Broken False)

echo env xs = do
  x <- mapM (expand env) xs
  liftIO $ TIO.hPutStrLn (out env) $ T.unwords x
  return env{status=True, ret=[Float 0]}

print env xs = do
  x <- mapM (expand env) xs
  liftIO $ TIO.hPutStr (out env) $ T.concat x
  return env{status=True, ret=[Float 0]}

show' env xs = do
  liftIO $ hPutStrLn (out env) $ unwords (map show xs)
  return env{ret=[Float 0], status=True}

get' env [] = return env
get' env xs = do
  ys <- mapM (parseNarName env) xs
  let zs = map (getVarMaybe env) ys in
    case sequence zs of
      Nothing -> return env{status=False, ret=[]}
      Just vs -> return env{status=True, ret=vs}
  where
    parseNarName env v = do
      t <- expand env v
      y <- liftIO $ runStateT (runParserT parseVar "snale" t) (parenv env)
      case y of
        (Right (Var var), _) -> return var
        _ -> return $ VarN "_"
--
--idx env [x, List xs] = do
--  n <- getIntForIdx xs x
--  return env{status=True, ret=[xs !! (n-1)]}
--idx env [x, y, List xs] = do
--  n <- getIntForIdx xs x
--  m <- getIntForIdx xs y
--  return env{status=True, ret=take (m-n+1) $ drop (n-1) xs}
--idx env xs = usagePrint "idx"

getIntForIdx :: [Val] -> Val -> Eval (Maybe Int)
getIntForIdx xs x = do
  n <- getInt x
  case n of
    0 -> return $ Nothing
    _ | abs n`less`xs -> return $ Nothing
      | n < 0 -> return $ Just $ length xs + n + 1
      | otherwise -> return $ Just n

setRet env (xs@(_:_:[])) =
  let vars = init xs
      cmd = last xs  in do
    renv <- eval [cmd] env
    venv <- foldM (\e->uncurry (setVar e)) env $ zip vars (ret renv)
    return venv{ret=ret renv}
setRet env xs = Eval $ throwError ([fromEno eINVAL], NumArgs "2 or more" $ length xs)

set env xs@(_:_:[]) =
  let v = last xs  in do
    renv <- foldM (\e name->setVar e name v) env (init xs)
    return renv{ret=[v]}
set env xs = Eval $ throwError ([fromEno eINVAL], NumArgs "2 or more" $ length xs)

setVar :: Env -> Val -> Val -> Eval Env
setVar env (Str _ name) v
  | name == "_" = return env{status=True}
  | otherwise = return $ env {vars=H.insert name v $ vars env, status=True}
setVar env (List x) (List v) =
  case zipMaybe x v of
    Just y -> do
      renv <- foldM (\e (n,y)->setVar e n y) env y
      if status renv then return renv
                     else return $ env{status=False, ret=ret renv}
    _ -> return $ env{status=False, ret=v}
setVar env _ v = return $ env{status=False, ret=[v]}

zipMaybe [] [] = Just []
zipMaybe (x:xs) (y:ys) = ((x, y):) <$> zipMaybe xs ys
zipMaybe _ _ = Nothing

--getOptEasy :: [T.Text] -> [T.Text] -> T.Text -> [Val] -> Eval ([Val], H.HashMap T.Text [Val])
--getOptEasy havArg noArg name (Str _ s:xs)
--  | s == "--" = return (xs, H.empty)
--  | T.isPrefixOf "--" s && elem s havArg = do
--    (vs, ret) <- getOptEasy havArg noArg name $ tail xs
--    case H.lookup s ret of
--      Just _  -> return (vs, H.adjust (head xs:) s ret)
--      Nothing -> return (vs, H.insert s [head xs] ret)
--  | T.isPrefixOf "--" s && elem s noArg = do
--    (vs, ret) <- getOptEasy havArg noArg name $ tail xs
--    return (vs, H.insert s [] ret)
--  | T.take 2 s `elem` havArg = do
--    (arg, ys) <- case () of
--                   _| T.length s == 2 && not (null xs) -> return (head xs, tail xs)
--                    | T.length s == 2 ->
--                      Eval $ throwError ([fromEno eNOENT], SomeError $ usageShow name)
--                    | otherwise -> return (Str _ $ T.drop 2 s, xs)
--    (vs, ret) <- getOptEasy havArg noArg name ys
--    if H.member (T.take 2 s) ret
--      then return (vs, H.adjust (arg:) (T.take 2 s) ret)
--      else return (vs, H.insert (T.take 2 s) [arg] ret)
--  | T.take 2 s `elem` noArg =
--    let ys = if T.length s == 2
--               then xs
--               else (Str _ $ "-" `T.append` T.drop 2 s):xs
--    in do
--      (vs, ret) <- getOptEasy havArg noArg name ys
--      return (vs, H.insert (T.take 2 s) [] ret)
--  | T.isPrefixOf "-" s && s /= "-" = Eval $ throwError ([fromEno eNOENT], SomeError $ usageShow name)
--getOptEasy havArg noArg name xs = return (xs, H.empty)

--exclusiveOpt :: [T.Text] -> T.Text -> H.HashMap T.Text [Val] -> Eval ()
--exclusiveOpt list name opt
--  | length (H.filterWithKey (\x y -> elem x list) opt) < 2 = return ()
--  | otherwise = Eval $ throwError ([fromEno eNOENT], SomeError $ usageShow name)

def env [Str _ name, body@Lambda{}] = def' env name body
def env (Str _ name: ys) = def' env name $ Prim Purely (\e _->return e{ret=ys}) []
def env [x, _] = Eval $ throwError ([fromEno eINVAL], TypeMismatch "string" x)
def env x = Eval $ throwError ([fromEno eINVAL], NumArgs "2" $ length x)

def' :: Env -> T.Text -> Val -> Eval Env
def' env name body = do
  fs <- liftIO $ readIORef $ funcs env
  liftIO $ writeIORef (funcs env) $ H.insert name body fs
  renv <- incFunID env
  return renv{status=True, ret=[]}

optHead :: [Val] -> T.Text -> (T.Text, T.Text, [Val])
optHead xs t | t /= "" = (T.take 1 t, T.tail t, xs)
optHead xs@(Str _ "":_) t = ("", t, xs)
optHead xs@(Str _ "-":_) t = ("", t, xs)
optHead xs@(Str _ "--":_) t = ("", t, xs)
optHead (Str _ x:xs) t | T.take 2 x == "--" = (T.drop 2 x, t, xs)
                     | T.head x == '-' = case T.drop 2 x of
                                           "" -> (T.tail x, t, xs)
                                           _  -> (tIndex x 1, T.drop 2 x, xs)
  where tIndex t n = T.singleton $ T.index t n
optHead xs t = ("", t, xs)

getSubOpt :: T.Text -> T.Text -> [Val] -> Eval (Val, [Val])
getSubOpt n "" [] = usagePrint n
getSubOpt _ "" (x:xs) = return (x, xs)
getSubOpt _ t xs = return (toStr t, xs)

read' env xs = read'' "" False True (-1) "\n" env xs
read'' :: T.Text -> Bool -> Bool -> Int -> String -> Env -> [Val] -> Eval Env
read'' t all delNL nChar sep env xs =
  let (o, s, ys) = optHead xs t in
    case o of
      "a" -> read'' s True delNL (-1) "" env ys
      "N" -> read'' s True False nChar sep env ys
      "n" -> do (x, ys) <- getSubOpt "read" s ys
                n <- getInt x
                read'' "" False delNL n sep env ys
      "s" -> do (x, ys) <- getSubOpt "read" s ys
                s <- T.unpack <$> expand env x
                read'' "" False delNL nChar s env ys
      "" -> do
        eof <- liftIO $ hIsEOF (inn env)
        if eof then 
          return env{status=False, ret=[]}
        else
          if nChar > 0 || sep /= "\n" then do
            cs <- liftIO $ hGetNChar sep nChar (inn env)
            if null cs then
              return env{status=True, ret=[toStr ""]}
            else
              return env{status=True, ret=[toStr $ T.pack cs]}
          else
            if all then do
              x <- liftIO $ TIO.hGetContents (inn env)
              if delNL then
                return env{status=True, ret=[toStr $ delNewLn x]}
              else
                return env{status=True, ret=[toStr x]}
            else do
              x <- liftIO $ TIO.hGetLine (inn env)
              return env{status=True, ret=[toStr x]}
  where
    hGetNChar :: String -> Int -> Handle -> IO String
    hGetNChar s 0 h = return []
    hGetNChar s n h = do
      eof <- liftIO $ hIsEOF h
      if eof then
        return []
      else do
        c <- hGetChar h
        if elem c s then
          return []
        else
          (:) c <$> hGetNChar s (n-1) h
    delNewLn "" = ""
    delNewLn c = case T.last c of
                   '\n' -> delNewLn $ T.init c
                   _    -> c

glob env [] = return env
glob env xs = do
  s <- mapM (expand env) xs
  p <- liftIO $ mapM ((\x -> globDir1' x (dir env)) . T.unpack) s
  case p of
    [] -> return env {status=False, ret=[]}
    _  -> return env {status=True, ret=map (toStr . T.pack) $ concat p}
  where
    globDir1' x@('/':_) dir = G.globDir1 (G.compile x) dir
    globDir1' x dir = map (makeRelative dir) <$> G.globDir1 (G.compile x) dir

split env [z, x] = do
  s <- expand env z
  t <- expand env x
  return env{status=True, ret=map toStr $ T.splitOn s t}
split env [x] = do
  t <- expand env x
  return env{status=True, ret=map toStr $ T.words t}
split env _ = usagePrint "split"

fork env xs = do
  liftIO $ E.mask_ $ do
    hs <- signalHandleClear
    pid <- P.forkProcessWithUnmask $ \m -> do
      P.getProcessID >>= P.createProcessGroupFor
      tinfo <- allocThreadInfo
      runEvalMain env{thread=tinfo} (eval xs env{thread=tinfo}) >>= exitEval
    signalHandleRestore hs
    return env {status=True, ret=[]}

timeo env ys@(x:xs) = do
  n <- getFloat x
  if n <= 0 then
    Eval $ throwError ([fromEno eNOENT], SomeError $ show n ++ " is not positive number")
  else do
    y <- liftIO $ timeout (floor $ n*1000000) $ runEval env $ eval xs env
    case y of
      Just (Right renv) -> return $ setRetEnv env renv
      Just (Left e)     -> Eval $ throwError e
      Nothing           -> return env{status=False, ret=[]}

keep' env xs = do
  x <- liftIO $ runEvalKeep env $ eval xs env
  case x of
    Right renv -> return renv
    Left e     -> Eval $ throwError e

not' env x = do
  renv <- eval x env
  if status renv
    then return renv{status=False}
    else return renv{status=True}

sub env x = do
  y <- mapM (expand env) x
  case map T.unpack y of
    [reg, rep, src] -> return env{status=True, ret=[toStr $ T.pack $ subStr reg rep src]}
      where
        subStr :: String -> String -> String -> String
        subStr reg rep src | match == "" = src
                           | otherwise = skipStr ++ (rep ++ subStr reg rep leftStr)
          where (skipStr, match, leftStr) = src =~ reg ::(String,String,String)
    _ -> usagePrint "sub"

mapr' env xs = case xs of
  _:_:_ -> case last xs of
    List l -> do
      envs <- mapM' env (\x-> eval (init xs ++ [x]) env) l
      return env {ret=concat $ map ret envs, status=all status envs}
    _ -> usagePrint "map"
  _ -> usagePrint "map"
  where
    mapM' :: Env -> (Val -> Eval Env) -> [Val] -> Eval [Env]
    mapM' _ _ [] = return []
    mapM' env f (x:xs) = do
      y <- case x of
             List l -> do
               envs <- mapM' env f l
               return env {ret=[List $ concat $ map ret envs], status=all status envs}
             _ -> f x
      (y:) <$> mapM' env f xs

map' = map'' "" 1
map'' t nopt env xs = let (o, s, ys) = optHead xs t in
  case o of
    "n" -> do (x, ys) <- getSubOpt "map" s ys
              n <- getInt x
              map'' "" n env ys
    ""  -> case ys of
      _:_:_ -> let y = last ys in case y of
        List l -> do
          envs <- mapM (\x-> eval (init ys ++ x) env) (foldListN nopt l)
          return env {ret=concat $ map ret envs, status=all status envs}
        Dict d -> do
          envs <- mapM (\x-> eval (init ys ++ [x]) env) d
          return env {ret=[Dict $ H.map (head . ret) $ H.filter (not . null . ret) envs], status=and $ H.map status envs}
        _ -> usagePrint "map"
      _ -> usagePrint "map"
    _ -> usagePrint "map"
  where
    foldListN n [] = []
    foldListN n l = take n l : foldListN n (drop n l)

foldr = fold'' foldM
fold' = fold'' foldM'

fold'' foldFn env xs = case xs of
  f:vs@(_:_:_) -> case last vs of
    List l -> setRetEnv env <$>
                foldFn (\e x -> eval (f : ret e ++ [x]) env) env{ret=init vs} l
    _ -> usagePrint "fold"
  _ -> usagePrint "fold"

foldM' :: (Env -> Val -> Eval Env) -> Env -> [Val] -> Eval Env
foldM' _ env [] = return env
foldM' f env (x:xs) = do
  y <- case x of
         List ys ->
           setRetEnv env <$>
             foldM' f env ys
         _ -> f env x
  foldM' f y xs

filter' env xs@(_:_:_) = let l = last xs in
  case l of
    List l -> do
      vs <- filterM (\x-> status <$> eval (init xs ++ [x]) env) l
      return env{ret=vs, status=null vs}
    _ -> usagePrint "filter"
filter' env _ = usagePrint "filter"

len' env xs = return env{status=True, ret=map (toFloat . olen) xs}
  where
    olen (List xs) = length xs
    olen _         = 1

lenc env xs = do
  l <- mapM (\x->toFloat . T.length <$> expand env x) xs
  return env{status=True, ret=l}

getenv env [x] = do
  y <- expand env x >>= liftIO . getEnv . T.unpack
  return env{status=True, ret=[toStr $ T.pack y]}
getenv env x = Eval $ throwError ([fromEno eINVAL], NumArgs "1" $ length x)

setenv env xs@[_, _] = do
  [n, v] <- map T.unpack <$> mapM (expand env) xs
  liftIO $ setEnv n v
  return env{status=True, ret=[toStr $ T.pack v]}
setenv env x = Eval $ throwError ([fromEno eINVAL], NumArgs "2" $ length x)

loop env (Str _ "-n":x) = loop' env x
loop env x = do
  y <- liftIO $ runEvalKeep env $ loop' env x
  case y of
    Right renv -> return $ setRetEnv env renv
    Left e     -> Eval $ throwError e

loop' env x@(cmd:arg) = do
  y <- liftIO $ runEvalFunc env $ eval x env
  case y of
    Right renv -> loop env (cmd:ret renv)
    Left e     -> Eval $ throwError e

trap env [] = usagePrint "trap"
trap env (f:ys) = do
  singnals <- case ys of
                [] -> return defaultSignal
                _  -> mapM txSignal ys
  liftIO $ do
    tinfo <- readIORef $ thread env
    case cmdMvar tinfo of
      Just _ -> return ()
      _ -> do
        mvar <- newEmptyMVar
        writeIORef (thread env) tinfo{cmdMvar=Just mvar}

  liftIO $ mapM (
             \x -> case x of
                     0 -> do tinfo <- readIORef (thread env)
                             writeIORef (thread env) tinfo{exitTrap=f}
                     _ -> do installHandler x (Catch $ trapHandler env f) Nothing
                             return ()
                 ) singnals
  return env{status=True, ret=[]}
  where
    trapHandler :: Env -> Val -> IO ()
    trapHandler env f = do
      x <- runEvalFunc env $ eval [f] env
      tinfo <- readIORef $ thread env
      let ThreadInfo tid exitMvar (Just cmdMvar) _ = tinfo in
        case x of
          Left (v, Exited s) -> do
            putMVar exitMvar env{status=s, ret=v}
            throwTo tid E.ThreadKilled
          _ -> putMVar cmdMvar ()
    txSignal (Str _ s) =
      case H.lookup s signalMap of
        Just x -> return x
        _ -> Eval $ throwError ([fromEno eINVAL], SomeError $ show s ++ " is not signal")

data Val = Float Double
         | Str (Maybe(IORef Cache)) T.Text
--         | Sym T.Text
         | Dict (H.HashMap T.Text Val)
         | Bool Bool
         | FD Word
         | VarM VarT
         | Var VarT
         | LinkedStr [Val]
         | Lambda LambdaType ParseEnv (Maybe Env) (Env -> Eval Env)
         | List [Val]
         | Prim LambdaType (Env -> [Val] -> Eval Env) [T.Text]
         | PrimFS (Env -> [Val] -> Eval Env)
         | Rd ((Env -> Eval Env) -> Env -> Eval Env)
instance Show Val where
  show (Float x)          = show x
  show (Str _ x)            = show x
--  show (Sym x)            = show x
  show (Bool True)        = "true"
  show (Bool False)       = "false"
  show (FD x)             = "&" ++ show x
  show (VarM x)           = "$@" ++ show x
  show (Var x)            = "$" ++ show x
  show (LinkedStr x)      = concatMap show x
  show Lambda{}           = "{LAMBDA}"
  show (List x)           = show x
  show Prim{}             = "_PRIMITIVE_"
  show PrimFS{}           = "_PRIMITIVE_"
instance Eq Val where
  (==) (Float x) (Float y)   = x == y
  (==) (Str _ x) (Str _ y) = x == y
--  (==) (Sym x) (Sym y) = x == y
  (==) (Bool x) (Bool y)     = x == y
  (==) (FD x) (FD y)         = x == y
  (==) _ _                   = False
instance Ord Val where
  compare (Float x) (Float y) = compare x y
  compare (Str _ x) (Str _ y) = compare x y
  compare (Float x) (Str _ y) = compare (T.pack $ show x) y
  compare (Str _ x) (Float y) = compare x (T.pack $ show y)

data Cache = NoCache
           | Cache {clsrId :: Int, cache :: Val}
           | NoFunc Int

data VarT = VarN T.Text
          | VarA Int
          | VarR Int
instance Show VarT where
  show (VarN x) = show x
  show (VarA x) = show x
  show (VarR x) = "?" ++ show x

toStr = Str Nothing

toFloat :: Integral a => a -> Val
toFloat = Float . fromIntegral . toInteger

fromEno :: Errno -> Val
fromEno (Errno n) = Float $ fromIntegral $ toInteger n

toInt :: Errno -> Int
toInt (Errno n) = fromIntegral $ toInteger n

data JobInfo = Threaded ThreadId (MVar Env)
             | Forked ProcessID
instance Show JobInfo where
  show Threaded{} = "_THREAD_"
  show Forked{}   = "_PROCESS_"

valExpand :: Env -> [Val] -> Eval [Val]
valExpand _ [] = return []
valExpand env (Var x:xs) = do
  y <- getVar env x
  (y :) <$> valExpand env xs
valExpand env (VarM (VarA (-1)):xs) = (args env ++) <$> valExpand env xs
valExpand env (VarM (VarR n):xs) | n < 1 = (ret env ++) <$> valExpand env xs
valExpand env (VarM x:xs) = do
  y <- getVar env x
  case y of
    List vs -> (vs ++) <$> valExpand env xs
    _       -> (y :)   <$> valExpand env xs
valExpand env (Lambda Expand _ Nothing x:xs) = do
  renv <- x env
  (ret renv ++) <$> valExpand env xs
valExpand env (Lambda PipeRd _ Nothing x:xs) = do
  (i,o) <- liftIO $ PI.createPipe
  liftIO $ PI.setFdOption o PI.CloseOnExec True
  h <- liftIO $ PI.fdToHandle o
  spawn env $ do x env{out=h}
                 liftIO $ hClose h
                 return env
  (toStr ("/dev/fd/" `T.append` TS.showt i) :) <$> valExpand env xs
valExpand env (Lambda PipeWt _ Nothing x:xs) = do
  (i,o) <- liftIO $ PI.createPipe
  liftIO $ PI.setFdOption i PI.CloseOnExec True
  h <- liftIO $ PI.fdToHandle i
  spawn env $ do x env{inn=h}
                 liftIO $ hClose h
                 return env
  (toStr ("/dev/fd/" `T.append` TS.showt o) :) <$> valExpand env xs
valExpand env (Lambda f p Nothing x:xs) = (Lambda f p (Just env) x:) <$> valExpand env xs
valExpand env (List x:xs) = do
  v <- valExpand env x
  (:) (List v) <$> valExpand env xs
valExpand env (LinkedStr x:xs) = do
 y:ys <- valExpand env x >>= mapM (expandL env)
 (List (map toStr (foldl' expLinkedStr y ys)) :) <$> valExpand env xs
  where
    expLinkedStr xs ys = do
      x <- xs
      y <- ys
      return $ x `T.append` y
valExpand env (x:xs) = (x:) <$> valExpand env xs

valExpandMaybe :: Env -> Maybe Env -> [Val] -> Eval [Val]
valExpandMaybe _ (Just env) xs = valExpand env xs
valExpandMaybe env _ xs        = valExpand env xs

getVar :: Env -> VarT -> Eval Val
getVar _ (VarN "~") = toStr . T.pack <$> liftIO (getEnv "HOME")
getVar e v = case getVarMaybe e v of
               Just val -> return val
               _        -> Eval $ throwError ([fromEno ePERM], UnboundVar (show v))

getVarMaybe :: Env -> VarT -> Maybe Val
getVarMaybe env (VarA (-1)) = Just $ List $ args env
getVarMaybe env (VarA n) = let vs = args env in
  if greater n vs then Just $ vs !! (n-1)
                  else Nothing
getVarMaybe env (VarR (-1)) = Just $ List $ ret env
getVarMaybe env (VarR 0) = getVarMaybe env (VarR 1)
getVarMaybe env (VarR n) = let vs = ret env in
  if greater n vs then Just $ vs !! (n-1)
                  else Nothing
getVarMaybe env (VarN t) = H.lookup t $ vars env

greater :: Int -> [a] -> Bool
greater n _      | n < 1 = True
greater _ []     = False
greater n (_:xs) = greater (n-1) xs

less x y = not $ greater x y

expand :: Env -> Val -> Eval T.Text
expand env (Float x) = return $ let i = round x in
                                  if fromInteger i == x then TS.showt i
                                                        else TS.showt x
expand env (Str _ x) = return x
expand env (Bool True) = return "true"
expand env (Bool False) = return "false"
expand env (FD x) = return $ "&" `T.append` TS.showt x
--expand env (Lambda _ (Just fenv) vs) = return "{LAMBDA}"
expand env (List vs) = T.unwords <$> mapM (expand env) vs
expand env x = Eval $ throwError ([fromEno eINVAL], TypeMismatch "expandable value" x)

expandL :: Env -> Val -> Eval [T.Text]
--expandL env (Lambda _ (Just fenv) vs) = return ["{LAMBDA}"]
expandL env (List vs)                 = concat <$> mapM (expandL env) vs
expandL env x                         = (:[]) <$> expand env x

getInt :: Val -> Eval Int
getInt (Float f) = return $ floor f
getInt (Str _ s) = case (TR.signed TR.rational) s of
                     Right (f, "") -> return $ floor f
                     _ -> case (TR.signed TR.hexadecimal) s of
                       Right (d, "") -> return d
                       _ -> Eval $ throwError ([], SomeError $
                            show s ++ " cannot be read as number")
getInt x = Eval $ throwError ([fromEno eINVAL], SomeError $ show x ++ " cannot be read as number")

readInt :: Val -> Maybe Int
readInt x =
  case x of
    Str _ s ->
      case TR.signed TR.decimal s of
        Right (d, "") -> Just d
        _             -> Nothing
    Float f -> Just $ floor f
    _ -> Nothing

data LambdaType = Normal
                | NoArgs
                | Expand
                | Purely
                | PipeRd
                | PipeWt
  deriving Eq

data ShError = Returned     Bool
             | Exited       Bool
             | Broken       Bool
             | SomeError    String
             | NumArgs      String Int
             | TypeMismatch String Val
             | UnboundVar   String
             | Internal     String
instance Show ShError where
  show (SomeError    s)   = s
  show (Internal     s)   = "Internal error: " ++ s
  show (NumArgs      s n) = "Expected " ++ s ++ " args, found values " ++ show n
  show (TypeMismatch s v) = "Invalid type: expected " ++ s ++ ", found " ++ show v
  show (UnboundVar   t)   = "Getting an unbound variable: " ++ t

newtype Eval a = Eval (ExceptT ([Val], ShError) IO a)
  deriving (Functor,
            Applicative,
            Monad,
            MonadIO,
            MonadError ([Val], ShError))

throwShError :: Env -> ([Val], ShError) -> Eval a
throwShError env (v, e) = do liftIO $ hPrint (err env) (show e)
                             Eval $ throwError (v, e)

--
-- parser
--

data ParseEnv = ParseEnv {allocCount :: Int
                        , addArg :: [Val] -> [Val]
                        , defFn :: HS.HashSet T.Text
                        , defPureFn :: HS.HashSet T.Text
                        , unDefPureFn :: HS.HashSet T.Text
--                        , varCount :: Int
--                        , lexDepth :: Int
--                        , varInfo :: H.HashMap T.Text (Int, Int)
                        , isErr :: Maybe Custom
                        , parseFlags :: Flags}
defaultParseEnv = ParseEnv 0 id HS.empty HS.empty HS.empty {-0 0 (H.empty)-} Nothing (Flags False False)
incAllocCount = modify (\x->x{allocCount=allocCount x + 1})
--incVarCount = modify (\x->x{varCount=varCount x + 1})

setPureFn :: T.Text -> Parser ()
setPureFn name = do x <- get
                    if not (interactiveMode $ parseFlags x) && (HS.member name (defPureFn x) || HS.member name (defFn x))
                      then multiDefine name
                      else put x{defPureFn=HS.insert name $ defPureFn x,
                                 unDefPureFn=HS.delete name $ unDefPureFn x}
chkPureFn :: T.Text -> Parser ()
chkPureFn name = do x <- get
                    if HS.member name (defFn x) then
                      notPureFunc name
                    else if HS.member name (defPureFn x) then
                      return ()
                    else
                      put x{unDefPureFn=HS.insert name $ unDefPureFn x}

setFn :: T.Text -> Parser ()
setFn name = do x <- get
                if not (interactiveMode $ parseFlags x) && (HS.member name (defPureFn x) || HS.member name (defFn x)) then 
                  multiDefine name
                else if HS.member name (unDefPureFn x) then
                  notPureFunc name
                else
                  put x{defFn=HS.insert name $ defFn x}
  
--setDefVar :: T.Text -> Parser ()
--setDefVar name = modify $ \x->x{defVar=HS.insert name $ defVar x}
--
--chkVar :: T.Text -> Parser ()
--chkVar name = do x <- get
--                 if HS.member name $ defVar x
--                   then return ()
--                   else notDefined name

data Custom = NotPureFunc T.Text
            | MultiDefine T.Text
            | NotDefined  T.Text
            | NotLiteral  T.Text
            | InvalidFormat String
  deriving (Eq, Show, Ord)

instance ShowErrorComponent Custom where
  showErrorComponent (NotPureFunc txt) = T.unpack txt ++ " is not a pure function"
  showErrorComponent (MultiDefine txt) = "multiple definition of " ++ T.unpack txt
  showErrorComponent (NotDefined txt) = "no definition of " ++ T.unpack txt
  showErrorComponent (NotLiteral txt) = "need literal for " ++ T.unpack txt
  showErrorComponent (InvalidFormat txt) = "invalid format specifier " ++ txt

throwSyntax :: Custom -> Parser (Env -> Eval Env)
throwSyntax e = do modify $ \x->x{isErr=Just e}
                   return return

notPureFunc :: T.Text -> Parser ()
notPureFunc t = modify $ \x->x{isErr=Just $ NotPureFunc t}

multiDefine :: T.Text -> Parser ()
multiDefine t = modify $ \x->x{isErr=Just $ MultiDefine t}

notDefined :: T.Text -> Parser ()
notDefined t = modify $ \x->x{isErr=Just $ NotDefined t}

notLiteral :: T.Text -> Parser ()
notLiteral t = modify $ \x->x{isErr=Just $ NotLiteral t}

invalidFormat :: String -> Parser ()
invalidFormat t = modify $ \x->x{isErr=Just $ InvalidFormat t}

chkParseErr = do x <- get
                 maybe (return ()) customFailure $ isErr x

type Parser = ParsecT Custom T.Text (StateT ParseEnv IO)

--
-- eval
--

evalScript :: Env -> [[Val]] -> Eval Env
evalScript = foldM _eval
  where
    _eval env (PrimFS fn:xs) = valExpand env xs >>= fn env
    _eval env xs             = eval xs env

eval = eval' normalDispatch
evalPure = eval' pureDispatch

valExpandInc :: Env -> [Val] -> [Val] -> Eval ([Val], [Val])
valExpandInc env [] [] = return ([], [])
valExpandInc env [] (x:xs) = do
  ys <- valExpand env [x]
  case ys of
    [] -> valExpandInc env [] xs
    _ -> return (ys, xs)
valExpandInc env xs ys = return (xs, ys)

eval' :: (Env -> [Val] -> Eval Env) -> [Val] -> Env -> Eval Env
eval' d xs env = do
  (ys, zs) <- valExpandInc env [] xs
  case ys of
    [] -> return env
    f@(Lambda Expand _ _ _):ys -> do
      renv <- d env [f]
      boolDispatch renv (status renv) ys zs
    Bool b:ys -> boolDispatch env b ys zs
    Dict x:ys -> valExpand env zs >>= (dictDispatch x env) . (ys ++)
    List x:ys -> valExpand env zs >>= (listDispatch x env) . (ys ++)
    ys -> valExpand env zs >>= (d env) . (ys ++)

boolDispatch :: Env -> Bool -> [Val] -> [Val] -> Eval Env
boolDispatch env b xs ys = do
  (xs2, ys2) <- valExpandInc env xs ys
  case xs2 of
    [] -> return env
    x:xs2 -> if b then return env{ret=[x], status=True}
             else do zs <- valExpand env ys2
                     return env{ret=xs2 ++ zs, status=True}

dictDispatch :: (H.HashMap T.Text Val) -> Env -> [Val] -> Eval Env
dictDispatch d env [] = return env{ret=dump d, status=True}
  where
    dump d = concat $ map (\(k, v)->[toStr k, v]) $ H.toList d
dictDispatch d env [x] = do
  k <- expand env x
  case H.lookup k d of
    Just v -> return env{ret=[v], status=True}
    _ -> return env{ret=[], status=False}
dictDispatch d env xs = do
  ss <- foldList2 <$> mapM (expand env) xs
  let nd = H.filterWithKey (\k _->include k ss) d in
    return env{ret=[Dict nd], status=not $ null nd}
  where
    include k ss = maybe False (\_->True) $ find (\(s, e)->k>=s && k<=e) ss

foldList2 [] = []
foldList2 [x] = [(x, x)]
foldList2 (x:y:xs) = (x, y):foldList2 xs

foldList2' [] = []
foldList2' [x] = []
foldList2' (x:y:xs) = (x, y):foldList2 xs

ins env x@(_:_:_) =
  case last x of
    Dict d -> ins' env d (init x)
    _ -> usagePrint "ins"
  where
    ins' env d [] = return env
    ins' env d [x] = return env
    ins' env d (x:v:xs) = do
      k <- expand env x
      ins' env (H.insert k v d) xs
ins env _ = usagePrint "ins"
  
del env (x:_) =
  case last x of
    Dict d -> del' env d (init x)
    _ -> usagePrint "del"
  where
    del' env d [] = return env
    del' env d [x] = return env
    del' env d (x:xs) = do
      k <- expand env x
      del' env (H.delete k d) xs
del env _ = usagePrint "del"

--listDispatch d env [] = let vs = map toStr $ H.keys d in
--                          return env{ret=vs, status=not $ null vs}

listDispatch :: [Val] -> Env -> [Val] -> Eval Env
listDispatch d env [] = return env{ret=d, status=True}
listDispatch d env [x] = do
  y <- getIntForIdx d x
  case y of
    Just n -> return env{status=True, ret=[d !! (n-1)]}
    _ -> return env{status=False, ret=[]}
listDispatch d env xs = do
  ss <- foldList2 <$> map (maybe (-1) id) <$> mapM (getIntForIdx d) xs
  let nd = concat $ map (\(n, m)->take (m-n+1) $ drop (n-1) d) ss in
    return env{status=not $ null nd, ret=nd}

normalDispatch :: Env -> [Val] -> Eval Env
normalDispatch env [] = return env
normalDispatch env (Prim _ fn _:args) = fn env args
normalDispatch env (x@Lambda{}:args) = evalFn env x args
normalDispatch env (Str r s:args) = 
  case r of
    Just x -> do
      c <- liftIO $ readIORef x
      case c of
        Cache i fn | i == (funID env) -> normalDispatch env (fn:args)
        NoFunc i | i == (funID env) -> mapM (expand env) args >>= \ys-> liftIO $ cmdExec s ys env
        _ -> do
           funcs <- liftIO $ readIORef $ funcs env
           case H.lookup s funcs of
             Just fn -> do
               liftIO $ writeIORef x $ Cache (funID env) fn
               normalDispatch env (fn:args)
             _ -> do
               liftIO $ writeIORef x $ NoFunc (funID env)
               mapM (expand env) args >>= \ys-> liftIO $ cmdExec s ys env 
    _ -> do 
      funcs <- liftIO $ readIORef $ funcs env
      case H.lookup s funcs of
        Just fn -> normalDispatch env (fn:args)
        _ -> mapM (expand env) args >>= \ys-> liftIO $ cmdExec s ys env 
normalDispatch env (x:args) = do
  t <- expand env x
  normalDispatch env (toStr t:args)

pureDispatch :: Env -> [Val] -> Eval Env
pureDispatch env [] = return env
pureDispatch env (Prim x fn _:args) | x == Purely || x == Expand = fn env args
pureDispatch env (y@(Lambda x _ _ _):args) | x == Purely || x == Expand = evalFn env y args
pureDispatch env (Str r s:args) = do
  case r of
    Just x -> do
      c <- liftIO $ readIORef x
      case c of
        Cache i fn | i == (funID env) -> pureDispatch env (fn:args)
        _ -> do
           funcs <- liftIO $ readIORef $ funcs env
           case H.lookup s funcs of
             Just fn -> do
               liftIO $ writeIORef x $ Cache (funID env) fn
               pureDispatch env (fn:args)
             _ -> Eval $ throwError ([fromEno eINVAL], SomeError "not a pure function")
    _ -> do 
      funcs <- liftIO $ readIORef $ funcs env
      case H.lookup s funcs of
        Just fn -> pureDispatch env (fn:args)
        _ -> Eval $ throwError ([fromEno eINVAL], SomeError "not a pure function")
pureDispatch _ _ = Eval $ throwError ([fromEno eINVAL], SomeError "not a pure function")

--evalMac :: Env -> Val -> [Val] -> Eval Env
--evalMac env (Lambda Normal _ body) arg = do
--  renv <- body env{args=arg}
--  return renv{args=args renv}
--evalMac env (Lambda NoArgs _ body) args = body env
--evalMac env x args = Eval $ throwError ([fromEno eINVAL], TypeMismatch "functon" x)

evalFn :: Env -> Val -> [Val] -> Eval Env
evalFn env (Lambda mode penv fenv body) arg = do
  genv <- genFuncEnv env penv fenv
  x <- liftIO $ runEvalFunc env $ body
         $ if mode == Expand || mode == NoArgs then genv
                                               else genv{args=arg}
  case x of
    Right renv -> return $ setRetEnv env renv
    Left e     -> Eval $ throwError e
evalFn env (Prim _ fn usage) args = fn env args

genFuncEnv :: Env -> ParseEnv -> Maybe Env -> Eval Env
genFuncEnv env penv Nothing =
--  rfs <- liftIO $ readIORef (funcs env) >>= newIORef
  
  return env{status=True, parenv=penv}
genFuncEnv env penv (Just fenv) =
--  rfs <- liftIO $ readIORef (funcs fenv) >>= newIORef
  return env{status=True, ret=ret fenv, vars=vars fenv, funcs=funcs fenv, parenv=penv}

genFuncSpace :: (Env -> Eval Env) -> Env -> Eval Env
genFuncSpace f env = do
  new <- liftIO $ readIORef (funcs env) >>= newIORef
  f env{funcs=new}

cmdExec :: T.Text -> [T.Text] -> Env -> IO Env
cmdExec c cs env = (do
    (_,_,_,h) <- createProcess_ "snale"
                   (proc (T.unpack c) (map T.unpack cs))
                   { std_in    = UseHandle $ inn env,
                     std_out   = UseHandle $ out env,
                     std_err   = UseHandle $ err env,
                     close_fds = False,
                     cwd       = Just $ dir env }
    ecode <- waitForProcess h
    case ecode of
      ExitSuccess     -> return env{status=True, ret=[Float 0]}
      ExitFailure ret ->
        if ret < 0
          then do
            tinfo <- readIORef $ thread env
            case cmdMvar tinfo of
              Just mvar -> takeMVar mvar >> return env{status=False, ret=[toFloat ret]}
              _ -> return env{status=False, ret=[toFloat ret]}
          else return env{status=False, ret=[toFloat ret]}
    ) `catchError` (errHandlerIO env)

errHandlerIO' printMsg f env e = do if printMsg then hPrint (err env) e
                                                else return ()
                                    n <- getErrno
                                    return $ f env{status=False, ret=[fromEno n]}

errHandlerIO = errHandlerIO' True id

--
-- completion
--

wordlist :: Env -> IO [String]
wordlist env = do
  p <- getEnv "PATH"
  c <- mapM (\x -> listDirectory x `catchError` (\e->return [])) $ S.splitOn ":" p
  funcs <- readIORef $ funcs env
  return $ nub $ concat c ++ map T.unpack (H.keys funcs)

unquotedCompleteFn :: Env -> CompletionFunc IO
unquotedCompleteFn env line@(left,_)
  | isArg left   = completeFilename line
  | otherwise    = completeWord (Just '\\') " \t;|>&}" (searchfn env) line

searchfn :: Env -> String -> IO [Completion]
searchfn env s = do
  w <- wordlist env
  return $ map simpleCompletion $ filter (isPrefixOf s) w

isArg :: String -> Bool
isArg s =
  let x = words $ isArg' "" s in
    case x of
      []  -> False
      [_] -> case head s of
               ' '  -> True
               '\t' -> True
               _    -> False
      _ -> True
  where
    isArg' :: String -> String -> String
    isArg' x [] = x
    isArg' x xs =
       case xs of
         '|':'|':ys -> x
         '&':'&':ys -> x
         '>':'-':ys -> x
         ';':ys     -> x
         '|':ys     -> x
         '}':ys     -> x
         y  :ys     -> isArg' (y:x) ys

completeFn :: Env -> CompletionFunc IO
completeFn env = completeQuotedWord (Just '\\') "'" listFiles $ unquotedCompleteFn env

--
-- main ~ repl
--

repl :: FilePath -> Prefs -> Env -> String -> IO ()
repl home pref env str = do
  e <- fileExist $ home </> ".snalerc"
  renv <- if e 
            then runEvalMain env (load env [toStr $ T.pack $ home </> ".snalerc"])
                   `E.catch` killThreadHandler env
            else return env
  tryloop home renv >>= exitEval . fst
  return ()
  where
    tryloop home env = do
      renv <- runInputTWithPrefs pref setting
                $ handle (\Interrupt -> return env)
                $ withInterrupt
                $ loop str ""
--      putStrLn $ show rpenv
      setCurrentDirectory (dir renv)
      tryloop home renv
      where
        setting =
          Settings { historyFile = Just (home ++ "/.snale")
                   , complete = completeFn env
                   , autoAddHistory = True
                   }

        loop :: String -> T.Text -> InputT IO Env
        loop str lines = do
          x <- getInputLine str
          case x of
            Nothing -> liftIO $ exitEval env >> return env
            Just s  -> do
--              case parse script "snale" cmd of
--                Right val ->
--                  liftIO $ runEvalMain env $ evalScript env val
--                Left err -> loop "  " cmd
              y <- liftIO $ runStateT (runParserT genEval "snale" cmd) (parenv env)
              case y of
                (Right val, rpenv) -> let nenv = env{parenv=rpenv} in do
                  z <- liftIO $ runEvalMain nenv $ val nenv
--                  liftIO $ putStrLn (show $ rpenv)
                  return z
                (Left e, rpenv) ->
                  case isErr rpenv of
                    Just _ -> do liftIO $ hPutStr stderr $ parseErrorTextPretty e
                                 return env
                    _ -> loop "  " cmd
              where
                cmd = mergeLines lines (T.pack s)
                mergeLines "" x = x`T.append`"\n"
                mergeLines y x  = y`T.append`x`T.append`"\n"


runEval :: Env -> Eval Env -> IO (Either ([Val], ShError) Env)
runEval env (Eval fn) = runExceptT $ catchError fn $ handler env
  where
    handler :: Env -> ([Val], ShError) -> ExceptT ([Val], ShError) IO Env
    handler env e@(v, Returned s) = throwError e
    handler env e@(v, Broken s)   = throwError e
    handler env e@(v, Exited s)   = throwError e
    handler env e                 = ignoreError env e

runEvalTry :: Env -> Eval Env -> IO (Either ([Val], ShError) Env)
runEvalTry env (Eval fn) = runExceptT $ catchError fn $ handler env
  where
    handler :: Env -> ([Val], ShError) -> ExceptT ([Val], ShError) IO Env
    handler env e@(v, Returned s) = throwError e
    handler env e@(v, Broken s)   = throwError e
    handler env e@(v, Exited s)   = throwError e
    handler env (v, e) = return env{status=False, ret=v}

runEvalFunc :: Env -> Eval Env -> IO (Either ([Val], ShError) Env)
runEvalFunc env (Eval fn) = runExceptT $ catchError fn $ handler env
  where
    handler :: Env -> ([Val], ShError) -> ExceptT ([Val], ShError) IO Env
    handler env (v, Returned s) = return env{status=s, ret=v}
    handler env e@(v, Broken s) = throwError e
    handler env e@(v, Exited s) = throwError e
    handler env e               = ignoreError env e

runEvalKeep :: Env -> Eval Env -> IO (Either ([Val], ShError) Env)
runEvalKeep env (Eval fn) = runExceptT $ catchError fn $ handler env
  where
    handler :: Env -> ([Val], ShError) -> ExceptT ([Val], ShError) IO Env
    handler env e@(v, Returned s) = throwError e
    handler env (v, Broken s)     = return env{status=s, ret=v}
    handler env e@(v, Exited s)   = throwError e
    handler env e                 = ignoreError env e

ignoreError :: Env -> ([Val], ShError) -> ExceptT ([Val], ShError) IO Env
ignoreError env e@(v, x) = if ignoreInterpreterError $ flags env
                             then liftIO (hPrint (err env) x) >> return env{status=False, ret=v}
                             else throwError e

runEvalMain :: Env -> Eval Env -> IO Env
runEvalMain env (Eval fn) = do
  Right x <- runExceptT $ catchError fn $ handler env
  return x
  where
    handler :: Env -> ([Val], ShError) -> ExceptT ([Val], ShError) IO Env
    handler env (v, Returned s) = return env{status=s, ret=v}
    handler env (v, Broken s) = return env{status=s, ret=v}
    handler env (v, Exited s) = return env{status=s, ret=v}
    handler env (v, e) = liftIO (hPrint (err env) e) >> return env{status=False, ret=v}

killThreadHandler env E.ThreadKilled = do
  tinfo <- readIORef $ thread env
  x <- tryReadMVar $ exitMvar tinfo
  case x of
    Just renv -> return renv
    _         -> return env{status=False, ret=[Float 130]}
killThreadHandler env e = E.throw e

allocThreadInfo :: IO (IORef ThreadInfo)
allocThreadInfo = do
  tid <- myThreadId
  exitMvar <- newEmptyMVar
  newIORef $ ThreadInfo tid exitMvar Nothing (Lambda NoArgs defaultParseEnv Nothing return)

signalHandleClear :: IO [Handler]
signalHandleClear =
  mapM (\x -> installHandler x Default Nothing)
    $ filter (\x -> not $ x == 0 || x == 9 || x == 19 || not (inSignalSet x fullSignalSet))
    $ H.elems signalMap

signalHandleRestore :: [Handler] -> IO [Handler]
signalHandleRestore h =
  mapM (\(x, y)-> installHandler y x Nothing)
    $ zip h
    $ filter (\x -> not $ x == 0 || x == 9 || x == 19 || not (inSignalSet x fullSignalSet))
    $ H.elems signalMap

options = [ Option ['c'] ["command"] (ReqArg id "COMMAND...") "command line" ]

trapExit :: Env -> IO Env
trapExit env = do
  tinfo <- readIORef $ thread env
  runEvalMain env $ eval (exitTrap tinfo:ret env) env

exitEval :: Env -> IO ()
exitEval env = do
  renv <- trapExit env
  if status renv
    then P.exitImmediately ExitSuccess
    else
      case readInt $ head $ ret renv of
        Just n | n == 0 -> P.exitImmediately $ ExitFailure $ toInt ePERM
               | n <= 0 -> P.exitImmediately $ ExitFailure 130
               | otherwise -> P.exitImmediately $ ExitFailure n
        _ -> P.exitImmediately $ ExitFailure $ toInt ePERM

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  env <- defaultEnv <$> getCurrentDirectory <*> newIORef defaultFuncs <*> allocThreadInfo <*> newIORef 0
  args <- getArgs
  case args of
    [] -> do
      home <- getEnv "HOME"
      pref <- readPrefs $ home </> ".snale_pref"
      repl home pref env{flags=Flags True False, parenv=(parenv env){parseFlags=Flags True False}}  "@ "
    _ -> case getOpt RequireOrder options args of
           (cmds, [], []) -> do
             x <- mapM (\x -> do y <- runStateT (runParserT genEval "snale" $ T.pack x) defaultParseEnv
                                 case y of
                                   (Right code, s) -> runEvalMain env $ code env{parenv=s}) cmds
             exitEval $ last x
           (_, file:args, []) -> do
             renv <- runEvalMain env (
                       load env{args=map (toStr . T.pack) args} [toStr $ T.pack file])
                       `E.catch` killThreadHandler env
             exitEval renv
           (_, _, es)   -> putStrLn (concat es) >> exitWith (ExitFailure $ toInt eINVAL)

genEval :: Parser (Env -> Eval Env)
genEval = do
  b <- get
  modify $ \y->y{allocCount=0{-, varCount=0, lexDepth=lexDepth b+1-}}
  x <- makeExprParser genCmd table
  chkParseErr
  c <- allocCount <$> get
  modify $ \y->y{allocCount=allocCount b{-, varCount=varCount b, lexDepth=lexDepth b-1-}}
  if c > 0 then return $ genFuncSpace x
           else return x
  where
    table :: [[Operator Parser (Env -> Eval Env)]]
    table = [ [ prefix (string "!") not'' ]
            , [ binary (string ";;") comma' ]
            , [ binary (string "|[2]") (pipe' (\o e->e{err=o}))
              , binary (string "|[2=1]" <|> string "|[1=2]") (pipe' (\o e->e{err=o, out=o}))
              , binary (string "|[1]" <|> string "|" >> notFollowedBy "|") (pipe' (\o e->e{out=o})) ]
            , [ binary (parseDoller "$$") genL
              , binary (parseDoller "$:") genMap
              , binary (parseDoller "$&") evalIf
              , binary (parseDoller "$|") evalElse
              , binary (parseDoller "$>") genNL
              , binary (parseDoller "$.") genL
              , binary (parseDoller "$") genL ]
            , [ binary (string "&&&") switch
              , binary (string "|||") switchNot
              , binary (string "&" >> notFollowedBy "&") para
              , binary (string "&&") evalIf
              , binary (string "||") evalElse
              , newLine ] ]
    newLine = InfixL (genNL <$ try (some (lineComment <|> symbol spaces "\n" <|> symbol spaces ";")))
    binary name f = InfixL (f <$ try (andBrank name))
    prefix name f = Prefix (f <$ try (andBrank name))
    parseDoller :: T.Text -> Parser T.Text
    parseDoller c = do string c
                       s <- get
                       case c of
                         "$$" -> do put s{addArg=(++ [VarM (VarR (-1))])}
                                    return c
                         "$>" -> do put s{addArg=(++ [VarM (VarR (-1))])}
                                    return c
                         "$." -> do put s{addArg=(Var (VarR 1) :)}
                                    return c
                         "$:" -> do put s{addArg=(\x->Prim Normal map' [] : x ++ [Var (VarR 1)])}
                                    return c
                         _ ->  do put s{addArg=(++ [Var (VarR 1)])}
                                  return c
                       

genL f g e = do
  re1 <- f e
  if null $ ret re1 then
    return re1
  else
    g re1

genMap f g e = do
  re1 <- f e
  g re1{ret=[List $ ret re1]}

switchNot f = switch (not'' f)
 
not'' f e = f e >>= \e->return e{status=not $ status e} 

comma' f g e = do
  re1 <- f e
  re2 <- g re1
  return re2{ret=ret re1 ++ ret re2}

pipe' h f g e  = do
  (i,o) <- liftIO createPipe
  mv <- spawn e $ do
          re <- f $ h o e
          liftIO $ hClose o
          return re
  re2 <- g e{inn=i}
  liftIO $ hClose i
  re1 <- liftIO $ readMVar mv 
  if status re1 then return $ sethandles re2 e
                else return $ sethandles (setRetEnv re2 re1) e

spawn :: Env -> Eval Env -> Eval (MVar Env)
spawn env cmd = liftIO $ do
  mvar <- newEmptyMVar
  forkIO $
    do renv <- runEvalMain env cmd
       putMVar mvar renv
  return mvar

switch f g e = do
  re <- f e
  if status re then do re2 <- g re
                       Eval $ throwError (ret re2, Returned $ status re2)
               else return re

para f g e = do
  mv <- spawn e $ do
        re <- f e
        return re
  re2 <- g e
  re1 <- liftIO $ readMVar mv
  let x = ret re1 ++ ret re2
  if status re1 then return re2{ret=x}
                else return re2{status=False, ret=x}

evalIf f g e = do
  x <- liftIO $ runEvalTry e $ f e
  re <- case x of
    Right re -> return re
    Left e -> Eval $ throwError e
  if status re then setRetEnv re <$> g re
               else return re

evalElse f = evalIf (not'' f)

genNL f g e = f e >>= g

genCmd :: Parser (Env -> Eval Env)
genCmd = do s <- get
            put $ s{addArg=id}
            (rds, cmd) <- partition isRd <$> many (andSpace $ parseWord "" <|> try parseRedirect)
            genRd rds <$> genCmd' ((addArg s) cmd)

  where isRd (Rd _) = True
        isRd _      = False

genRd :: [Val] -> (Env -> Eval Env) -> (Env -> Eval Env)
genRd [] cmd = cmd
genRd (Rd r:xs) cmd = genRd xs $ r cmd

genCmd' :: [Val] -> Parser (Env -> Eval Env)
genCmd' [] = return return
genCmd' (c@(Lambda Expand _ _ _):cs) = return $ \e -> do renv <- evalFn e c []
                                                         boolDispatch renv (status renv) [] cs
genCmd' (c@Lambda{}:cs) = return $ \e -> valExpand e cs >>= evalFn e c
genCmd' (Str _ "let":cs) = return $ \e -> valExpand e cs >>= set e 
genCmd' (Str _ "letr":cs) = return $ \e -> valExpand e cs >>= setRet e 
genCmd' (Str _ "def":cs) = do incAllocCount
                              case cs of
                                Str _ name:Lambda Purely _ _ _:_ -> setPureFn name
                                Str _ name:Lambda Expand _ _ _:_ -> setPureFn name
                                Str _ name:Lambda {}:_ -> setFn name
                                Str _ name:_:_ -> setPureFn name
                                _:Lambda x _ _ _:_ | x == Normal || x == NoArgs -> return()
                                _ -> notLiteral "function name"
                              return $ \e -> valExpand e cs >>= def e
genCmd' (Str _ "load":cs) = do incAllocCount 
                               return $ \e -> valExpand e cs >>= load e
genCmd' xs@(Str _ s:cs) = case H.lookup s defaultFuncs of
                            Just (Prim _ f _) -> return $ \e -> valExpand e cs >>= f e
                            _ -> return $ eval xs
genCmd' xs = return $ eval xs

genExpr' :: [Val] -> Parser (Env -> Eval Env)
genExpr' [] = return return
genExpr' (Lambda Expand _ _ b:[]) = return b
genExpr' (Lambda Expand _ _ b:cs) = return $ \e -> do renv <- b e
                                                      boolDispatch renv (status renv) [] cs
genExpr' (c@(Lambda Purely _ _ _):cs) = return $ \e -> valExpand e cs >>= evalFn e c
genExpr' (c@(Lambda{}):cs) = throwSyntax $ NotPureFunc "it"
genExpr' [x] = return $ \e->do y <- valExpand e [x]
                               case y of
                                 [] -> Eval $ throwError ([], SomeError $
                                         "expect single value but " ++ show x ++ " is empty")
                                 Bool b:_ -> return e{status=b, ret=y}
                                 _ -> return e{status=True, ret=y}
genExpr' xs@(Str _ s:cs) = case H.lookup s defaultFuncs of
                           Just (Prim x f _) -> do if x == Purely || x == Expand
                                                     then return ()
                                                     else notPureFunc s
                                                   return $ \e -> valExpand e cs >>= f e 
                           _ -> do chkPureFn s
                                   return $ evalPure xs
genExpr' xs@(x:_) = return $ evalPure xs

parseRedirect :: Parser Val
parseRedirect = (string "<" >> parseRedirectTo ReadMode (\e h->e{inn=h}))
            <|> (string ">>" >> ((string "[2]" >> parseRedirectTo AppendMode (\e h->e{err=h}))
                             <|> (string "[1]" >> parseRedirectTo AppendMode  (\e h->e{out=h}))
                             <|> ((string "[2=1]" <|> string "[1=2]") >> parseRedirectTo AppendMode (\e h->e{err=h, out=h}))
                             <|> (parseRedirectTo AppendMode (\e h->e{out=h}))))
            <|> (string ">" >> ((string "[2]" >> parseRedirectTo WriteMode (\e h->e{err=h}))
                            <|> (string "[1]" >> parseRedirectTo WriteMode (\e h->e{out=h}))
                            <|> ((string "[2=1]" <|> string "[1=2]") >> parseRedirectTo WriteMode (\e h->e{err=h, out=h}))
                            <|> (parseRedirectTo WriteMode (\e h->e{out=h}))))
  where
    parseRedirectTo m f = spaceAnd $ 
      (do string "&1"; return $ Rd (\cmd e->setRetEnv e  <$> cmd (f e (out e)))) <|>
      (do string "&2"; return $ Rd (\cmd e->setRetEnv e  <$> cmd (f e (err e)))) <|>
      (do x <- parseWord ""
          return $ Rd $ \cmd e-> do  [y] <- valExpand e [x]
                                     file <- mkPath e <$> expand e y
                                     z <- liftIO $ withFile file m $ \h->runEval e $ cmd (f e h)
                                     case z of
                                       Right re -> return $ sethandles re e
                                       Left e   -> Eval $ throwError e)

parseWord :: String -> Parser Val
parseWord b = do
  xs <- some $ parseClsr <|> parseList <|> parseDoller <|> parseTilde <|> parseSym b <|> parseStr
  return $ case xs of
    [y] -> y
    _ -> LinkedStr xs

lineComment :: Parser T.Text
lineComment = comment >> symbol spaces "\n"

comment = char '#' >> skipMany (notChar '\n')

parseDoller :: Parser Val
parseDoller = try $ do
  char '$'
  x <- parseVar
  optional (char '^')
  return x
--  x <- optional varname
--  case x of
--    Nothing -> do y <- optional $ char '$'
--                  case y of
--                    Nothing -> return $ Sym "$"
--                    _ -> return $ Sym "$$"
--    Just x -> do optional (char '^')
--                 return $ Var x

parseVar = do
  n <- optional decimal
  case n of
    Just n -> return $ Var $ VarA n
    _ -> do
      x <- alphaNumChar <|> oneOf ("?*@" :: String)
      case x of
        '?' -> do
          n <- optional decimal
          case n of
            Just n -> return $ Var $ VarR n
            _ -> return $ Var $ VarR 0
        '*' -> return $ Var $ VarA (-1)
        '@' -> do
          y <- optional parseVar
          case y of
            Just (Var v) -> return $ VarM $ v
            Nothing -> return $ VarM $ VarA (-1)
        _   -> do
          xs <- many (alphaNumChar <|> oneOf ("_?:" :: String))
          return $ Var $ VarN (T.pack $ x:xs)

parseTilde :: Parser Val
parseTilde = try $ char '~' >> notFollowedBy (noneOf ("/ \t\n;|&'\"`<>$)}[]"::String)) >> return (Var $ VarN "~")

parseClsr :: Parser Val
parseClsr = do
  between (symbol brank "{") (string "}" <?> "brace")            (genLambda NoArgs genEval)
  <|> between (symbol brank "@{") (string "}" <?> "brace")       (genLambda Normal genEval)
  <|> between (symbol brank "(") (string ")" <?> "parentheses")  (genLambda Expand genExpr)
  <|> between (symbol brank "@(") (string ")" <?> "parentheses") (genLambda Purely genExpr)
  <|> between (symbol brank "<{") (string "}" <?> "brace")       (genLambda PipeRd genEval)
  <|> between (symbol brank ">{") (string "}" <?> "brace")       (genLambda PipeWt genEval)
  <|> try (do string "@"
              x <- parseSym "^-+*/%=~"
              optional $ char '^'
              genLambda Expand (genExpr' [x, VarM $ VarA (-1)]))
  <|> parseList

genLambda :: LambdaType -> Parser (Env -> Eval Env) -> Parser Val
genLambda t f = do p1 <- get
                   modify $ \p->p{unDefPureFn=HS.empty} 
                   x <- f
                   p2 <- get 
                   put p1{unDefPureFn=HS.union (unDefPureFn p1) (unDefPureFn p2), isErr=takeJust (isErr p1) (isErr p2)} 
                   return $ Lambda t p2 Nothing x
  where takeJust x@(Just _) _ = x
        takeJust _ x@(Just _) = x
        takeJust x _          = x

parseList = List <$> between (symbol brank "[") (string "]" <?> "square bracket") (some $ andBrank $ parseWord "")

parseSym :: String -> Parser Val
parseSym b = do
  t <- T.pack <$> some (noneOf ("# \\\t\n;|&'\"`<>$)}[]" ++ b) <|> escape)
  ref <- liftIO $ newIORef NoCache
  return $ Str (Just ref) t
  where
    escape :: Parser Char
    escape = do x <- char '\\' >> anyChar
                return $ case x of 'n' -> '\n'
                                   'r' -> '\r'
                                   't' -> '\t'
                                   _ -> x 

parseStr :: Parser Val
parseStr = toStr . T.pack <$> (quoted '\'' <|> quoted '"')
  where
    quoted :: Char -> Parser String
    quoted q = between (symbol brank $ T.singleton q) (symbol brank $ T.singleton q) (many (notChar q <|> escQuote q))

    escQuote :: Char -> Parser Char
    escQuote q = string (T.pack [q, q]) >> return q

parens = between (symbol brank "(") (symbol brank ")")

genExpr :: Parser (Env -> Eval Env)
genExpr = try (makeExprParser (--parens genExpr
                               ((\f e->return e{status=True, ret=[Float f]}) <$> float')
--                           <|> ((\l e->return e{status=True, ret=[l]}) <$> andBrank parseList)
--                           <|> try (do andBrank $ string "if"
--                                       e1 <- parens genExpr
--                                       e2 <- parens genExpr
--                                       e3 <- parens genExpr
--                                       return $ \e->do b <- e1 e >>= getBool . head . ret
--                                                       if b then e2 e
--                                                            else e3 e)
                           <|> (try (char '%' >> genFormat))
                           <|> (some (andSpace $ parseWord "!^-+*/%=~") >>= genExpr'))
                           table)
  where
    genFormat = do
      fmt <- ('%':) <$> some (noneOf (" \t\n" :: String))
      let c = last fmt in
        if c `elem` ("doxXbfFgGeEsv" :: String) then do
          f <- brankAnd genExpr
          return $ \e -> do
            re <- f e
            let v = head $ ret re in
              if c == 's' || c == 'v' then do
                s <- T.unpack <$> expand e v
                return e{ret=[toStr $ T.pack $ printf fmt s], status=True}
              else if c == 'd' || c == 'd' || c == 'o' || c == 'x' || c == 'X' || c == 'b' then do
                n <- getInt v
                return e{ret=[toStr $ T.pack $ printf fmt n], status=True}
              else do
                n <- getFloat v
                return e{ret=[toStr $ T.pack $ printf fmt n], status=True}
        else
          throwSyntax $ InvalidFormat fmt
      
    parseWord b = do
      xs <- some $ parseClsr <|> parseList <|> parseDoller <|> parseSym b <|> parseStr
      return $ case xs of
        [y] -> y
        _ -> LinkedStr xs
    float' = andBrank $ try float
                    <|> (fromInteger <$> (try decimal
                                      <|> try (string "0o" >> octal)
                                      <|> try (string "0x" >> hexadecimal)))
                    <|> try (S.toRealFloat <$> scientific)
    table :: [[Operator Parser (Env -> Eval Env)]]
    table = [ [ binary  "^"  pow' ]
            , [ prefix  "-"  neg
              , prefix  "+"  id
              , prefix  "!"  not''' ]
            , [ binary  "*"  mul'
              , binary  "/"  div'
              , binary  "%"  mod'  ]
            , [ binary  "+"  plus'
              , binary  "-"  minus' ]
            , [ binary  "<=" le'
              , binary  "<"  lt'
              , binary  ">=" ge'
              , binary  ">"  gt' ]
            , [ binary  "==" eq'
              , binary  "="  same'
              , binary  "~"  match' ]
            , [ binary  "&&" and'
              , binary  "||" or' ] ]
    binary  name f = InfixL  (f <$ try (symbol brank name))
    prefix  name f = Prefix  (f <$ try (symbol brank name))
    postfix name f = Postfix (f <$ try (symbol brank name))

brank :: Parser ()
brank = skipMany (skipSome (oneOf (" \t\n"::String)) <|> comment)

andBrank :: Parser a -> Parser a
andBrank x = x >>= \y-> brank >> return y

brankAnd :: Parser a -> Parser a
brankAnd x = brank >> x

spaces :: Parser ()
spaces = skipMany (oneOf (" \t"::String))

andSpace :: Parser a -> Parser a
andSpace x = x >>= \y-> spaces >> return y

spaceAnd :: Parser a -> Parser a
spaceAnd x = spaces >> x

getFloat :: Val -> Eval Double
getFloat (Float d) = return d
getFloat (Str _ s) = case (TR.signed TR.rational) s of
                     Right (d, "") -> return d
                     _ -> case (TR.signed TR.hexadecimal) s of
                       Right (d, "") -> return $ fromIntegral d
                       _ -> Eval $ throwError ([], SomeError $
                            show s ++ " cannot be read as number")
getFloat x = Eval $ throwError ([], SomeError $
                 show x ++ " cannot be read as number")

getBool :: Val -> Eval Bool
getBool (Bool x) = return x
getBool x = Eval $ throwError ([fromEno eINVAL], TypeMismatch "bool" x)

getStr :: Val -> String
getStr (Str _ x) = T.unpack x
getStr x = show x

setUni f x e = do
  n <- ret <$> x e >>= (getFloat . head)
  return e{ret=[Float $ f n], status=True}
setBin f x y e = do
  n <- ret <$> x e >>= (getFloat . head)
  m <- ret <$> y e >>= (getFloat . head)
  return e{ret=[Float $ f n m], status=True}
setUniBool f x e = do
  n <- x e
  return e{ret=ret n, status=f $ status n}
setBinBB f x y e = do
  n <- status <$> x e
  m <- y e
  return e{ret=ret m, status=f n (status m)}
setBinNB f x y e = do
  n <- ret <$> x e >>= (getFloat . head)
  m <- ret <$> y e >>= (getFloat . head)
  return e{ret=[Float m], status=f n m}
setBinVB f x y e = do
  n <- (head . ret) <$> x e
  m <- y e 
  return e{ret=ret m, status=f n (head $ ret m)}

match' x y e = do
  n <- (head . ret) <$> x e 
  m <- (head . ret) <$> y e 
  case match'' n m of
    [[]] -> return e{ret=[], status=False}
    x -> return e{ret=concat x, status=True}
  where
    match'' :: Val -> Val -> [[Val]]
    match'' (List a) (List b) = concat $ zipWith match'' a b
    match'' x y = case getStr x =~ getStr y of
                    []    -> [[]]
                    [z]:_ -> [[toStr $ T.pack z]]

neg = setUni ((-1)*)
minus' = setBin (-)
--inc = setUni (1+)
mul' = setBin (*)
div' = setBin (/) 
plus' = setBin (+)
pow' = setBin (**)
mod' = setBin $ \x y-> fromInteger $ mod (floor x) (floor y)
not''' = setUniBool not
lt' = setBinNB (<)
le' = setBinNB (<=)
gt' = setBinNB (>)
ge' = setBinNB (>=)
eq' = setBinVB (==)
and' = setBinBB (&&)
or' = setBinBB (||)

same' :: (Env -> Eval Env) -> (Env -> Eval Env) -> Env -> Eval Env
same' x y e = do
  z <- head . ret <$> x e
  (ret <$> y e) >>= same'' e z
  where
    same'' e x@(Str{}) z@(y@(Float m):_) = do
      n <- getFloat x
      return e{ret=z, status=n == m}
    same'' e y@(Float m) z@(x@(Str{}):_) = do
      n <- getFloat x
      return e{ret=z, status=n == m}
    same'' e x z@(y:_) = return e{ret=z, status=x == y}
    same'' e _ z = return e{ret=z, status=False} 
