module Evaluator (eval, primitiveBindings) where

import LispVal
import Control.Monad.Error

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = do
    result <- eval env pred
    case result of
        Bool False -> eval env alt
        _          -> eval env conseq
eval env form@(List (Atom "cond" : clauses))
    | null clauses = throwError $ BadSpecialForm "no clause in cond expression: " form
    | otherwise    = case head clauses of
        List [Atom "else", expr] -> eval env expr
        List [pred, expr] -> eval env $ List [Atom "if", pred, expr,
                                              List (Atom "cond" : tail clauses)]
        _ -> throwError $ BadSpecialForm "ill-formed cond expression: " form
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarArgs varargs env [] body
eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
eval env val@(String _) = return val
eval env val@(Bool _) = return val
eval env val@(Character _) = return val
eval env val@(Number _) = return val
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args = 
    if num params /= num args && varargs == Nothing
       then throwError $ NumArgs (num params) args
       else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody env = liftM last $ mapM (eval env) body
          bindVarArgs arg env = case arg of
                                    Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
                                    Nothing -> return env

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map makePrimitiveFunc primitives)
    where makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericOp (+)),
              ("-", numericOp (-)),
              ("*", numericOp (*)),
              ("/", numericOp div),
              ("mod", numericOp mod),
              ("quotient", numericOp quot),
              ("remainder", numericOp rem),
              ("number?", unaryOp (return . id) Bool isNumber),
              ("symbol?", unaryOp (return . id) Bool isSymbol),
              ("string?", unaryOp (return . id) Bool isString),
              ("boolean?", unaryOp (return . id) Bool isBoolean),
              ("pair?", unaryOp (return . id) Bool isPair),
              ("null?", unaryOp (return . id) Bool isNull),
              ("list?", unaryOp (return . id) Bool isList),
              ("char?", unaryOp (return . id) Bool isCharacter),
              ("symbol->string", unaryOp unpackSymbol String id),
              ("string->symbol", unaryOp unpackString Atom id),
              ("not", unaryOp unpackBool Bool not),
              ("and", boolOp (&&)),
              ("or", boolOp (||)),
              ("=", numBoolBinOp (==)),
              ("<", numBoolBinOp (<)),
              (">", numBoolBinOp (>)),
              ("/=", numBoolBinOp (/=)),
              (">=", numBoolBinOp (>=)),
              ("<=", numBoolBinOp (<=)),
              ("string=?", strBoolBinOp (==)),
              ("string<?", strBoolBinOp (<)),
              ("string>?", strBoolBinOp (>)),
              ("string<=?", strBoolBinOp (<=)),
              ("string>=?", strBoolBinOp (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", binaryOp (return . id) Bool eqv),
              ("eqv?", binaryOp (return . id) Bool eqv)]

unaryOp :: (LispVal -> ThrowsError a) -> (b -> LispVal) -> (a -> b) -> [LispVal]
    -> ThrowsError LispVal
unaryOp unpack pack op [x] = unpack x >>= return . pack . op
unaryOp _ _ _ args         = throwError $ NumArgs 1 args

binaryOp :: (LispVal -> ThrowsError a) -> (b -> LispVal) -> (a -> a -> b) -> [LispVal]
    -> ThrowsError LispVal
binaryOp unpack pack op [x,y] = do
    x' <- unpack x
    y' <- unpack y
    return . pack $ x' `op` y'
binaryOp _ _ _ args = throwError $ NumArgs 2 args

operation :: (LispVal -> ThrowsError a) -> (a -> LispVal) -> (a -> a -> a) -> [LispVal]
    -> ThrowsError LispVal
operation unpack pack op args@(x:y:ys) = mapM unpack args >>= return . pack . foldl1 op
operation _      _ _  args          = throwError $ NumArgs 2 args

numericOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericOp op = operation unpackNumber Number op

boolOp :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolOp op = operation unpackBool Bool op

numBoolBinOp :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinOp op = binaryOp unpackNumber Bool op

strBoolBinOp :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinOp op = binaryOp unpackString Bool op

unpackNumber :: LispVal -> ThrowsError Integer
unpackNumber (Number n) = return n
unpackNumber notNum = throwError $ TypeMismatch "number" notNum

unpackSymbol :: LispVal -> ThrowsError String
unpackSymbol (Atom s) = return s
unpackSymbol notSymbol = throwError $ TypeMismatch "symbol" notSymbol

unpackString :: LispVal -> ThrowsError String
unpackString (String s) = return s
unpackString notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

car :: [LispVal] -> ThrowsError LispVal
car [(List (x:_))] = return x
car [(DottedList (x:_) _)] = return x
car [arg] = throwError $ TypeMismatch "pair" arg
car args  = throwError $ NumArgs 1 args

cdr :: [LispVal] -> ThrowsError LispVal
cdr [(List (_:xs))] = return $ List xs
cdr [(DottedList [_] x)] = return x
cdr [(DottedList (_:xs) x)] = return $ DottedList xs x
cdr [arg] = throwError $ TypeMismatch "pair" arg
cdr args  = throwError $ NumArgs 1 args

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List xs] = return $ List (x:xs)
cons [x, DottedList xs y] = return $ DottedList (x:xs) y
cons [x, y] = return $ DottedList [x] y
cons args = throwError $ NumArgs 2 args

eqv :: LispVal -> LispVal -> Bool
eqv (Bool arg1) (Bool arg2) = arg1 == arg2
eqv (Number arg1) (Number arg2) = arg1 == arg2
eqv (String arg1) (String arg2) = arg1 == arg2
eqv (Atom arg1) (Atom arg2) = arg1 == arg2
eqv (DottedList xs x) (DottedList ys y) = eqv (List $ xs ++ [x]) (List $ ys ++ [y])
eqv (List arg1) (List arg2) = (length arg1 == length arg2) && (and $ zipWith eqv arg1 arg2)
eqv _ _ = False

isNumber, isSymbol, isString, isBoolean, isPair, isNull, isList, isCharacter
    :: LispVal -> Bool
isNumber (Number _) = True
isNumber _          = False
isSymbol (Atom _) = True
isSymbol _        = False
isString (String _) = True
isString _          = False
isBoolean (Bool _) = True
isBoolean _        = False
isPair (DottedList _ _) = True
isPair (List (x:xs)) = True
isPair _             = False
isNull (List []) = True
isNull _         = False
isList (List _) = True
isList _        = False
isCharacter (Character _) = True
isCharacter _             = False
