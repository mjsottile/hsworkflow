>   {-# OPTIONS_GHC -fglasgow-exts #-}

>   module WOOLPrimitives where
>   import WOOLTypes
>   import WOOLOperators
>   import Control.Monad.State


=========== Replicate ==

>   data    Replicate = Replicate
>       deriving ( Show )

>   instance (Monad m,MonadIO m) => CanRun m Replicate where
>       run _ [intag] outtagsx act@Replicate =
>           let
>               replicateAll _ [] = return ()
>               replicateAll val (outtag:outtags) =
>                   do
>                       debugT $ "replicateAll " ++ outtag ++ " " ++ (show val)
>                       push outtag $! val
>                       replicateAll val outtags
>           in
>           do
>               hd <- pull intag
>               debugT $ "Replicate " ++
>                           (show intag) ++
>                           " <<< " ++
>                           (show hd)
>               _ <-    if (isMissing hd) then do
>                           return ()
>                       else
>                           replicateAll hd outtagsx
>
>               return act
>
>       run _ _ _ act = return act



>   makeReplicate :: (Monad m,MonadIO m) => Tag -> [Tag] -> (WorkflowT m OperatorID)
>   makeReplicate intag outtags =
>       let act = Runnable $ Replicate
>       in
>           installActivity act [intag] outtags


>   newReplicate :: (Monad m,MonadIO m) => Int -> (WorkflowT m OperatorID)
>   newReplicate numOuts =
>       let act = Runnable $ Replicate
>       in do
>           installOperator act ( Signature (1, numOuts ) )



=============== Delay ==

This Delay activity will consume a value, and after a number of ticks,
will emit the value. Between consumption and emission, no additional
values will be consumed.

>   data    Delay = Delay{
>                           delayTicks      :: Int,
>                           delayTicksLeft  :: Int,
>                           delayValue      :: Token
>                       }
>       deriving ( Show )



>   makeDelay :: (Monad m,MonadIO m) => Tag -> Tag -> Int -> (WorkflowT m OperatorID)
>   makeDelay intag outtag ticks =
>       let act = Runnable $ Delay{
>                                       delayTicks=ticks,
>                                       delayTicksLeft=0,
>                                       delayValue=(toMissingToken)
>                                   }
>       in
>           installActivity act [intag] [outtag]




>   instance (Monad m,MonadIO m) => CanRun m Delay where
>       run oid [intag] [outtag] act@Delay{    delayTicks=ticks, delayTicksLeft=ticksLeft,
>                                           delayValue=value } =
>           do
>               _ <-
>                   if ticksLeft == 0 then
>                       do
>                           hd <- pull intag
>                           debugT $ "Delay [" ++
>                                       (show intag) ++
>                                       " <<< " ++
>                                       (show hd) ++
>                                       "] delay " ++
>                                       (show ticks) ++
>                                       " >>> " ++
>                                       (show outtag)

>                           _ <-    if (isMissing hd) then do
>                                       return ()
>                                   else
>                                       updateActivity oid $ Runnable act{ delayTicksLeft=ticks, delayValue=hd }
>                           return ()
>                   else if ticksLeft == 1 then
>                       do
>                           debugT $ "Delay [" ++
>                                       (show value) ++
>                                       "] FIRING " ++
>                                       " >>> " ++
>                                       (show outtag)
>                           updateActivity oid $ Runnable act{ delayTicksLeft=0, delayValue=(toMissingToken) }
>                           push outtag $! value
>                   else
>                       do
>                           updateActivity oid $ Runnable act{ delayTicksLeft=(ticksLeft-1) }
>               return act
>
>       run _ _ _ act = return act


=============== Serializer ==
    
>   data    Serializer = Serializer
>       deriving ( Show )


>   newSerializer :: (Monad m,MonadIO m) => (WorkflowT m OperatorID)
>   newSerializer = makeSerializer "" ""


>   makeSerializer :: (Monad m,MonadIO m) => Tag -> Tag -> (WorkflowT m OperatorID)
>   makeSerializer intag outtag =
>       let act = Runnable $ Serializer
>       in
>           installActivity act [intag] [outtag]



>   instance (Monad m,MonadIO m) => CanRun m Serializer where
>       run _ [intag] [outtag] act =
>           let
>               serializerAll [] = return ()
>               serializerAll (v:vs) =
>                   do
>                       debugT $ "serializerAll " ++ outtag ++ " " ++ (show v)
>                       push outtag $! v
>                       serializerAll vs
>           in
>           do
>               hd <- pull intag
>               debugT $ "serializer " ++
>                           (show intag) ++
>                           " <<< " ++
>                           (show hd)
>               let vs = toList hd
>               serializerAll vs
>
>               -- _ <-    if ( isMissing hd ) then do
>               --             return ()
>               --         else if ( isList hd ) then do
>               --             serializerAll vs
>               --         else
>               --             return ()
>
>               return act
>
>       run _ _ _ act = return act


=========== Take ==

>   data    Take = Take{
>                           takeN :: Int,
>                           takeData :: [Token]
>                       }
>       deriving ( Show )


>   newTake :: (Monad m,MonadIO m) => Int -> (WorkflowT m OperatorID)
>   newTake n = makeTake "" "" n

>   makeTake :: (Monad m,MonadIO m) => Tag -> Tag -> Int -> (WorkflowT m OperatorID)
>   makeTake intag outtag n =
>       let act = Runnable $ Take{
>                                   takeN=n,
>                                   takeData=[]
>                               }
>       in
>           installActivity act [intag] [outtag]

>   instance (Monad m,MonadIO m) => CanRun m Take where
>       run oid [intag] [outtag] act@Take{ takeN=n, takeData=d } =
>           do
>               hd <- pull intag
>               _ <-    if ( isMissing hd ) then do
>                           return ()
>                       else
>                           let
>                               newdata = d ++ [hd]
>                           in
>                               if (length newdata) >= n then
>                                   do
>                                       push outtag $! toListToken newdata
>                                       debugT $ "Take " ++ (show n) ++ " " ++
>                                                   (show intag) ++
>                                                   " >>> " ++
>                                                   (show outtag) ++
>                                                   " --- Flushing: " ++ (show newdata)
>                                       updateActivity oid $ Runnable act{ takeData=[] }
>                               else
>                                   do
>                                       debugT $ "Take " ++ (show n) ++ " " ++
>                                                   (show intag) ++
>                                                   " >>> " ++
>                                                   (show outtag) ++
>                                                   " --- Amending: " ++ (show newdata)
>                                       updateActivity oid $ Runnable act{ takeData=newdata }
>
>               return act
>
>       run _ _ _ act = return act


=============== Timeout ==
    
>   data    Timeout m = (Monad m) =>
>                               Timeout{
>                                   timeoutAction :: [Token] -> WorkflowT m Token,
>                                   timeoutDelay :: Int
>                               }

>   instance (Monad m) => Show (Timeout m) where
>       show a = "Timeout { timeoutDelay = " ++ (show (timeoutDelay a)) ++ "}"


>   makeTimeout :: (Monad m,MonadIO m) => Tag -> (WFState m -> Token) -> Int -> (WorkflowT m OperatorID)
>   makeTimeout tag func delay =
>       let
>           glue _ =
>               do
>                   st <- get
>                   return $ func st
>
>           act = Runnable $ Timeout{
>                                   timeoutAction=glue,
>                                   timeoutDelay=delay
>                               }
>       in
>           installActivity act [] [tag]

>   makeTimeout2 :: (Monad m,MonadIO m) => Tag -> ([Token] -> (WorkflowT m Token)) -> Int -> (WorkflowT m OperatorID)
>   makeTimeout2 tag func delay =
>       let act = Runnable $ Timeout{
>                                   timeoutAction=func,
>                                   timeoutDelay=delay
>                               }
>       in
>           installActivity act [] [tag]


>   instance (Monad m,MonadIO m) => CanRun m (Timeout m) where
>       run oid [] [tag] act@Timeout{ timeoutAction=f, timeoutDelay=delay } =
>           if (delay-1) > 0 then
>               do
>                   updateActivity oid $ Runnable Timeout{
>                                                       timeoutAction=f,
>                                                       timeoutDelay=(delay-1)
>                                                   }
>                   return act
>           else
>               do
>                   debugT $ "Timeout " ++ (show delay) ++ " " ++ (show tag)
>                   result <- f []
>                   push tag $! result
>                   return act
>
>       run _ _ _ act = return act



=========== Counter ==

>   newCounter :: (Monad m,MonadIO m) => Int -> Int -> (WorkflowT m OperatorID)
>   newCounter start limit = makeCounter start limit ""

>   newCounterSignal :: (Monad m,MonadIO m) => Int -> Int -> (WorkflowT m OperatorID)
>   newCounterSignal start limit = makeCounterSignal start limit ""


>   makeCounter :: (Monad m,MonadIO m) => Int -> Int -> Tag -> (WorkflowT m OperatorID)
>   makeCounter start limit outtag =
>       let
>           mseval _ ([ cd, ld ]) [] = do
>                               let c = toInt cd
>                               let l = toInt ld
>                               x <-    if c <= l then do
>                                           return $ ( [ toIntToken (c+1), toIntToken l ], [toIntToken c] )
>                                       else do
>                                           return $ ( [ toIntToken c, toIntToken l ], [] )
>                               return x
>
>           mseval _ _ _ = return ([],[])
>       in do
>           makeFunctionMS   mseval    [ toIntToken start, toIntToken limit ] []       [outtag]


>   makeCounterSignal :: (Monad m,MonadIO m) => Int -> Int -> Tag -> (WorkflowT m OperatorID)
>   makeCounterSignal start limit outtag =
>       let
>           mseval _ ([ cd, ld ]) [] =
>               if isSignal cd then do
>                   return $ ( [ cd, ld ], [] )
>               else
>                   let
>                       c = toInt cd
>                       l = toInt ld
>                       result =
>                           if c <= l then
>                               ( [ toIntToken (c+1), toIntToken l ], [toIntToken c] )
>                           else
>                               ( [ toSignalToken, toIntToken l ], [toSignalToken] )
>                   in do
>                       return result
>
>           mseval _ _ _ = return ([],[])
>
>       in do
>           makeFunctionMS   mseval    [ toIntToken start, toIntToken limit ] []       [outtag]


=========== Loop ==

>   makeLoop :: (Monad m,MonadIO m) => Tag -> Tag -> Tag -> (WorkflowT m OperatorID)
>   makeLoop intag bodytag donetag =
>       let
>           mseval _ [ d ] = do
>               -- liftIO $ putStrLn $ "loop=" ++ (show d)
>               x <-    if isSignal d then do
>                           return $ ( [ toMissingToken, toSignalToken ] )
>                       else do
>                           return $ ( [ d, toMissingToken ] )
>               return x
>
>           mseval _ _ = return ([])
>
>       in do
>           makeFunctionM   mseval      [intag]       [bodytag, donetag]

>   newLoop :: (Monad m,MonadIO m) => (WorkflowT m OperatorID)
>   newLoop = makeLoop "" "" ""


=========== Wait ==

>   newWait :: (Monad m,MonadIO m) => (WorkflowT m OperatorID)
>   newWait = makeWait "" "" ""

>   makeWait :: (Monad m,MonadIO m) => Tag -> Tag -> Tag -> (WorkflowT m OperatorID)
>   makeWait locktag valtag outtag =
>       let
>           mseval _ [ _::Token, v::Token ] = do
>               -- liftIO $ putStrLn $ "wait=" ++ (show v)
>               return $ ( [ v ] )
>
>           mseval _ _ = return ([])
>
>       in do
>           makeFunctionM   mseval      [locktag,valtag]       [outtag]



=========== Reduce ==


>   newReduce :: (Monad m,MonadIO m) => Token -> (WorkflowT m OperatorID)
>   newReduce initial = makeReduce initial ""

>   makeReduce :: (Monad m,MonadIO m) => Token -> Tag -> (WorkflowT m OperatorID)
>   makeReduce initial intag =
>       let
>           -- mseval opId state       [ToToken MissingToken] = do { return ( state, [] ); }
>           mseval _ [ oldval ]  [inval] =
>               if ( isString inval ) then do
>                   return ( [ toStringToken ( (toString oldval) ++ (toString inval) ) ], [] )
>               else if ( isInt inval ) then do
>                   return ( [ toIntToken  ( (toInt oldval) + (toInt inval)) ], [] )
>               else do
>                   return $ ([oldval], [])
>
>           mseval _ _ _ = return ([],[])
>
>       in do
>           makeFunctionMS   mseval    [ initial ] [intag]       ["GROUND"]
