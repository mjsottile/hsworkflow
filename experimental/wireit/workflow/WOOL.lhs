(This is literate Haskell code; suffix file with .lhs)

WOOL in Haskell

>   module WOOL
>       (
>           WorkflowT, OperatorID, Token (ToToken), Tag, WFState (..), PortIndex, Pipe (..),
>           Signature (..),Operator(..),

>           defaultInitialState,

>           toSignalToken,
>           toIntToken,
>           toStringToken, makeStringTokenList,
>           toPairToken,
>           toMissingToken,
>           toListToken,
>           toDoubleToken,
>           CanFlow (..),

>           getPipeContents, getWorkflowPipeData,
>           makePipe, makeInPipe, makeOutPipe, installPipe,
>           push,

>           makeFunction, makeFunctionM, makeFunctionS, makeFunctionMS,
>           makeTake, makeTimeout2, makeReplicate, makeSerializer,
>           makeWorkflow, newWorkflowOperator, newWorkflow,

>           getOperator,

>           startWorkflow, proceedWorkflow, stepWorkflow, runWorkflowNCycles,
>           uninstallOperator, stopExecT,
>       ) where

>   import WOOLTypes
>   import WOOLOperators
>   import WOOLPrimitives
>   import WOOLWorkflow

