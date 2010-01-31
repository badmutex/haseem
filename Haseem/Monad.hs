module Haseem.Monad where



import Control.Monad.Writer
import Control.Concurrent.CHP.Monad



class DoConfig a where
    doConfig :: a -> Haseem a b


newtype DoConfig c =>
    Haseem c a = MkHaseem {
                   runH :: WriterT c CHP a
                 }


runHaseem :: DoConfig c => Haseem c a -> IO (Maybe (a,c))
runHaseem hasime = runCHP . runWriterT . runH $ hasime
