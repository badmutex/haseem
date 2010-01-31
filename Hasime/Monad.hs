module Hasime.Monad where



import Control.Monad.Writer
import Control.Concurrent.CHP.Monad



class DoConfig a where
    doConfig :: a -> Hasime a b


newtype DoConfig c =>
    Hasime c a = MkHasime {
                   runH :: WriterT c CHP a
                 }


runHasime :: DoConfig c => Hasime c a -> IO (Maybe (a,c))
runHasime hasime = runCHP . runWriterT . runH $ hasime
