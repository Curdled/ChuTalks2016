import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class
import Prelude hiding (log)

type LoggingIO l a = WriterT l IO a

log :: String -> LoggingIO [String] ()
log s = tell [s]

example :: LoggingIO [String] ()
example = do log "Print 1"
             lift (print 1)

main = do str <- execWriterT example
          (print str)

