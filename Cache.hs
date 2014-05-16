module Cache where
import System.IO.Unsafe
import Data.IORef
import qualified Data.Map as M

type Cache k a = IORef (M.Map k a)
type CacheOnce k a = IORef (Maybe (k, a))

initCache :: IO a -> a
initCache = unsafePerformIO

newCache :: (Eq k, Ord k) => IO (Cache k a)
newCache = newIORef (M.empty)

newCacheOnce :: Eq k => IO (CacheOnce k a)
newCacheOnce = newIORef Nothing

cacheOnce :: Eq k => CacheOnce k a -> (k -> IO a) -> k -> IO a
cacheOnce ref f key = do
    m <- readIORef ref
    case m of
        Just (k, v) | k == key -> return v
        _ -> do
            v <- f key
            writeIORef ref (Just (key, v))
            return v

cache :: (Eq k, Ord k) => Cache k a -> (k -> IO a) -> k -> IO a
cache ref f key = do
    m <- readIORef ref
    case M.lookup key m of
        Just v  -> return v
        _       -> do
            v <- f key
            modifyIORef ref (M.insert key v)
            return v
