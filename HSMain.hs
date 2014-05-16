{-# LANGUAGE ForeignFunctionInterface #-}
module HSMain where
import Main
 
foreign export ccall hs_MAIN :: IO ()
 
hs_MAIN :: IO ()
hs_MAIN = main
