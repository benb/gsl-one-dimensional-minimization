{-# LANGUAGE ForeignFunctionInterface #-}
module Numeric.GSL.OneDimensionalMinimization (brent,goldenSection) where
import Bindings.Gsl.OneDimensionalMinimization
import Bindings.Gsl.ErrorHandling
import Bindings.Gsl.MathematicalFunctions
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Storable
import Foreign.Marshal.Utils (with)
import Debug.Trace
import System.IO.Unsafe(unsafePerformIO)



minx f last l u epsabs epsrel  = do iter<-c'gsl_min_fminimizer_iterate f
                                    case iter of
                                        x | x==c'GSL_EBADFUNC -> error "Function returned NaN or Inf"
                                          | x==c'GSL_FAILURE -> error "Best estimate could not be improved"
                                          | otherwise  -> do lower<-c'gsl_min_fminimizer_x_lower f
                                                             upper<-c'gsl_min_fminimizer_x_upper f
                                                            -- print lower
                                                            -- print upper
                                                             stopping<-c'gsl_min_test_interval lower upper epsabs epsrel
                                                             --putStrLn ("Stopping? " ++ (show stopping) ++ " " ++ (show c'GSL_SUCCESS)) 
                                                             if (stopping == c'GSL_SUCCESS)
                                                               then
                                                                 c'gsl_min_fminimizer_x_minimum f
                                                               else
                                                                 minx f last l u epsabs epsrel


data GSL_Min_Methods = Brent | GoldenSection
                                            

foreign import ccall "wrapper" 
        wrap3 :: (CDouble -> Ptr () -> IO CDouble) -> IO (FunPtr (CDouble -> Ptr () -> IO CDouble))

wrap2 :: (Double -> Double) -> (CDouble -> Ptr() -> IO CDouble) 
wrap2 f = f2 where
          f2 x y = do return $ realToFrac $ f (realToFrac x)
             
wrap :: (Double -> Double) -> IO (C'gsl_function) -- (FunPtr (CDouble -> Ptr () -> IO CDouble))
wrap f = do f1 <- (wrap3 (wrap2 f)) 
            return $ C'gsl_function f1 nullPtr

brent f l u guess abs rel = unsafePerformIO $ wrapped_min Brent f l u guess abs rel
goldenSection f l u guess abs rel  = unsafePerformIO $ wrapped_min GoldenSection f l u guess abs rel 

wrapped_min :: GSL_Min_Methods -> (Double -> Double) -> Double -> Double -> Double -> Double -> Double -> IO Double 
wrapped_min method f l u guess epsabs epsrel = do c'gsl_set_error_handler_off 
                                                  alg <- case method of 
                                                           Brent -> peek $ p'gsl_min_fminimizer_brent
                                                           GoldenSection -> peek $ p'gsl_min_fminimizer_goldensection
                                                  f' <- wrap f  
                                                  ans <- with f' $ raw_min alg (realToFrac l) (realToFrac u) (realToFrac guess) (realToFrac epsabs) (realToFrac epsrel)
                                                  freeHaskellFunPtr $ c'gsl_function'function f'
                                                  return $ realToFrac ans
                     
raw_min method l u guess epsabs epsrel f  = do gsl_min_fminimizer <- c'gsl_min_fminimizer_alloc method
                                               return_i <- c'gsl_min_fminimizer_set gsl_min_fminimizer f guess l u
                                               minimum<-minx gsl_min_fminimizer last l u epsabs epsrel
                                               c'gsl_min_fminimizer_free gsl_min_fminimizer
                                               return minimum

