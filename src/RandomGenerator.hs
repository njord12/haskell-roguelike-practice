module RandomGenerator
where
import Data.Word (Word64, Word32)
import Data.Bits (Bits(shiftR, xor, (.|.), shiftL, (.&.)))
import System.Random
import Data.Int (Int32)
import Foreign (Bits(rotateR))

data PCGen = PCGen Word64 Word64 deriving (Read, Show)   

instance RandomGen PCGen where
    next gen = 
        let 
            (outWord, nextGen) = stepGen gen
            outInt = fromIntegral (fromIntegral outWord :: Int32) :: Int
        in
            (outInt, nextGen)
    genRange _ = (fromIntegral (minBound :: Int32), fromIntegral (maxBound :: Int32))
    split gen@(PCGen state inc) = 
        let
            (q,nGen1@(PCGen sa ia)) = stepGen gen
            (w,nGen2@(PCGen sb ib)) = stepGen nGen1
            (e,nGen3@(PCGen sc ic)) = stepGen nGen2
            (r,nGen4@(PCGen sd id)) = stepGen nGen3
            stateA = sd `rotateR` 5
            stateB = sd `rotateR` 3
            incA = ((fromIntegral q) `shiftL` 32) .|. (fromIntegral w)
            incB = ((fromIntegral e) `shiftL` 32) .|. (fromIntegral r)
            outA = PCGen stateA (incA .|. 1)
            outB = PCGen stateB (incB .|. 1)
        in 
            (outA, outB)

instance Random PCGen where
    random gen =  
        let
            (x, newGen) = random gen
        in
            (PCGen x x, newGen)
    randomR _ gen = random gen




stepGen :: PCGen -> (Word32, PCGen)
stepGen (PCGen state inc) =
    let
        newState = state * 6364136223846793005  + (inc .|. 1)
        xorShifted = fromIntegral (((state `shiftR` 18) `xor` state) `shiftR` 27) :: Word32
        rot = fromIntegral (state `shiftR` 59) :: Word32
        out = (xorShifted `shiftR` (fromIntegral rot)) .|. (xorShifted `shiftL` fromIntegral ((-rot) .&. 31))
    in
        (out, PCGen newState inc)
