module Main (main) where
import Foreign (Ptr, Storable(..), plusPtr, mallocBytes)
import Foreign.Storable
import Foreign.Marshal.Alloc
import Data.IORef
import Control.Monad
import System.Environment
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf
main
  = do (!n) <- getArgs >>= readIO . head
       initialize
       offset_momentum
       energy 0 planets >>= printf "%.9f\n"
       replicateM_ n (advance planets)
       energy 0 planets >>= printf "%.9f\n"
offset_momentum
  = do (!m) <- foldr (.+.) (Vec 0 0 0) `fmap`
                 (mapM momentum . take (nbodies - 1) . iterate next $ next planets)
       setVec (vel planets) $ (-1 / solar_mass) *. m
  where momentum (!p) = liftM2 (*.) (mass p) (getVec (vel p))
 
energy :: Double -> Ptr Double -> IO Double
energy e (!p)
  | p == end = return e
  | otherwise =
    do (!p1) <- getVec (pos p)
       (!v1) <- getVec (vel p)
       m1 <- mass p
       (!e) <- energy2 p1 m1 e p2
       energy (e + 0.5 * m1 * magnitude2 v1) p2
  where (!p2) = next p
energy2 (!p1) !m1 e p
  | p == end = return e
  | otherwise =
    do p2 <- getVec (pos p)
       v2 <- getVec (vel p)
       (!m2) <- mass p
       let distance = sqrt . magnitude2 $ p1 .-. p2
       energy2 p1 m1 (e - m1 * m2 / distance) (next p)
 
advance :: Ptr Double -> IO ()
advance p1
  = when (p1 /= end) $
      do pos1 <- getVec $ pos p1
         (!m1) <- mass p1
         let go !(!p2)
               | p2 /= end =
                 do pos2 <- getVec (pos p2)
                    (!m2) <- mass p2
                    let vel2 = vel p2
                        (!difference) = pos1 .-. pos2
                        distance2 = magnitude2 difference
                        distance = sqrt distance2
                        magnitude = delta_t / (distance2 * distance)
                        (!mass_magn) = magnitude *. difference
                    vel1 -= m2 *. mass_magn
                    vel2 += m1 *. mass_magn
                    go (next p2)
               | otherwise =
                 do v1 <- getVec vel1
                    p1 += delta_t *. v1
         go p2
         advance p2
  where vel1 = vel p1
        p2 = next p1
 
planets :: Ptr Double
planets = unsafePerformIO $ mallocBytes (7 * nbodies * 8)
 
nbodies :: Int
nbodies = 5
 
solar_mass, delta_t, days_per_year :: Double
days_per_year = 365.24
solar_mass = 4 * pi ** 2
delta_t = 1.0e-2
initialize = mapM_ newPlanet planets
  where dp = days_per_year
        planets
          = [0, 0, 0, 0, 0, 0, 1 * solar_mass, 4.841431442464721,
             (-1.1603200440274284), (-0.10362204447112311),
             1.660076642744037e-3 * dp, 7.699011184197404e-3 * dp,
             (-6.90460016972063e-5) * dp, 9.547919384243266e-4 * solar_mass,
             8.34336671824458, 4.124798564124305, (-0.4035234171143214),
             (-2.767425107268624e-3) * dp, 4.998528012349172e-3 * dp,
             2.3041729757376393e-5 * dp, 2.858859806661308e-4 * solar_mass,
             12.894369562139131, (-15.111151401698631), (-0.22330757889265573),
             2.964601375647616e-3 * dp, 2.3784717395948095e-3 * dp,
             (-2.9658956854023756e-5) * dp, 4.366244043351563e-5 * solar_mass,
             15.379697114850917, (-25.919314609987964), 0.17925877295037118,
             2.6806777249038932e-3 * dp, 1.628241700382423e-3 * dp,
             (-9.515922545197159e-5) * dp, 5.1513890204661145e-5 * solar_mass]
 
data Vector3 = Vec !Double !Double !Double
 
end :: Ptr Double
end = inc planets $ nbodies * 7
 
next :: Ptr Double -> Ptr Double
next (!p) = inc p 7
 
cursor :: IORef (Ptr Double)
cursor = unsafePerformIO $ newIORef planets
 
inc :: Ptr Double -> Int -> Ptr Double
inc (!ptr) n = plusPtr ptr (n * 8)
 
newPlanet :: Double -> IO ()
newPlanet !(!d)
  = do (!ptr) <- readIORef cursor
       pokeElemOff ptr 0 d
       writeIORef cursor (inc ptr 1)
 
pos :: Ptr Double -> Ptr Double
pos (!ptr) = ptr
 
vel :: Ptr Double -> Ptr Double
vel ptr = inc ptr 3
 
mass :: Ptr Double -> IO Double
mass ptr = peekElemOff ptr 6
(!((!(Vec x y (!z))))) .+. ((!(Vec u v (!w))))
  = Vec (x + u) (y + v) (z + w)
(!(Vec (!x) y (!z))) .-. ((!(Vec (!u) (!v) w)))
  = Vec (x - u) (y - v) (z - w)
(!k) *. (!(Vec x y (!z))) = Vec (k * x) (k * y) (k * z)
magnitude2 (Vec x (!y) z) = x * x + y * y + z * z
getVec (!p) = liftM3 Vec (peek p) (f 1) (f 2)
  where f = peekElemOff p
setVec (!p) (Vec x y z)
  = do poke p x
       pokeElemOff p 1 y
       pokeElemOff p 2 z
 
infix 4 +=
 
infix 4 -=
(!v1) += (!(Vec u v w))
  = do (!x) <- peek v1
       poke v1 (x + u)
       y <- peekElemOff v1 1
       pokeElemOff v1 1 (y + v)
       z <- peekElemOff v1 2
       pokeElemOff v1 2 (z + w)
(!v1) -= (Vec (!u) (!v) w)
  = do (!x) <- peek v1
       poke v1 (x - u)
       (!y) <- peekElemOff v1 1
       pokeElemOff v1 1 (y - v)
       z <- peekElemOff v1 2
       pokeElemOff v1 2 (z - w)