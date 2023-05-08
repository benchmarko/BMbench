{-***********************************************************************
  * BM Bench - bmbench.hs (Haskell)
  * (c) Marco Vieth, 2005
  * http://www.benchmarko.de
  *
  * 14.04.2005 0.01  first tests
  *
  *
  * Usage:
  * ghc -O3 -fasm bmbench.hs -o bmbench.ghc_run
  * bmbench([bench1], [bench2], [n]) ...
  *
  *
  ***********************************************************************-}

import System(getArgs, getProgName, exitWith, ExitCode(..))

square :: Int -> Int
square n = n * n

--
-- bench01 (Integer 16/32 bit)
-- (sum of 1..n) mod 65536
-- (Javascript seems to have no Integer arithmetic, so same as bench02...)
{-
  function bench01(loops, n) {
    var x = 0;
    var sum1 = myint(((n / 2) * (n + 1)) % 65536); -- assuming n even! (sum should be ...)
    // do not use '& 0xffff' since this converts sum1 to 0 for stand alonw engine
    while (loops-- > 0) {
      for (var i = n; i > 0; i--) {
        x += i;
      }
      if (loops > 0) {   // some more loops left?
        x %= 65536;      // (do not use &= 0xffff)
        x -= sum1;       // yes, set x back to 0
        if (x != 0) {    // now x must be 0 again
          x++;           // force error for many wrong computations
          break;         // Error   //alert("Error(bench01): x="+ x);
        }
      }
    }
    //if (isInt(x)) { System.stdout.writeln("Yes, it is integer!"); }
    return x % 65536;
  }
-}

main = do
       arg <- getArgs
       case arg of
         [number] -> putStrLn (show (square (read number)))
         _        -> do
                     progname <- getProgName
                     putStrLn ("Usage: " ++ progname ++ " number")
                     exitWith (ExitFailure 1)


{-
//
// BM Bench - bmbench.js (JavaScript)
// ...
-}
