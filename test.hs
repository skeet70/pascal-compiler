import System.Environment  
import System.IO  
import System.Directory  
import DigitFSA
  
main = do 
    let tuple = ("0000101001", [], 0, 0)
        in putStrLn $ digitFSA tuple
