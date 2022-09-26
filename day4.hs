import qualified Data.ByteString as ByteString
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as BC
import Data.HexString as HS
import Data.Text (unpack)
import Data.List (findIndex)



key="yzbqklnj"

doHash :: String -> String
doHash a = Data.Text.unpack (HS.toText (HS.fromBytes (MD5.hash (BC.pack a))))

checkHash a = take 5 (doHash a) == "00000"

checkHash2 a = take 6 (doHash a) == "000000"

part1 = findIndex checkHash (map ((key ++) . show) [0..])
part2 = findIndex checkHash2 (map ((key ++) . show) [0..])
