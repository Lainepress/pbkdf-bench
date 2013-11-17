{-# LANGUAGE OverloadedStrings #-}

import qualified Crypto.PasswordStore           as PWS
import           Crypto.PBKDF.ByteString

--import qualified Data.Binary                    as B
--import           Data.Bits
import qualified Data.ByteString.Char8            as BS
--import qualified Data.ByteString.UTF8           as BU
--import qualified Data.ByteString.Lazy           as BLC
--import qualified Crypto.Hash                      as CH
--import           Crypto.MAC.HMAC
--import           Data.Byteable

import qualified Data.ByteString.Base64           as B64

import           System.Environment


main :: IO ()
main =
 do args <- getArgs
    case args of
      ["pwstore"] -> test pwstr_hash
      ["pbkdf"  ] -> test pbkdf_hash
      _           -> error "usage: bench (pwstore|pbkdf)"
  where
    test f  = BS.putStrLn $ f test_pw test_st

    test_pw = BS.pack $ "mary had a little lamb"
    
    test_st = BS.pack $ "salt and peper"

pbkdf_hash :: BS.ByteString -> BS.ByteString -> BS.ByteString
pbkdf_hash pw_s na_s = B64.encode $ sha256PBKDF2 pw_s na_s icount 32

{-
pbkdf_hash :: BS.ByteString -> BS.ByteString -> BS.ByteString
pbkdf_hash pw st = B64.encode $
    pbkdf2
        PBKDF
            { pbkdf_PRF    =
                PRF
                    { prf_hmac = hmac sha256 64
                    , prf_hash = sha256
                    , prf_hLen = 32
                    } 
            , pbkdf_P      = pw
            , pbkdf_S      = st
            , pbkdf_c      = icount
            , pbkdf_dkLen  = 32
            }    
  where
    sha256  = toBytes . (CH.hash :: BS.ByteString -> CH.Digest CH.SHA256)
-}
pwstr_hash :: BS.ByteString -> BS.ByteString -> BS.ByteString
pwstr_hash pw st = B64.encode $ PWS.pbkdf2 pw (PWS.importSalt st) icount

icount :: Int
icount = 1000000
