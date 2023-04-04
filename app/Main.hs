{-# LANGUAGE  OverloadedStrings #-}
import Network.Socket
    ( defaultHints,
      getAddrInfo,
      openSocket,
      connect,
      close,
      AddrInfo(addrSocketType, addrAddress),
      SocketType(Stream) )
import Network.Socket.ByteString ( sendAll, recv )
import qualified Data.ByteString.Char8 as B8
import Data.Serialize (runPut)
import Data.Serialize.Put (putInt32be, putByteString)
import Hahahafka.Protocol.Message.Metadata
    ( KDecode(decode),
      KEncode(encode),
      MetadataResponseV0,
      MetadataReqV0(MetadataReqV0) )
import Text.Pretty.Simple (pPrint)

resolveAddr host port = do
    let hints = defaultHints { addrSocketType = Stream }
    head <$> getAddrInfo (Just hints) (Just host) (Just port)

metadata = encode $ MetadataReqV0 []

req = runPut $ do
    putInt32be size
    putByteString metadata
      where
        size = fromIntegral $ B8.length metadata

res :: B8.ByteString -> Either String MetadataResponseV0
res = decode

main = do
    addr <- resolveAddr "localhost" "9092"
    sock <- openSocket addr
    connect sock $ addrAddress addr
    let buff = req
    --
    sendAll sock buff
    msg1 <- recv sock 2048
    B8.putStrLn $ B8.pack $ show $ B8.length msg1
    pPrint $ res msg1
    --
    close sock
