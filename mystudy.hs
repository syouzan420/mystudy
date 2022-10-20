import System.IO(hSetBuffering, stdout, BufferMode(NoBuffering))
import System.Random
import Myfile(fileR,fileW,isFile)
import Useful(sepChar)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering 
  e <- isFile 
  c <- if e then fileR
       else fileW "" >> return ""
  mLoop c

mLoop :: String -> IO ()
mLoop c = do
  let cs = lines c
      yns = map (\x -> let (y:n:[]) = sepChar ';' x in (y,n)) cs
  mc <- putStr "i:入力 t:確認 q:終了＞" >> getLine
  nc <- case mc of
    "i" -> do
      yg <- putStr "用語:" >> getLine
      ny <- putStr "内容:" >> getLine
      ic <- confirm
      let nd = if ic then yg++";"++ny else ""
          ncs = if ic then cs++[nd] else cs
          nc = if ic then unlines ncs else c
      if ic then fileW nc >> putStrLn "記録したよ" else putStrLn "やめたよ"
      return nc
    "t" -> do
      sfyn <- sfls 5 yns
      putMyData sfyn
      return c
    "q" -> return "q"
  if (nc=="q") then return () else mLoop nc

putMyData :: [(String,String)] -> IO ()
putMyData [] = return ()
putMyData ((y,n):xs) = do
  putStr "○" >> putStrLn y
  getLine
  putStr "☆" >> putStrLn n
  getLine
  putMyData xs


confirm :: IO Bool
confirm = do
  putStrLn "これでいい？ (Y/n)"
  y <- getLine
  if (y=="n" || y=="N" || y=="だめ") then return False else return True

getRNum :: Int -> IO Int
getRNum i = randomRIO (0,i-1)

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle (x:[]) = return [x]
shuffle ls = do
  let lng = length ls
  rn <- getRNum lng
  return (drop (rn+1) ls ++ [ls!!rn] ++take rn ls) 

sfls :: Int -> [a] -> IO [a]
sfls 0 ls = return ls
sfls i ls = do
  nls <- shuffle ls
  sfls (i-1) nls
