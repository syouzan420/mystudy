import System.IO(hSetBuffering, stdout, BufferMode(NoBuffering))
import System.Random(randomRIO)
import Myfile(fileR,fileW,isFile)
import Useful(sepChar,getIndex,replCon)

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
      yns = map (\x -> let (y:n:c:[]) = sepChar ';' x in (y,n,c)) cs
  mc <- putStr "i:入力 u:確認 a:練習 q:終了＞" >> getLine
  nc <- case mc of
    mc' | mc'=="i" || mc'=="い" -> do
      yg <- putStr "用語:" >> getLine
      ny <- putStr "内容:" >> getLine
      ic <- confirm
      let nd = if ic then yg++";"++ny++";0" else ""
          ncs = if ic then cs++[nd] else cs
          nc = if ic then unlines ncs else c
      if ic then fileW nc >> putStrLn "記録したよ" else putStrLn "やめたよ"
      return nc
    mc' | mc'=="u" || mc'=="う" -> do
      sfyn <- sfls 5 yns
      putMyData sfyn
      return c
    mc' | mc'=="a" || mc'=="あ" -> do
      sfyn <- sfls 5 yns
      ncs <- testMyData cs sfyn
      let nc = unlines ncs 
      if (c==nc) then return () else fileW nc
      return nc
    "q" -> return "q"
    _   -> return c
  if (nc=="q") then return () else mLoop nc

putMyData :: [(String,String,String)] -> IO ()
putMyData [] = return ()
putMyData ((y,n,c):xs) = do
  let che = if (c=="0") then " ---" else " *-*"
  putStr "○" >> putStr y >> putStrLn che
  getLine
  putStr "☆" >> putStrLn n
  getLine
  putMyData xs

testMyData :: [String] -> [(String,String,String)] -> IO [String] 
testMyData cs [] = return cs 
testMyData cs ((_,_,"1"):xs) = testMyData cs xs
testMyData cs ((y,n,c):xs) = do
  putStr "○" >> putStrLn y
  getLine
  putStr "☆" >> putStrLn n
  ir <- isRemember
  let ncs = if ir then changeData cs (y++";"++n++";1") else cs
  testMyData ncs xs

changeData :: [String] -> String -> [String]
changeData cs s =
  let fe = head$sepChar ';' s
      fcs = map (head . sepChar ';') cs
      i = getIndex fe fcs 
   in replCon i s cs

confirm :: IO Bool
confirm = do
  putStrLn "これでいい？ (Y/n)"
  y <- getLine
  if (y=="n" || y=="N" || y=="だめ" || y=="dame") then return False else return True

isRemember :: IO Bool
isRemember = do
  putStrLn "**********？ (N/y)"
  n <- getLine
  if (n=="y" || n=="Y" || n=="うん" || n=="はい" || n=="おぼえた" || n=="unn") 
     then return True else return False

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
