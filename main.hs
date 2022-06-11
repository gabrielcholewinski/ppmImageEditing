import System.Environment
import Data.List
import Test.QuickCheck
import System.Random
{- @Copyright2018, Projeto desenvolvido por
 -                                         Ruhan Victor 51779,
 -                                      Gabriel Freitas 51034,
 -                                       Carlos Marques 51964
 -}
data PPM = PPM [Int] [[(Int,Int,Int)]] deriving (Eq)
data Flag = Flag String (PPM -> PPM)

listaFlags :: [Flag]
listaFlags = [(Flag "-fh" fh), (Flag "-fv" fv), (Flag "-rc" rc), (Flag "-gc" gc),
         (Flag "-bc" bc), (Flag "-gs" gs), (Flag "-hh" hh), (Flag "-hw" hw)]

instance Show (PPM) where
  show (PPM us ps) = unlines (["P3"] ++ cortarListas  (map (\x -> show x) (us ++ (concat $ map (\(x,y,z) -> x:y:(z:[])) (concat ps)))))

instance Show (Flag) where
  show (Flag str flag) = show str

instance Eq (Flag) where
  (Flag str1 flag1) == (Flag str2 flag2) = str1 == str2 

instance Arbitrary PPM where
  arbitrary = do
     width <- choose (1,255)
     height <- choose (1,255)
     maxValuePix <- choose (1,255)
     return $ PPM [width,height,maxValuePix] $ randomPPM width height maxValuePix

randomPPM :: Int -> Int -> Int -> [[(Int, Int, Int)]]
randomPPM width height maxValuePix = alinharListas width $ tuplarListas $ (randomList (width*height*3) maxValuePix) 

randomList :: Int -> Int -> [Int] 
randomList seed maxValuePix = map (randomNumber maxValuePix) [1..seed]

randomNumber :: Int -> Int -> Int
randomNumber maxValuePix seed = fst $ randomR (0,maxValuePix) (mkStdGen seed)

instance Arbitrary Flag where
  arbitrary = oneof [return (Flag "-fh" fh),
                     return (Flag "-fv" fv),
                     return (Flag "-rc" rc),
                     return (Flag "-gc" gc),
                     return (Flag "-bc" bc),
                     return (Flag "-gs" gs),
                     return (Flag "-hh" hh),
                     return (Flag "-hw" hw)]

prop_twoReversesIsOriginal :: PPM -> Flag -> Bool
prop_twoReversesIsOriginal image flag | (flag == (Flag "-fh" fh)) = image == (aplicarFlags image $ duplicateFlag $ flagToString flag)
                                      | (flag == (Flag "-fv" fv)) = image == (aplicarFlags image $ duplicateFlag $ flagToString flag)
                                      | otherwise = True -- Para conseguir maior semelhanca ao output do professor quando corre os testes,
                                                         --                decidimos fazer com pattern-matching ao inves de usar Property;

flagToString :: Flag -> String
flagToString (Flag str _) = str

duplicateFlag :: String -> [String]
duplicateFlag flag = [flag, flag]

prop_nPixeisEqualdimensions :: PPM -> Bool
prop_nPixeisEqualdimensions (PPM us ps) = dimension == length' ps
                                        where dimension = ((us!!0)*(us!!1))
-- Metodo length auxiliar que conta as dimensoes corretas do formato PPM
length' :: [[(Int,Int,Int)]] -> Int
length' [] = 0
length' ((x:xs)) = (length x)+(length' xs)

prop_pixeisIsntLarger :: PPM -> Bool
prop_pixeisIsntLarger (PPM us ps) = isLess ps n
                                  where n = us!!2

isLess :: [[(Int,Int,Int)]] -> Int -> Bool
isLess [] _ = True
isLess (x:xs) k = divTup x k && isLess xs k

divTup :: [(Int,Int,Int)] -> Int -> Bool
divTup [] _ = True
divTup (x:xs) k = compTup x k && divTup xs k

compTup :: (Int,Int,Int) -> Int -> Bool
compTup (a,b,c) k = (a<=k && b<=k && c<=k)
--metodo que aplica quickCheck nos 3 testes
tt :: IO ()
tt = do
     quickCheck (prop_twoReversesIsOriginal)
     quickCheck (prop_nPixeisEqualdimensions)
     quickCheck (prop_pixeisIsntLarger)

main :: IO ()
main = do
       args <- getArgs
       if (args !! 0) == "-t" then tt
       else do
       conteudo <- readFile (args !! 0)
       let linhas = filter (\x -> (not $ '#' `elem` x) && (x /= "")) (tail $ lines conteudo)
           strings = concat $ map (\x -> words x) linhas
           numeros = map (\x -> read x) strings
           unidades = take 3 numeros
           pixeis = alinharListas (unidades !! 0) (tuplarListas (drop 3 numeros))
           imagem = aplicarFlags (PPM unidades pixeis) (drop 2 args)
       writeFile (args !! 1) (show imagem)

aplicarFlags :: PPM -> [String] -> PPM
aplicarFlags x [] = x
aplicarFlags x (y:ys) = aplicarFlags ((encontrarFuncao y) x) ys

encontrarFuncao :: String -> (PPM -> PPM)
encontrarFuncao x = extrairFuncao $ listaFlags !! (snd $
                    foldl (\(acc, ind) (Flag nome funcao) -> if x == nome then (acc + 1, acc) else (acc + 1, ind)) (0,0) listaFlags)

extrairFuncao :: Flag -> (PPM -> PPM)
extrairFuncao (Flag nome funcao) = funcao

tuplarListas :: [Int] -> [[(Int, Int, Int)]]
tuplarListas [] = []
tuplarListas (x:y:z:lista) = [zip3 [x] [y] [z]] ++ tuplarListas lista

alinharListas :: Int -> [[(Int, Int, Int)]] -> [[(Int, Int, Int)]]
alinharListas _ [] = []
alinharListas n xs = [(concat $ take n xs)] ++ (alinharListas n (drop n xs))

cortarListas :: [String] -> [String]
cortarListas [] = []
cortarListas xs = foldl (\x acc -> (x ++ " ") ++ acc) [] (take 20 xs) : cortarListas (drop 20 xs)

fh :: PPM -> PPM
fh (PPM us ps) = PPM us (map reverse ps)

fv :: PPM -> PPM
fv (PPM us ps) = (PPM us (reverse ps))

rc :: PPM -> PPM
rc (PPM us ps) = (PPM us ((map (map (\(x,y,z) -> (x, 0, 0)))) ps))

gc :: PPM -> PPM
gc (PPM us ps) = (PPM us ((map (map (\(x,y,z) -> (0, y, 0)))) ps))

bc :: PPM -> PPM
bc (PPM us ps) = (PPM us ((map (map (\(x,y,z) -> (0, 0, z)))) ps))

gs :: PPM -> PPM
gs (PPM us ps) = (PPM us ((map (map (\x -> tuploMedias x))) ps))

tuploMedias :: (Int, Int, Int) -> (Int, Int, Int)
tuploMedias (x, y, z) = (n,n,n) where n = div (x + y + z) 3

hh :: PPM -> PPM
hh (PPM [x,y,z] ps) = (PPM  [x, div y 2, z] (hhAuxiliar ps))

hhAuxiliar :: [[(Int, Int, Int)]] -> [[(Int, Int, Int)]]
hhAuxiliar [] = []
hhAuxiliar (x:[]) = []
hhAuxiliar (x:y:xs) = (zipWith (media2Tuplos) x y) : hhAuxiliar xs

hw :: PPM -> PPM
hw (PPM [x,y,z] ps) = (PPM [div x 2, y, z] (map (\x -> hwAuxiliar x) ps))

hwAuxiliar :: [(Int,Int,Int)] -> [(Int,Int,Int)]
hwAuxiliar [] = []
hwAuxiliar (x:[]) = []
hwAuxiliar (x:y:xs) = (media2Tuplos x y) : hwAuxiliar xs

somar2Tuplos :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
somar2Tuplos (a,b,c) (x,y,z) = (a+x,b+y,c+z)

media2Tuplos :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
media2Tuplos x y = (\(x,y,z) -> (div x 2,div y 2,div z 2)) (somar2Tuplos x y)

somar3Tuplos :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
somar3Tuplos (a,b,c) (x,y,z) (g,h,i) = (a+x+g,b+y+h,c+z+i)

media3Tuplos :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
media3Tuplos x y z = (\(x,y,z) -> (div x 3,div y 3,div z 3)) (somar3Tuplos x y z)
