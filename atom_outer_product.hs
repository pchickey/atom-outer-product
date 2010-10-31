-- outerprod.hs
-- pch 09oct10
-- compute the outer product (vector_mul (transpose a) a) of vector a
-- currently for a of length 4.
import Language.Atom
import Data.Int

main :: IO ()
main = do
  (schedule1, _, _, _, _) <- compile "outerprod" 
    defaults { cFuncName = "outerprod"
             , cCode = funcTestPrePostCode 
             }
    outerprod
  _ <- compile "outerprod_bench" 
    defaults { cFuncName = "outerprod"
             , cCode = benchTestPrePostCode 
             } 
    outerprod
  putStrLn "Naive Outer Product Implementation"
  putStrLn $ reportSchedule schedule1
  
  (schedule2, _, _, _, _) <- compile "betterop" 
    defaults { cFuncName = "outerprod"
             , cCode = funcTestPrePostCode
             } 
    betterop
  _ <- compile "betterop_bench" 
    defaults { cFuncName = "outerprod"
             , cCode = benchTestPrePostCode
             } 
    betterop
  putStrLn "Improved Outer Product Implementation"
  putStrLn $ reportSchedule schedule2
 
  (schedule2, _, _, _, _) <- compile "op_triangle" 
    defaults { cFuncName = "outerprod"
             , cCode = funcTestPrePostCode
             } 
    op_triangle
  _ <- compile "op_triangle_bench" 
    defaults { cFuncName = "outerprod"
             , cCode = benchTestPrePostCode
             } 
    op_triangle
  putStrLn "Triangular Outer Product Implementation"
  putStrLn $ reportSchedule schedule2


preCode :: String
preCode = 
  unlines
  [ "#include <stdlib.h>"
  , "#include <stdio.h>"
  , "float in[4];"
  , "float out[16];"
  , "unsigned char running = 1;"
  ]

funcTestPrePostCode :: [Name] -> [Name] -> [(Name, Type)] -> (String, String)
funcTestPrePostCode _ _ _ =
  ( preCode
  , unlines
    [ "int main(int argc, char* argv[]) {"
    , "  if (argc < 5) {"
    , "    printf(\"usage: outerprod <a1> <a2> <a3> <a4>\\n\");"
    , "  }"
    , "  else {"
    , "    in[0] = atof(argv[1]);"
    , "    in[1] = atof(argv[2]);"
    , "    in[2] = atof(argv[3]);"
    , "    in[3] = atof(argv[4]);"
    , "    printf(\"Computing outer product of vector [%f %f %f %f]...\\n\","
    , "           in[0], in[1], in[2], in[3]);"
    , "    while(running) {"
    , "      outerprod();"
    , "    }"
    , "    printf(\"outer product result:\\n\");       "
    , "    printf(\"[ %7.4f %7.4f %7.4f %7.4f \\n\"   "
    , "           \"  %7.4f %7.4f %7.4f %7.4f \\n\"   " 
    , "           \"  %7.4f %7.4f %7.4f %7.4f \\n\"   " 
    , "           \"  %7.4f %7.4f %7.4f %7.4f ] \\n\"  "
    , "            , out[0], out[1], out[2], out[3]    "
    , "            , out[4], out[5], out[6], out[7]    "
    , "            , out[8], out[9], out[10], out[11]  "
    , "            , out[12], out[13], out[14], out[15]); "
    , "  }"
    , "  return 0;"
    , "}"
    ]
  )

benchTestPrePostCode :: [Name] -> [Name] -> [(Name, Type)] -> (String, String)
benchTestPrePostCode _ _ _ =
  ( "#define NUM_RUNS 10000000\n" ++ -- 100000000 for faster computers
    "#include <time.h>\n" ++
    preCode
  , unlines
    [ "int main(int argc, char* argv[]) {"
    , " clock_t runtime;"
    , " long long i; // NUM_RUNS may be very large."
    , " float in[4];"
    , " float out[16];"
    , " in[0] = 1.0; in[1] = 2.0; in[3] = 3.0; in[4] = 4.0;"
    , " for(i = 0; i < NUM_RUNS; i++) {"
    , "    while(running) {"
    , "      outerprod();"
    , "    }"
    , "    running = 1;"
    , "  }"
    , "  runtime = clock();"
    , "  printf(\"outerprod run %d times in %f seconds\\n\", NUM_RUNS, (double)runtime / CLOCKS_PER_SEC); "
    , "  return 0;"
    , "}"
    ]
  )


outerprod :: Atom ()
outerprod = do
  -- externs
  let input = array' "in" Float
      output = array' "out" Float
      running = bool' "running"
  
  -- state vars
  row <- word32 "row" 0
  col <- word32 "col" 0
  
  atom "do_mul" $ do
    let out_idx = ((value row) * 4) + (value col)
        mulresult ::  E Float 
        mulresult = (input !. value row) * (input !. value col)
    (output ! out_idx) <== mulresult
  
  atom "adv_col" $ do
    cond $ value col <. 4
    col <== value col + 1

  atom "adv_row" $ do
    cond $ value col ==. 4
    col <== 0
    row <== value row + 1

  atom "check_finish" $ do
    cond $ value row ==. 4
    running <== false
    -- reset state
    row <== 0
    col <== 0
  

docell :: (Assign a, NumE a, IntegralE c) => A a -> A a -> (c,c) -> Atom ()
docell input output (row,col) = do
      output ! (out_idx row col) <== (input !. Const row) * (input !. Const col)
  where
  out_idx :: (IntegralE a) => a -> a -> E a
  out_idx r c = Const $ 4*r+c   


betterop :: Atom ()
betterop = do
  let input :: A Float 
      input = array' "in" Float
      output :: A Float 
      output = array' "out" Float
      running = bool' "running"
  
  let elems :: [(Int32,Int32)]
      elems =  [(r,c) | r <- [0,1,2,3], c <- [0,1,2,3] ]

  atom "do_mul" $ do
    cond $ value running ==. true 
    mapM (docell input output) elems
    running <== false

 
out_idx :: (IntegralE a) => a -> a -> E a
out_idx r c = Const $ 4*r+c   

docells :: (Assign a, NumE a, IntegralE c) => A a -> A a -> (c,c) -> Atom ()
docells input output (row,col) 
  | row == col = do 
      let mulresult = (input !. Const row) * (input !. Const col)
      output ! (out_idx row col) <== mulresult 
  | row /= col = do    
      -- let varname = "mulresult" ++ show(row) ++ show(col)
      -- mulresult <- var varname 3 
      -- using let mulresult gives a 10% speed improvement over docell on my netbook, gcc 4.4.1, -O0
      -- no difference for -03. I guess gcc is smart 
      let mulresult = (input !. Const row) * (input !. Const col)
      output ! (out_idx row col) <== {- value -} mulresult 
      output ! (out_idx col row) <== {- value -} mulresult 

op_triangle :: Atom ()
op_triangle = do
  let input :: A Float 
      input = array' "in" Float
      output :: A Float 
      output = array' "out" Float
      running = bool' "running"

  let tri_elems :: Int32 -> [(Int32,Int32)]
      tri_elems n =  [(r,c) | r <- [0..n], c <- [0..n], r >= c ]

  atom "do_mul" $ do
    cond $ value running ==. true 
    mapM (docells input output) (tri_elems 3)
    running <== false

