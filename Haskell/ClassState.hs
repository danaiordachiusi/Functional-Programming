module ClassState
where

import Data.Map

-- Utilizat pentru a obține informații despre Variabile sau Funcții
data InstrType = Var | Func  deriving (Show, Eq)

-- TODO - Trebuie definit ClassState
data ClassState = K { nume :: String
                    , var :: [[String]]
                    , func :: [[String]]
                    , parent :: ClassState}

initEmptyClass :: ClassState
initEmptyClass = K {nume = "", var = [], func = [], parent = initEmptyClass}

insertIntoClass :: ClassState -> InstrType -> [String] -> ClassState
insertIntoClass K { nume = n
                  , var = varlist
                  , func = funcList
                  , parent = p} Var str = K { nume = n
                                            , var = str:varlist
                                            , func = funcList
                                            , parent = p}
insertIntoClass K { nume = n
                  , var = varlist
                  , func = funcList
                  , parent = p} Func str = K { nume = n
                                             , var = varlist
                                             , func = str:funcList
                                             , parent = p}


getValues :: ClassState -> InstrType -> [[String]]
getValues K { nume = _
            , var = varlist
            , func = funcList
            , parent = _} Var = varlist
getValues K { nume = _
            , var = varlist
            , func = funcList
            , parent = _} Func = funcList

getName :: ClassState -> String
getName K { nume = n
                  , var = varlist
                  , func = funcList
                  , parent = p} = n


