module Parser
where

import Util
import Data.Maybe
import InferenceDataType
import ClassState
import Data.Map as M
import Data.List

-- Definire Program

data Program = Prog { listaClase :: [ClassState]
                    , clasaGlobal :: ClassState}
                   -- , newvars :: Map String String}
data Instruction = Instr [String]

initEmptyProgram :: Program
initEmptyProgram = (Prog [] K{nume = "Global", var = [], func = [], parent = initEmptyClass})

getVars :: Program -> [[String]]
getVars (Prog clase glb) = getValues glb Var

getClasses :: Program -> [String]
getClasses (Prog clase glb) = sort("Global":(Prelude.map getName clase))

getParentClass :: String -> Program -> String
getParentClass numeclasa (Prog clase glb) = nume (parent (head (Data.List.filter (\x -> (nume x) == numeclasa) clase)))

getFuncsForClass :: String -> Program -> [[String]]
getFuncsForClass numeclasa (Prog clase glb)
    | numeclasa == "Global" = getValues glb Func
    | otherwise = getValues (head (Data.List.filter (\x -> (nume x) == numeclasa) clase)) Func

hasType :: String -> Program -> Bool
hasType tname prog = length (Prelude.filter (\c -> c == tname) (getClasses prog)) > 0

-- Instruction poate fi ce considerați voi
parse :: String -> [Instruction]
parse text = Prelude.map (\s -> (Instr (words s))) (lines text)

insertClass::Instruction->Program->Program
insertClass (Instr wordList) (Prog cls glb)
    | length (Data.List.filter (\x -> (nume x) == className) cls) /= 0 = (Prog cls glb)
    | otherwise = (Prog newClassL glb)
    where 
    	className = head $ tail wordList
    	parentName = (head $ tail $ tail $ tail $ wordList)
        extendsList = Data.List.filter (\x -> (nume x) == parentName) cls
        newClassL
            | length wordList == 2 = (K className [] [] glb) : cls
            | length wordList == 4 = if length extendsList /= 0
            	                        then (K className [] [] (head extendsList)) : cls
            	                        else (K className [] [] glb) : cls

insertVar::Instruction->Program->Program
insertVar (Instr wordList) (Prog cls glb) = (Prog cls newglb)
    where 
        newglb
            | (length $ foundClass) /= 0 = (insertIntoClass glb Var instr)
            | otherwise = glb
        foundClass = Data.List.filter (\x -> (nume x) == (head $ tail instr)) cls
        line = unwords (tail wordList)
        instr = words (Prelude.map (\c -> if c == '=' then ' ' else c) line)

insertFunc :: Instruction -> Program -> Program
insertFunc (Instr wordList) (Prog cls glb)
    | length foundClass == 0 && className /= "Global" = (Prog cls glb)
	| className == "Global" && checkFunc glb funcList (Prog cls glb) = (Prog cls (insertIntoClass glb Func funcList))
    | length foundClass /= 0 && checkFunc (head foundClass) funcList (Prog cls glb) = (Prog ((insertIntoClass (head foundClass) Func funcList):restClass) glb) 
	| otherwise = (Prog cls glb)
	where
		line = unwords wordList
		parsed = words (Prelude.map (\c -> if (c == '=' || c == ':' || c == ',' || c == ')' || c == '(') then ' ' else c) line)
		className = head $ tail $ parsed
		restList = tail $ tail parsed
		funcList = (head restList):((head parsed):(tail restList))
		foundClass = Data.List.filter (\x -> (nume x) == className) cls
		restClass = Data.List.filter (\x -> (nume x) /= className) cls

checkFunc :: ClassState -> [String] -> Program -> Bool
checkFunc className funcList (Prog cls glb) = (Prelude.filter (\n -> hasType n (Prog cls glb)) (tail funcList)) == tail funcList

interpret :: Instruction -> Program -> Program
interpret (Instr wordList) (Prog cls glb)
    | wordList == [] = (Prog cls glb)
    | head wordList == "class" = insertClass (Instr wordList) (Prog cls glb)            
    | head wordList == "newvar" = insertVar (Instr wordList) (Prog cls glb)
    | head wordList == "infer" = (Prog cls glb)
    | otherwise = insertFunc (Instr wordList) (Prog cls glb)
        
infer :: Expr -> Program -> Maybe String
infer = undefined


