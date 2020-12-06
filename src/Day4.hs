{-# LANGUAGE FlexibleContexts #-}

module Day4 where

import qualified Data.Map as M
import Data.Maybe ( mapMaybe, fromJust )
import Text.Regex.TDFA( (=~) )

import Lib ( break', splitList )

--
-- Global data
--

-- The fields that must be present (part 1 and part 2) and validated (part 2)
requiredFields :: [String]
requiredFields = ["ecl","eyr","hcl","hgt", "byr","iyr","pid"]

-- Map of validation functions
validationMap :: M.Map String (String -> Bool)
validationMap = 
    M.fromList $ zip requiredFields [ validateEyeColor
                                    , validateExpirationYear
                                    , validateHairColor
                                    , validateHeight
                                    , validateBirthYear
                                    , validateIssueYear
                                    , validatePassportID
                                    ]


-- Makes the input for all the passports from the list of lines in the input file.
-- Each passport is separated by a blank line, and the passport input can be on one or
-- multiple lines. After applying lines to the file input, the blank line are empty strings,
-- and each passport is a list of one or more strings (one for each line), thus use
-- concatMap to make a single list of the field:value inputs.
makeInput :: [String] -> [[String]]
makeInput strs = map makeOne $ splitList (=="") strs
    where 
        makeOne xs = concatMap words xs
        
makePassport :: [String] -> M.Map String String
makePassport strs = M.fromList input
    where
        input = map (break' (==':')) strs

-- Check that each required field is included in the passport
validatePassport :: M.Map String String -> [String] -> Bool
validatePassport passport fields =
    length (mapMaybe (`M.lookup` passport) fields) == length fields

part1 :: [[String]] -> [String] -> Int
part1 passports fields =
    length $ filter (==True) $ map validateOne passports
    where
        validateOne input = validatePassport (makePassport input) fields 

--
-- Part 2 - Validation of different fields
--

validateYear :: String -> Int -> Int -> Bool
validateYear str low high =
    reMatch str && (low <= yr && yr <= high)
    where
        reMatch s = s =~ "[0-9]{4}" :: Bool
        yr = read str :: Int

validateBirthYear :: String -> Bool
validateBirthYear yr = validateYear yr 1920 2002

validateIssueYear :: String -> Bool
validateIssueYear yr = validateYear yr 2010 2020

validateExpirationYear :: String -> Bool
validateExpirationYear yr = validateYear yr 2020 2030

validatePassportID :: String -> Bool
validatePassportID str = str =~ "\\`[0-9]{9}\\'" :: Bool

validateHeight :: String -> Bool
validateHeight height =
    case matches of
        []                -> False
        [n, "in", _, _]   -> between n 59 76
        [_, _, n, "cm"]   -> between n 150 193
    where 
        heightMatch h = h =~ "([0-9]+)(in)|([0-9]+)(cm)" :: (String, String, String, [String])
        (_,_,_,matches) = heightMatch height
        between s l h = l <= n && n <= h
            where n = read s :: Int

validateHairColor :: String -> Bool
validateHairColor haircolor = 
    haircolor =~ "\\`#[0-9a-f]{6}\\'" :: Bool

validateEyeColor :: String -> Bool
validateEyeColor eyecolor = eyecolor =~ "amb|blu|brn|gry|grn|hzl|oth" :: Bool

-- validate field by applying the validation function (looked up in validationMap) to the 
-- value in the passport Map
validateField :: M.Map String String -> String -> Maybe Bool
validateField passport field = do
    value <- M.lookup field passport
    return $ fromJust (M.lookup field validationMap) value

-- Check that each required field in the passport satisfies the validation requirements
validatePassport' :: M.Map String String -> [String] -> Bool
validatePassport' passport fields =
    length (filter (==True) $ mapMaybe (validateField passport) fields) == length fields

part2 :: [[String]] -> [String] -> Int
part2 passports fields =
   length $ filter (==True) $ map validateOne passports
    where
        validateOne input = validatePassport' (makePassport input) fields 

main :: IO ()
main = do
    contents <- readFile "./test/data/day-4-input.txt"
    print $ part1 (makeInput $ lines contents) requiredFields
    print $ part2 (makeInput $ lines contents) requiredFields



