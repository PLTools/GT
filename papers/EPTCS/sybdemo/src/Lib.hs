{-# LANGUAGE InstanceSigs, ExplicitForAll, DeriveDataTypeable #-}
module Lib
    ( increase, increase2, genCom
    ) where

import Data.Generics (everywhere, mkT, Data, Typeable)

data Company  = C [Dept]               deriving (Eq, Show, Typeable, Data)
data Dept     = D Name Manager [Unit]  deriving (Eq, Show, Typeable, Data)
data Unit     = PU Employee | DU Dept  deriving (Eq, Show, Typeable, Data)
data Employee = E Person Salary        deriving (Eq, Show, Typeable, Data)
data Person   = P Name Address         deriving (Eq, Show, Typeable, Data)
data Salary   = S Float                deriving (Eq, Show, Typeable, Data)
type Manager  = Employee
type Name     = String
type Address = String 

genCom :: Company
genCom = C [D "Research" ralf [PU joost, PU marlow],D "Strategy" blair []]
    where 
    ralf, joost, marlow, blair :: Employee
    ralf   = E (P "Ralf"   "Amsterdam") (S 8000)
    joost  = E (P "Joost"  "Amsterdam") (S 1000)
    marlow = E (P "Marlow" "Cambridge") (S 2000)
    blair  = E (P "Blair"  "London")    (S 100000)


increase k (C ds) = C (map (incD k) ds)
incD :: Float -> Dept -> Dept
incD k (D nm mgr us) = D nm (incE k mgr) (map (incU k) us)
incU :: Float -> Unit -> Unit
incU k (PU e) = PU (incE k e)
incU k (DU d) = DU (incD k d)
incE :: Float -> Employee -> Employee
incE k (E p s) = E p (incS k s)
incS :: Float -> Salary -> Salary
incS k (S s) = S (s * (1+k))


increase2 :: Float -> Company -> Company
increase2 k = everywhere (mkT (incS k))
        
