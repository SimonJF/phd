module Polar where

-- Plain GV
data GVSessionTy = Bang GVTy GVSessionTy -- Send value
                 | Query GVTy GVSessionTy -- Receive value
                 | EndBang
                 | EndQuery

data GVTy = GVST GVSessionTy
          | GVOne
          | GVZero
          | GVSum GVTy GVTy
          | GVPair GVTy GVTy
          | GVLinFun GVTy GVTy
          | GVInt

dualof :: GVSessionTy -> GVSessionTy
dualof (Bang t st) = Query t (dualof st)
dualof (Query t st) = Bang t (dualof st)
dualof EndBang = EndQuery
dualof EndQuery = EndBang



instance Show GVSessionTy where
  show (Bang ty st) = "!" ++ (show ty) ++ "." ++ (show st)
  show (Query ty st) = "?" ++ (show ty) ++ "." ++ (show st)
  show EndBang = "end!"
  show EndQuery = "end?"

instance Show GVTy where
  show (GVST st) = "(" ++ show st ++ ")"
  show GVOne = "1"
  show GVZero = "0"
  show (GVSum t1 t2) = (show t1) ++ " + " ++ (show t2)
  show (GVPair t1 t2) = (show t1) ++ " * " ++ (show t2)
  show (GVLinFun t1 t2) = (show t1) ++ " -o " ++ (show t2)
  show GVInt = "Int"



-- Polarised GV

data PolarGVTy = PGVST PolarGVSessionTy
               | PGVOne
               | PGVZero
               | PGVSum PolarGVTy PolarGVTy
               | PGVPair PolarGVTy PolarGVTy
               | PGVLinFun PolarGVTy PolarGVTy
               | PGVInt

data PolarGVSessionTy = PGVSTBangTy PolarGVSessionTyBang
                      | PGVSTQueryTy PolarGVSessionTyQuery

data PolarGVSessionTyBang = PGVSTBang PolarGVTy PolarGVSessionTyBang
                          | PGVSTEndBang

data PolarGVSessionTyQuery = PGVSTQuery PolarGVTy PolarGVSessionTyQuery
                           | PGVSTEndQuery



instance Show PolarGVSessionTy where
  show (PGVSTBangTy st) = show st
  show (PGVSTQueryTy st) = show st


instance Show PolarGVSessionTyBang where
  show (PGVSTBang ty st) = "!" ++ (show ty) ++ "." ++ (show st)
  show PGVSTEndBang = "end!"

instance Show PolarGVSessionTyQuery where
  show (PGVSTQuery ty st) = "?" ++ (show ty) ++ "." ++ (show st)
  show PGVSTEndQuery = "end?"

instance Show PolarGVTy where
  show (PGVST st) = "(" ++ show st ++ ")"
  show PGVOne = "1"
  show PGVZero = "0"
  show (PGVSum t1 t2) = (show t1) ++ " + " ++ (show t2)
  show (PGVPair t1 t2) = (show t1) ++ " * " ++ (show t2)
  show (PGVLinFun t1 t2) = (show t1) ++ " -o " ++ (show t2)
  show PGVInt = "Int"


-- Polarised dual
polarDualof :: PolarGVSessionTy -> PolarGVSessionTy
polarDualof (PGVSTBangTy st) = PGVSTQueryTy $ polarDualofBang st
polarDualof (PGVSTQueryTy st) = PGVSTBangTy $ polarDualofQuery st

polarDualofBang :: PolarGVSessionTyBang -> PolarGVSessionTyQuery
polarDualofBang (PGVSTBang pgvty st) = PGVSTQuery pgvty (polarDualofBang st)
polarDualofBang PGVSTEndBang = PGVSTEndQuery

polarDualofQuery :: PolarGVSessionTyQuery -> PolarGVSessionTyBang
polarDualofQuery (PGVSTQuery pgvty st) = PGVSTBang pgvty (polarDualofQuery st)
polarDualofQuery PGVSTEndQuery = PGVSTEndBang


-- Polarise a plain GV type
polariseTy :: GVTy -> PolarGVTy
polariseTy (GVST st) = PGVST (polariseSessionTy st)
polariseTy GVOne = PGVOne
polariseTy GVZero = PGVZero
polariseTy (GVSum t1 t2) = PGVSum (polariseTy t1) (polariseTy t2)
polariseTy (GVPair t1 t2) = PGVSum (polariseTy t1) (polariseTy t2)
polariseTy (GVLinFun t1 t2) = PGVLinFun (polariseTy t1) (polariseTy t2)
polariseTy GVInt = PGVInt


polariseSessionTy :: GVSessionTy -> PolarGVSessionTy
polariseSessionTy (Bang ty st) = PGVSTBangTy $ PGVSTBang (polariseTy ty) (polariseSessionTyBang st)
polariseSessionTy (Query ty st) = PGVSTQueryTy $ PGVSTQuery (polariseTy ty) (polariseSessionTyQuery st)
polariseSessionTy EndBang = PGVSTBangTy PGVSTEndBang
polariseSessionTy EndQuery = PGVSTQueryTy PGVSTEndQuery

-- Polarisation for bang types
-- TODO: GADTs?
polariseSessionTyBang :: GVSessionTy -> PolarGVSessionTyBang
polariseSessionTyBang (Bang t st) = PGVSTBang (polariseTy t) (polariseSessionTyBang st)
polariseSessionTyBang (Query t st) = PGVSTBang continuation PGVSTEndBang
  where continuation = PGVST . PGVSTBangTy $ PGVSTBang (polariseTy t) (polarDualofQuery (polariseSessionTyQuery st))
polariseSessionTyBang EndBang = PGVSTEndBang
polariseSessionTyBang EndQuery = PGVSTBang (PGVST $ PGVSTBangTy PGVSTEndBang) PGVSTEndBang


-- Polarisation for query types
polariseSessionTyQuery :: GVSessionTy -> PolarGVSessionTyQuery
polariseSessionTyQuery (Bang t st) = PGVSTQuery continuation PGVSTEndQuery
  where continuation = PGVST . PGVSTBangTy $ PGVSTBang (polariseTy t) (polariseSessionTyBang st)
polariseSessionTyQuery (Query t st) = PGVSTQuery (polariseTy t) (polariseSessionTyQuery st)
polariseSessionTyQuery EndBang = PGVSTQuery (PGVST $ PGVSTBangTy PGVSTEndBang) PGVSTEndQuery
polariseSessionTyQuery EndQuery = PGVSTEndQuery



-- Test nonsense

intIn2Out1 :: GVSessionTy
intIn2Out1 = Query GVInt (Query GVInt (Bang GVInt EndBang))
