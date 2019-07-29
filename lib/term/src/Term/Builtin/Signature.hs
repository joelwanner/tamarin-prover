{-# LANGUAGE OverloadedStrings #-}
-- |
-- Copyright   : (c) 2010-2012 Benedikt Schmidt
-- License     : GPL v3 (see LICENSE)
-- 
-- Maintainer  : Benedikt Schmidt <beschmi@gmail.com>
--
-- Builtin function symbols and signatures.
module Term.Builtin.Signature where

import Term.LTerm
import qualified Data.Set as S

----------------------------------------------------------------------
-- Builtin symbols (pair and inv are defined in Term.Term)
----------------------------------------------------------------------

-- | Binary builtin function symbols.
sdecSym, sencSym, adecSym, aencSym, signSym, revealSignSym :: NoEqSym
sdecSym = ("sdec", (2, (Public, Just "oSenc")))
sencSym = ("senc", (2, (Public, Just "oSenc")))
adecSym = ("adec", (2, (Public, Just "oAenc")))
aencSym = ("aenc", (2, (Public, Just "oAenc")))
signSym = ("sign", (2, (Public, Just "oSign")))
revealSignSym = ("revealSign", (2, (Public, Nothing)))

-- | Ternary builtin function symbols.
verifySym, revealVerifySym :: NoEqSym
verifySym = ("verify", (3, (Public, Just "oVerify")))
revealVerifySym = ("revealVerify", (3, (Public, Nothing)))

-- | Unary builtin function symbols.
pkSym, hashSym, extractMessageSym :: NoEqSym
pkSym = ("pk", (1, (Public, Just "oGenPk")))
hashSym = ("h", (1, (Public, Just "oHash")))
extractMessageSym = ("getMessage", (1, (Public, Nothing)))

-- | Nullary builtin function symbols.
trueSym :: NoEqSym
trueSym = ("true", (0, (Public, Nothing)))

-- | Operation symbols for adversary budget.
opSyms :: [NoEqSym]
opSyms = [ (op, (0, (Public, Nothing))) | op <- opNames ]
  where opNames = [ "oRecv", "oSend", "oSenc", "oHash", "oAenc", "oSign", "oVerify", "oGenPk" ]

----------------------------------------------------------------------
-- Builtin signatures
----------------------------------------------------------------------

-- | The signature for symmetric encryption.
symEncFunSig :: NoEqFunSig
symEncFunSig = S.fromList $ [ sdecSym, sencSym ]

-- | The signature for asymmetric encryption.
asymEncFunSig :: NoEqFunSig
asymEncFunSig = S.fromList $ [ adecSym, aencSym, pkSym ]

-- | The signature for cryptographic signatures.
signatureFunSig :: NoEqFunSig
signatureFunSig = S.fromList $ [ signSym, verifySym, trueSym, pkSym ]

-- | The signature for cryptographic signatures that are message-revealing.
revealSignatureFunSig :: NoEqFunSig
revealSignatureFunSig = S.fromList $ [ revealSignSym, revealVerifySym, extractMessageSym, trueSym, pkSym ]

-- | The signature for hashing.
hashFunSig :: NoEqFunSig
hashFunSig = S.fromList [ hashSym ]

-- | The signature for adversary budget.
budgetFunSig :: NoEqFunSig
budgetFunSig = S.fromList opSyms

