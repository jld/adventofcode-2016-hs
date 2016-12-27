{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}
module Insn(Insn(..), Src(..), Reg, Imm) where
import Data.Bits
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU

data Insn = Inc Reg
          | Dec Reg
          | Tgl Src
          | Cpy Src Reg
          | Jnz Src Src
          | Nop
          deriving (Eq, Show)

data Src = SrcImm Imm
         | SrcReg Reg
         deriving (Eq, Show)
srcReg = SrcReg . Reg
srcImm = SrcImm . Imm

newtype Reg = Reg Int
            deriving (Eq, Show)
newtype Imm = Imm Int
            deriving (Eq, Show)

kF_MASK = 7
kF_SHIFT = 3

k0F_ARITY = 1
k0F_UNARY = 0
k0F_BINARY = 1

k0F_MODE0 = 2
k0F_IMM0 = 0
k0F_REG0 = 2

k0F_MODE1 = 4
k0F_IMM1 = 0
k0F_REG1 = 4

k1U_INC = 0
k1U_DEC = 1
k1U_TGL = 2
k1B_CPY = 0
k1B_JNZ = 1

encodeInsn Nop = (0, 0)

encodeInsn (Inc (Reg r)) =
  (k0F_UNARY .|. k0F_REG0 .|. r `shiftL` kF_SHIFT,
   k1U_INC)
encodeInsn (Dec (Reg r)) =
  (k0F_UNARY .|. k0F_REG0 .|. r `shiftL` kF_SHIFT,
   k1U_DEC)
encodeInsn (Tgl (SrcImm (Imm i))) =
  (k0F_UNARY .|. k0F_IMM0 .|. i `shiftL` kF_SHIFT,
   k1U_TGL)
encodeInsn (Tgl (SrcReg (Reg r))) =
  (k0F_UNARY .|. k0F_REG0 .|. r `shiftL` kF_SHIFT,
   k1U_TGL)

encodeInsn (Cpy (SrcImm (Imm isrc)) (Reg rdst)) =
  (k0F_BINARY .|. k0F_IMM0 .|. k0F_REG1 .|. isrc `shiftL` kF_SHIFT,
   k1B_CPY                              .|. rdst `shiftL` kF_SHIFT)
encodeInsn (Cpy (SrcReg (Reg rsrc)) (Reg rdst)) =
  (k0F_BINARY .|. k0F_REG0 .|. k0F_REG1 .|. rsrc `shiftL` kF_SHIFT,
   k1B_CPY                              .|. rdst `shiftL` kF_SHIFT)
encodeInsn (Jnz (SrcImm (Imm isrc)) (SrcImm (Imm idst))) =
  (k0F_BINARY .|. k0F_IMM0 .|. k0F_IMM1 .|. isrc `shiftL` kF_SHIFT,
   k1B_JNZ                              .|. idst `shiftL` kF_SHIFT)
encodeInsn (Jnz (SrcReg (Reg rsrc)) (SrcImm (Imm idst))) =
  (k0F_BINARY .|. k0F_REG0 .|. k0F_IMM1 .|. rsrc `shiftL` kF_SHIFT,
   k1B_JNZ                              .|. idst `shiftL` kF_SHIFT)
encodeInsn (Jnz (SrcImm (Imm isrc)) (SrcReg (Reg rdst))) =
  (k0F_BINARY .|. k0F_IMM0 .|. k0F_REG1 .|. isrc `shiftL` kF_SHIFT,
   k1B_JNZ                              .|. rdst `shiftL` kF_SHIFT)
encodeInsn (Jnz (SrcReg (Reg rsrc)) (SrcReg (Reg rdst))) =
  (k0F_BINARY .|. k0F_REG0 .|. k0F_REG1 .|. rsrc `shiftL` kF_SHIFT,
   k1B_JNZ                              .|. rdst `shiftL` kF_SHIFT)

decodeInsn w0 w1
  | w0f == k0F_UNARY .|. k0F_REG0
  , w1f == k1U_INC
  = Inc (Reg w0i)
  | w0f == k0F_UNARY .|. k0F_REG0
  , w1f == k1U_DEC
  = Dec (Reg w0i)
  | w0f == k0F_UNARY .|. k0F_IMM0
  , w1f == k1U_TGL
  = Tgl (SrcImm (Imm w0i))
  | w0f == k0F_UNARY .|. k0F_REG0
  , w1f == k1U_TGL
  = Tgl (SrcReg (Reg w0i))
  | w0f == k0F_BINARY .|. k0F_IMM0 .|. k0F_REG1
  , w1f == k1B_CPY
  = Cpy (SrcImm (Imm w0i)) (Reg w1i)
  | w0f == k0F_BINARY .|. k0F_REG0 .|. k0F_REG1
  , w1f == k1B_CPY
  = Cpy (SrcReg (Reg w0i)) (Reg w1i)
  | w0f == k0F_BINARY .|. k0F_IMM0 .|. k0F_IMM1
  , w1f == k1B_JNZ
  = Jnz (SrcImm (Imm w0i)) (SrcImm (Imm w1i))
  | w0f == k0F_BINARY .|. k0F_REG0 .|. k0F_IMM1
  , w1f == k1B_JNZ
  = Jnz (SrcReg (Reg w0i)) (SrcImm (Imm w1i))
  | w0f == k0F_BINARY .|. k0F_IMM0 .|. k0F_REG1
  , w1f == k1B_JNZ
  = Jnz (SrcImm (Imm w0i)) (SrcReg (Reg w1i))
  | w0f == k0F_BINARY .|. k0F_REG0 .|. k0F_REG1
  , w1f == k1B_JNZ
  = Jnz (SrcReg (Reg w0i)) (SrcReg (Reg w1i))
  | otherwise
  = Nop
  where w0f = w0 .&. kF_MASK
        w1f = w1 .&. kF_MASK
        w0i = w0 `shiftR` kF_SHIFT
        w1i = w1 `shiftR` kF_SHIFT

newtype instance VU.MVector s Insn = MV_Insn (VU.MVector s Int)
newtype instance VU.Vector    Insn = V_Insn  (VU.Vector    Int)

instance VGM.MVector VU.MVector Insn where
  {-# INLINE basicLength #-}
  basicLength (MV_Insn v) = VGM.basicLength v `div` 2

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice off len (MV_Insn v) =
    MV_Insn $ VGM.basicUnsafeSlice (off * 2) (len * 2) v

  {-# INLINE basicOverlaps #-}
  basicOverlaps (MV_Insn v0) (MV_Insn v1) =
    VGM.basicOverlaps v0 v1

  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew len = MV_Insn <$> VGM.basicUnsafeNew (len * 2)

  {-# INLINE basicInitialize #-}
  basicInitialize (MV_Insn v) = VGM.basicInitialize v

  basicUnsafeRead v idx = do
    let (MV_Insn elem) = VGM.basicUnsafeSlice idx 1 v
    w0 <- VGM.basicUnsafeRead elem 0
    w1 <- VGM.basicUnsafeRead elem 1
    return $ decodeInsn w0 w1

  basicUnsafeWrite v idx insn = do
    let (w0, w1) = encodeInsn insn
    let (MV_Insn elem) = VGM.basicUnsafeSlice idx 1 v
    VGM.basicUnsafeWrite elem 0 w0
    VGM.basicUnsafeWrite elem 1 w1

instance VG.Vector VU.Vector Insn where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (MV_Insn v) = V_Insn <$> VG.basicUnsafeFreeze v

  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (V_Insn v) = MV_Insn <$> VG.basicUnsafeThaw v
  
  {-# INLINE basicLength #-}
  basicLength (V_Insn v) = VG.basicLength v `div` 2

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice off len (V_Insn v) =
    V_Insn $ VG.basicUnsafeSlice (off * 2) (len * 2) v

  basicUnsafeIndexM v idx = do
    let (V_Insn elem) = VG.basicUnsafeSlice idx 1 v
    w0 <- VG.basicUnsafeIndexM elem 0
    w1 <- VG.basicUnsafeIndexM elem 1
    return $ decodeInsn w0 w1

instance VU.Unbox Insn where
