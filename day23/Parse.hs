module Parse where
import Insn
import qualified Data.Vector.Unboxed as VU

parse_reg "a" = Reg 0
parse_reg "b" = Reg 1
parse_reg "c" = Reg 2
parse_reg "d" = Reg 3
parse_reg x = error ("Unknown register " ++ show x)

parse_src s
  | [(n, "")] <- reads s = SrcImm $ Imm n
  | otherwise            = SrcReg $ parse_reg s

insn_of_words ["cpy", src, dst] = Cpy (parse_src src) (parse_reg dst)
insn_of_words ["inc", reg] = Inc (parse_reg reg)
insn_of_words ["dec", reg] = Dec (parse_reg reg)
insn_of_words ["tgl", rel] = Tgl (parse_src rel)
insn_of_words ["out", rel] = Out (parse_src rel)
insn_of_words ["jnz", src, rel] = Jnz (parse_src src) (parse_src rel)
insn_of_words l = error ("Unrecognized insn " ++ show (unwords l))

parse_insn = insn_of_words . words
parse_prog = VU.fromList . map parse_insn . lines
parse_file = fmap parse_prog . readFile
