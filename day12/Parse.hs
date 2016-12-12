module Parse where
import Machine

parse_reg "a" = Reg 0
parse_reg "b" = Reg 1
parse_reg "c" = Reg 2
parse_reg "d" = Reg 3
parse_reg x = error ("Unknown register " ++ show x)

parse_src s
  | [(n, "")] <- reads s = FromImm n
  | otherwise            = FromReg $ parse_reg s

insn_of_words ["cpy", src, dst] = ICpy (parse_src src) (parse_reg dst)
insn_of_words ["inc", reg] = IInc (parse_reg reg)
insn_of_words ["dec", reg] = IDec (parse_reg reg)
insn_of_words ["jnz", src, rel] = IJnz (parse_src src) (read rel)
insn_of_words l = error ("Unrecognized insn " ++ show (unwords l))

parse_insn = insn_of_words . words
parse_prog = map parse_insn . lines
parse_file = fmap parse_prog . readFile
