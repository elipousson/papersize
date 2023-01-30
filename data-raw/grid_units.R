grid_units <-
  c(
    "npc", "cm", "centimeter", "centimetre", "centimeters", "centimetres",
    "in", "inch", "inches", "mm", "millimeter", "millimetre", "millimeters",
    "millimetres", "pt", "point", "points", "pc", "picas", "bp", "bigpts",
    "dd", "dida", "cc", "cicero", "sp", "scaledpts", "lines", "char",
    "native", "snpc", "strwidth", "strheight", "grobheight"
  )

usethis::use_data(grid_units, overwrite = TRUE)
