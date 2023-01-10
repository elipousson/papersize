page_extras <- list()

page_extras$margins <- tibble::tribble(
        ~name,    ~t,    ~r,    ~b,    ~l, ~unit,
  "extrawide",     2,     2,     2,     2,  "in",
       "wide",   1.5,   1.5,   1.5,   1.5,  "in",
   "standard",     1,     1,     1,     1,  "in",
     "narrow",  0.75,  0.75,  0.75,  0.75,  "in",
    "minimal", 0.125, 0.125, 0.125, 0.125,  "in",
       "none",     0,     0,     0,     0,  "in",
  "extrawide",     6,     6,     6,     6,  "cm",
       "wide",     4,     4,     4,     4,  "cm",
   "standard",     3,     3,     3,     3,  "cm",
     "narrow",     2,     2,     2,     2,  "cm",
    "minimal",   0.5,   0.5,   0.5,   0.5,  "cm",
       "none",     0,     0,     0,     0,  "cm"
  )

usethis::use_data(page_extras, overwrite = TRUE)
