ss <- "1P-WHYSIhVu0TR-_023hmNbNrcYY0dA4_957shbTC6Bc"

paper_sizes <- googlesheets4::read_sheet(ss)

usethis::use_data(paper_sizes, overwrite = TRUE)

card_sizes <- page_sizes[page_sizes$type == "card", ]

card_sizes <- dplyr::select(card_sizes, -c(series, size, standard, type))

usethis::use_data(card_sizes, overwrite = TRUE)
