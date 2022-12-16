url <- "https://docs.google.com/spreadsheets/d/1P-WHYSIhVu0TR-_023hmNbNrcYY0dA4_957shbTC6Bc/edit?usp=sharing"

paper_sizes <- googlesheets4::read_sheet(url)

usethis::use_data(paper_sizes, overwrite = TRUE)

card_sizes <- paper_sizes[paper_sizes$type == "card",]

card_sizes <- dplyr::select(card_sizes, -c(series, size, standard, type))

usethis::use_data(card_sizes, overwrite = TRUE)
