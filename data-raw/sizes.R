ss <- "1P-WHYSIhVu0TR-_023hmNbNrcYY0dA4_957shbTC6Bc"

paper_sizes <- googlesheets4::read_sheet(ss)

paper_sizes[["name"]] <- as.character(paper_sizes[["name"]])
paper_sizes[["size"]] <- as.character(paper_sizes[["size"]])

paper_sizes[paper_sizes[["size"]] == "NULL", ]$size <- NA_character_

usethis::use_data(paper_sizes, overwrite = TRUE)

card_sizes <- paper_sizes[paper_sizes$type == "card", ]

card_sizes <- dplyr::select(card_sizes, -c(series, size, standard, type))

usethis::use_data(card_sizes, overwrite = TRUE)
