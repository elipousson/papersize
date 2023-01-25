test_that("as_asp works", {
  expect_equal(
    as_asp(8.5 / 11),
    8.5 / 11,
    tolerance = 0.0000001
    )
  expect_equal(
    as_asp("11:17"),
    11 / 17,
    tolerance = 0.0000001
  )
  expect_equal(
    as_asp("3/2", sep = "/"),
    3 / 2,
    tolerance = 0.0000001
  )
  expect_equal(
    as_asp(page = "letter"),
    8.5 / 11,
    tolerance = 0.0000001
  )
  expect_equal(
    as_asp(page = "letter", orientation = "landscape"),
    11 / 8.5,
    tolerance = 0.0000001
  )
  expect_equal(
    as_asp(page = "letter", orientation = "portrait", flipped = TRUE),
    11 / 8.5,
    tolerance = 0.0000001
  )
  expect_equal(
    as_asp(page = make_page_size(8.5, 11, "in"), flipped = TRUE),
    11 / 8.5,
    tolerance = 0.0000001
  )
})
