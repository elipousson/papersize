test_that("get_social_size works", {
  expect_true(
    get_social_size(name = "Instagram post")[["name"]] == "Instagram post"
  )
  expect_true(
    all(get_social_size(platform = "Twitter")[["platform"]] == "Twitter")
  )
  expect_true(
    all(get_social_size(format = "story")[["size"]] == "story")
  )
})
