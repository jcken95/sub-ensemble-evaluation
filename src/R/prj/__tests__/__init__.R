# See https://klmr.me/box/articles/testing.html
box::use(testthat[...])

.on_load <- function(ns) {
  test_dir(box::file())
}

box::export()
