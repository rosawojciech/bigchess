test_that('find_engine works', {

  engine_path <- find_engine()
  expect_type(engine_path, 'character')
  if (!is.null(engine_path)) {
    expect_true(grepl('/extdata/engine', engine_path))
  } else {
    expect_true(is.null(engine_path))
  }

})
