#' Find a chess engine
#'
#' A helper function allowing unit tests to run using a user-supplied chess
#' engine that is compatible with the local hardware/OS.
#'
#' @details The `find_engine()` function returns the path to a chess engine that
#'   has been placed in the `bigchess` subdirectory `/inst/extdata/engine`. The
#'   purpose is to allow unit tests and examples to use a chess engine without
#'   having to edit every test and example to add the engine's path by hand.
#'
#'   Instead, a UCI-compatible chess engine suitable for the user's machine may
#'   be placed in the `/inst/extdata/engine` subdirectory, and the
#'   `find_engine()` function can be used wherever an example or unit test would
#'   otherwise require editing to add the path to an engine.
#'
#'   When an engine is found the function will return a string with the engine's
#'   full path. If more than one engine is found, only the first one will be
#'   used. If no engine is found, `find_engine()` will return `NULL`.
#'
#' @note The `/engine` directory contains a text file named
#'   'Engine_goes_here-README' explaining the purpose of the directory. If any
#'   other files are placed in `/inst/extdata/engine` that are not
#'   UCI-compatible chess engines capable of running on the user's machine, any
#'   code making use of `find_engine()` is expected to fail.
#'
#' @return A string containing the path to an engine, or `NULL` if no engine is
#'   found.
#' @export
#'
#' @examples
#' find_engine()
find_engine <- function() {

  # Set up the path to a chess engine for the unit tests and examples
  engine_dir  <- system.file("extdata/engine", package = "bigchess")
  files       <- list.files(engine_dir)
  files       <- files[files != 'Engine_goes_here-README'][1]
  engine_path <- file.path(engine_dir, files)

  # Check that the engine exists
  if (!file.exists(engine_path)) {
    engine_path   <- NULL
  }

  return(engine_path)

}
