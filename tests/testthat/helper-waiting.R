# Making operation types wait, for the length of a test.
#
# Nothing in `OBT_OPERATIONS` waits any more: every entry point the package
# calls is written on the workbook side. The waiting machinery is still live
# code, because the next operation written ahead of its entry point sets
# `waits_on` again, so it is still tested. The tests that need a waiting
# operation make one here rather than leaning on a type that happens to wait.
#
# Every reader of a spec goes through `operation_spec()`, so mocking that one
# function is the whole seam. The mock answers the real spec for every type
# but the named ones, and a named one comes back with `waits_on` filled.

# Mark one or more types as waiting until the calling test finishes.
local_waiting_type <- function(
  types,
  needs = "an entry point a script can call",
  .env = parent.frame()
) {
  real <- operation_spec

  testthat::local_mocked_bindings(
    operation_spec = function(type) {
      spec <- real(type)

      if (type %in% types) {
        spec$waits_on <- needs
      }

      spec
    },
    .env = .env
  )

  invisible(types)
}
