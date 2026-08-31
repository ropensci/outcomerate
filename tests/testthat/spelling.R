error_on_ci <- identical(Sys.getenv("CI"), "true")
spelling::spell_check_test(vignettes = TRUE, error = error_on_ci)
