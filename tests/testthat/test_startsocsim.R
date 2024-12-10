# Testing create_simulation_folder()
# Test whether different directory structures can be created.
test_that("We can create a named subdirectory within the current working directory", {
              expect_true(dir.exists(create_simulation_folder(basedir = ".", simdir = "test")))
})
test_that("We can create a randomly named subdirectory within the current working directory", {
              expect_true(dir.exists(create_simulation_folder(basedir = ".", simdir = NULL)))
})
test_that("Both arguments can be NULL (let the defaults do their thing)", {
              expect_true(dir.exists(create_simulation_folder(basedir = NULL, simdir = NULL)))
})
test_that("We can create a named subdirectory inside the user's home directory", {
              expect_true(dir.exists(create_simulation_folder(basedir = "~", simdir = "test")))
})
# TODO: Maybe we should trust that the user passes a directory with the correct
# permissions and, if they do not, make the program fail gracefully.
test_that("We can create a named subdirectory outside the user's home directory", {
              expect_true(dir.exists(create_simulation_folder(
                # On Windows, R defaults to "C:/Users/username/Documents".
                basedir = ifelse(Sys.info()["sysname" == "Windows"],
                                 dirname(dirname("~")),
                                 dirname("~")),
                simdir = "test")))
})

# Testing create_sup_file()
test_that("We're able to create a sup file in the current working directory.", {
              expect_no_error(create_sup_file(simdir = ".", simname = "test.sup"))
})
test_that("The function returns a string.", {
              expect_type(create_sup_file(simdir = ".", simname = "test.sup"),
                          "character")
})
test_that("The requested file actually exists afterwards.", {
              expect_true(file.exists(create_sup_file(simdir = ".",
                                                      simname = "test.sup")))
})

# Testing get_supervisory_content
test_that("We get back some text.", {
              expect_type(get_supervisory_content(simdir = ".",
                                                  sup_fn = create_sup_file(simdir = ".")))
})

# Testing simulation_time_to_years()
test_that("The simulation cannot run for a negative number of months.", {
              expect_error(simulation_time_to_years(simulation_time = -2L,
                                                    pre_simulation_time = 0L,
                                                    start_year = 0L))
})
test_that("Simulation time cannot be zero", {
              expect_error(simulation_time_to_years(simulation_time = 0L,
                                                    pre_simulation_time = 0L,
                                                    start_years = 0L))
})
test_that("Simulation time is numeric", {
              expect_type(simulation_time_to_years(simulation_time = 100L,
                                                   pre_simulation_time = 10L,
                                                   start_years = 1L),
                          "numeric")
})
test_that("Result is greater than zero", {
              expect_gt(simulation_time_to_years(simulation_time = 100L,
                                                 pre_simulation_time = 10L,
                                                 start_yeras = 1L),
                        0)
})


# TODO: either transform to expect_no_warning and downgrade "benign" warnings
# in socsim() to message() or leave at expect_no_error() and upgrade the 
# tryCatch block in socsim() to give an actual error if the simulation fails.
expect_no_error("Socsim runs with: local folder, default supfile, inprocess",
                {
                    socsim(folder = ".",
                           supfile = rsocsim::create_sup_file("."),
                           process_method = "inprocess")
                })  
expect_no_error("Socsim runs with: local folder, default supfile, future",
                {
                    socsim(folder = ".",
                           supfile = rsocsim::create_sup_file("."),
                           process_method = "future")
                })  
expect_no_error("Socsim runs with: local folder, default supfile, clustercall",
                {
                    socsim(folder = ".",
                           supfile = rsocsim::create_sup_file("."),
                           process_method = "clustercall")
                })  

# TODO: create test for print_last_line_of_logfile()
# TODO: create test for run1simulationwithfile_from_binary()
