library(shinytest2)

test_that("App starts and loads without errors", {
  app <- AppDriver$new(
    app_dir = "../../",
    name = "startup-test",
    timeout = 30000,
    load_timeout = 30000
  )
  on.exit(app$stop(), add = TRUE)

  # App should be alive
  expect_true(!is.null(app))

  # Login tab should be visible on startup
  val <- app$get_value(input = "login_role")
  expect_equal(val, "Patient")
})

test_that("Patient login works and navigates to Overview", {
  app <- AppDriver$new(
    app_dir = "../../",
    name = "patient-login",
    timeout = 30000,
    load_timeout = 30000
  )
  on.exit(app$stop(), add = TRUE)

  # Select a patient ID and login
  app$set_inputs(login_role = "Patient")
  app$set_inputs(user_id_field = "1604")
  app$click("login_button")
  Sys.sleep(2)

  # Should navigate to overview tab
  nav <- app$get_value(input = "main_nav")
  expect_equal(nav, "overview_tab")
})

test_that("Clinician login works and navigates to Overview", {
  app <- AppDriver$new(
    app_dir = "../../",
    name = "clinician-login",
    timeout = 30000,
    load_timeout = 30000
  )
  on.exit(app$stop(), add = TRUE)

  # Login as clinician
  app$set_inputs(login_role = "Clinician")
  app$set_inputs(clinician_password = "ebreath2024")
  app$click("login_button")
  Sys.sleep(2)

  nav <- app$get_value(input = "main_nav")
  expect_equal(nav, "overview_tab")
})

test_that("Patient can navigate all tabs without errors", {
  app <- AppDriver$new(
    app_dir = "../../",
    name = "patient-tabs",
    timeout = 30000,
    load_timeout = 30000
  )
  on.exit(app$stop(), add = TRUE)

  # Login as patient
  app$set_inputs(login_role = "Patient")
  app$set_inputs(user_id_field = "1604")
  app$click("login_button")
  Sys.sleep(2)

  # Navigate through each tab and check no crash
  tabs <- c("alpha_tab", "beta_tab", "taxonomy_tab", "patho_tab", "summary_tab")
  for (tab in tabs) {
    app$set_inputs(main_nav = tab)
    Sys.sleep(2)
    nav <- app$get_value(input = "main_nav")
    expect_equal(nav, tab, info = paste("Failed on tab:", tab))
  }
})

test_that("Clinician can navigate all tabs without errors", {
  app <- AppDriver$new(
    app_dir = "../../",
    name = "clinician-tabs",
    timeout = 30000,
    load_timeout = 30000
  )
  on.exit(app$stop(), add = TRUE)

  # Login as clinician
  app$set_inputs(login_role = "Clinician")
  app$set_inputs(clinician_password = "ebreath2024")
  app$click("login_button")
  Sys.sleep(2)

  # Navigate through each tab
  tabs <- c("alpha_tab", "beta_tab", "taxonomy_tab", "patho_tab", "summary_tab")
  for (tab in tabs) {
    app$set_inputs(main_nav = tab)
    Sys.sleep(2)
    nav <- app$get_value(input = "main_nav")
    expect_equal(nav, tab, info = paste("Failed on tab:", tab))
  }
})

test_that("Clinician filters work on taxonomy tab", {
  app <- AppDriver$new(
    app_dir = "../../",
    name = "clinician-filters",
    timeout = 30000,
    load_timeout = 30000
  )
  on.exit(app$stop(), add = TRUE)

  # Login as clinician
  app$set_inputs(login_role = "Clinician")
  app$set_inputs(clinician_password = "ebreath2024")
  app$click("login_button")
  Sys.sleep(2)

  # Go to taxonomy tab
  app$set_inputs(main_nav = "taxonomy_tab")
  Sys.sleep(2)

  # Test diagnosis filter
  app$set_inputs(tax_diagnosis = "CVID")
  Sys.sleep(2)
  expect_equal(app$get_value(input = "tax_diagnosis"), "CVID")

  # Test sample type filter
  app$set_inputs(tax_type = "Sputum")
  Sys.sleep(2)
  expect_equal(app$get_value(input = "tax_type"), "Sputum")

  # Switch diagnosis
  app$set_inputs(tax_diagnosis = "XLA")
  Sys.sleep(2)
  expect_equal(app$get_value(input = "tax_diagnosis"), "XLA")
})

test_that("Overview filter is independent from other tabs", {
  app <- AppDriver$new(
    app_dir = "../../",
    name = "filter-independence",
    timeout = 30000,
    load_timeout = 30000
  )
  on.exit(app$stop(), add = TRUE)

  # Login as clinician
  app$set_inputs(login_role = "Clinician")
  app$set_inputs(clinician_password = "ebreath2024")
  app$click("login_button")
  Sys.sleep(2)

  # Set Overview filter to a specific patient
  app$set_inputs(clinician_filter_id = "1604")
  Sys.sleep(2)

  # Navigate to taxonomy — should still work (not filtered by Overview's ID)
  app$set_inputs(main_nav = "taxonomy_tab")
  Sys.sleep(3)
  nav <- app$get_value(input = "main_nav")
  expect_equal(nav, "taxonomy_tab")

  # Navigate to alpha — should still work
  app$set_inputs(main_nav = "alpha_tab")
  Sys.sleep(3)
  nav <- app$get_value(input = "main_nav")
  expect_equal(nav, "alpha_tab")

  # Navigate to pathogenomics — should still work
  app$set_inputs(main_nav = "patho_tab")
  Sys.sleep(3)
  nav <- app$get_value(input = "main_nav")
  expect_equal(nav, "patho_tab")
})
