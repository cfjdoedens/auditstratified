test_that("HHH == 0.95", {
  expect_equal(haro_nog_nodige_zekerheid(ihr = "H", ibr = "H", car = "H"), 0.95)
})
test_that("MHH == 0.92", {
  expect_equal(round(
    haro_nog_nodige_zekerheid(ihr = "M", ibr = "H", car = "H"),
    digits = 2
  ), 0.92)
})
test_that("HHM == 0.90", {
  expect_equal(haro_nog_nodige_zekerheid(ihr = "H", ibr = "H", car = "M"), 0.90)
})
test_that("HMH == 0.90", {
  expect_equal(round(
    haro_nog_nodige_zekerheid(ihr = "H", ibr = "M", car = "H"),
    digits = 2
  ), 0.90)
})
test_that("HLH == 0.85", {
  expect_equal(round(
    haro_nog_nodige_zekerheid(ihr = "H", ibr = "L", car = "H"),
    digits = 2
  ), 0.85)
})
test_that("MMH == 0.85", {
  expect_equal(round(
    haro_nog_nodige_zekerheid(ihr = "M", ibr = "M", car = "H"),
    digits = 2
  ), 0.85)
})
test_that("MHM == 0.84", {
  expect_equal(round(
    haro_nog_nodige_zekerheid(ihr = "M", ibr = "H", car = "M"),
    digits = 2
  ), 0.84)
})
test_that("HMM == 0.81", {
  expect_equal(round(
    haro_nog_nodige_zekerheid(ihr = "H", ibr = "M", car = "M"),
    digits = 2
  ), 0.81)
})
test_that("HHL == 0.80", {
  expect_equal(haro_nog_nodige_zekerheid(ihr = "H", ibr = "H", car = "L"), 0.80)
})
test_that("LHH == 0.88", {
  expect_equal(round(
    haro_nog_nodige_zekerheid(ihr = "L", ibr = "H", car = "H"),
    digits = 2
  ), 0.88)
})
test_that("MLH == 0.77", {
  expect_equal(round(
    haro_nog_nodige_zekerheid(ihr = "M", ibr = "L", car = "H"),
    digits = 2
  ), 0.77)
})
test_that("LMH == 0.76", {
  expect_equal(round(
    haro_nog_nodige_zekerheid(ihr = "L", ibr = "M", car = "H"),
    digits = 2
  ), 0.76)
})
test_that("LHM == 0.75", {
  expect_equal(round(
    haro_nog_nodige_zekerheid(ihr = "L", ibr = "H", car = "M"),
    digits = 2
  ), 0.75)
})
test_that("HLM == 0.71", {
  expect_equal(round(
    haro_nog_nodige_zekerheid(ihr = "H", ibr = "L", car = "M"),
    digits = 2
  ), 0.71)
})
test_that("MMM == 0.69", {
  expect_equal(round(
    haro_nog_nodige_zekerheid(ihr = "M", ibr = "M", car = "M"),
    digits = 2
  ), 0.69)
})
test_that("MHL == 0.68", {
  expect_equal(round(
    haro_nog_nodige_zekerheid(ihr = "M", ibr = "H", car = "L"),
    digits = 2
  ), 0.68)
})
test_that("LLH == 0.63", {
  expect_equal(round(
    haro_nog_nodige_zekerheid(ihr = "L", ibr = "L", car = "H"),
    digits = 2
  ), 0.63)
})
test_that("HML == 0.62", {
  expect_equal(round(
    haro_nog_nodige_zekerheid(ihr = "H", ibr = "M", car = "L"),
    digits = 2
  ), 0.62)
})
test_that("MLM == - (0.53)", {
  expect_equal(round(
    haro_nog_nodige_zekerheid(ihr = "M", ibr = "L", car = "M"),
    digits = 2
  ), 0.53)
})
test_that("LMM == - (0.52)", {
  expect_equal(round(
    haro_nog_nodige_zekerheid(ihr = "L", ibr = "M", car = "M"),
    digits = 2
  ), 0.52)
})
test_that("LHL == - (0.50)", {
  expect_equal(round(
    haro_nog_nodige_zekerheid(ihr = "L", ibr = "H", car = "L"),
    digits = 2
  ), 0.50)
})
test_that("HLL == - (0.41)", {
  expect_equal(round(
    haro_nog_nodige_zekerheid(ihr = "H", ibr = "L", car = "L"),
    digits = 2
  ), 0.41)
})
test_that("MML == - (0.39)", {
  expect_equal(round(
    haro_nog_nodige_zekerheid(ihr = "M", ibr = "M", car = "L"),
    digits = 2
  ), 0.39)
})
test_that("LLM == - (0.26)", {
  expect_equal(round(
    haro_nog_nodige_zekerheid(ihr = "L", ibr = "L", car = "M"),
    digits = 2
  ), 0.26)
})
test_that("MLL == - (0.07)", {
  expect_equal(round(
    haro_nog_nodige_zekerheid(ihr = "M", ibr = "L", car = "L"),
    digits = 2
  ), 0.12)
})
test_that("LML == - (0.04)", {
  expect_equal(round(
    haro_nog_nodige_zekerheid(ihr = "L", ibr = "M", car = "L"),
    digits = 2
  ), 0.09)
})
test_that("LLL == - (0.00)", {
  expect_equal(round(
    haro_nog_nodige_zekerheid(ihr = "L", ibr = "L", car = "L"),
    digits = 2
  ), 0.05)
})
