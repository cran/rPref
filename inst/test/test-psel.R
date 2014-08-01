
# Tests for psel/psel.indices and the preference constructors

test_that("Test Preference selection", {
  expect_that(psel(mtcars, low(mpg))$mpg, equals(c(10.4, 10.4)))
  expect_that(psel(mtcars, low(mpg), top = 5)$mpg, equals(c(10.4, 10.4, 13.3, 14.3, 14.7)))
  
  expect_that(psel(mtcars, low(mpg) & low(hp))$hp, equals(205))
  expect_that(sort(psel(mtcars, high(mpg) * high(hp))$mpg), equals(c(15, 15.8, 17.3, 19.7, 30.4, 32.4, 33.9)))
  
  expect_that(sort(psel(mtcars, true(mpg < 15) * true(am == 0))$mpg), equals(c(10.4, 10.4, 13.3, 14.3, 14.7)))
})


test_that("Test Preference selection with indices", {
  expect_that(sort(psel.indices(mtcars, high(am) * true(vs == 1))), equals(c(3, 18, 19, 20, 26, 28, 32)))
  expect_that(psel.indices(mtcars, low(mpg + hp), top = 5), equals(order(mtcars$mpg + mtcars$hp)[1:5]))
})


test_that("Test base preference macros", {
  expect_that(psel(mtcars, pos(carb, 2))$carb, equals(rep(2, 10)))
  expect_that(psel(mtcars, around(mpg, 20))$mpg, equals(19.7))
  expect_that(psel(mtcars, between(hp, 110, 120))$hp, equals(c(110, 110, 110, 113)))
  expect_that(psel(mtcars, between(hp, 115, 122))$hp, equals(c(123, 123)))
  
  expect_that(sort(psel(mtcars, layered(cyl, c(4, 6), 8), top = 4)$cyl), equals(c(4, 6, 6, 6)))
  expect_that(psel(mtcars, -layered(cyl, c(4, 6), 8))$cyl, equals(rep(8, 14)))
})

test_that("Test if environments are found correctly", {
  
  test_fun <- function() {
    f <- function(x) -x
    return(low(f(mpg)))
  }
  expect_that(psel(mtcars, test_fun()), equals(psel(mtcars, -low(mpg))))
})


library(dplyr)

test_that("Behavior of group_by function from dplyr package", {
  expect_that(attr(group_by(mtcars[1:5,], cyl),'indices'), equals(list(2, c(0,1,3), 4)))
})


test_that("Grouped preference selection", {
  expect_that(psel(group_by(mtcars, cyl), low(mpg))$mpg, equals(c(21.4, 17.8, 10.4, 10.4)))
  expect_that(psel(group_by(mtcars, cyl), low(mpg), top=3)$mpg, equals(c(21.4, 21.5, 22.8, 17.8, 18.1, 19.2, 10.4, 10.4, 13.3)))
  
  expect_that(as.data.frame(summarise(psel(group_by(mtcars, cyl), low(mpg) * low(hp)), n())), equals(data.frame(cyl=c(4,6,8),'n()'=c(5,2,2), check.names=FALSE)))
  
  expect_that(psel(group_by(mtcars, cc = cyl * carb), true(hp==110) & low(hp))$cc, equals(c(4, 6, 8, 16, 16, 24, 24, 32, 36, 64)))
})