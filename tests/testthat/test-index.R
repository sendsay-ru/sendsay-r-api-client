test_that("request without authentication", {
  res <- sendsay(action="ping")

  expect_equal(is.null(res$pong), FALSE)
})

test_that("request with custom host", {
  res <- sendsay(action="ping", .host="https://api.sendsay.ru")

  expect_equal(is.null(res$pong), FALSE)
})

test_that("request with authentication", {
  auth_res <- sendsay(action="login", login="demo", passwd="demo")

  res <- sendsay(action="pong", .session=auth_res$session)

  expect_equal(is.null(res$ping), FALSE)
})
