test_that("Fidelity test", {
  data("LT")
  Qxt = LT
  expect_equal(getqxt(Qxt, nag=nrow(Qxt), t=ncol(Qxt))$Qxt,getqxt(Qxt, nag=nrow(Qxt), t=ncol(Qxt))$Qxt_test ,tolerance=0.005)

  Qx = LT[, 11]
  expect_equal( getqx(Qx, nag=length(Qx))$Qxt, getqx(Qx, nag=length(Qx))$Qxt_test,tolerance=0.005)

   })

test_that("zero-one test", {
  data("LT")
  Qxt = LT
  q = getqxt(Qxt, nag=nrow(Qxt), t=ncol(Qxt))$qxt
  n = nrow(q)
  t = ncol(q)
  expect_true(all(q>0 & q<=1))

  Qx = LT[, 11]
  q = getqx(Qx, nag=length(Qx))$qx
  n = nrow(q)
  t = ncol(q)
  expect_true(all(q>0 & q<=1))

})
