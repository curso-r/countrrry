test_that("funcao calcular_estatisticas() funciona", {
  # expect_equal(2 * 2, 4)

  # espero que dê erro se não informado o continente
  expect_error(calcular_estatisticas())

  estat_asia <- calcular_estatisticas("Ásia")

  # espero que seja uma tibble
  expect_s3_class(estat_asia, "tbl")

  # sempre ter 3 colunas!
  expect_equal(ncol(estat_asia), 3)

  # deve dar erro quando usar um continente não disponível na base
  expect_error(calcular_estatisticas("América"))

  # calcular_estatisticas("Américas", anos = 1000)

  expect_snapshot(calcular_estatisticas("Américas"))
})
