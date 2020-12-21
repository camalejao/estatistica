# funções para ajudar na prova

intervalo_confianca_var_con <- function(alfa, dp, media, n) {
  z = qnorm(1 - (alfa/2), 0, 1)
  z = round(z, 2)
  erro = z * (dp / sqrt(n))
  erro = round(erro, 5)
  cat("[ ", media - erro, " - ", media + erro, " ]")
}

intervalo_confianca_var_des <- function(alfa, dp, media, n) {
  if (n < 30) {
    # ou t.test(vetor da amostra, conf = 1 - alfa)
    t = qt(1 - (alfa/2), df = n - 1)
    t = round(t, 2)
    erro = t * (dp / sqrt(n))
  } else {
    z = qnorm(1 - (alfa/2), 0, 1)
    z = round(z, 2)
    erro = z * (dp / sqrt(n))
  }
  erro = round(erro, 5)
  cat("[ ", media - erro, " - ", media + erro, " ]")
}

intervalo_confianca_proporcao_amostral <- function(alfa, n, sucessos) {
  p = sucessos / n
  z = qnorm(1 - (alfa/2), 0, 1)
  z = round(z, 2)
  erro = z * sqrt((p * (1 - p))/n)
  erro = round(erro, 5)
  cat("[ ", p - erro, " - ", p + erro, " ]")
}

intervalo_confianca_prop_p_con <- function(alfa, n, p) {
  z = qnorm(1 - (alfa/2), 0, 1)
  z = round(z, 2)
  erro = z * sqrt((p * (1 - p))/n)
  erro = round(erro, 5)
  cat("[ ", p - erro, " - ", p + erro, " ]")
}

prop_amostral_encontrar_n <- function(alfa, erro) {
  p = 0.5
  z = qnorm(1 - (alfa/2), 0, 1)
  z = round(z, 3)
  n = (z**2) * p * (1 - p) / (erro**2)
  print(n)
}


teste_bilateral <- function(alfa, media_hipotese, dp, media_amostra, n) {
  z = qnorm(1 - (alfa/2))
  z = round(z, 2)
  cat("Z tabelado = ", -z, "e", z, "\n")
  z_calc = (media_amostra - media_hipotese) / (dp / sqrt(n))
  cat("Z calculado = ", z_calc, "\n")
  print(z_calc > -z && z_calc < z)
}

teste_esquerda <- function(alfa, media_hipotese, dp, media_amostra, n) {
  z = qnorm(1 - alfa)
  z = round(z, 2)
  cat("Z tabelado = ", -z, "\n")
  z_calc = (media_amostra - media_hipotese) / (dp / sqrt(n))
  cat("Z calculado = ", z_calc, "\n")
  print(z_calc > -z)
}

teste_direita <- function(alfa, media_hipotese, dp, media_amostra, n) {
  z = qnorm(1 - alfa)
  z = round(z, 2)
  cat("Z tabelado = ", z, "\n")
  z_calc = (media_amostra - media_hipotese) / (dp / sqrt(n))
  cat("Z calculado = ", z_calc, "\n")
  print(z_calc < z)
}

teste_t_bilateral <- function(alfa, media_amostra, media_hipotese, dp, n) {
  t = qt(1 - (alfa/2), df = n - 1)
  cat(-t, t, "\n")
  t_obs = (media_amostra - media_hipotese) / (dp/sqrt(n))
  print(t_obs)
  print(t_obs > -t && t_obs < t)
}

teste_t_esquerda <- function(alfa, media_amostra, media_hipotese, dp, n) {
  t = qt(1 - alfa, df = n - 1)
  print(-t)
  t_obs = (media_amostra - media_hipotese) / (dp/sqrt(n))
  print(t_obs)
  print(t_obs > -t)
}

teste_t_direita <- function(alfa, media_amostra, media_hipotese, dp, n) {
  t = qt(1 - alfa, df = n - 1)
  print(t)
  t_obs = (media_amostra - media_hipotese) / (dp/sqrt(n))
  print(t_obs)
  print(t_obs < t)
}








