
# FS=\frac{c' + (\gamma z \cos^2\beta - m\,\gamma_w z \cos^2\beta)\tan\varphi'}{\gamma z \sin\beta\cos\beta}

set.seed(123)

# Número de simulaciones
n <- 100000

# Constantes del problema
beta_deg <- 30          # ángulo del talud (grados)
z <- 2.0                # profundidad del plano de falla (m)
gamma_w <- 9.81         # peso unitario del agua (kN/m3)

beta <- beta_deg * pi / 180

# Variables aleatorias (ajusta rangos/distribuciones a tu sitio)
c_kpa <- runif(n, min = 5, max = 20)         # cohesión efectiva c' (kPa)
phi_deg <- runif(n, min = 22, max = 35)      # fricción efectiva phi' (grados)
gamma_sat <- runif(n, min = 17, max = 21)    # peso unitario suelo (kN/m3)
m <- runif(n, min = 0.0, max = 1.0)          # razón de presión de poros (0 seco, 1 saturado)

phi <- phi_deg * pi / 180

# Factor de seguridad para cada simulación
numerador <- c_kpa + (gamma_sat * z * cos(beta)^2 - m * gamma_w * z * cos(beta)^2) * tan(phi)
denominador <- gamma_sat * z * sin(beta) * cos(beta)
fs <- numerador / denominador

# Resultados
resultado <- list(
  parametros = list(
    n = n,
    beta_deg = beta_deg,
    z_m = z,
    gamma_w_kN_m3 = gamma_w
  ),
  resumen_fs = c(
    media = mean(fs),
    p5 = quantile(fs, 0.05),
    p50 = quantile(fs, 0.50),
    p95 = quantile(fs, 0.95)
  ),
  prob_falla = mean(fs < 1),
  fs = fs
)

resultado

# --- Distribuciones adaptadas ---

# 1) Cohesión c' ~ Lognormal con media=12 kPa y sd=4 kPa
mean_c <- 12
sd_c <- 4
sdlog_c <- sqrt(log(1 + (sd_c^2 / mean_c^2)))
meanlog_c <- log(mean_c) - 0.5 * sdlog_c^2
c_kpa <- rlnorm(n, meanlog = meanlog_c, sdlog = sdlog_c)

# 2) Fricción φ' ~ Normal(28, 3), truncada [20, 40]
phi_deg <- rnorm(n, mean = 28, sd = 3)
phi_deg <- pmin(pmax(phi_deg, 20), 40)

# 3) Peso unitario γsat ~ Normal(19, 1), truncada [16, 22]
gamma_sat <- rnorm(n, mean = 19, sd = 1)
gamma_sat <- pmin(pmax(gamma_sat, 16), 22)

# 4) m ~ Beta(2, 5) (más probabilidad de condiciones parcialmente secas)
m <- rbeta(n, shape1 = 2, shape2 = 5)

phi <- phi_deg * pi / 180

# Factor de seguridad
numerador <- c_kpa + (gamma_sat * z * cos(beta)^2 - m * gamma_w * z * cos(beta)^2) * tan(phi)
denominador <- gamma_sat * z * sin(beta) * cos(beta)
fs <- numerador / denominador
pf = mean(fs < 1)
beta_reliability = (mean(fs) - 1.0) / sd(fs)

resultado <- list(
  supuestos = list(
    c = "Lognormal(mean=12, sd=4)",
    phi = "Normal truncada [20,40], mean=28, sd=3",
    gamma_sat = "Normal truncada [16,22], mean=19, sd=1",
    m = "Beta(2,5)"
  ),
  resumen_fs = c(
    media = mean(fs),
    p5 = unname(quantile(fs, 0.05)),
    p50 = unname(quantile(fs, 0.50)),
    p95 = unname(quantile(fs, 0.95))
  ),
  prob_falla = pf,
  indice_confiabilidad = beta_reliability
)

resultado