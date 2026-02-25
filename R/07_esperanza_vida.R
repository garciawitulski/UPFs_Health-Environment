# 07_esperanza_vida.R
# Tabla de vida con causa eliminada. Ganancia e30 si se eliminan muertes atribuibles a UPF.

root <- if (exists("PROJECT_ROOT")) PROJECT_ROOT else getwd()
data_dir <- file.path(root, "data")
out_dir <- file.path(root, "output")
E70 <- 18.5
age_order <- c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69")

library(readr)
library(dplyr)

build_lt <- function(deaths_vec, pop_vec) {
  n <- 5
  m_x <- deaths_vec / replace(pop_vec, pop_vec == 0, 1)
  q_x <- (n * m_x) / (1 + (n/2) * m_x)
  q_x[m_x >= 2] <- 0.999
  l_x <- c(100000, 100000 * cumprod(1 - q_x[-length(q_x)]))
  l70 <- l_x[8] * (1 - q_x[8])
  L_x <- n * (l_x + c(l_x[-1], l70)) / 2
  T_x <- rev(cumsum(rev(L_x)))
  T_x[8] <- T_x[8] + l70 * E70
  e_x <- T_x / l_x
  data.frame(age_group = age_order, deaths = deaths_vec, pop = pop_vec, l_x = l_x, e_x = e_x)
}

pop_path <- file.path(data_dir, "poblacion_argentina_2018_ennys.csv")
if (!file.exists(pop_path)) pop_path <- file.path(data_dir, "poblacion_argentina_2018.csv")
pop_df <- read_csv(pop_path, show_col_types = FALSE)
def_df <- read_csv(file.path(data_dir, "defunciones_2019_por_estrato.csv"), show_col_types = FALSE)
res <- read_csv(file.path(out_dir, "resultados_paper_argentina.csv"), show_col_types = FALSE)

pop_agg <- pop_df %>% group_by(age_group) %>% summarise(population = sum(population), .groups = "drop")
deaths_agg <- def_df %>% group_by(age_group) %>% summarise(deaths = sum(deaths), .groups = "drop")
attr_agg <- res %>% group_by(age_group) %>% summarise(deaths_attr = sum(deaths_attr), .groups = "drop")

deaths_dict <- setNames(deaths_agg$deaths, deaths_agg$age_group)
pop_dict <- setNames(pop_agg$population, pop_agg$age_group)
attr_dict <- setNames(attr_agg$deaths_attr, attr_agg$age_group)
deaths_vec <- unname(deaths_dict[age_order])
pop_vec <- unname(replace(pop_dict[age_order], is.na(pop_dict[age_order]), 1))
attr_vec <- unname(replace(attr_dict[age_order], is.na(attr_dict[age_order]), 0))

lt_actual <- build_lt(deaths_vec, pop_vec)
deaths_sin_upf <- pmax(0, deaths_vec - attr_vec)
lt_sin <- build_lt(deaths_sin_upf, pop_vec)
e30_actual <- lt_actual$e_x[1]
e30_sin <- lt_sin$e_x[1]
ganancia <- e30_sin - e30_actual

# Por sexo
results_sex <- list()
for (sx in c(0, 1)) {
  pop_s <- setNames((pop_df %>% filter(sexo_m == sx))$population, (pop_df %>% filter(sexo_m == sx))$age_group)
  deaths_s <- setNames((def_df %>% filter(sexo_m == sx))$deaths, (def_df %>% filter(sexo_m == sx))$age_group)
  attr_s <- setNames((res %>% filter(sexo_m == sx))$deaths_attr, (res %>% filter(sexo_m == sx))$age_group)
  d_vec <- unname(replace(deaths_s[age_order], is.na(deaths_s[age_order]), 0))
  p_vec <- unname(replace(pop_s[age_order], is.na(pop_s[age_order]), 1))
  a_vec <- unname(replace(attr_s[age_order], is.na(attr_s[age_order]), 0))
  lt_a <- build_lt(d_vec, p_vec)
  lt_b <- build_lt(pmax(0, d_vec - a_vec), p_vec)
  results_sex[[length(results_sex) + 1]] <- data.frame(
    tipo = if (sx == 1) "Hombres" else "Mujeres",
    e30_actual = lt_a$e_x[1],
    e30_sin_UPF = lt_b$e_x[1],
    ganancia_anos = lt_b$e_x[1] - lt_a$e_x[1]
  )
}

out_list <- list(
  data.frame(tipo = "Total", e30_actual = e30_actual, e30_sin_UPF = e30_sin, ganancia_anos = ganancia),
  bind_rows(results_sex)
)
out_df <- bind_rows(out_list)
write_csv(out_df, file.path(out_dir, "esperanza_vida_perdida_UPF.csv"))
message("Ganancia e30 (total): ", round(ganancia, 3), " anos. Guardado: esperanza_vida_perdida_UPF.csv")
