bootstrap_script_dir <- function() {
  for (i in rev(seq_along(sys.frames()))) {
    if (!is.null(sys.frames()[[i]]$ofile)) {
      return(dirname(normalizePath(sys.frames()[[i]]$ofile, winslash = "/", mustWork = FALSE)))
    }
  }
  args <- commandArgs(trailingOnly = FALSE)
  hit <- args[grepl("^--file=", args)]
  if (length(hit)) return(dirname(normalizePath(sub("^--file=", "", hit[1]), winslash = "/", mustWork = FALSE)))
  getwd()
}

source(file.path(bootstrap_script_dir(), "..", "00_config.R"), local = TRUE)

RR_REF <- 0.357
RR_BY_CAUSE <- c(cvd = 1.29, cerebrovascular = 1.34)

rr_func <- function(x_pct, rr_point) {
  x <- pmax(pmin(x_pct / 100, 1), 0.001)
  exp(log(rr_point) / RR_REF * x)
}

paf_point <- function(mean_upf, rr_point) {
  rr <- rr_func(mean_upf, rr_point)
  paf <- (rr - 1) / rr
  paf[!is.finite(paf) | rr <= 0] <- 0
  pmax(0, pmin(1, paf))
}

upf <- read_calc_csv("upf_exposure_by_stratum.csv") %>%
  mutate(sexo_m = as.integer(sexo_m), pct_upf_mean = as.numeric(pct_upf_mean))

def_causa <- read_calc_csv("cause_specific_mortality_by_stratum.csv") %>%
  mutate(sexo_m = as.integer(sexo_m), deaths = as.numeric(deaths))

res_list <- list()
for (causa_i in names(RR_BY_CAUSE)) {
  rr_i <- as.numeric(RR_BY_CAUSE[[causa_i]])
  df <- def_causa %>%
    filter(causa == causa_i) %>%
    left_join(select(upf, sexo_m, age_group, pct_upf_mean), by = c("sexo_m", "age_group")) %>%
    mutate(
      PAF = paf_point(pct_upf_mean, rr_i),
      deaths_attr = PAF * deaths,
      causa = causa_i
    )
  res_list[[causa_i]] <- select(df, sexo_m, age_group, causa, deaths, PAF, deaths_attr)
}

res <- bind_rows(res_list)
summ <- res %>%
  group_by(causa) %>%
  summarise(deaths = sum(deaths), deaths_attr = sum(deaths_attr), .groups = "drop") %>%
  mutate(pct_attr = 100 * deaths_attr / deaths)

write_calc_csv(res, "resultados_por_causa.csv")
write_calc_csv(summ, "resumen_muertes_por_causa.csv")

message("Saved calculation outputs: resultados_por_causa.csv, resumen_muertes_por_causa.csv")
