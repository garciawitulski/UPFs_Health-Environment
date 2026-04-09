bootstrap_script_dir <- function() {
  for (i in rev(seq_along(sys.frames()))) {
    if (!is.null(sys.frames()[[i]]$ofile)) return(dirname(normalizePath(sys.frames()[[i]]$ofile, winslash = "/", mustWork = FALSE)))
  }
  args <- commandArgs(trailingOnly = FALSE)
  hit <- args[grepl("^--file=", args)]
  if (length(hit)) return(dirname(normalizePath(sub("^--file=", "", hit[1]), winslash = "/", mustWork = FALSE)))
  getwd()
}
source(file.path(bootstrap_script_dir(), "00_config.R"), local = TRUE)

tex_path <- file.path(FIGSRC_DIR, "Figure_5.tex")
if (!file.exists(tex_path)) {
  stop("Missing Figure 5 source: ", tex_path)
}

latexmk <- Sys.which("latexmk")
if (!nzchar(latexmk)) {
  stop("`latexmk` not found on PATH.")
}

old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(FIGSRC_DIR)

cmd_out <- system2(
  latexmk,
  c("-pdf", "-interaction=nonstopmode", "-halt-on-error", "Figure_5.tex"),
  stdout = TRUE,
  stderr = TRUE
)
status <- attr(cmd_out, "status")
if (!is.null(status) && status != 0) {
  stop(paste(c("Figure_5.tex failed to compile.", cmd_out), collapse = "\n"))
}

pdf_src <- file.path(FIGSRC_DIR, "Figure_5.pdf")
if (!file.exists(pdf_src)) {
  stop("Expected compiled PDF not found: ", pdf_src)
}

sync_binary(pdf_src, dest_name = "Figure_5.pdf", sync_submission = TRUE)
png_out <- file.path(FIG_DIR, "Figure_5.png")
png_ok <- convert_pdf_to_png(pdf_src, png_out)
if (png_ok && IS_NESTED_SUBMISSION) {
  file.copy(png_out, file.path(SUBMISSION_ROOT, "Figure_5.png"), overwrite = TRUE)
  file.copy(png_out, file.path(SUBMISSION_FIG_DIR, "Figure_5.png"), overwrite = TRUE)
}

message("Saved Figure_5 to ", FIG_DIR, if (png_ok) " (pdf + png)" else " (pdf only)")
