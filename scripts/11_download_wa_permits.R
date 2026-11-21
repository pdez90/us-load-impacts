# =============================================================================
# 11_download_wa_permits.R  --  Washington Ecology approval orders for data centers
# -----------------------------------------------------------------------------
# Run from the repo root on a machine with internet access:  Rscript scripts/11_download_wa_permits.R
# In : data-raw/permits/wa/wa_list.csv   Ecology's data center list, entered by hand from
#                                        https://ecology.wa.gov/air-climate/air-quality/data-centers (Nov 2026)
# Out: data-raw/permits/wa/pdf/<order>.pdf, and data-raw/permits/wa/pdf/SOURCE
# Ecology's robots.txt does not exclude these documents (it excludes the admin area, Special-Pages,
# Asset-Collections/Link-Assets, Shared-Text and Shared-Contacts). The script waits 10 s between requests, says
# who it is, and stops at the first refusal (403 or 429) rather than retrying. If it is refused, open each URL in
# the list in a browser and save it as data-raw/permits/wa/pdf/<order>.pdf instead.
# Only rows with a link are fetched; the others are listed by Ecology without a document.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
L <- fread("data-raw/permits/wa/wa_list.csv")
DIR <- "data-raw/permits/wa/pdf"; dir.create(DIR, showWarnings = FALSE, recursive = TRUE)
UA <- "us-load-impacts research script (https://github.com/pdez90/us-load-impacts)"
options(timeout = 300)
todo <- L[!is.na(url) & nzchar(url) & grepl("ecology\\.wa\\.gov", url)]
got <- character()
for (i in seq_len(nrow(todo))) {
  f <- file.path(DIR, paste0(gsub("[^A-Za-z0-9-]+", "_", todo$order_no[i]), ".pdf"))
  if (file.exists(f) && file.size(f) > 1e4) { cat("have", f, "\n"); next }
  if (length(got)) Sys.sleep(10)
  cat("fetching", todo$order_no[i], todo$facility[i], "\n")
  tmp <- paste0(f, ".part")
  ok <- tryCatch(download.file(todo$url[i], tmp, mode = "wb", quiet = TRUE, method = "libcurl", headers = c("User-Agent" = UA)) == 0,
                 error = function(e) { message("  ", conditionMessage(e)); FALSE })
  is_pdf <- ok && file.exists(tmp) && identical(readBin(tmp, "raw", 4), charToRaw("%PDF"))
  if (!is_pdf) {
    unlink(tmp)
    stop("Ecology did not serve a PDF for ", todo$order_no[i], " (", todo$url[i], "). Stopping without retrying; save the remaining ",
         "documents by hand from a browser into ", DIR, ".")
  }
  file.rename(tmp, f); got <- c(got, todo$order_no[i])
}
writeLines(c(sprintf("Downloaded %s from the links in data-raw/permits/wa/wa_list.csv", format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
             paste("  ", list.files(DIR, pattern = "\\.pdf$"))), file.path(DIR, "SOURCE"))
cat(sprintf("%d new, %d PDFs in %s; %d listed by Ecology without a document\n", length(got), length(list.files(DIR, pattern = "\\.pdf$")),
            DIR, sum(is.na(L$url) | !nzchar(L$url))))
