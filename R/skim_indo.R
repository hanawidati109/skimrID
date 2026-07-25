#' Indonesian Statistical Summary
#'
#' Provides descriptive summaries of datasets in Indonesian by
#' extending the functionality of the original skimr package.
#' The output includes dataset information, descriptive statistics,
#' missing values, completeness rate, outlier counts,
#' and inline histograms for numeric variables.
#'
#' @param data A data frame.
#'
#' @return
#' Prints a descriptive summary of the dataset in Indonesian.
#'
#' @examples
#' skim_indo(iris)
#'
#' @export

skim_indo <- function(data){

  if(!is.data.frame(data)){
    stop("Input harus berupa data frame.")
  }

  hasil <- skimr::skim(data)

  ################################################
  ## Fungsi Menghitung Outlier (IQR)
  ################################################

  hitung_outlier <- function(x){

    if(!is.numeric(x)) return(NA)

    q1 <- stats::quantile(x,0.25,na.rm=TRUE)

    q3 <- stats::quantile(x,0.75,na.rm=TRUE)

    iqr <- q3-q1

    sum(
      x < (q1-1.5*iqr) |
        x > (q3+1.5*iqr),
      na.rm=TRUE
    )

  }

  ################################################
  ## Ringkasan Dataset
  ################################################

  n_row <- nrow(data)

  n_col <- ncol(data)

  n_numeric <- sum(sapply(data,is.numeric))

  n_factor <- sum(sapply(data,is.factor))

  n_character <- sum(sapply(data,is.character))

  cat("\n")

  cat("=========================================\n")

  cat("            RINGKASAN DATA\n")

  cat("=========================================\n\n")

  cat(sprintf("%-25s : %s\n","Nama Dataset",
              deparse(substitute(data))))

  cat(sprintf("%-25s : %d\n","Jumlah Baris",
              n_row))

  cat(sprintf("%-25s : %d\n","Jumlah Kolom",
              n_col))

  cat("\n")

  cat("-----------------------------------------\n")

  cat("Frekuensi Tipe Variabel\n")

  cat("-----------------------------------------\n\n")

  cat(sprintf("%-15s : %d\n",
              "Numerik",
              n_numeric))

  cat(sprintf("%-15s : %d\n",
              "Faktor",
              n_factor))

  cat(sprintf("%-15s : %d\n",
              "Karakter",
              n_character))

  cat("\n")

  ################################################
  ## NUMERIC
  ################################################

  num <- hasil[hasil$skim_type == "numeric", ]

  if(nrow(num) > 0){

    cat("=========================================\n")

    cat("TIPE VARIABEL : NUMERIK\n")

    cat("=========================================\n\n")

    num_show <- data.frame(

      Variabel = num$skim_variable,

      `Jml Hilang` = num$n_missing,

      `Tingkat Lengkap` = round(num$complete_rate,3),

      `Rata-rata` = round(num$numeric.mean,2),

      `Simp. Baku` = round(num$numeric.sd,2),

      Minimum = round(num$numeric.p0,2),

      `Kuartil 1` = round(num$numeric.p25,2),

      Median = round(num$numeric.p50,2),

      `Kuartil 3` = round(num$numeric.p75,2),

      Maksimum = round(num$numeric.p100,2),

      `Jml Pencilan` = sapply(
        data[num$skim_variable],
        hitung_outlier
      ),

      Histogram = num$numeric.hist,

      check.names = FALSE

    )

    print(
      num_show,
      row.names = FALSE
    )

    cat("\n")

  }
  ################################################
  ## FACTOR
  ################################################

  fac <- hasil[hasil$skim_type == "factor", ]

  if(nrow(fac) > 0){

    cat("=========================================\n")

    cat("TIPE VARIABEL : FAKTOR\n")

    cat("=========================================\n\n")

    fac_show <- data.frame(

      Variabel = fac$skim_variable,

      `Jml Hilang` = fac$n_missing,

      `Tingkat Lengkap` = round(fac$complete_rate,3),

      Berurut = fac$factor.ordered,

      `Jumlah Kategori` = fac$factor.n_unique,

      `Kategori Terbanyak` = fac$factor.top_counts,

      check.names = FALSE

    )

    print(
      fac_show,
      row.names = FALSE
    )

    cat("\n")

  }

  ################################################
  ## CHARACTER
  ################################################

  chr <- hasil[hasil$skim_type == "character", ]

  if(nrow(chr) > 0){

    cat("=========================================\n")

    cat("TIPE VARIABEL : KARAKTER\n")

    cat("=========================================\n\n")

    chr_show <- data.frame(

      Variabel = chr$skim_variable,

      `Jml Hilang` = chr$n_missing,

      `Tingkat Lengkap` = round(chr$complete_rate,3),

      `Panjang Minimum` = chr$character.min,

      `Panjang Maksimum` = chr$character.max,

      `String Kosong` = chr$character.empty,

      `Jumlah Unik` = chr$character.n_unique,

      check.names = FALSE

    )

    print(
      chr_show,
      row.names = FALSE
    )

    cat("\n")

  }

}
