#' Extended Statistical Summary
#'
#' Provides extended descriptive statistics for numeric variables.
#' In addition to the basic statistics available in skimr,
#' this function calculates the coefficient of variation (CV),
#' skewness, kurtosis, range, interquartile range (IQR),
#' and Shapiro-Wilk normality test results.
#'
#' @param data A data frame.
#'
#' @return
#' A data frame containing extended descriptive statistics,
#' including mean, standard deviation, coefficient of variation,
#' skewness, kurtosis, range, interquartile range (IQR),
#' p-values from the Shapiro-Wilk normality test,
#' and normality status.
#'
#' @examples
#' skim_extended(iris)
#'
#' @importFrom stats sd IQR na.omit shapiro.test
#' @export

skim_extended <- function(data){

  if (!is.data.frame(data)) {
    stop("Input harus berupa data frame")
  }

  # Ambil variabel numerik
  numeric_data <- data[sapply(data, is.numeric)]

  # Jika tidak ada variabel numerik
  if (ncol(numeric_data) == 0) {
    stop("Tidak terdapat variabel numerik dalam data")
  }

  hasil <- data.frame(
    Variabel = names(numeric_data),

    Mean = unname(
      sapply(numeric_data, mean, na.rm = TRUE)
    ),

    SD = unname(
      sapply(numeric_data, sd, na.rm = TRUE)
    ),

    CV = unname(
      round(
        (sapply(numeric_data, sd, na.rm = TRUE) /
           sapply(numeric_data, mean, na.rm = TRUE)) * 100,
        2
      )
    ),

    Skewness = unname(
      round(
        sapply(
          numeric_data,
          moments::skewness,
          na.rm = TRUE
        ),
        3
      )
    ),

    Kurtosis = unname(
      round(
        sapply(
          numeric_data,
          moments::kurtosis,
          na.rm = TRUE
        ),
        3
      )
    ),

    Range = unname(
      round(
        sapply(
          numeric_data,
          function(x){
            max(x, na.rm = TRUE) -
              min(x, na.rm = TRUE)
          }
        ),
        3
      )
    ),

    IQR = unname(
      round(
        sapply(
          numeric_data,
          IQR,
          na.rm = TRUE
        ),
        3
      )
    ),

    stringsAsFactors = FALSE,
    row.names = NULL
  )

  # Uji normalitas Shapiro-Wilk
  hasil$Nilai_P <- unname(
    sapply(
      numeric_data,
      function(x){

        x <- na.omit(x)

        if(length(x) < 3){
          return(NA)
        }

        round(
          shapiro.test(x)$p.value,
          4
        )
      }
    )
  )

  hasil$Status_Normalitas <- ifelse(
    hasil$Nilai_P > 0.05,
    "Normal",
    "Tidak Normal"
  )

  rownames(hasil) <- NULL

  return(hasil)
}
