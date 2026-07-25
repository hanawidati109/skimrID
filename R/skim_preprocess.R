#' Automatic Data Preprocessing
#'
#' Automatically preprocesses a dataset by removing duplicate rows,
#' imputing missing values, treating outliers using Winsorization,
#' and applying log transformation to highly skewed variables.
#' The function also generates detailed preprocessing reports
#' summarizing every preprocessing step that was applied.
#'
#' @param data A data frame.
#' @param remove_duplicate Logical. Remove duplicated rows.
#' @param impute_missing Logical. Impute missing values.
#' @param winsorize Logical. Winsorize outliers using the IQR method.
#' @param transform_skew Logical. Apply log transformation.
#' @param skew_threshold Numeric. Threshold for skewness.
#'
#' @return
#' A list containing three components:
#' \itemize{
#' \item \code{data}: The cleaned dataset after preprocessing.
#' \item \code{detail}: A detailed report describing each preprocessing action
#' applied to every variable, including missing value imputation,
#' Winsorization, and log transformation.
#' \item \code{summary}: A summary of the preprocessing steps indicating
#' whether each operation was applied or not.
#' }
#'
#' @examples
#' hasil <- skim_preprocess(airquality)
#'
#' hasil$data
#' hasil$detail
#' hasil$summary
#'
#' @importFrom stats median quantile
#'
#' @export

skim_preprocess <- function(
    data,
    remove_duplicate = TRUE,
    impute_missing = TRUE,
    winsorize = TRUE,
    transform_skew = TRUE,
    skew_threshold = 1
){

  if(!is.data.frame(data)){
    stop("Input harus berupa data frame.")
  }

  cleaned <- data

  ####################################################
  ## DETAIL REPORT
  ####################################################

  detail <- data.frame(
    Variable = character(),
    Preprocessing = character(),
    Before = character(),
    After = character(),
    Status = character(),
    stringsAsFactors = FALSE
  )

  ####################################################
  ## SUMMARY REPORT
  ####################################################

  summary <- data.frame(
    Step = character(),
    Status = character(),
    Detail = character(),
    stringsAsFactors = FALSE
  )

  ####################################################
  ## REMOVE DUPLICATE
  ####################################################

  if(remove_duplicate){

    before_row <- nrow(cleaned)

    cleaned <- unique(cleaned)

    removed <- before_row - nrow(cleaned)

    if(removed > 0){

      summary <- rbind(
        summary,
        data.frame(
          Step="Remove Duplicate",
          Status="Applied",
          Detail=paste(removed,"row removed")
        )
      )

    }else{

      summary <- rbind(
        summary,
        data.frame(
          Step="Remove Duplicate",
          Status="Not Needed",
          Detail="No duplicate rows detected"
        )
      )

    }

  }else{

    summary <- rbind(
      summary,
      data.frame(
        Step="Remove Duplicate",
        Status="Skipped",
        Detail="Disabled by user"
      )
    )

  }

  ####################################################
  ## MISSING VALUE IMPUTATION
  ####################################################

  if(impute_missing){

    detail_missing <- c()

    total_missing <- 0

    for(col in names(cleaned)){

      nmis <- sum(is.na(cleaned[[col]]))

      total_missing <- total_missing + nmis

      if(is.numeric(cleaned[[col]])){

        if(nmis>0){

          med <- median(cleaned[[col]],na.rm=TRUE)

          cleaned[[col]][is.na(cleaned[[col]])] <- med

          detail <- rbind(
            detail,
            data.frame(
              Variable=col,
              Preprocessing="Median Imputation",
              Before=paste(nmis,"Missing"),
              After="0 Missing",
              Status="Applied"
            )
          )

          detail_missing <- c(
            detail_missing,
            paste0(col," (",nmis,")")
          )

        }else{

          detail <- rbind(
            detail,
            data.frame(
              Variable=col,
              Preprocessing="Median Imputation",
              Before="0 Missing",
              After="0 Missing",
              Status="Not Needed"
            )
          )

        }

      }else{

        if(nmis>0){

          mode_value <- names(sort(table(cleaned[[col]]),decreasing=TRUE))[1]

          cleaned[[col]][is.na(cleaned[[col]])] <- mode_value

          detail <- rbind(
            detail,
            data.frame(
              Variable=col,
              Preprocessing="Mode Imputation",
              Before=paste(nmis,"Missing"),
              After="0 Missing",
              Status="Applied"
            )
          )

          detail_missing <- c(
            detail_missing,
            paste0(col," (",nmis,")")
          )

        }else{

          detail <- rbind(
            detail,
            data.frame(
              Variable=col,
              Preprocessing="Mode Imputation",
              Before="0 Missing",
              After="0 Missing",
              Status="Not Needed"
            )
          )

        }

      }

    }

    if(total_missing>0){

      summary <- rbind(
        summary,
        data.frame(
          Step="Missing Imputation",
          Status="Applied",
          Detail=paste(detail_missing,collapse=", ")
        )
      )

    }else{

      summary <- rbind(
        summary,
        data.frame(
          Step="Missing Imputation",
          Status="Not Needed",
          Detail="No missing value detected"
        )
      )

    }

  }else{

    summary <- rbind(
      summary,
      data.frame(
        Step="Missing Imputation",
        Status="Skipped",
        Detail="Disabled by user"
      )
    )

  }

  ####################################################
  ## WINSORIZATION
  ####################################################
  if(winsorize){

    detail_outlier <- c()

    total_outlier <- 0

    for(col in names(cleaned)){

      if(is.numeric(cleaned[[col]])){

        x <- cleaned[[col]]

        q1 <- quantile(x,0.25,na.rm=TRUE)

        q3 <- quantile(x,0.75,na.rm=TRUE)

        iqr <- q3-q1

        lower <- q1-1.5*iqr

        upper <- q3+1.5*iqr

        nout <- sum(x<lower | x>upper,na.rm=TRUE)

        total_outlier <- total_outlier+nout

        if(nout>0){

          x[x<lower] <- lower

          x[x>upper] <- upper

          cleaned[[col]] <- x

          detail <- rbind(
            detail,
            data.frame(
              Variable=col,
              Preprocessing="Winsorization",
              Before=paste(nout,"Outliers"),
              After="0 Outliers",
              Status="Applied"
            )
          )

          detail_outlier <- c(
            detail_outlier,
            paste0(col," (",nout,")")
          )

        }else{

          detail <- rbind(
            detail,
            data.frame(
              Variable=col,
              Preprocessing="Winsorization",
              Before="0 Outliers",
              After="0 Outliers",
              Status="Not Needed"
            )
          )

        }

      }

    }

    if(total_outlier>0){

      summary <- rbind(
        summary,
        data.frame(
          Step="Winsorization",
          Status="Applied",
          Detail=paste(detail_outlier,collapse=", ")
        )
      )

    }else{

      summary <- rbind(
        summary,
        data.frame(
          Step="Winsorization",
          Status="Not Needed",
          Detail="No outliers detected"
        )
      )

    }

  }else{

    summary <- rbind(
      summary,
      data.frame(
        Step="Winsorization",
        Status="Skipped",
        Detail="Disabled by user"
      )
    )

  }

  ####################################################
  ## LOG TRANSFORMATION
  ####################################################

  if(transform_skew){

    detail_skew <- c()

    total_skew <- 0

    for(col in names(cleaned)){

      if(is.numeric(cleaned[[col]])){

        before_skew <- moments::skewness(cleaned[[col]],na.rm=TRUE)

        if(abs(before_skew)>skew_threshold){

          cleaned[[col]] <- log1p(cleaned[[col]])

          after_skew <- moments::skewness(
            cleaned[[col]],
            na.rm=TRUE
          )

          detail <- rbind(
            detail,
            data.frame(
              Variable=col,
              Preprocessing="Log Transformation",
              Before=paste("Skew =",round(before_skew,2)),
              After=paste("Skew =",round(after_skew,2)),
              Status="Applied"
            )
          )

          total_skew <- total_skew+1

          detail_skew <- c(
            detail_skew,
            paste0(col,
                   " (",
                   round(before_skew,2),
                   " -> ",
                   round(after_skew,2),
                   ")")
          )

        }else{

          detail <- rbind(
            detail,
            data.frame(
              Variable=col,
              Preprocessing="Log Transformation",
              Before=paste("Skew =",round(before_skew,2)),
              After=paste("Skew =",round(before_skew,2)),
              Status="Not Needed"
            )
          )

        }

      }

    }

    if(total_skew>0){

      summary <- rbind(
        summary,
        data.frame(
          Step="Log Transformation",
          Status="Applied",
          Detail=paste(detail_skew,collapse=", ")
        )
      )

    }else{

      summary <- rbind(
        summary,
        data.frame(
          Step="Log Transformation",
          Status="Not Needed",
          Detail="No variable exceeded skewness threshold"
        )
      )

    }

  }else{

    summary <- rbind(
      summary,
      data.frame(
        Step="Log Transformation",
        Status="Skipped",
        Detail="Disabled by user"
      )
    )

  }

  ####################################################
  ## RETURN
  ####################################################

  return(

    list(

      data = cleaned,

      detail = detail,

      summary = summary

    )

  )

}
