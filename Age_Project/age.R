#' Write age counts data for export to the API
#'
#' @param df          a data frame of survey responses
#' @param cw_list     a named list containing geometry crosswalk files from zip5 values
#' @param params      a named list with entires "s_weight", "s_mix_coef", "num_filter",
#'                    "start_time", and "end_time"
#'
#' @export
write_age_count_data <- function(df, cw_list, params)
{
  ## output files can only use surveys with age range
  df <- df[!is.na(df$D2), ]
  
  for (i in seq_along(cw_list))
  {
    for (metric in c(1:7))
    {
      df_out <- summarize_age_count(df, cw_list[[i]], metric, "weight_unif", params)
      write_data_api(df_out, params, names(cw_list)[i], sprintf("raw_age_%d", metric))
      
      df_out <- summarize_age_count(df, cw_list[[i]], metric, "weight_unif", params, 6)
      write_data_api(df_out, params, names(cw_list)[i], sprintf("smoothed_age_%d", metric))
    }
  }
}

#' Summarize age count at a geographic level
#'
#' @param df               a data frame of survey responses
#' @param crosswalk_data   a named list containing geometry crosswalk files from zip5 values
#' @param metric           a number from 1 to 7 to indicate the age categoty from "age18" to "age75"
#' @param var_weight       name of the variable containing the survey weights
#' @param params           a named list with entries "s_weight", "s_mix_coef", "num_filter",
#'                         "start_time", and "end_time"
#' @param smooth_days      integer; how many does in the past should be pooled into the
#'                         estimate of a day
#'
#' @importFrom dplyr inner_join group_by mutate n case_when first
#' @importFrom rlang .data
#' @export
summarize_age_count <- function(
  df, crosswalk_data, metric, var_weight, params, smooth_days = 0L
)
{
  df <- inner_join(df, crosswalk_data, by = "zip5")

  df_out <- as_tibble(expand.grid(
    day = unique(df$day), geo_id = unique(df$geo_id), stringsAsFactors = FALSE
  ))
  df_out$val <- NA_real_
  df_out$sample_size <- NA_real_
  df_out$se <- NA_real_
  df_out$effective_sample_size <- NA_real_
  past_n_days_matrix <- past_n_days(df_out$day, smooth_days)
  
  for (i in seq_len(nrow(df_out)))
  {
    allowed_days <- past_n_days_matrix[i,]
    index <- which(!is.na(match(df$day, allowed_days)) & (df$geo_id == df_out$geo_id[i]))
    if (length(index))
    {
      new_row <- compute_age_response(
        response = df[df$day == df_out$day[i] & df$geo_id == df_out$geo_id[i],],
        metric = metric)
      
      df_out$val[i] <- new_row$val
      #df_out$se[i] <- new_row$se
      df_out$sample_size[i] <- new_row$sample_size
      df_out$effective_sample_size[i] <- new_row$sample_size
    }
  }
  
  df_out <- df_out[rowSums(is.na(df_out[, c("val", "sample_size", "geo_id", "day")])) == 0,]
  df_out <- df_out[df_out$sample_size >= params$num_filter &
                     df_out$effective_sample_size >= params$num_filter, ]
  print(df_out)
}

#' Returns response estimates for a single geographic area.
#'
#' This function takes vectors as input and computes the count response values
#' (a point estimate named "val", a standard error named "se", and an effective
#' sample size named "effective_sample_size").
#'
#' @param response D2 column under specific geo_id and day

#'
#' @export
compute_age_response <- function(response, metric)
{
  assert(all( response$D2 >= 1 & response&D2 <= 7 ))
  
  total <- nrow(response)
  sample_size <- switch(metric, 
                        nrow(response[response$D2 == 1]),
                        nrow(response[response$D2 == 2]),
                        nrow(response[response$D2 == 3]),
                        nrow(response[response$D2 == 4]),
                        nrow(response[response$D2 == 5]),
                        nrow(response[response$D2 == 6]),
                        nrow(response[response$D2 == 7]))
  val <- sample_size / total  # percentage
  
  return(list(
    val = val,
    #se = se,
    sample_size = sample_size
  ))
}
