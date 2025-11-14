#' @title Linear Transition Between Linear Models
#' @export
#'
#' @description
#' Applies temporally-weighted linear transformation between two consecutive
#' calibrations. This function interpolates between the slope and offset
#' parameters from calibration 1 (earliest) and calibration 2 (latest) using
#' temporal weights to create a smooth transition across the calibration window.
#' The transformation accounts for gradual sensor drift by blending calibration
#' parameters based on time position.
#'
#' @param df Tibble containing sensor data bounded by two calibrations. Must
#'   include lead calibration parameters (slope_lead, offset_lead) for the
#'   temporal interpolation.
#' @param raw_col Character string specifying the column name containing raw
#'   observation values from the inverse linear model transformation
#' @param slope_col Character string specifying the column name containing
#'   slope coefficients from the first (earliest) calibration
#' @param offset_col Character string specifying the column name containing
#'   offset coefficients from the first (earliest) calibration
#' @param wt_col Character string specifying the column name containing temporal
#'   weight parameters for interpolation between calibrations
#'
#' @return The input dataframe with an additional column containing linearly
#'   transformed values. Column name follows the pattern:
#'   "{raw_col_prefix}_lm_trans" (e.g., "mean_lm_trans" from "mean_raw").
#'   Returns NA values when calibration parameters are missing.
#'
#' @details
#' The function uses hard-coded "slope_lead" and "offset_lead" columns for the
#' second calibration parameters. The temporal interpolation formula is:
#' y = (m₁ - wt(m₁ - m₂))x + (b₁ - wt(b₁ - b₂))
#' where wt represents the temporal weight between 0 and 1.
#'
#' @seealso [cal_wt()]
#' @seealso [cal_inv_lm()]
#' @seealso [cal_one_point_drift()]
#' @seealso [cal_two_point_drift()]

cal_lin_trans_lm <- function(df, raw_col, slope_col, offset_col, wt_col){

  # Create output column name for linearly transformed values
  transformed_col <- paste0(str_split_1(raw_col, "_")[1], "_lm_trans")

  # Extract calibration coefficients from bounding calibrations

  # Calibration 1
  slope_1 <- df[[slope_col]][1]
  offset_1 <- df[[offset_col]][1]

  # Calibration 2
  # Hard coded for now
  slope_2 <- df[["slope_lead"]][1]
  offset_2 <- df[["offset_lead"]][1]

  # Handle missing calibration data
  if (any(is.na(c(slope_1, offset_1, slope_2, offset_2)))) {
    df <- df %>%
      mutate(!!raw_col := NA_integer_)
    return(df)
  }

  # Calculate parameter differences for temporal interpolation
  slope_delta <- slope_1 - slope_2
  offset_delta <- offset_1 - offset_2

  # Apply temporally-weighted linear model: y = (m_1-wt(m_1-m_2))x+(b_1-wt(b_1-b_2))
  df <- df %>%
    mutate(!!transformed_col := ((slope_1-(.data[[wt_col]]*slope_delta))*.data[[raw_col]])+(offset_1-(.data[[wt_col]]*offset_delta)))

  return(df)
}
