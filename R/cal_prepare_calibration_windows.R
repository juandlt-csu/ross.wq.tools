#' @title Prepare Sensor Calibration Data Windows for Back Calibration
#' @export
#'
#' @description
#' Segments joined sensor-calibration data into calibration windows bounded by
#' consecutive calibrations. This function creates data chunks where each chunk
#' contains sensor data from one calibration period plus the first row from the
#' next calibration, enabling temporal interpolation between calibration
#' parameters. Processes data hierarchically by year and site-parameter
#' combinations to maintain organization structure.
#'
#' @param sensor_calibration_data_list Nested list containing joined sensor and
#'   calibration data from join_sensor_calibration_data(), organized by year
#'   and site-parameter combinations
#'
#' @seealso [join_sensor_calibration_data()]
#' @seealso [back_calibrate()]

cal_prepare_calibration_windows <- function(sensor_calibration_data_list) {

  # TODO: This function does very little now, consider assimilating it with previous prep functions

  # Process each year of joined sensor-calibration data
  prepped_yearly_site_param_chunks <- sensor_calibration_data_list %>%
    map(function(year){ # Iterate over each year's site-parameter list

      # Process each site-parameter combination within the year
      prepped_yearly_site_param_chunks <- year %>%
        map(function(site_param_df){

          # Split data by calibration dates to create calibration periods
          calibration_chunks <- site_param_df %>%
            # This was when the field calibration was done or when the sensor was swapped
            group_by(file_date) %>%
            group_split() %>%
            # Remove chunks with missing sonde data
            discard(~all(is.na(.x$sonde_serial)))

          # Handle single calibration case (no temporal interpolation needed)
          if(length(calibration_chunks) == 1) return(calibration_chunks)

          return(calibration_chunks)
        })
    })

  # Return nested structure: year -> site-parameter -> calibration chunks
  # Each chunk contains sensor data bounded by two consecutive calibrations
  # enabling temporal interpolation of calibration parameters
  return(prepped_yearly_site_param_chunks)
}


