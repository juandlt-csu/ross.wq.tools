#' @title Prepare Sensor Calibration Data for Back Calibration
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

# I am adding the cal_prov_df here, but I don't think that this will be the final solution
# Just added this here because I needed a way to get that info in this function.

cal_prepare_sensor_calibration_data <- function(
    sensor_calibration_data_list,
    cal_prov_df
) {

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

          # Create calibration windows by pairing consecutive calibrations

          # TODO: This is where the update needs to occur

          # Right now there is no check to make sure that the calibration that is
          # done on the sensor is the same as the sensor that was just swapped out.

          # Right now I am in a year, and within that year I am in site param df,
          # and the goal of the function that I am in is to prepare a calibration
          # chunk that will get passed into `{cal_back_calibrate}`, and thus
          # will get passed through the relevant back calibration pipes

          prepped_calibration_chunks <- calibration_chunks %>%
            map(function(chunk) {
              prepped_chunk <- chunk %>%
                left_join(cal_prov_df,
                          by = c("parameter" = "sensor", "sensor_serial", "sensor_date"),
                          relationship = "many-to-many")
              return(prepped_chunk)
            })

          # So prepped calibration chunks is organized by when the sensor was
          # calibrated in the field or swapped in the field, but the two calibrations
          # that should be used are tied to the sensor itself being calibrated.
          # I think this should fix the problem.

          # TLDR: Data organized by field visits, but the calibrations are tied
          # to the sensors calibrations, not the field visits

          return(prepped_calibration_chunks)
        })
    })

  # Return nested structure: year -> site-parameter -> calibration chunks
  # Each chunk contains sensor data bounded by two consecutive calibrations
  # enabling temporal interpolation of calibration parameters
  return(prepped_yearly_site_param_chunks)
}


