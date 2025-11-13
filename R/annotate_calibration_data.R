annotate_calibration_data <- function(raw_calibration_df) {

  # I want to split the data into good and bad calibrations

  # This is a rough draft of the final code.

  # Lets make per sensor histograms (raw, non-normalized)
  # Anything that is greater than 3sds we will remove (for now, this is an example)
  # We actually don't have the final yardstick that we want to use for this.

  # undo the form that it is in and then put it back together so that it plays nice
  # with this stuff.

  # Split up the data by sensor type in order to make the big histograms
  # We don't need to split this information, we just need to tag calibrations
  # as good or bad.

  annotated_calibration_data <- raw_calibration_df %>%
    split(f = .$sensor) %>%
    map(unnest, cols = calibration_coefs) %>%
    map_dfr(function(sensor_df){

      clean_df <- sensor_df %>%
        mutate(
          offset = as.numeric(offset),
          slope = as.numeric(slope)
        )

      # Check if we have valid (finite) data for each parameter
      offset_has_data <- any(is.finite(clean_df$offset), na.rm = TRUE)
      slope_has_data <- any(is.finite(clean_df$slope), na.rm = TRUE)

      # Calculate stats only if we have finite data
      if (offset_has_data) {
        finite_offsets <- clean_df$offset[is.finite(clean_df$offset)]
        offset_mean <- mean(finite_offsets)
        offset_sd <- sd(finite_offsets)
        offset_ci_lower <- offset_mean - (3*offset_sd)
        offset_ci_upper <- offset_mean + (3*offset_sd)
      }

      if (slope_has_data) {
        finite_slopes <- clean_df$slope[is.finite(clean_df$slope)]
        slope_mean <- mean(finite_slopes)
        slope_sd <- sd(finite_slopes)
        slope_ci_lower <- slope_mean - (3*slope_sd)
        slope_ci_upper <- slope_mean + (3*slope_sd)
      }

      annotated_df <- clean_df %>%
        mutate(
          correct_calibration = case_when(
            # Both parameters have finite data - check both
            offset_has_data & slope_has_data ~
              is.finite(offset) & is.finite(slope) &
              between(offset, offset_ci_lower, offset_ci_upper) &
              between(slope, slope_ci_lower, slope_ci_upper),

            # Only offset has finite data - check only offset
            offset_has_data & !slope_has_data ~
              is.finite(offset) & between(offset, offset_ci_lower, offset_ci_upper),

            # Only slope has finite data - check only slope
            !offset_has_data & slope_has_data ~
              is.finite(slope) & between(slope, slope_ci_lower, slope_ci_upper),

            # Neither has finite data - mark as FALSE
            TRUE ~ FALSE
          )
        )

      return(annotated_df)
    })

  return(annotated_calibration_data)

}
