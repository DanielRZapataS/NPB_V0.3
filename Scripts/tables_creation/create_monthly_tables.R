#' create monthly tables. 
#'
#' @param month_to_create: current month to model
#' @param original_path: original path data folder
#' @param staging_path: staging path data folder
#' @param feature_path: feature purchase path data folder
#' @param master_path: master path data folder
#'
#' @return: None (void)

create_monthly_tables <-
  function(month_to_create,
           original_path,
           staging_path,
           feature_path,
           master_path) {
    staging_maker(month_to_create, threshold = 100000, original_path, staging_path)
    feature_purchase_frequency(month_to_create, staging_path, feature_path)
    master_maker(
      month_to_create,
      staging_path,
      master_path,
      months_ago = 3,
      months.to.seatch = 3
    )
  }
