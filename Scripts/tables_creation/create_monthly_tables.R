# create monthly tables 

staging_maker(month_to_create, threshold = 100000, original_path, staging_path)
feature_purchase_frequency(month_to_create, staging_path, feature_path) 
master_maker(
  month_to_create,
  model_type_creation = "crediservice",
  staging_path,
  master_path,
  months_ago = 3,
  months.to.seatch = 3
)     
