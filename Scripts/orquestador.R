# Based on the parameters stored in the configuration file (settings.json), it executes
# the requested function
# - Creation: It launchs the creation scripts to generate the master tables to train the new models
# - Scoring: It launchs the scoring scripts either the new leads or to check the performance
# - Training: It launchs the training scripts to create new models (either tcredito or crediservice)
# It gives back an error if the parameter if not "Creation", "Scoring" or "Training"
switch(
  job,
  "Creation" = {
      create_monthly_tables(month_to_create,
                            original_path,
                            staging_path,
                            feature_path,
                            master_path)
  },
  "Scoring" = {
    score_mensual(model_alias_score,
                  date_to_score,
                  model_type_score,
                  performance_calculation)
  },
  "Training" = {
    create_model(train_months,
                 test_month,
                 model_alias_modeling,
                 model_type_modeling)
  },
  "Ensamble" = {
    create_npb(
      models_path,
      models,
      date_to_predict,
      performance_npb,
      results_path,
      model_alias_npb)
  },
  stop("It only accepts Creation|Scoring|Training ")
)