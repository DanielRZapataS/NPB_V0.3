# Based on the parameters stored in the configuration file (settings.json), it executes
# the requested function
# - Creation: It launchs the creation scripts to generate the master tables to train the new models
# - Scoring: It launchs the scoring scripts either the new leads or to check the performance
# - Training: It launchs the training scripts to create new models (either Cartera or Tarjeta)
# It gives back an error if the parameter if not "Creation", "Scoring" or "Training"
switch(
  job,
  "Creation" = {
      create_monthly_tables(current_date = max(dates_to_create), model_type_creation)
  },
  "Scoring" = {
    scoreMensual(dates_to_score, model_type_score, model_alias_score)
  },
  "Training" = {
    createModel(model_type_modeling)
  },
  stop("It only accepts Creation|Scoring|Training ")
)