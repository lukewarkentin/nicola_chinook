# Run full analysis


library(here)

source("R/get_weather_data.R")
source("R/get_flow_data_write_csv_all_months.R")
source("R/process_data_make_brood_table.R")
source("R/compile_all_data_for_model.R")
source("R/run_models_do_model_averaging.R")
source("R/make_figs_results.R")
source("R/make_fig_changing_stressors.R")
source("R/calculate_clearcut_areas_by_catchment.R")