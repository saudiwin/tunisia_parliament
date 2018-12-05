# README

This repository contains code and data for the paper "When National Unity Governments are neither National, United, nor Governments: The Case of Tunisia" by Robert Kubinec and Sharan Grewal. This repository includes the full rollcall votes of the first and second sessions of Tunisia's parliament since its transition to democracy in 2011.

The R scripts that create plots assume that fitted models are available in the `data/` folder. The models may take some time to run (up to a few hours), so there are fitted models available that can be downloaded from [this link](https://drive.google.com/drive/folders/1F-rmWSGpwFruy6quyYEiaWuHNoM2ZK1M?usp=sharing).

The following is a description of the included files:

1. `R_scrips\run_bawsala_combine_session.R`: This file runs a random-walk and stationary time-varying idealpoint model on cleavages (Islamist and secularist) with the combined 1st and 2nd sessions of the Tunisian parliament using R package `idealstan`.
2. `R_scrips\run_bawsala_2nd_session.R`: This file runs a random-walk and stationary time-varying idealpoint model on parties with only the **second session** of the Tunisian parliament using R package `idealstan`.
3. `R_scripts\bawsala_plot.R`: Given fitted model objects in the `data` folder, create plots of time-varying ideal points and bill discrimination parameters.
4. `R_scripts\bawsala_predict.R`: Given fitted model objects in the `data` folder, create predictive validity checks from the cleavage-level model.
5. `data\combine_sessions.rds`: The combined 1st and 2nd parliaments' voting data.