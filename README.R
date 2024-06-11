

Run each of these scripts in order based on the number in front of the script.

(1) install.R will install the needed packages
(2) load_libraries.R is a function that will load the needed package.skeleton
(3) paths.R defines the paths to data
(4) population_data.R uploads population data for NC from file and organizes it by county per year
(5) clean_temperature_data.R uploads temperature data from file and organizes it in the correct way
(6) clean_opioid_death_data.R uploads death data from file and organizes it in the correct way
(7) extract_SVI.R uploads SVI data from file and sorts NC counties into tertiles
(8) match_referent_days.R is a function that matches each death with controls 
(9) generate_csv_data.R runs the functions defined in previous scripts to create the needed csv files 
*** NOTE - the match_referent_days.R will not run, but the resulting .csv is already saved to file and will be used in the dlnm
(10) dlnm.R is a function to run the dlnm
*** NOTE - the plotting function to visualize the dlnm outputs is still in progress

please feel free to view the uncleaned scripts under 'IGNORE (Uncleaned Scripts)' if something is unclear

email me if you have any questions. Thank you very much for your help :D