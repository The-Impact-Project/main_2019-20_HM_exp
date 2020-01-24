# main_2019-20_HM_exp

## Summary
This contains the list pull, randomization, and analysis files for the 7 state HM study from Fall 2019 to 2020. The program is being run in AZ, CO, FL, ME, MI, NV, and PA. UPDATE: Because things are a bit of shitshow, we ended up cutting PA and NV from this experiment; those states were not able to start their programs on time for it to make sense to run the survey there, and we did not want to delay the survey.

## Programs
The programs vary from state to state. However they all started in October or November 2019 and culminated in a mid-point survey in December. Check with each state director for better info on each program, or look in the google drive in the experiments folder.

## Listpull

#### Targeting And Randomization
Lists were pulled differently in each state. In every state, we only targeted those with general turnout scores above 40. We added the following criteria in the various states:
- In FL, we ran an [EIP to build a custom persuasion model](https://github.com/andy-zack/FL-EIP-2019) that was used for targeting.
- In ME, we got a list of people with high ticketsplitter scores from an in-state organization. We targeted them as well as people with NM scores over 29.2
- In AZ, MI, and PA, we bought ticketsplitter data from Catalist, and targeted those with high scores and people with high NM scores.
- CO and NV are targeted to all registered voters to be able to get the best data back after the midsurvey in December.
- NC is on hold until new districts are join.

#### Technical Steps
After pulling SQL data, minimal processing happens. This includes reformating zip code columns and creating a `college` field. This data is saved as `raw_[STATE]_dat.Rds`

Then I filter out people based on this criteria:
- Remove people in households with more than 7 people registered at the same place
- Remove people with more than 10 people sharing a landline phone
- Remove people with more than 3 people sharing a cell phone
- Remove people that do not have the "mailable" flag
- Remove people 95 or older
- Sometimes remove people without phone numbers (except for states where we need more people)
- Remove people with a 1-character first name

Next, the remaining phone numbers are sent to AMM to do a phone screen. This screen typcially takes one day and cuts out 15-20% of phone numbers. People with bad phone numbers are cut from the experiment.

Next, if we still have more than enough people for the experiment, we randomly cut a subset, and then run the `tiptools::balance_randomization()` function. This clusters on `HHID` which is address+city. The clustering ensures that if there are multiple people in a household, they get assigned to the same treatment. It blocks on district (either house or senate) and checks balance on:
- `os_score`
- `am_score`
- `ts_tsmart_offyear_general_turnout_score`
- `ts_tsmart_evangelical_raw_score`
- `ts_tsmart_otherchristian_raw_score`
- `ts_tsmart_prochoice_score`
- `ts_tsmart_path_to_citizen_score`

This is a somewhat strange list for balance, but it was what seemed to be correlated with persuasion in the FL EIP.

When necessary, if there are not enough people in the experiment to meet the targeting goal based on that state's budget, we add people to a "not in experiment" group. These are folks that are not in the same household as anyone in the experiment and do not share a phone number with anyone in the experiment. They also either do not have a phone number on file, or they have a phone number that was marked as bad during the phone screen, so we cannot survey these folks.

The randomized data is saved as `[STATE]_randomized_dat[DATE].Rds` for each state separately.

I also made a separate file for each state that is meant to be sent to vendors and state directors. This is a subset of the full `randomized_dat` that just includes fields they will likely need, and does not include people in the control group. They do include:
- **treatment** which are people in the experiment that should get treated
- **not in experiment** as described above, these are folks that might be good targets but don't have good phone numbers so we cannot survey them. They are added in states/districts where we don't have enough people in the experiment to reach the number of people the state directors wanted to target.

## Phone Screen
AMM performed the phone screens for this project. We sent them a separate list for each state. Each list was roughly 70K phone numbers, though it varied a lot by state. They charge a little under $0.01 per number. Roughly 15% of them came back as bad numbers. AMM takes roughly one day to run a screen

## Files

#### Code
- `pull_[STATE]_list.R` has the SQL list pull as well as the R that randomizes and saves data.
- `combine_lists_for_survey.R` combineds the randomized files from `pull_[STATE]_list.R`. The purpose of this is to have one file that can has all the data across states called `all_randomized_dat.Rds`. It also creates the CSVs that we sent to AMM, so that they can run the survey.
- `analyze_combined_surveys.Rmd` is the main analysis code for this project. It creates a long report. It has to be run in conjunction with `build_XRF_model.R`. We ended up not using much of the data from this though because our lawyer decided we cannot look at favorability or use the persuasion model described in the report.
- `build_XRF_model.R` Runs the `causaltools::X_RF()` code to build the TIP persuasion model, though we ended up not using this because it did not get legal approval. To run this, you need to first run the `analyze_combined_surveys.Rmd` code, then run this, and then run the Rmd code a second time.
- `analyze_maine_survey.Rmd` just makes a short report based on the data from the state of Maine. This was done because the Maine survey happened first, and they wanted some report on it.
- `florida_report.Rmd` is the markdown document that creates a short report with some graphs on Florida results and shows the breakdown by districts there. It is basically a very short version of the main report

#### Data and Output
This is not in the repo but can be found in each states folder in the experiment directory on google drive.
- `data/TMC-[STATE]-Screen.csv` are the screen results files sent back from AMM
- `audience_reports/[State] HM Audience.pdf` are Markdown documents that show demographic info on the audience in each state.
- `[STATE]_data_for_vendors[DATE]` are the treatment+not in experiment people to send to vendors to contact.
- `[STATE]_numbers_for_screen_[DATE].csv` are the list of phone numbers sent to AMM to screen
- `[STATE]_randomzied_dat[DATE].Rds` are the full list of randomized people for the experiments
- `raw_[STATE]_dat.Rds` is the list after it was pulled from the voter file but before it was screeneed or randomized

## Notes
 - We did not order ticketsplitter data for AZ leg. district 27 until after the rest of the data was recieved from Catalist. Therefore, that ticketsplitter data is in a separate table, and that district was dropped from the experiment. We are still targeting 5,000 people in that district, but they are not phone screened and they are ALL "not in experiment."
