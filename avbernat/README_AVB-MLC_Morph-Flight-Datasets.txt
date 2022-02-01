This README_AVB-MLC_Morph-Flight-Datasets.txt file was generated on 2022-02-02 by AVB.



GENERAL INFORMATION

1. Datasets: 2013-2020_Morphology_Data, Seasonal_Flight_Data

2. Author Information

	A. Principal Investigator Contact Information
		Name: Meredith L. Cenzer
		Institution: University of Chicago
		Address: 1101 East 57th Street, Z210, Chicago, IL, 60637
		Email: mlcenzer@uchicago.edu
		Contributions: Collected 2013-2020_Morphology_Data and collected field bugs for both datasets.
					      Assisted collections for Seasonal_Flight_Data.
					      Code reviewed both datasets.

	B. Co-Investigator Contact Information
		Name: Anastasia V. Bernat
		Institution: University of Chicago
		Address: 1101 East 57th Street, Z202, Chicago, IL, 60637
		Email: avbernat@uchicago.edu
		Contributions: Collected Seasonal_Flight_Data and assisted recent collections for 2013-2020_Morphology_Data.
					      Processed and analyzed both datasets.
					      Manuscript submission(s).
					      Dryad submission of both datasets.

	C. Alternate Contact Information
		Name: Anastasia V. Bernat
		Institution: Pacific Northwest National Laboratory
		Email: <TBD>

	C. Acknowledged Co-Investigator
		Name: Ana Silberg
		Institution: University of Chicago
		Address: 1101 East 57th Street, Z202, Chicago, IL, 60637
		Contributions: Assisted recent collections for 2013-2020_Morphology_Data.
					      Assisted collections for Seasonal_Flight_Data.

3. Dates of Data Collections: 

	A. 2013-2020_Morphology_Data:	
		field collections and morphology measurements conducted from 2013-04 to 2020-04

	B. Seasonal_Flight_Data:
		Fall 2019
			Field collections conducted from 2019-10-02 to 2019-10-06
			Flight trials
				set 1 (duration: 30 min) ran from 2019-10-15 to 2019-10-23 
				set 2 (duration: 60 min) ran from 2019-10-23 to 2019-10-29 
				set 3 (duration: 90 min) ran from 2019-10-30 to 2019-11-04 
				set 4 (duration: unlimited) ran from 2019-11-05 to 2019-11-08
			Morphology measurements conducted from 2019-11 to 2019-12
		Winter 2020
			Field collections conducted from 2020-02-03 to 2020-02-09
			Flight trials 
				set 1 (duration: unlimited) ran from 2020-02-17 to 2020-02-28
				set 2 (duration: unlimited) ran from 2020-03-03 to 2020-03-10
			Morphology measurements conducted from 2020-03 to 2020-04

4. Geographic Location of Data Collection:
		
		State, Country: Florida, United States of America (USA)
			Max Latitude:   29.66679°
			Min Latitude:   24.96424°

			Max Longitude: -80.34592°
			Min Longitude: -82.36091°

			Exact Collection Sites (by descending max latitude):
				Mainland 
					Gainesville
						23rd & 8th,  (29.66679°, -82.35761°)
					Leesburg
						Mount & 8th, (28.81527°, -81.88217°)
					Lake Wales
						Polk Ave,    (27.93615°, -81.57455°)	
					Lake Placid
						110N Main,   (27.29866°, -81.36612°)
					Ft. Myers		 (26.63387°, -81.8798°)
					Homestead
						SW 210th Terrace, (25.57106°, -80.45500°)
						SW 142nd St,      (25.55090°, -80.42110°)
						SW 296th St,      (25.49197°, -80.48562°)

				Islands
					North Key Largo
						Charlemagne, (25.19515°, -80.34592°)	
					Key Largo
						JP, 		    (25.12858°, -80.40809°)
						KLMRL, 		 (25.10002°, -80.43752°)		
					Plantation Key
						Mohawk St, 	 (24.98519°, -80.54710°)
						Aregood Ln,  (24.97357°, -80.55392°)
						Founder's,   (24.96984°, -80.55743°)
						

5. Information About Funding Sources: This work was funded by the University of Chicago.



SHARING/ACCESS INFORMATION

1. Links to other publicly accessible locations of the data: 
	GitHub Repository: https://github.com/mlcenzer/SBB-dispersal

2. Links to publications that use the data: 
	Bernat, A. Building an Enhanced Flight Mill for the Study of Tethered Insect Flight. 
	J. Vis. Exp. (169), e62171, doi:10.3791/62171 (2021).

3. There are no current licenses or restrictions placed on the data. 

4. There are no ancillary datasets. 

5. This data was not dervied from another source. 

6. Recommended citation for this joint dataset: 
	Bernat, A. and Cenzer, M. 2022. Data from: Dispersal-behavioral plasticity within an 
	insect-host system undergoing human-induced rapid evolutionary change (HIREC). Dryad.



DATA & FILE OVERVIEW

1. Directory Sturcture and File List: 

	AVB-MLC_Morph-Flight-Datasets
	|_ 2013-2020_Morphology_Data
	|	|_ allmorphology05.18.21.csv .................	# raw data of morphology data of bugs collected from 2013 to 2020
	|_ Seasonal_Flight_Data
		|_ 2019_Fall
		|	|_ Processed
		|	|	|_ full_data-Fall2019.csv ..............	# all raw data processed and merged together by bug and set number 
		|	|_ Raw
		|		|_ windaq_recordings ...................	# WDH recordings of flight trials, not converted to TXT	
		|		|_ 1.bug_demographics_data_coor.csv	....	# handwritten demographic data (e.g. population, sex) of bugs 
		|		|_ 1.dispersal_data ...................	# handwritten flight trial data, bug per row
		|		|_ 3.bug_morphology.csv ................	# microscope measured but handwritten transfer of bug morphology
		|_ 2020_Winter
			|_ Processed
			|	|_ all_flight_data-Winter2020.csv ......	# all raw data processed and merged together by bug and set number
			|_ Raw
				|_ windaq_recordings ...................	# WDH recordings of flight trials, not converted to TXT		
				|_ 1.demographic_data.csv ..............	# handwritten demographic data (e.g. population, sex) of bugs 
				|_ 1.trials-time-processed-Dec10.2020.csv # handwritten flight trial data, bug per row
				|_ 3.egg_data-final.csv ................	# handwritten counts of female egg laying
				|_ 4.tested-morph.csv ..................	# microscope measured but handwritten transfer of bug morphology
				|_ 5.not_tested-morph.csv ..............	# microscope measured but handwritten transfer of bug morphology


2. Relationship between files: 
	
	Raw data files are most frequently merged based on bug ID. 

	Key and value dictionary pairs using ID, set number, and channel letter are
	used to connect event marker notation within WinDaq recordings to handwritten
	flight trial data. 

3. There is no other related data collected that was excluded from the current data package.  

4. This is the latest version of the joint dataset. 
   Please see https://github.com/mlcenzer/SBB-dispersal for file and script version history.
	


METHODOLOGICAL INFORMATION

1. Description of methods used for collection/generation of data: 
	
	A. Protocol of field collection of soapberry bugs follows <reference pages follows page 7 in...e.g.>***

			Bernat, A. and Cenzer, M. 2022. Dispersal-behavioral plasticity within an 
			insect-host system undergoing human-induced rapid evolutionary change (HIREC). biorxiv. <temp>

	B. Protocol for flight trials follows <reference pages>***

			Bernat, A. and Cenzer, M. 2022. Dispersal-behavioral plasticity within an 
			insect-host system undergoing human-induced rapid evolutionary change (HIREC). biorxiv. <temp>

			as well as 

			Bernat, A. Building an Enhanced Flight Mill for the Study of Tethered Insect Flight. 
			J. Vis. Exp. (169), e62171, doi:10.3791/62171 (2021).

			Additional materials are offered at https://github.com/mlcenzer/SBB-dispersal/avbernat/Protocols/
			which includes the flight-trial-recording-protocol.docx and flight-trial-windaq-software-protocol.docx

	C. Flight mill creation and WinDaq file processing follows 

			Bernat, A. Building an Enhanced Flight Mill for the Study of Tethered Insect Flight. 
			J. Vis. Exp. (169), e62171, doi:10.3791/62171 (2021). 

			also available at

			Anastasia Bernat 2021. Building An Enhanced Flight Mill for the Study of Tethered Insect Flight. 
			protocols.io. https://dx.doi.org/10.17504/protocols.io.bteznjf6
			
			Link to WinDaq Recording and Playback Software for Hardware Product DI-1100 is located at 
			https://www.dataq.com/resources/pdfs/manuals/windaq_software.pdf

	D. Protocol for bug morphology measuring using the Zeiss Stemi 1000 Microscope 7x – 35x is located at
		https://github.com/mlcenzer/SBB-dispersal/avbernat/Protocols/morph-microscope-protocol.docx

2. Methods for processing the data: 

	A. 2013-2020_Morphology_Data:
		Once SBB were collected from the field, morphology measurements were taken of each field SBB. 
		Any morphology data but also specifically morphology measurements in allmorphology05.18.21.csv 
		were conducted using either Mitutoyo digital calipers or a Zeiss Stemi 1000 Microscope 7x – 35x. 
		For any field bugs collected and measured, the CSV sheet was continually updated either manually 
		or through code (i.e. see "update_on_<date>" folders within the SBB-dispersal repository:
		https://github.com/mlcenzer/SBB-dispersal/tree/master/avbernat/All_Morphology).
		Updates not only included amendeing new data but also cleaning typos, adding coordinates,
		defining known diapausing status, and adding the exact known field collection date. To include these
		new columns, the values were mapped, via code, according to the known host plant, population, year,
		month, and season of collection.

	B. Seasonal_Flight_Data:
		To measure the flight performance of a SBB, a flight mill machine was built and used, which functioned
		by recording flight movement via voltage measurements (see Bernat, A., 2021). In other words, the 
		machine acted like a "insect-powered carousel" where a SBB magnetically attached to one end of a flight 
		mill arm would fly in circles while the flag on the other end of the arm would break an IR sensor beam 
		at each revolution. Breaking the IR sensor would create a drop in voltage that is recorded. These dips
		or troughs of voltage would be processed after converting the WDH files into TXT files. 

		Processing the recording TXT files required at least three central python scripts: one that split files,
		one that stanardizes the voltages by converting changes in voltage into a TXT file of flight instances,
		and one that computes the total distance, max and average speeds, total duration, and various other flight 
		performance metrics. The list below summaries such scripts, respectively, by season. Additional scripts 
		and their auxiliary functions are breifly descriped below.

			Fall 2019
					
					0.appending_files.py ............... # appends recording TXT files that are split into two parts of 
																		one larger flight trial
					0.demographics.py .................. # merges handwritten trial data with demographic data

					1.splitting_files .................. # splits recording TXT files with multiple bugs per file into 
																		individual TXT files for each bug 
					2.standardize_peaks_multiple_files . # determines flight instances
					3.flight_analysis_multiple_files ... # computes and generates a flight statistics summary CSV sheet

					4.merging_data.py .................. # merges flight stats with handwritten trial data,
																		SBB demographic data, and morphology data
					flight_graphs.py ................... # plots speed (m/s) vs. seconds from start (s)
					get_IDs_expanded.py ................ # creates a dictionary that maps filenames to SBB ID

			Winter 2020
					appending_files.py ................. # appends recording TXT files that are split into two parts of 
																		one larger flight trial
					0.checking_event_markers.py ........ # checks event marker and handwritten trial data accuracy
					0.over_night_flyers-test.py ........ # estimates when overnight flyers stopped flying
					1.updating_overnight_time.py ....... # updates the overnight flyers on the handwritten trial data with
																		their WDH file observed flight end time.
					
					3.split_files.py ................... # splits recording TXT files with multiple bugs per file into 
																		individual TXT files for each bug 
					4.standardize_troughs.py ........... # determines flight instances
					5.flight_analysis.py ............... # computes and generates a flight statistics summary CSV sheet
					6.merging_data.py .................. # merges flight stats with handwritten trial data, egg laying data,
																		SBB demographic data, and morphology data

		All scripts and intermediately processed data are available on the SBB-dispersal GitHub repository. To properly
		clean and process the data, users will need to also account for a list of a few potential data input or 
		collection inconsistencies:

				* Remove extraneous rows for bugs who were not tested but still recorded in the handwritten trial data.
				* Check for event markers in the wrong channel letter (e.g. A instead of B, and vice versa).
				* Remove extraneous event markers.
				* Check for missing event markers.
				* Check for moments of voltage loss.
				* Check for non-functioning channels, which either remain at 0 V or show sporatic voltage changes.
				* Check for moments in which a bug escapes from the flight mill arm (read through "notes" column).
				* Check for moments in which a bug is flying in place (read through "notes" column).
				* Check for extraneous noise by running noise diagnostics in the stanardize_troughs.py.
				* Check for typos (e.g. males that "laid eggs", extreme morphology measurements)

		Once all inconsistencies are accounted for, the data can be merged into a final sheet for data analysis in R.
			
			Fall 2019 ..... full_data-Fall2019.csv
			Winter 2020 ... all_flight_data-Winter2020.csv


3. Instrument- or software-specific information needed to interpret the data: 

	A. For field collecting, no specific instruments are needed to attract or capture bugs.

	B. For flight trials, a flight mill machine is needed to record insect flight behavior. 
		Software will depend on, for example, the data loggers used. Users could use the DATAQ 
		Model DI-1100 data loggers, which are accompanied by the WinDAQ Acquisition Waveform 
		Recording and Playback Software.

	C. For WinDaq recording file processing, three main Python scripts (split_files.py, standardize_troughs.py,
		and flight_analysis.py) were written and implemented using Python version 3.8.5 (Python Software Foundation. 
		Python Language Reference, version 3.8. Available at http://www.python.org). Packages used included 

				os                # operating system functionality
				sys               # Python runtime functionality
				csv               # read and write data

				re                # string manipulation
				math              # mathematical functions
				time              # datetime manipulation
				datetime          # datetime manipulation

				numpy             # data manipulation 
				matplotlib.pyplot # data plotting

		For data analysis, R Studio was used using version 4.0.5 (2021 The R Foundation for Statistical Computing)
		to generate draft scripts as well as appendix_A-wing_summary.Rmd and appendix_B-flight_summary.Rmd scripts. 
		Libraries in appendix_A-wing_summary.Rmd included

				lme4         # fit regressions 
				binom        # binomial confidence intervals
				
				zoo          # data manipulation
				dplyr        # data manipulation
				lubridate    # datetime manipulation

				ggformula    # ggplot plotting
				cowplot      # ggplot helper functions to arrange multi-panel figures
				kableExtra   # table formatting
				plotrix	    # color grardient designing

		Libraries, aside from aformentioned lme4, binom, dplyr, lubridage, and kableExtra, in  
		appendix_B-flight_summary.Rmd included

				nnet         # multinomial modeling 
				rethinking   # Bayesian data analysis and plotting
				cvms         # cross-validating regressions
				popbio       # logistic regression plotting

				plot.matrix  # enables matrix/heatmap plotting

	D. For morphology measurements, either Mitutoyo digital calipers or a Zeiss Stemi 1000 Microscope 7x – 35x were used. 
	   Units were mm and um, respectively. The microscope was accompanied by ZEISS ZEN 3.1 (blue edition) software. 


4. Standards and calibration information:

		Ensure that the flight mill channels exhibit a mean voltage close to 5 V and 
		their sampling rate is 100 Hz.

		When running trough diagnostics in the standardize_troughs.py script, it is 
		recommended to use min and max deviation values close to 0.1 V. Check the 
		trough_diagnostic.png file to ensure this recommendation holds.

		To calibrate the Zeiss Microscope for morphology measureing, follow the manual 
		scaling on page 20 of the online manual,
		https://asset-downloads.zeiss.com/catalogs/download/mic/8db1eb8d-7b2a-46e8-8427-f259dcfd1fb3/EN_quick-guide_ZEN-blue-edition_first-steps.pdf

5. Experimental conditions: 

		A. Incubator housing bug homes (rearing incubator): 
				The incubator was illuminated with Philips 17 W, 24 in florescent lighting 
				and was set at 28 C/27.5 C (day/night), 70% relative humidity, and a 24 hour light/0 hour dark cycle.

		B. Incubator housing flight mill (testing incubator):
				The incubator was set to the same conditions as the rearing incubator except 
				it was set at a 14 hour light/10 hour dark cycle (sunrise at 8 AM and sunset at 10 PM).

		C. Flight mill arm circular flight path: 0.6283 m calculated from a flight mill arm radius of 0.1 m.


6. Describe any quality-assurance (QA) procedures performed on the data:

		Data cleaning was routinely considered throughout data processing and analysis as aformentioned in 
		section 2 of "METHODOLOGICAL INFORMATION".
		
		For flight trial processing, the following QA was conducted:
			Recently split trial recording files were standardized using a 0.1 V min and max deviation values, using the
			standardize_troughs.py script. This was determined after reading the trough_diagnostic.png and after concluding 
			that flight events (i.e. voltage troughs) were most accurately identified at high min and max deviation values.



DATA-SPECIFIC INFORMATION FOR: allmorphology05.18.21.csv

1. Number of variables: 18
2. Number of cases/rows: 3,737
3. Variable List: 
		pophost ............	# host plant (scientific name used) SBB collected from
		population ......... # census-designated location name where SBB collected from
		sex ................	# binary biological category, either male (M) or female (F)
		beak ...............	# SBB beak length (mm)
		thorax .............	# SBB thorax width (mm)
		wing ...............	# SBB wing length (mm)
		body ...............	# SBB body length (mm)
		month	.............. # month SBB collected 
		year ...............	# year SBB collected 
		month_since_start	..	# months since the first collection month; April 2013 initializes the count
		season ............. # season SBB collected; either Fall, Winter, Spring, or Summer
		w_morph ............ # wing morph; either long-winged (L), short-winged (S) or ambigiuous (LS or SL)
		lat ................ # latitude of location SBB was collected (°)
		long ...............	# longitude of location SBB was collected (°)
		diapause ........... # denotes whether SBB was diapausing or not
		field_date_collected # exact date <MM.DD.YYYY> SBB was collected
		notes	.............. # additional descriptions of SBB features; e.g. "wings torn" or "marked"
		site ............... # location name acting as proxi to latitude and longitude; 
									  e.g. intersection of two streets 
4. Missing data codes: 

		"" (an empty string) or NA suggests that the value was not recorded for all variables except 
		in the "notes" column where "" or NA suggests that there were not immediately 
		concerning or notable features about the SBB 

5. There are no other specialized formats or abbreviations used not already aforementioned.


DATA-SPECIFIC INFORMATION FOR: full_data-Fall2019.csv

1. Number of variables: 41
2. Number of cases/rows: 578
3. Variable List: 
		ID .............. # assigned unique identification number per SBB 
		chamber ......... # consists of a channel letter (either A or B) and channel number (1-4),
							     which correspond to a data logger and its recording channels, respectively.
		set_number ...... # a number referencing a group of trails conducted within a day, each day can 
							     have multiple set numbers
		average_speed ... # mean speed (m/s) measured across a trial
		max_speed ....... # fastest speed (m/s) measured in the entire trial
		total_duration .. # trial length (s) 
		total_flight_time # time SBB spent flying within its trial (s)
		portion_flying .. # portion of time SBB spent flying compared to the trial duration
		distance ........ # total distance SBB spent flying within its trial (m/s)
		test_date ....... # day SBB flight tested <MM.DD>
		time_start ...... # time SBB began flight trial, recorded in 12-hour clock <HH:MM>
		time_end ........ # time SBB ended flight trial, recorded in 12-hour clock <HH:MM>
		
		population ...... # census-designated location name where SBB collected from
		site ............ # location name acting as proxi to latitude and longitude; 
							     e.g. intersection of two streets 
		host_plant ...... # host plant (scientific name used) SBB collected from
		sex ............. # binary biological category, either male (M) or female (F)
		flew ............ # binary flight category, either yes (Y) if a SBB flew or no (N) for absent flight behavior
		flight_type ..... # observed flight behavior categories; bursting (B), continuous (C) or a mix (e.g. BC, CB)
		died ............ # marks only yes (Y) if and when a SBB died before its trial
		NOTES ........... # additional descriptions of trial day observations; e.g. "paint came off"
		mass ............ # mass measurement (g) of a SBB right before its flight trial
		short.wing ...... # marks only yes (Y) if SBB was short-winged
		eggs ............ # marks only yes (Y) if female SBB had laid eggs the day of its flight trial

		latitude ........ # latitude of location SBB was collected from the field (°)
		longitude ....... # longitude of location SBB was collected from the field (°)
		beak ............ # SBB beak length (mm)
		thorax .......... # SBB thorax width (mm)
		wing ............ # SBB wing length (mm)
		body ............ # SBB body length (mm)
		w_morph ......... # wing morph; either long-winged (L), short-winged (S) or ambigiuous (LS or SL)
		notes ........... # additional morphology descriptions of SBB features; e.g. "wings torn"

		... (only most relevant variables shown)

4. Missing data codes: 
		
		For columns like "short.wing", "died" or "eggs", "" (an empty string) or NA suggests that the SBB
		exhibited the negative (i.e. they were not short-winged, they did not die before their trial 
		or did not lay eggs that day). 

		For the "notes" or "NOTES" columns, "" or NA suggests there there were no immediately concerning or 
		notable behaviors or features of the SBB.

		For columns like "flight_type", "" or NA suggests that the bug originally did not fly so it would
		not have a flight type.

		For columns like "box", "time_end", "mass", "sex", "beak", "thorax", "wing", "body", "" or NA suggests
		that the value was not recorded.

5. There are no other specialized formats or abbreviations used not already aforementioned.


DATA-SPECIFIC INFORMATION FOR: all_flight_data-Winter2020.csv

1. Number of variables: 39
2. Number of cases/rows: 758
3. Variable List: 
		ID ............... # assigned unique identification number per SBB 
		trial_type ....... # SBBs were tested at most twice either in trial 1 (T1) or trial 2 (T2)
		chamber .......... # consists of a channel letter (either A or B) and channel number (1-4),
							      which correspond to a data logger and its recording channels, respectively.
		set_number ....... # a number referencing a group of trails conducted within a day, each day was
								   given a unique set number
		average_speed .... # mean speed (m/s) measured across a trial
		max_speed ........ # fastest speed (m/s) measured in the entire trial
		recording_duration # trial length (s) 
		total_flight_time  # time SBB spent flying within its trial (s)
		portion_flying ... # portion of time SBB spent flying compared to the trial duration
		distance ......... # total distance SBB spent flying within its trial (m/s)
		test_date ........ # date SBB flight tested <YYYY-DD-MM>
		time_start ....... # time SBB began flight trial, recorded in 24-hour clock <HH:MM>
		time_end ......... # time SBB ended flight trial, recorded in 24-hour clock <HH:MM>
		
		population ....... # census-designated location name where SBB collected from
		site ............. # location name acting as proxi to latitude and longitude; 
							      e.g. intersection of two streets 
		host_plant ....... # host plant (scientific name used) SBB collected from
		sex .............. # binary biological category, either male (M) or female (F)
		flew ............. # binary flight category, either yes (Y) if a SBB flew or no (N) for absent flight behavior
		flight_type ...... # observed flight behavior categories; bursting (B), continuous (C) or a mix (e.g. BC, CB)
		NOTES ............ # additional descriptions of trial day observations; e.g. "paint came off"
		mass ............. # mass measurement (g) of a SBB right before its flight trial
		short.wing ....... # marks only yes (Y) if SBB was short-winged
		EWM .............. # abbreivated from "eggs when massed"; marks only yes (Y) if female SBB had laid eggs 
								   the day of its flight trial
		total_eggs ....... # sum of all eggs laid per female SBB during the entire flight testing period
		tested ........... # binary category, either "yes" if a SBB was tested or "no" if it was not 

		latitude ......... # latitude of location SBB was collected from the field (°)
		longitude ........ # longitude of location SBB was collected from the field (°)
		beak ............. # SBB beak length (mm)
		thorax ........... # SBB thorax width (mm)
		wing ............. # SBB wing length (mm)
		body ............. # SBB body length (mm)
		w_morph .......... # wing morph; either long-winged (L), short-winged (S) or ambigiuous (LS or SL)
		morph_notes ...... # additional morphology descriptions of SBB features; e.g. "wings torn"

		... (only most relevant variables shown)

4. Missing data codes: 

		A SBB with an ID of "0" suggests that the bug was not tested and, in turn, not given a unique ID.
		Additionally, if a SBB was not tested all of its values are either "0", "" (an empty string), or NA 
		except for its morphology measurements, sex, population, site, and host plant.

		For SBB that were tested, missing values can be interpreted as follows
			"" (an empty string) or NA suggests that the value was not recorded (e.g. "mass" or "EWM"). 
			However, in the "NOTES" or "morph_notes" column, "" or NA suggests that there were not any 
			immediately concerning or notable features about the SBB. Additionally, for columns like 
			"flight_type", "" or NA suggests that the bug originally did not fly so it would
			not exhibit a flight type.

5. There are no other specialized formats or abbreviations used not already aforementioned.


DATA-SPECIFIC INFORMATION FOR: windaq_recordings, i.e. considering any WDH file once converted to txt file 

1. Number of variables: 11
2. Number of cases/rows: varies
3. Variable List*: 
		
		* column names are undefined, so they are instead specified within the split_files.py script

		TBF .......... # time (s) from the beginning of the file
		1 ............ # voltage (V) reading for channel 1
		2 ............ # voltage (V) reading for channel 2
		3 ............ # voltage (V) reading for channel 3
		4 ............ # voltage (V) reading for channel 4
		event_happened # an arbitrary number denoting that an event occured at that given TBF 
		event_num .... # an incrementing number denoting that an event occured at that given TBF 
		buffer ....... # a column of only zeros, except the first row which denotes the start of the file
		date ......... # date of the recording, CST or UTC/GMT-06:00
		time ......... # time of the recording, CST or UTC/GMT-06:00
		event_marker . # string phrase input into the event marker when new SBB was loaded,
							  e.g. "bug 192 is on" 

4. There are no missing values that occur in these files except those due to human error. 
	For example, researchers could have forgotten to enter an event marker, mistyped the
	ID of a bug, or placed the event marker in the wrong data logger window.  

5. There are no other specialized formats or abbreviations used not already aforementioned.
