## Code book for the Getting and Cleaning Data Project

Variable			             |Description						                               |Values
---------------------------|-----------------------------------------------------|-----------------------
subject				             |Subject ID, who performed the activity	|
activity			             |Activity ID (from source file)|
activity_name			         |Activity Name|
feature				             |Feature ID (from source file)|
feature_name			         |Feature Name (from source file)|
signal_domain			         |Domain of the signal			    	|	                    time, frequency	
calc_type			             |Type of calculation					     |                     mean, std
axis				               |Axis that the signals were captured on|			          X, Y, Z
magnitude			             |Magnitude of signals, calculated using Euclidean norm| TRUE, FALSE
jerk				               |Jerk signal						                             |   TRUE, FALSE
gyro_acc			             |Instrument used to measure activity			           |   gyro, acc	
body_gravity			         |Accerlation type					                         |   body, gravity
value				               |measurement value|
