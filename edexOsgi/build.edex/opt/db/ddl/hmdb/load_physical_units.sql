/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
--
-- Data for Name: physical_units; Type: TABLE DATA; Schema: public; Owner: pguser
--

COPY physical_units (physical_units, units_id, units_long_name, units_class, units_system) FROM stdin;
m/s^2	100	meters_per_second_squared	acceleration	SI
deg.	200	degrees	angle	English
rad	201	radians	angle	SI
ft^2	300	square_feet	area	English
m^2	301	square_meters	area	SI
g/cm^3	400	grams_per_cubic_centimeter	density	SI
g/m^3	401	grams_per_cubic_meter	density	SI
1/s^2	500	per_second_squared	diff_vorticity_advection	SI
none	600	dimensionless	dimensionless	\N
erg	700	erg	energy	SI
J	701	Joule	energy	SI
K^2/(m^2*s)	800	sq_Kelvin_per_sq_meter_per_sec	frontogenesis	SI
thm	900	therm	heat	English
cal(it)	1001	internationalCalorie	heat	English
cal	1002	thermochemicalCalorie	heat	English
in	1100	inches	length	English
ft	1101	feet	length	English
yd	1102	yard	length	English
mi	1103	statute_Miles	length	English
nmi	1104	nautical_Miles	length	English
mm	1105	millimeters	length	SI
cm	1106	centimeters	length	SI
dm	1107	decimeters	length	SI
m	1108	meters	length	SI
dam	1109	dekameters	length	SI
hm	1110	hectometers	length	SI
km	1111	kilometers	length	SI
mg	1200	milligram	mass	SI
g	1201	gram	mass	SI
kg	1202	kilogram	mass	SI
lb	1203	pound	mass	English
slug	1204	slug	mass	English
scruple	1205	scruple	mass	English
g/(kg*12hr)	1300	grams_per_kg_per_12_hours	moisture_change_rate	SI
g/kg	1400	grams_per_kilogram	moisture_content	SI
dB	1500	dB	peak_power	SI
K/(mb*1e5s)	1600	Kelvin_per_mb_per_10^5_sec	potential_vorticity	SI
g/m^2	1700	grams_per_square_meter	precipitable_water	SI
Pa	1800	Pascals	pressure	SI
hPa	1801	hectoPascals	pressure	SI
mb	1802	millibars	pressure	SI
inHg	1803	inches_of_Hg	pressure	English
ub/s	1900	microbars_per_second	pressure_change_rate	SI
%	2000	percent	probability	\N
fract	2001	fractional	probability	\N
1/1e5s	2100	per_10^5_seconds	rate_of_change	SI
1/Gs	2101	per_gigasecond	rate_of_change	SI
1/s	2102	per_second	rate_of_change	SI
dBZ	2200	decibles(Z)	reflectivity	SI
J/kg	2300	joules_per_kilogram	specific_energy	SI
m^2/s^2	2301	meters_squared_per_sec_squared	specific_energy	SI
cm/d	2400	centimeters_per_day	speed	SI
cm/hr	2401	centimeters_per_hour	speed	SI
cm/min	2402	centimeters_per_mimute	speed	SI
cm/s	2403	centimeters_per_second	speed	SI
ft/d	2404	feet_per_day	speed	English
ft/hr	2405	feet_per_hour	speed	English
ft/min	2406	feet_per_mimute	speed	English
ft/s	2407	feet_per_second	speed	English
in/d	2408	inches_per_day	speed	English
in/hr	2409	inches_per_hour	speed	English
in/min	2410	inches_per_mimute	speed	English
in/s	2411	inches_per_second	speed	English
km/d	2412	kilometers_per_day	speed	SI
km/hr	2413	kilometers_per_hour	speed	SI
km/min	2414	kilometers_per_mimute	speed	SI
km/s	2415	kilometers_per_second	speed	SI
m/d	2416	meters_per_day	speed	SI
m/hr	2417	meters_per_hour	speed	SI
m/min	2418	meters_per_mimute	speed	SI
m/s	2419	meters_per_second	speed	SI
mi/d	2420	statute_Miles_per_day	speed	English
mph	2421	statute_Miles_per_hour	speed	English
mi/min	2422	statute_Miles_per_mimute	speed	English
mi/s	2423	statute_Miles_per_second	speed	English
mm/d	2424	millimeters_per_day	speed	SI
mm/hr	2425	millimeters_per_hour	speed	SI
mm/min	2426	millimeters_per_mimute	speed	SI
mm/s	2427	millimeters_per_second	speed	SI
kt	2428	knots	speed	English
fur/fort	2429	furlongs_per_fortnight	speed	English
F	2500	Fahrenheit	temperature	English
R	2501	Rankine	temperature	English
C	2502	Celcius	temperature	SI
K	2503	Kelvin	temperature	SI
C/12hr	2600	Celsius_per_12_hours	temperature_change_rate	SI
C/hr	2601	Celsius_per_hour	temperature_change_rate	SI
K/(m^2*s)	2700	Kelvin_per_sq_meter_per_second	temperature_flux	SI
C/km	2800	Celsius_per_kilometer	temperature_gradient	SI
s	2900	second	time	SI
min	2901	minute	time	SI
hr	2902	hour	time	SI
d	2903	day	time	SI
mon	2904	month	time	SI
yr	2905	year	time	SI
in^3	3000	cubic_inch	volume	English
ft^3	3001	cubic_feet	volume	English
yd^3	3002	cubic_yard	volume	English
pt	3003	pint	volume	English
qt	3004	quart	volume	English
gal	3005	gallon	volume	English
m^3	3006	cubic_meters	volume	SI
ml	3007	milliliters	volume	SI
l	3008	liters	volume	SI
eighths_of_sky	2250	eighths_of_sky	sky_cover	\N
tenths_of_sky	2251	tenths_of_sky	sky_cover	\N
hhmm	3100	24-h clock time, hhmm format	time_of_day	\N
\.


