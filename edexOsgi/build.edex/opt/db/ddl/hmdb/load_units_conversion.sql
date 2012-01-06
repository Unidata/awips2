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
-- Data for Name: units_conversion; Type: TABLE DATA; Schema: public; Owner: pguser
--

COPY units_conversion (convert_from, convert_to, units_class, multiply_by, then_add, reference) FROM stdin;
deg.	rad	angle	0.01745329252	0	none: 180 deg. = PI rad, PI = 3.141592654
rad	deg.	angle	57.295779510000003	0	none: 180 deg. = PI rad, PI = 3.141592654
cm	dam	length	0.001	0	none.
cm	ft	length	0.032808398949999998	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
cm	in	length	0.39370078739999997	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
cm	km	length	1.0000000000000001e-05	0	none.
cm	m	length	0.01	0	none.
cm	mi	length	6.2137119220000002e-06	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
cm	mm	length	10	0	none.
dam	cm	length	1000	0	none.
dam	ft	length	32.808398949999997	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
dam	in	length	393.70078740000002	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
dam	km	length	0.01	0	none.
dam	m	length	10	0	none.
dam	mi	length	0.0062137119220000009	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
dam	mm	length	10000	0	none.
ft	cm	length	30.48	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft	dam	length	0.03048	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft	in	length	12	0	none.
ft	km	length	0.00030479999999999998	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft	m	length	3.048	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft	mi	length	0.00018939393939393939	0	none: (5280.0 ft / mi)
ft	mm	length	304.80000000000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in	cm	length	2.54	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in	dam	length	0.0025400000000000002	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in	ft	length	0.083333333333333329	0	none.
in	km	length	2.5400000000000001e-05	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in	m	length	0.025399999999999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in	mi	length	1.5782828282828279e-05	0	none: (5280.0 ft / mi)
in	mm	length	25.399999999999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
km	cm	length	100000	0	none.
km	dam	length	100	0	none.
km	ft	length	3280.8398950000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
km	in	length	39370.078739999997	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
km	m	length	1000	0	none.
km	mi	length	0.62137119220000003	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
km	mm	length	1000000	0	none.
m	cm	length	100	0	none.
m	dam	length	0.10000000000000001	0	none.
m	ft	length	3.2808398950000002	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
m	in	length	39.370078739999997	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
m	km	length	0.001	0	none.
m	mi	length	0.00062137119220000007	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
m	mm	length	1000	0	none.
mi	cm	length	160934.39999999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mi	dam	length	160.93440000000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mi	ft	length	5280	0	none: (5280.0 ft / mi)
mi	in	length	63360	0	none: (5280.0 ft / mi)
mi	km	length	1.6093440000000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mi	m	length	1609.3440000000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mi	mm	length	1609344	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mm	cm	length	0.10000000000000001	0	none.
mm	dam	length	0.0001	0	none.
mm	ft	length	0.0032808398950000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
mm	in	length	0.039370078740000002	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
mm	km	length	9.9999999999999995e-07	0	none.
mm	m	length	0.001	0	none.
mm	mi	length	6.2137119220000004e-07	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
g	kg	mass	0.001	0	none.
g	lb	mass	0.0022046226219999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (0.45359237 kg / pound)
kg	g	mass	1000	0	none.
kg	lb	mass	2.204622622	0	CRC Handbook of Chemistry and Physics, 76th ed.: (0.45359227 kg / pound)
lb	g	mass	453.59237000000002	0	CRC Handbook of Chemistry and Physics, 76th ed.: (0.45359237 kg / pound)
lb	kg	mass	0.45359237000000002	0	CRC Handbook of Chemistry and Physics, 76th ed.: (0.45359237 kg / pound)
hPa	inHg	pressure	0.02952874414	0	none: 1013.25 hPa = 29.92 inHg
hPa	mb	pressure	1	0	1 mb = 1 hPa
hPa	Pa	pressure	100	0	none.
inHg	hPa	pressure	33.865307489999999	0	none: 1013.25 hPa = 29.92 inHg
inHg	mb	pressure	33.865307489999999	0	none: 1013.25 hPa = 29.92 inHg; 1 mb = 1 hPa
inHg	Pa	pressure	3386.530749	0	none: 1013.25 hPa = 29.92 inHg
mb	hPa	pressure	1	0	1 mb = 1 hPa
mb	inHg	pressure	0.02952874414	0	none: 1013.25 hPa = 29.92 inHg; 1 mb = 1 hPa
mb	Pa	pressure	100	0	1 mb = 1 hPa
Pa	hPa	pressure	100	0	none.
Pa	inHg	pressure	0.00029528744140000003	0	none: 1013.25 hPa = 29.92 inHg
Pa	mb	pressure	100	0	1 mb = 1 hPa
C	K	temperature	1	273.14999999999998	CRC Handbook of Chemistry and Physics, 76th ed.: (C = K - 273.15)
C	F	temperature	1.8	32	none: (F = (9.0 * C / 5.0) + 32.0)
F	C	temperature	0.55555555555555558	-17.777777777777779	none: (F = (9.0 * C / 5.0) + 32.0)
F	K	temperature	0.55555555555555558	255.37222222222221	CRC Handbook of Chemistry and Physics, 76th ed.: (C = K - 273.15);  none: (F = (9.0 * C / 5.0) + 32.0))
K	C	temperature	1	-273.14999999999998	CRC Handbook of Chemistry and Physics, 76th ed.: (C = K - 273.15)
K	F	temperature	1.8	-459.67000000000002	CRC Handbook of Chemistry and Physics, 76th ed.: (C = K - 273.15);  none: (F = (9.0 * C / 5.0) + 32.0))
d	hr	time	24	0	none: (24 hr / day)
d	min	time	1440	0	none: (24 hr / day)
d	s	time	86400	0	none: (24 hr / day)
hr	d	time	0.041666666666666671	0	none: (24 hr / day)
hr	min	time	60	0	none.
hr	s	time	3600	0	none.
min	d	time	0.00069444444444444436	0	none: (24 hr / day)
min	hr	time	0.01666666666666667	0	none.
min	s	time	60	0	none.
s	d	time	0.0001157407407407407	0	none: (24 hr / day)
s	hr	time	0.00027777777777777778	0	none.
s	min	time	0.01666666666666667	0	none.
kt	m/s	speed	0.51449329219999995	0	?/foa/src/localization/nationalData/contourStyle.rules: equator-to-pole = 10000 km, 1 degree latitute = 60 nmi
kt	mph	speed	1.1506873929999999	0	?/foa/src/localization/nationalData/contourStyle.rules: equator-to-pole = 10000 km, 1 degree latitute = 60 nmi; CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
m/s	kt	speed	1.944	0	?/foa/src/localization/nationalData/contourStyle.rules: equator-to-pole = 10000 km, 1 degree latitute = 60 nmi
mph	kt	speed	0.86904576	0	?/foa/src/localization/nationalData/contourStyle.rules: equator-to-pole = 10000 km, 1 degree latitute = 60 nmi; CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
cm/d	cm/hr	speed	0.041666666666666671	0	none: (24 hr / day)
cm/d	cm/min	speed	0.00069444444444444436	0	none: (24 hr / day)
cm/d	cm/s	speed	1.157407407407407e-05	0	none: (24 hr / day)
cm/d	ft/d	speed	0.032808398949999998	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
cm/d	ft/hr	speed	0.0013670166230000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
cm/d	ft/min	speed	2.2783610380000001e-05	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
cm/d	ft/s	speed	3.7972683969999999e-07	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
cm/d	in/d	speed	0.39370078739999997	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
cm/d	in/hr	speed	0.016404199479999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
cm/d	in/min	speed	0.00027340332460000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
cm/d	in/s	speed	4.556722076e-06	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
cm/d	km/d	speed	1.0000000000000001e-05	0	none.
cm/d	km/hr	speed	4.1666666666666672e-07	0	none: (24 hr / day)
cm/d	km/min	speed	6.9444444444444427e-09	0	none: (24 hr / day)
cm/d	km/s	speed	1.157407407407407e-10	0	none: (24 hr / day)
cm/d	m/d	speed	0.01	0	none.
cm/d	m/hr	speed	0.00041666666666666669	0	none: (24 hr / day)
cm/d	m/min	speed	6.9444444444444439e-06	0	none: (24 hr / day)
cm/d	m/s	speed	1.157407407407407e-07	0	none: (24 hr / day)
cm/d	mi/d	speed	6.2137119220000002e-06	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
cm/d	mph	speed	2.5890466339999998e-07	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
cm/d	mi/min	speed	4.3150777240000003e-09	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
cm/d	mi/s	speed	7.1917962060000019e-11	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
cm/d	mm/d	speed	10	0	none.
cm/d	mm/hr	speed	0.41666666666666669	0	none: (24 hr / day)
cm/d	mm/min	speed	0.0069444444444444441	0	none: (24 hr / day)
cm/d	mm/s	speed	0.0001157407407407407	0	none: (24 hr / day)
cm/hr	cm/d	speed	24	0	none: (24 hr / day)
cm/hr	cm/min	speed	0.01666666666666667	0	none.
cm/hr	cm/s	speed	0.00027777777777777778	0	none.
cm/hr	ft/d	speed	0.78740157480000006	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
cm/hr	ft/hr	speed	0.032808398949999998	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
cm/hr	ft/min	speed	0.00054680664920000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
cm/hr	ft/s	speed	9.1134441529999986e-06	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
cm/hr	in/d	speed	9.448818897999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
cm/hr	in/hr	speed	0.39370078739999997	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
cm/hr	in/min	speed	0.0065616797899999994	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
cm/hr	in/s	speed	0.0001093613298	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
cm/hr	km/d	speed	0.00024000000000000001	0	none: (24 hr / day)
cm/hr	km/hr	speed	1.0000000000000001e-05	0	none.
cm/hr	km/min	speed	1.666666666666667e-07	0	none.
cm/hr	km/s	speed	2.777777777777778e-09	0	none.
cm/hr	m/d	speed	0.23999999999999999	0	none: (24 hr / day)
cm/hr	m/hr	speed	0.01	0	none.
cm/hr	m/min	speed	0.00016666666666666669	0	none.
cm/hr	m/s	speed	2.7777777777777779e-06	0	none.
cm/hr	mi/d	speed	0.0001491290861	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
cm/hr	mph	speed	6.2137119220000002e-06	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
cm/hr	mi/min	speed	1.035618654e-07	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
cm/hr	mi/s	speed	1.72603109e-09	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
cm/hr	mm/d	speed	240	0	none: (24 hr / day)
cm/hr	mm/hr	speed	10	0	none.
cm/hr	mm/min	speed	0.16666666666666671	0	none.
cm/hr	mm/s	speed	0.0027777777777777779	0	none.
cm/min	cm/d	speed	1440	0	none: (24 hr / day)
cm/min	cm/hr	speed	60	0	none.
cm/min	cm/s	speed	0.01666666666666667	0	none.
cm/min	ft/d	speed	47.244094490000002	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
cm/min	ft/hr	speed	1.9685039369999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
cm/min	ft/min	speed	0.032808398949999998	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
cm/min	ft/s	speed	0.00054680664920000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
cm/min	in/d	speed	566.92913390000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
cm/min	in/hr	speed	23.622047240000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
cm/min	in/min	speed	0.39370078739999997	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
cm/min	in/s	speed	0.0065616797899999994	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
cm/min	km/d	speed	0.0144	0	none: (24 hr / day)
cm/min	km/hr	speed	0.00059999999999999995	0	none.
cm/min	km/min	speed	1.0000000000000001e-05	0	none.
cm/min	km/s	speed	1.666666666666667e-07	0	none.
cm/min	m/d	speed	14.4	0	none: (24 hr / day)
cm/min	m/hr	speed	0.59999999999999998	0	none.
cm/min	m/min	speed	0.01	0	none.
cm/min	m/s	speed	0.00016666666666666669	0	none.
cm/min	mi/d	speed	0.0089477451679999976	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
cm/min	mph	speed	0.00037282271529999998	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
cm/min	mi/min	speed	6.2137119220000002e-06	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
cm/min	mi/s	speed	1.035618654e-07	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
cm/min	mm/d	speed	14400	0	none: (24 hr / day)
cm/min	mm/hr	speed	600	0	none.
cm/min	mm/min	speed	10	0	none.
cm/min	mm/s	speed	0.16666666666666671	0	none.
cm/s	cm/d	speed	86400	0	none: (24 hr / day)
cm/s	cm/hr	speed	3600	0	none.
cm/s	cm/min	speed	60	0	none.
cm/s	ft/d	speed	2834.645669	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
cm/s	ft/hr	speed	118.1102362	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
cm/s	ft/min	speed	1.9685039369999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
cm/s	ft/s	speed	0.032808398949999998	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
cm/s	in/d	speed	34015.748030000002	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
cm/s	in/hr	speed	1417.3228349999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
cm/s	in/min	speed	23.622047240000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
cm/s	in/s	speed	0.39370078739999997	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
cm/s	km/d	speed	0.8640000000000001	0	none: (24 hr / day)
cm/s	km/hr	speed	0.0035999999999999999	0	none.
cm/s	km/min	speed	0.00059999999999999995	0	none.
cm/s	km/s	speed	1.0000000000000001e-05	0	none.
cm/s	m/d	speed	864	0	none: (24 hr / day)
cm/s	m/hr	speed	36	0	none.
cm/s	m/min	speed	0.59999999999999998	0	none.
cm/s	m/s	speed	0.01	0	none.
cm/s	mi/d	speed	0.53686471010000003	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
cm/s	mph	speed	0.02236936292	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
cm/s	mi/min	speed	0.00037282271529999998	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
cm/s	mi/s	speed	6.2137119220000002e-06	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
cm/s	mm/d	speed	864000	0	none: (24 hr / day)
cm/s	mm/hr	speed	36000	0	none.
cm/s	mm/min	speed	600	0	none.
cm/s	mm/s	speed	10	0	none.
ft/d	cm/d	speed	30.48	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/d	cm/hr	speed	1.27	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
ft/d	cm/min	speed	0.02116666666666667	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
ft/d	cm/s	speed	0.00035277777777777781	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
ft/d	ft/hr	speed	0.041666666666666671	0	none: (24 hr / day)
ft/d	ft/min	speed	0.00069444444444444436	0	none: (24 hr / day)
ft/d	ft/s	speed	1.157407407407407e-05	0	none: (24 hr / day)
ft/d	in/d	speed	12	0	none.
ft/d	in/hr	speed	0.5	0	none: (24 hr / day)
ft/d	in/min	speed	0.0083333333333333332	0	none: (24 hr / day)
ft/d	in/s	speed	0.00013888888888888889	0	none: (24 hr / day)
ft/d	km/d	speed	0.00030479999999999998	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/d	km/hr	speed	1.27e-05	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
ft/d	km/min	speed	2.116666666666667e-07	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
ft/d	km/s	speed	3.5277777777777781e-09	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
ft/d	m/d	speed	0.30480000000000002	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/d	m/hr	speed	0.012699999999999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
ft/d	m/min	speed	0.0002116666666666667	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
ft/d	m/s	speed	3.5277777777777779e-06	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
ft/d	mi/d	speed	0.00018939393939393939	0	none: (5280.0 ft / mi)
ft/d	mph	speed	7.8914141414141414e-06	0	none: (24 hr / day); none: (5280.0 ft / mi)
ft/d	mi/min	speed	3.1523568999999999e-07	0	none: (24 hr / day); none: (5280.0 ft / mi)
ft/d	mi/s	speed	2.1920594839999999e-09	0	none: (24 hr / day); none: (5280.0 ft / mi)
ft/d	mm/d	speed	304.80000000000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/d	mm/hr	speed	12.699999999999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
ft/d	mm/min	speed	0.2116666666666667	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
ft/d	mm/s	speed	0.0035277777777777781	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
ft/hr	cm/d	speed	713.51999999999998	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
ft/hr	cm/hr	speed	30.48	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/hr	cm/min	speed	5.0800000000000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/hr	cm/s	speed	0.0084666666666666675	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/hr	ft/d	speed	24	0	none: (24 hr / day)
ft/hr	ft/min	speed	0.01666666666666667	0	none.
ft/hr	ft/s	speed	0.00027777777777777778	0	none.
ft/hr	in/d	speed	288	0	none: (24 hr / day)
ft/hr	in/hr	speed	12	0	none.
ft/hr	in/min	speed	0.20000000000000001	0	none.
ft/hr	in/s	speed	0.0033333333333333331	0	none.
ft/hr	km/d	speed	0.0071352000000000004	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
ft/hr	km/hr	speed	0.00030479999999999998	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/hr	km/min	speed	5.0800000000000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/hr	km/s	speed	8.466666666666667e-08	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/hr	m/d	speed	7.1352000000000002	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
ft/hr	m/hr	speed	0.30480000000000002	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/hr	m/min	speed	0.0050799999999999986	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/hr	m/s	speed	8.4666666666666674e-05	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/hr	mi/d	speed	0.0045454545454545452	0	none: (24 hr / day); none: (5280.0 ft / mi)
ft/hr	mph	speed	0.00018939393939393939	0	none: (5280.0 ft / mi)
ft/hr	mi/min	speed	3.156565656565657e-06	0	none: (5280.0 ft / mi)
ft/hr	mi/s	speed	5.2609427600000002e-08	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/hr	mm/d	speed	7135.1999999999998	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
ft/hr	mm/hr	speed	304.80000000000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/hr	mm/min	speed	5.0800000000000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/hr	mm/s	speed	0.084666666666666682	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/min	cm/d	speed	43891.199999999997	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
ft/min	cm/hr	speed	1828.8	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/min	cm/min	speed	30.48	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/min	cm/s	speed	0.50800000000000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/min	ft/d	speed	1440	0	none: (24 hr / day)
ft/min	ft/hr	speed	60	0	none.
ft/min	ft/s	speed	0.01666666666666667	0	none.
ft/min	in/d	speed	17280	0	none: (24 hr / day)
ft/min	in/hr	speed	720	0	none.
ft/min	in/min	speed	12	0	none.
ft/min	in/s	speed	0.20000000000000001	0	none.
ft/min	km/d	speed	0.43891200000000002	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
ft/min	km/hr	speed	0.018287999999999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/min	km/min	speed	0.00030479999999999998	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/min	km/s	speed	5.0799999999999996e-06	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/min	m/d	speed	438.91199999999998	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
ft/min	m/hr	speed	18.288	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/min	m/min	speed	0.30480000000000002	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/min	m/s	speed	0.0050799999999999986	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/min	mi/d	speed	0.27272727272727271	0	none: (24 hr / day); none: (5280.0 ft / mi)
ft/min	mph	speed	0.01136363636363636	0	none: (5280.0 ft / mi)
ft/min	mi/min	speed	0.00018939393939393939	0	none: (5280.0 ft / mi)
ft/min	mi/s	speed	3.156565656565657e-06	0	none: (5280.0 ft / mi)
ft/min	mm/d	speed	438912	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
ft/min	mm/hr	speed	18288	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/min	mm/min	speed	304.80000000000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/min	mm/s	speed	5.0800000000000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/s	cm/d	speed	2633472	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
ft/s	cm/hr	speed	109728	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/s	cm/min	speed	1828.8	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/s	cm/s	speed	30.48	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/s	ft/d	speed	86400	0	none: (24 hr / day)
ft/s	ft/hr	speed	3600	0	none.
ft/s	ft/min	speed	60	0	none.
ft/s	in/d	speed	1036800	0	none: (24 hr / day)
ft/s	in/hr	speed	43200	0	none.
ft/s	in/min	speed	720	0	none.
ft/s	in/s	speed	12	0	none.
ft/s	km/d	speed	26.334720000000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
ft/s	km/hr	speed	1.09728	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/s	km/min	speed	0.018287999999999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/s	km/s	speed	0.00030479999999999998	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/s	m/d	speed	26334.720000000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
ft/s	m/hr	speed	1097.28	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/s	m/min	speed	18.288	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/s	m/s	speed	0.30480000000000002	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/s	mi/d	speed	16.36363636363636	0	none: (24 hr / day); none: (5280.0 ft / mi)
ft/s	mph	speed	0.68181818181818177	0	none: (5280.0 ft / mi)
ft/s	mi/min	speed	0.01136363636363636	0	none: (5280.0 ft / mi)
ft/s	mi/s	speed	0.00018939393939393939	0	none: (5280.0 ft / mi)
ft/s	mm/d	speed	26334720	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
ft/s	mm/hr	speed	1097280	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/s	mm/min	speed	18288	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
ft/s	mm/s	speed	304.80000000000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/d	cm/d	speed	2.54	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/d	cm/hr	speed	0.10583333333333329	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
in/d	cm/min	speed	0.0017638888888888891	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
in/d	cm/s	speed	2.939814814814815e-05	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
in/d	ft/d	speed	0.083333333333333329	0	none.
in/d	ft/hr	speed	0.003472222222222222	0	none: (24 hr / day)
in/d	ft/min	speed	5.7870370370370373e-05	0	none: (24 hr / day)
in/d	ft/s	speed	9.6450617279999994e-07	0	none: (24 hr / day)
in/d	in/hr	speed	0.041666666666666671	0	none: (24 hr / day)
in/d	in/min	speed	0.00069444444444444436	0	none: (24 hr / day)
in/d	in/s	speed	1.157407407407407e-05	0	none: (24 hr / day)
in/d	km/d	speed	2.5400000000000001e-05	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/d	km/hr	speed	1.0583333333333331e-06	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
in/d	km/min	speed	1.7638888888888889e-08	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
in/d	km/s	speed	2.9398148148148149e-10	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
in/d	m/d	speed	0.025399999999999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/d	m/hr	speed	0.001058333333333333	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
in/d	m/min	speed	1.7638888888888889e-05	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
in/d	m/s	speed	2.9398148148148149e-07	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
in/d	mi/d	speed	1.5782828282828279e-05	0	none: (5280.0 ft / mi)
in/d	mph	speed	6.5761784509999997e-07	0	none: (24 hr / day); none: (5280.0 ft / mi)
in/d	mi/min	speed	1.096029742e-08	0	none: (24 hr / day); none: (5280.0 ft / mi)
in/d	mi/s	speed	1.8267162359999999e-10	0	none: (24 hr / day); none: (5280.0 ft / mi)
in/d	mm/d	speed	25.399999999999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/d	mm/hr	speed	1.0583333333333329	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
in/d	mm/min	speed	0.017638888888888891	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
in/d	mm/s	speed	0.00029398148148148149	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
in/hr	cm/d	speed	60.960000000000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
in/hr	cm/hr	speed	2.54	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/hr	cm/min	speed	0.042333333333333327	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/hr	cm/s	speed	0.00070555555555555562	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/hr	ft/d	speed	2	0	none: (24 hr / day)
in/hr	ft/hr	speed	0.083333333333333329	0	none.
in/hr	ft/min	speed	0.0013888888888888889	0	none.
in/hr	ft/s	speed	2.314814814814815e-05	0	none.
in/hr	in/d	speed	24	0	none: (24 hr / day)
in/hr	in/min	speed	0.01666666666666667	0	none.
in/hr	in/s	speed	0.00027777777777777778	0	none.
in/hr	km/d	speed	0.00060959999999999996	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
in/hr	km/hr	speed	2.5400000000000001e-05	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/hr	km/min	speed	4.2333333333333329e-07	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/hr	km/s	speed	7.055555555555557e-09	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/hr	m/d	speed	0.60960000000000003	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
in/hr	m/hr	speed	0.025399999999999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/hr	m/min	speed	0.00042333333333333329	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/hr	m/s	speed	7.0555555555555559e-06	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/hr	mi/d	speed	0.00037878787878787879	0	none: (24 hr / day); none: (5280.0 ft / mi)
in/hr	mph	speed	1.5782828282828279e-05	0	none: (5280.0 ft / mi)
in/hr	mi/min	speed	2.6304713800000001e-07	0	none: (5280.0 ft / mi)
in/hr	mi/s	speed	4.3841189670000013e-09	0	none: (5280.0 ft / mi)
in/hr	mm/d	speed	609.60000000000002	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
in/hr	mm/hr	speed	25.399999999999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/hr	mm/min	speed	0.42333333333333328	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/hr	mm/s	speed	0.0070555555555555562	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/min	cm/d	speed	3657.5999999999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
in/min	cm/hr	speed	152.40000000000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/min	cm/min	speed	2.54	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/min	cm/s	speed	0.042333333333333327	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/min	ft/d	speed	120	0	none.
in/min	ft/hr	speed	5	0	none.
in/min	ft/min	speed	0.083333333333333329	0	none.
in/min	ft/s	speed	0.0013888888888888889	0	none.
in/min	in/d	speed	1440	0	none: (24 hr / day)
in/min	in/hr	speed	60	0	none.
in/min	in/s	speed	0.01666666666666667	0	none.
in/min	km/d	speed	0.036575999999999997	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
in/min	km/hr	speed	0.001524	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/min	km/min	speed	2.5400000000000001e-05	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/min	km/s	speed	4.2333333333333283e-73	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/min	m/d	speed	36.576000000000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
in/min	m/hr	speed	1.524	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/min	m/min	speed	0.025399999999999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/min	m/s	speed	0.00042333333333333329	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/min	mi/d	speed	0.022727272727272731	0	none: (24 hr / day); none: (5280.0 ft / mi)
in/min	mph	speed	0.00094696969696969721	0	none: (5280.0 ft / mi)
in/min	mi/min	speed	1.5782828282828279e-05	0	none: (5280.0 ft / mi)
in/min	mi/s	speed	2.6304713800000001e-07	0	none: (5280.0 ft / mi)
in/min	mm/d	speed	36576	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
in/min	mm/hr	speed	1524	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/min	mm/min	speed	25.399999999999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/min	mm/s	speed	0.42333333333333328	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/s	cm/d	speed	219456	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
in/s	cm/hr	speed	9144	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/s	cm/min	speed	152.40000000000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/s	cm/s	speed	2.54	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/s	ft/d	speed	7200	0	none: (24 hr / day)
in/s	ft/hr	speed	300	0	none.
in/s	ft/min	speed	5	0	none.
in/s	ft/s	speed	0.083333333333333329	0	none.
in/s	in/d	speed	86400	0	none: (24 hr / day)
in/s	in/hr	speed	3600	0	none.
in/s	in/min	speed	60	0	none.
in/s	km/d	speed	2.1945600000000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
in/s	km/hr	speed	0.091439999999999994	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/s	km/min	speed	0.001524	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/s	km/s	speed	2.5400000000000001e-05	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/s	m/d	speed	2194.5599999999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
in/s	m/hr	speed	91.439999999999998	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/s	m/min	speed	1.524	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/s	m/s	speed	0.025399999999999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/s	mi/d	speed	1.363636363636364	0	none: (24 hr / day); none: (5280.0 ft / mi)
in/s	mph	speed	0.056818181818181823	0	none: (5280.0 ft / mi)
in/s	mi/min	speed	0.00094696969696969721	0	none: (5280.0 ft / mi)
in/s	mi/s	speed	1.5782828282828279e-05	0	none: (5280.0 ft / mi)
in/s	mm/d	speed	2194560	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
in/s	mm/hr	speed	91440	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/s	mm/min	speed	1524	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
in/s	mm/s	speed	25.399999999999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
km/d	cm/d	speed	100000	0	none.
km/d	cm/hr	speed	4166.666666666667	0	none: (24 hr / day)
km/d	cm/min	speed	69.444444444444443	0	none: (24 hr / day)
km/d	cm/s	speed	1.157407407407407	0	none: (24 hr / day)
km/d	ft/d	speed	3280.8398950000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
km/d	ft/hr	speed	136.70166230000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
km/d	ft/min	speed	2.2783610379999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
km/d	ft/s	speed	0.037972683969999997	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
km/d	in/d	speed	39370.078739999997	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
km/d	in/hr	speed	1640.419948	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
km/d	in/min	speed	27.340332459999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
km/d	in/s	speed	0.45567220759999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
km/d	km/hr	speed	0.041666666666666671	0	none: (24 hr / day)
km/d	km/min	speed	0.00069444444444444436	0	none: (24 hr / day)
km/d	km/s	speed	1.157407407407407e-05	0	none: (24 hr / day)
km/d	m/d	speed	1000	0	none.
km/d	m/hr	speed	41.666666666666671	0	none: (24 hr / day)
km/d	m/min	speed	0.69444444444444442	0	none: (24 hr / day)
km/d	m/s	speed	0.01157407407407407	0	none: (24 hr / day)
km/d	mi/d	speed	0.62137119220000003	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
km/d	mph	speed	0.025890466340000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
km/d	mi/min	speed	0.00043150777239999978	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
km/d	mi/s	speed	7.1917962060000003e-06	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
km/d	mm/d	speed	1000000	0	none.
km/d	mm/hr	speed	41666.666666666672	0	none: (24 hr / day)
km/d	mm/min	speed	694.44444444444446	0	none: (24 hr / day)
km/d	mm/s	speed	11.574074074074071	0	none: (24 hr / day)
km/hr	cm/d	speed	2400000	0	none: (24 hr / day)
km/hr	cm/hr	speed	100000	0	none.
km/hr	cm/min	speed	1666.666666666667	0	none.
km/hr	cm/s	speed	27.777777777777779	0	none.
km/hr	ft/d	speed	78740.157479999994	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
km/hr	ft/hr	speed	3280.8398950000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
km/hr	ft/min	speed	54.680664919999998	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
km/hr	ft/s	speed	0.91134441529999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
km/hr	in/d	speed	944881.8898	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
km/hr	in/hr	speed	39370.078739999997	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
km/hr	in/min	speed	656.16797900000017	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
km/hr	in/s	speed	10.93613298	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
km/hr	km/d	speed	24	0	none: (24 hr / day)
km/hr	km/min	speed	0.01666666666666667	0	none.
km/hr	km/s	speed	0.00027777777777777778	0	none.
km/hr	m/d	speed	24000	0	none: (24 hr / day)
km/hr	m/hr	speed	1000	0	none.
km/hr	m/min	speed	16.666666666666671	0	none.
km/hr	m/s	speed	0.27777777777777779	0	none.
km/hr	mi/d	speed	14.912908610000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
km/hr	mph	speed	0.62137119220000003	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
km/hr	mi/min	speed	0.010356186539999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
km/hr	mi/s	speed	0.00017260310900000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
km/hr	mm/d	speed	24000000	0	none: (24 hr / day)
km/hr	mm/hr	speed	1000000	0	none.
km/hr	mm/min	speed	16666.666666666672	0	none.
km/hr	mm/s	speed	277.77777777777783	0	none.
km/min	cm/d	speed	144000000	0	none: (24 hr / day)
km/min	cm/hr	speed	6000000	0	none.
km/min	cm/min	speed	100000	0	none.
km/min	cm/s	speed	1666.666666666667	0	none.
km/min	ft/d	speed	4724409.449	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
km/min	ft/hr	speed	196850.39369999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
km/min	ft/min	speed	3280.8398950000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
km/min	ft/s	speed	54.680664919999998	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
km/min	in/d	speed	56692913.390000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
km/min	in/hr	speed	2362204.7239999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
km/min	in/min	speed	39370.078739999997	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
km/min	in/s	speed	656.16797900000017	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
km/min	km/d	speed	1440	0	none: (24 hr / day)
km/min	km/hr	speed	60	0	none.
km/min	km/s	speed	0.01666666666666667	0	none.
km/min	m/d	speed	1440000	0	none: (24 hr / day)
km/min	m/hr	speed	60000	0	none.
km/min	m/min	speed	1000	0	none.
km/min	m/s	speed	16.666666666666671	0	none.
km/min	mi/d	speed	894.77451680000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
km/min	mph	speed	37.282271530000003	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
km/min	mi/min	speed	0.62137119220000003	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
km/min	mi/s	speed	0.010356186539999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
km/min	mm/d	speed	1440000000	0	none: (24 hr / day)
km/min	mm/hr	speed	60000000	0	none.
km/min	mm/min	speed	1000000	0	none.
km/min	mm/s	speed	16666.666666666672	0	none.
km/s	cm/d	speed	8640000000	0	none: (24 hr / day)
km/s	cm/hr	speed	360000000	0	none.
km/s	cm/min	speed	6000000	0	none.
km/s	cm/s	speed	100000	0	none.
km/s	ft/d	speed	283464566.89999998	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
km/s	ft/hr	speed	11811023.619999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
km/s	ft/min	speed	196850.39369999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
km/s	ft/s	speed	3280.8398950000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
km/s	in/d	speed	3401574803	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
km/s	in/hr	speed	141732283.5	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
km/s	in/min	speed	2362204.7239999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
km/s	in/s	speed	39370.078739999997	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
km/s	km/d	speed	86400	0	none: (24 hr / day)
km/s	km/hr	speed	3600	0	none.
km/s	km/min	speed	60	0	none.
km/s	m/d	speed	86400000	0	none: (24 hr / day)
km/s	m/hr	speed	3600000	0	none.
km/s	m/min	speed	60000	0	none.
km/s	m/s	speed	1000	0	none.
km/s	mi/d	speed	53686.471010000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
km/s	mph	speed	2236.9362919999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
km/s	mi/min	speed	37.282271530000003	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
km/s	mi/s	speed	0.62137119220000003	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
km/s	mm/d	speed	86400000000	0	none: (24 hr / day)
km/s	mm/hr	speed	3600000000	0	none.
km/s	mm/min	speed	60000000	0	none.
km/s	mm/s	speed	1000000	0	none.
m/d	cm/d	speed	100	0	none.
m/d	cm/hr	speed	4.166666666666667	0	none: (24 hr / day)
m/d	cm/min	speed	0.06944444444444442	0	none: (24 hr / day)
m/d	cm/s	speed	0.0011574074074074069	0	none: (24 hr / day)
m/d	ft/d	speed	3.2808398950000002	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
m/d	ft/hr	speed	0.1367016623	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
m/d	ft/min	speed	0.0022783610379999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
m/d	ft/s	speed	3.7972683970000003e-05	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
m/d	in/d	speed	39.370078739999997	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
m/d	in/hr	speed	1.6404199479999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
m/d	in/min	speed	0.027340332459999998	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
m/d	in/s	speed	0.00045567220760000003	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
m/d	km/d	speed	0.001	0	none.
m/d	km/hr	speed	4.1666666666666672e-05	0	none: (24 hr / day)
m/d	km/min	speed	6.9444444444444437e-07	0	none: (24 hr / day)
m/d	km/s	speed	1.1574074074074071e-08	0	none: (24 hr / day)
m/d	m/hr	speed	0.041666666666666671	0	none: (24 hr / day)
m/d	m/min	speed	0.00069444444444444436	0	none: (24 hr / day)
m/d	m/s	speed	1.157407407407407e-05	0	none: (24 hr / day)
m/d	mi/d	speed	0.00062137119220000007	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
m/d	mph	speed	2.589046634e-05	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
m/d	mi/min	speed	4.3150777239999998e-07	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
m/d	mi/s	speed	7.1917962059999996e-09	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
m/d	mm/d	speed	1000	0	none.
m/d	mm/hr	speed	41.666666666666671	0	none: (24 hr / day)
m/d	mm/min	speed	0.69444444444444442	0	none: (24 hr / day)
m/d	mm/s	speed	0.01157407407407407	0	none: (24 hr / day)
m/hr	cm/d	speed	2400	0	none: (24 hr / day)
m/hr	cm/hr	speed	100	0	none.
m/hr	cm/min	speed	1.666666666666667	0	none.
m/hr	cm/s	speed	0.02777777777777778	0	none.
m/hr	ft/d	speed	7.8740157479999997	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
m/hr	ft/hr	speed	3.2808398950000002	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
m/hr	ft/min	speed	0.054680664919999997	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
m/hr	ft/s	speed	0.00091134441529999992	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
m/hr	in/d	speed	944.88188979999995	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
m/hr	in/hr	speed	39.370078739999997	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
m/hr	in/min	speed	0.65616797900000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
m/hr	in/s	speed	0.010936132980000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
m/hr	km/d	speed	0.024	0	none: (24 hr / day)
m/hr	km/hr	speed	0.001	0	none.
m/hr	km/min	speed	1.6666666666666671e-05	0	none.
m/hr	km/s	speed	2.7777777777777781e-07	0	none.
m/hr	m/d	speed	24	0	none: (24 hr / day)
m/hr	m/min	speed	0.01666666666666667	0	none.
m/hr	m/s	speed	0.00027777777777777778	0	none.
m/hr	mi/d	speed	0.014912908609999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
m/hr	mph	speed	0.00062137119220000007	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
m/hr	mi/min	speed	1.035618654e-05	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
m/hr	mi/s	speed	1.72603109e-07	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
m/hr	mm/d	speed	24000	0	none: (24 hr / day)
m/hr	mm/hr	speed	1000	0	none.
m/hr	mm/min	speed	16.666666666666671	0	none.
m/hr	mm/s	speed	0.27777777777777779	0	none.
m/min	cm/d	speed	144000	0	none: (24 hr / day)
m/min	cm/hr	speed	6000	0	none.
m/min	cm/min	speed	100	0	none.
m/min	cm/s	speed	1.666666666666667	0	none.
m/min	ft/d	speed	4724.4094489999998	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
m/min	ft/hr	speed	196.85039370000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
m/min	ft/min	speed	3.2808398950000002	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
m/min	ft/s	speed	0.054680664919999997	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
m/min	in/d	speed	56692.913389999987	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
m/min	in/hr	speed	2362.2047240000002	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
m/min	in/min	speed	39.370078739999997	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
m/min	in/s	speed	0.65616797900000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
m/min	km/d	speed	1.4399999999999999	0	none: (24 hr / day)
m/min	km/hr	speed	0.059999999999999998	0	none.
m/min	km/min	speed	0.001	0	none.
m/min	km/s	speed	1.6666666666666671e-05	0	none.
m/min	m/d	speed	1440	0	none: (24 hr / day)
m/min	m/hr	speed	60	0	none.
m/min	m/s	speed	0.01666666666666667	0	none.
m/min	mi/d	speed	0.89477451680000009	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
m/min	mph	speed	0.037282271530000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
m/min	mi/min	speed	0.00062137119220000007	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
m/min	mi/s	speed	1.035618654e-05	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
m/min	mm/d	speed	1440000	0	none: (24 hr / day)
m/min	mm/hr	speed	60000	0	none.
m/min	mm/min	speed	1000	0	none.
m/min	mm/s	speed	16.666666666666671	0	none.
m/s	cm/d	speed	8640000	0	none: (24 hr / day)
m/s	cm/hr	speed	360000	0	none.
m/s	cm/min	speed	6000	0	none.
m/s	cm/s	speed	100	0	none.
m/s	ft/d	speed	283464.56689999998	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
m/s	ft/hr	speed	11811.02362	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
m/s	ft/min	speed	196.85039370000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
m/s	ft/s	speed	3.2808398950000002	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
m/s	in/d	speed	3401574.8029999998	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
m/s	in/hr	speed	141732.28349999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
m/s	in/min	speed	2362.2047240000002	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
m/s	in/s	speed	39.370078739999997	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
m/s	km/d	speed	86.40000000000002	0	none: (24 hr / day)
m/s	km/hr	speed	3.6000000000000001	0	none.
m/s	km/min	speed	0.059999999999999998	0	none.
m/s	km/s	speed	0.001	0	none.
m/s	m/d	speed	86400	0	none: (24 hr / day)
m/s	m/hr	speed	3600	0	none.
m/s	m/min	speed	60	0	none.
m/s	mi/d	speed	53.686471009999998	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
m/s	mph	speed	2.2369362920000002	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
m/s	mi/min	speed	0.037282271530000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
m/s	mi/s	speed	0.00062137119220000007	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
m/s	mm/d	speed	86400000	0	none: (24 hr / day)
m/s	mm/hr	speed	3600000	0	none.
m/s	mm/min	speed	60000	0	none.
m/s	mm/s	speed	1000	0	none.
mi/d	cm/d	speed	160934.39999999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mi/d	cm/hr	speed	6705.6000000000004	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
mi/d	cm/min	speed	111.76000000000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
mi/d	cm/s	speed	1.8626666666666669	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
mi/d	ft/d	speed	5280	0	none: (5280.0 ft / mi)
mi/d	ft/hr	speed	220	0	none: (24 hr / day); none: (5280.0 ft / mi)
mi/d	ft/min	speed	3.666666666666667	0	none: (24 hr / day); none: (5280.0 ft / mi)
mi/d	ft/s	speed	0.061111111111111109	0	none: (24 hr / day); none: (5280.0 ft / mi)
mi/d	in/d	speed	63360	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mi/d	in/hr	speed	2640	0	none: (24 hr / day); none: (5280.0 ft / mi)
mi/d	in/min	speed	44	0	none: (24 hr / day); none: (5280.0 ft / mi)
mi/d	in/s	speed	0.73333333333333328	0	none: (24 hr / day); none: (5280.0 ft / mi)
mi/d	km/d	speed	1.6093440000000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mi/d	km/hr	speed	0.067055999999999991	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
mi/d	km/min	speed	0.0011176000000000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
mi/d	km/s	speed	1.8626666666666669e-05	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
mi/d	m/d	speed	1609.3440000000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mi/d	m/hr	speed	67.055999999999997	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
mi/d	m/min	speed	1.1175999999999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
mi/d	m/s	speed	0.01862666666666667	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
mi/d	mph	speed	0.041666666666666671	0	none: (24 hr / day)
mi/d	mi/min	speed	0.00069444444444444436	0	none: (24 hr / day)
mi/d	mi/s	speed	1.157407407407407e-05	0	none: (24 hr / day)
mi/d	mm/d	speed	1609344	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mi/d	mm/hr	speed	67056	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
mi/d	mm/min	speed	1117.5999999999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
mi/d	mm/s	speed	18.626666666666669	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
mph	cm/d	speed	3862425.6000000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
mph	cm/hr	speed	160934.39999999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mph	cm/min	speed	2682.2399999999998	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mph	cm/s	speed	44.703999999999994	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mph	ft/d	speed	126720	0	none: (24 hr / day); none: (5280.0 ft / mi)
mph	ft/hr	speed	5280	0	none: (5280.0 ft / mi)
mph	ft/min	speed	88	0	none: (5280.0 ft / mi)
mph	ft/s	speed	1.466666666666667	0	none: (5280.0 ft / mi)
mph	in/d	speed	1520640	0	none: (24 hr / day); none: (5280.0 ft / mi)
mph	in/hr	speed	63360	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mph	in/min	speed	1056	0	none: (5280.0 ft / mi)
mph	in/s	speed	17.600000000000001	0	none: (5280.0 ft / mi)
mph	km/d	speed	38.624256000000003	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
mph	km/hr	speed	1.6093440000000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mph	km/min	speed	0.0268224	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mph	km/s	speed	0.00044704	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mph	m/d	speed	38624.256000000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
mph	m/hr	speed	1609.3440000000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mph	m/min	speed	26.822399999999998	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mph	m/s	speed	0.44703999999999988	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mph	mi/d	speed	24	0	none: (24 hr / day)
mph	mi/min	speed	0.01666666666666667	0	none.
mph	mi/s	speed	0.00027777777777777778	0	none.
mph	mm/d	speed	38624256	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
mph	mm/hr	speed	1609344	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mph	mm/min	speed	26822.400000000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mph	mm/s	speed	447.04000000000002	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mi/min	cm/d	speed	231745536	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
mi/min	cm/hr	speed	9656064	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mi/min	cm/min	speed	160934.39999999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mi/min	cm/s	speed	2682.2399999999998	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mi/min	ft/d	speed	7603200	0	none: (24 hr / day); none: (5280.0 ft / mi)
mi/min	ft/hr	speed	316800	0	none: (5280.0 ft / mi)
mi/min	ft/min	speed	5280	0	none: (5280.0 ft / mi)
mi/min	ft/s	speed	88	0	none: (5280.0 ft / mi)
mi/min	in/d	speed	91238400	0	none: (24 hr / day); none: (5280.0 ft / mi)
mi/min	in/hr	speed	3801600	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mi/min	in/min	speed	63360	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mi/min	in/s	speed	1056	0	none: (5280.0 ft / mi)
mi/min	km/d	speed	2317.4553599999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
mi/min	km/hr	speed	96.560640000000006	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mi/min	km/min	speed	1.6093440000000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mi/min	km/s	speed	0.0268224	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mi/min	m/d	speed	2317455.3599999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
mi/min	m/hr	speed	96560.639999999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mi/min	m/min	speed	1609.3440000000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mi/min	m/s	speed	26.822399999999998	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mi/min	mi/d	speed	1440	0	none: (24 hr / day)
mi/min	mph	speed	60	0	none.
mi/min	mi/s	speed	0.01666666666666667	0	none.
mi/min	mm/d	speed	2317455360	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
mi/min	mm/hr	speed	96560640	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mi/min	mm/min	speed	1609344	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mi/min	mm/s	speed	26822.400000000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mi/s	cm/d	speed	1390473.216	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
mi/s	cm/hr	speed	579363840	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mi/s	cm/min	speed	9656064	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mi/s	cm/s	speed	160934.39999999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mi/s	ft/d	speed	456192000	0	none: (24 hr / day); none: (5280.0 ft / mi)
mi/s	ft/hr	speed	19008000	0	none: (5280.0 ft / mi)
mi/s	ft/min	speed	316800	0	none: (5280.0 ft / mi)
mi/s	ft/s	speed	5280	0	none: (5280.0 ft / mi)
mi/s	in/d	speed	5474304000	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
mi/s	in/hr	speed	228096000	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mi/s	in/min	speed	3801600	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mm/s	mm/hr	speed	3600	0	none.
mi/s	in/s	speed	63360	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mi/s	km/d	speed	139047.3216	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
mi/s	km/hr	speed	5793.6383999999998	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mi/s	km/min	speed	96.560640000000006	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mi/s	km/s	speed	1.6093440000000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mi/s	m/d	speed	139047321.59999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
mi/s	m/hr	speed	5793638.4000000004	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mi/s	m/min	speed	96560.639999999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mi/s	m/s	speed	1609.3440000000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mi/s	mi/d	speed	86400	0	none: (24 hr / day)
mi/s	mph	speed	3600	0	none.
mi/s	mi/min	speed	60	0	none.
mi/s	mm/d	speed	139047321600	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
mi/s	mm/hr	speed	5793638400	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mi/s	mm/min	speed	96560640	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mi/s	mm/s	speed	1609344	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mm/d	cm/d	speed	0.10000000000000001	0	none.
mm/d	cm/hr	speed	0.0041666666666666666	0	none: (24 hr / day)
mm/d	cm/min	speed	6.9444444444444444e-05	0	none: (24 hr / day)
mm/d	cm/s	speed	1.157407407407407e-06	0	none: (24 hr / day)
mm/d	ft/d	speed	0.0032808398950000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
mm/d	ft/hr	speed	0.0001367016623	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
mm/d	ft/min	speed	2.278361038e-06	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
mm/d	ft/s	speed	3.7972683970000003e-08	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
mm/d	in/d	speed	0.039370078740000002	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
mm/d	in/hr	speed	0.0016404199480000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
mm/d	in/min	speed	2.7340332459999999e-05	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
mm/d	in/s	speed	4.5567220759999999e-07	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
mm/d	km/d	speed	9.9999999999999995e-07	0	none.
mm/d	km/hr	speed	4.1666666666666669e-08	0	none: (24 hr / day)
mm/d	km/min	speed	6.9444444444444439e-10	0	none: (24 hr / day)
mm/d	km/s	speed	1.157407407407407e-11	0	none: (24 hr / day)
mm/d	m/d	speed	0.001	0	none.
mm/d	m/hr	speed	4.1666666666666672e-05	0	none: (24 hr / day)
mm/d	m/min	speed	6.9444444444444437e-07	0	none: (24 hr / day)
mm/d	m/s	speed	1.1574074074074071e-08	0	none: (24 hr / day)
mm/d	mi/d	speed	6.2137119220000004e-07	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mm/d	mph	speed	2.589046634e-08	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
mm/d	mi/min	speed	4.3150777240000001e-10	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
mm/d	mi/s	speed	7.1917962060000002e-12	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
mm/d	mm/hr	speed	0.041666666666666671	0	none: (24 hr / day)
mm/d	mm/min	speed	0.00069444444444444436	0	none: (24 hr / day)
mm/d	mm/s	speed	1.157407407407407e-05	0	none: (24 hr / day)
mm/hr	cm/d	speed	2.3999999999999999	0	none: (24 hr / day)
mm/hr	cm/hr	speed	0.10000000000000001	0	none.
mm/hr	cm/min	speed	0.001666666666666667	0	none.
mm/hr	cm/s	speed	2.7777777777777779e-05	0	none.
mm/hr	ft/d	speed	0.078740157480000003	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
mm/hr	ft/hr	speed	0.0032808398950000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
mm/hr	ft/min	speed	5.4680664919999999e-05	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
mm/hr	ft/s	speed	9.1134441530000005e-07	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
mm/hr	in/d	speed	0.9448818898000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
mm/hr	in/hr	speed	0.039370078740000002	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
mm/hr	in/min	speed	0.00065616797900000003	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
mm/hr	in/s	speed	1.0936132980000001e-05	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
mm/hr	km/d	speed	2.4000000000000001e-05	0	none: (24 hr / day)
mm/hr	km/hr	speed	9.9999999999999995e-07	0	none.
mm/hr	km/min	speed	1.666666666666667e-08	0	none.
mm/hr	km/s	speed	2.7777777777777782e-10	0	none.
mm/hr	m/d	speed	0.024	0	none: (24 hr / day)
mm/hr	m/hr	speed	0.001	0	none.
mm/hr	m/min	speed	1.6666666666666671e-05	0	none.
mm/hr	m/s	speed	2.7777777777777781e-07	0	none.
mm/hr	mi/d	speed	1.4912908610000001e-05	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
mm/hr	mph	speed	6.2137119220000004e-07	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mm/hr	mi/min	speed	1.0356186539999999e-08	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mm/hr	mi/s	speed	1.7260310900000001e-10	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mm/hr	mm/d	speed	24	0	none: (24 hr / day)
mm/hr	mm/min	speed	0.01666666666666667	0	none.
mm/hr	mm/s	speed	0.00027777777777777778	0	none.
mm/min	cm/d	speed	144	0	none: (24 hr / day)
mm/min	cm/hr	speed	6	0	none.
mm/min	cm/min	speed	0.10000000000000001	0	none.
mm/min	cm/s	speed	0.001666666666666667	0	none.
mm/min	ft/d	speed	4.7244094489999986	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
mm/min	ft/hr	speed	0.19685039369999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
mm/min	ft/min	speed	0.0032808398950000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
mm/min	ft/s	speed	5.4680664919999999e-05	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
mm/min	in/d	speed	56.692913390000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
mm/min	in/hr	speed	2.3622047240000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
mm/min	in/min	speed	0.039370078740000002	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
mm/min	in/s	speed	0.00065616797900000003	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
mm/min	km/d	speed	0.0014400000000000001	0	none: (24 hr / day)
mm/min	km/hr	speed	5.9999999999999988e-05	0	none.
mm/min	km/min	speed	9.9999999999999995e-07	0	none.
mm/min	km/s	speed	1.666666666666667e-08	0	none.
mm/min	m/d	speed	1.4399999999999999	0	none: (24 hr / day)
mm/min	m/hr	speed	0.059999999999999998	0	none.
mm/min	m/min	speed	0.001	0	none.
mm/min	m/s	speed	1.6666666666666671e-05	0	none.
mm/min	mi/d	speed	0.00089477451680000002	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
mm/min	mph	speed	3.7282271530000002e-05	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mm/min	mi/min	speed	6.2137119220000004e-07	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mm/min	mi/s	speed	1.0356186539999999e-08	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mm/min	mm/d	speed	1440	0	none: (24 hr / day)
mm/min	mm/hr	speed	60	0	none.
mm/min	mm/s	speed	0.01666666666666667	0	none.
mm/s	cm/d	speed	8640	0	none: (24 hr / day)
mm/s	cm/hr	speed	360	0	none.
mm/s	cm/min	speed	6	0	none.
mm/s	cm/s	speed	0.10000000000000001	0	none.
mm/s	ft/d	speed	283.46456690000002	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
mm/s	ft/hr	speed	11.81102362	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
mm/s	ft/min	speed	0.19685039369999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
mm/s	ft/s	speed	0.0032808398950000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
mm/s	in/d	speed	3401.574803	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day)
mm/s	in/hr	speed	141.73228349999999	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
mm/s	in/min	speed	2.3622047240000001	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
mm/s	in/s	speed	0.039370078740000002	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch)
mm/s	km/d	speed	0.086400000000000005	0	none: (24 hr / day)
mm/s	km/hr	speed	0.0035999999999999999	0	none.
mm/s	km/min	speed	5.9999999999999988e-05	0	none.
mm/s	km/s	speed	9.9999999999999995e-07	0	none.
mm/s	m/d	speed	86.40000000000002	0	none: (24 hr / day)
mm/s	m/hr	speed	3.6000000000000001	0	none.
mm/s	m/min	speed	0.059999999999999998	0	none.
mm/s	m/s	speed	0.001	0	none.
mm/s	mi/d	speed	0.05368647101	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (24 hr / day); none: (5280.0 ft / mi)
mm/s	mph	speed	0.0022369362919999998	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mm/s	mi/min	speed	3.7282271530000002e-05	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mm/s	mi/s	speed	6.2137119220000004e-07	0	CRC Handbook of Chemistry and Physics, 76th ed.: (2.54 cm / inch); none: (5280.0 ft / mi)
mm/s	mm/d	speed	86400	0	none: (24 hr / day)
mm/s	mm/min	speed	60	0	none.
fur/fort	kt	speed	0.00032330571429999998	0	file "/usr/share/lib/unittab" used by UNIX command "units": (220.0 yards / furlong, 14.0 days / fortnight; none: (24 hr / day); (see above for in->cm, kt->m/s)
fur/fort	m/s	speed	0.00016630952379999999	0	file "/usr/share/lib/unittab" used by UNIX command "units": (220.0 yards / furlong, 14.0 days / fortnight; none: (24 hr / day); (see above for in->cm)
scruple	kg	mass	0.0012959782	0	file "/usr/share/lib/unittab" used by UNIX command "units": (20.0 grains / scruple, 7000.0 grains / pound);  CRC Handbook of Chemistry and Physics, 76th ed.: (0.45359237 kg / pound)
\.


