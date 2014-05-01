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
-- Data for Name: hydromet_element; Type: TABLE DATA; Schema: public; Owner: pguser
--

COPY hydromet_element (element_id, bufr_descriptor, element_name, element_descrip, element_subtype, phys_element_id, duration_type, aggregation, product_name, source_process, revision_number) FROM stdin;
25739	3272	MaxTemp	NGM MOS daytime maximum temperature forecast in degrees Fahrenheit	Contin_Int_Ele	Temp	Matches_Valid_Period	Maximum	AEV_DATA	AWIPS_VERIF	0
25740	3273	MinTemp	NGM MOS nighttime minimum temperature forecast in degrees Fahrenheit	Contin_Int_Ele	Temp	Matches_Valid_Period	Minimum	AEV_DATA	AWIPS_VERIF	0
25741	15362	PoP	NGM MOS probability of precipitation >=.01"	Contin_Int_Ele	Prob	Matches_valid_Period	None	AEV_DATA	AWIPS_VERIF	0
25742	15451	PrecipType	NGM MOS Precipitation type categorical forecast	Categorical_Ele	Precip Type	Instantaneous	None	AEV_DATA	AWIPS_VERIF	0
25743	15453	SnowAccumulation	NGM MOS snow accumulation categorical forecast	Categorical_Ele	Depth	Matches_Valid_Period	None	AEV_DATA	AWIPS_VERIF	0
25744	5131	CloudAmount	NGM MOS sky cover categorical forecast	Categorical_Ele	Sky Cover	Instantaneous	None	AEV_DATA	AWIPS_VERIF	0
25745	15454	Ceiling	NGM MOS ceiling height categorical forecast	Categorical_Ele	Dist	Instantaneous	None	AEV_DATA	AWIPS_VERIF	0
25746	15455	Visibility	NGM MOS horizontal visibility categorical forecast	Categorical_Ele	Dist	Instantaneous	None	AEV_DATA	AWIPS_VERIF	0
25747	3016	WindSpeed	NGM MOS wind speed forecast in knots	Contin_Int_Ele	Speed	Instantaneous	None	AEV_DATA	AWIPS_VERIF	0
25748	2817	WindDirection	NGM MOS wind direction forecast in degrees from true north	Discrete_Ele	Dir	Instantaneous	None	AEV_DATA	AWIPS_VERIF	0
25756	15454	CloudHeight	TAF cloud height forecast in 100's of feet	Discrete_Ele	Dist	Instantaneous	None	TAF_HMDB	TDL_TAF_DEC_4.1	0
25757	15455	Visibility	TAF horizontal visibility forecast in statute miles	Discrete_Ele	Dist	Instantaneous	Minimum	TAF_HMDB	TDL_TAF_DEC_4.1	0
25758	2818	WindSpeed	TAF wind speed forecast in knots	Contin_Int_Ele	Speed	Instantaneous	None	TAF_HMDB	TDL_TAF_DEC_4.1	0
25759	2817	WindDirection	TAF wind direction forecast in 10's of degrees from true north	Discrete_Ele	Dir	Instantaneous	None	TAF_HMDB	TDL_TAF_DEC_4.1	0
25760	3073	Temperature	METAR observed temperature in degrees Celcius	Contin_Real_Ele	Temp	Instantaneous	None	MTR	TDL_MET_DEC_3.0	0
25763	3347	PrecipAmount1	METAR 1hr observed liquid equivalent precipitation, inches	Contin_Real_Ele	Depth	Matches_Valid_Period	None	MTR	TDL_MET_DEC_3.0	0
25764	3348	PrecipAmount3	METAR 3hr observed liquid equivalent precipitation, inches	Contin_Real_Ele	Depth	Matches_Valid_Period	None	MTR	TDL_MET_DEC_3.0	0
25765	3349	PrecipAmount6	METAR 6hr observed liquid equivalent precipitation, inches	Contin_Real_Ele	Depth	Matches_Valid_Period	None	MTR	TDL_MET_DEC_3.0	0
25766	3351	PrecipAmount24	METAR 24hr observed liquid equivalent precipitation, inches	Contin_Real_Ele	Depth	Matches_Valid_Period	None	MTR	TDL_MET_DEC_3.0	0
25767	5123	CurrentWeather	METAR observed weather - METAR table	Boolean_Element	Weather	Instantaneous	None	MTR	TDL_MET_DEC_3.0	0
25823	\N	Temp_in_tenths	METAR observed temperature in tenths of degrees Celcius	Contin_Real_Ele	Temp	Instantaneous	None	MTR	TDL_MET_DEC_3.0	0
25769	2818	WindSpeed	METAR observed wind speed in knots	Contin_Real_Ele	Speed	Instantaneous	None	MTR	TDL_MET_DEC_3.0	0
25770	2817	WindDirection	METAR observed wind direction in degrees from true north	Discrete_Ele	Dir	Instantaneous	None	MTR	TDL_MET_DEC_3.0	0
25771	5171	CloudAmount	METAR observed low cloud coverage (categorical)	Categorical_Ele	Sky Cover	Instantaneous	None	MTR	TDL_MET_DEC_3.0	0
25772	5172	CloudAmount	METAR observed middle cloud coverage (categorical)	Categorical_Ele	Sky Cover	Instantaneous	None	MTR	TDL_MET_DEC_3.0	0
25773	5173	CloudAmount	METAR observed high cloud coverage (categorical)	Categorical_Ele	Sky Cover	Instantaneous	None	MTR	TDL_MET_DEC_3.0	0
25774	5133	CloudHeight	METAR observed cloud layer height in feet	Discrete_Ele	Dist	Instantaneous	None	MTR	TDL_MET_DEC_3.0	0
25807	\N	Significant Weather	TAF significant weather forecast	Boolean_Element	Weather	Instantaneous	None	TAF_HMDB	TDL_TAF_DEC_4.1	0
25776	3341	SnowDepth	METAR observed depth of the snow on the ground in inches	Contin_Real_Ele	Depth	Instantaneous	None	MTR	TDL_MET_DEC_3.0	0
25777	3083	MaxTemp	AEV local forecast maximum temperature in degrees Fahrenheit	Contin_Int_Ele	Temp	Matches_Valid_Period	Maximum	AEV_DATA	AWIPS_VERIF	0
25778	3084	MinTemp	AEV local forecast minimum temperature in degrees Fahrenheit	Contin_Int_Ele	Temp	Matches_Valid_Period	Minimum	AEV_DATA	AWIPS_VERIF	0
25779	15362	PoP	AEV local forecast probability of precipitation >=.01"	Discrete_Ele	Depth	Matches_Valid_Period	None	AEV_DATA	AWIPS_VERIF	0
25780	15451	PrecipType	AEV local forecast precipitation type category	Categorical_Ele	Precip Type	 Matches_Valid_Period	None	AEV_DATA	AWIPS_VERIF	0
25781	15453	SnowAccumulation	AEV local forecast snow accumulation category	Contin_Int_Ele	Depth	Matches_Valid_Period	None	AEV_DATA	AWIPS_VERIF	0
25782	5131	CloudAmount	AEV local forecast cloud coverage category	Categorical_Ele	Sky Cover	Instantaneous	None	AEV_DATA	AWIPS_VERIF	0
25783	2818	WindSpeed	AEV local forecast significant wind speed	Contin_Int_Ele	Speed	Instantaneous	None	AEV_DATA	AWIPS_VERIF	0
25784	15454	Ceiling	AEV ceiling height local forecast in 100's of feet	Discrete_Ele	Dist	Instantaneous	None	AEV_DATA	AWIPS_VERIF	0
25785	15455	Visibility	AEV horizontal visibility local forecast in statute miles	Discrete_Ele	Dist	Instantaneous	Minimum	AEV_DATA	AWIPS_VERIF	0
25786	2818	WindSpeed	AEV wind speed local forecast in knots	Contin_Int_Ele	Speed	Matches_Valid_Period	Maximum	AEV_DATA	AWIPS_VERIF	0
25787	2817	WindDirection	AEV wind direction local forecast in degrees from true north	Discrete_Ele	Dir	Matches_Valid_Period	None	AEV_DATA	AWIPS_VERIF	0
25788	5121	Visibility	METAR manually observed horizontal visibility in statute miles	Discrete_Ele	Dist	Instantaneous	Minimum	MTR	TDL_MET_DEC_3.0	0
25790	\N	CloudAmount	TAF Categorical sky cover forecast	Categorical_Ele	Sky Cover	Instantaneous	None	TAF_HMDB	TDL_TAF_DEC_4.1	0
25791	\N	ShearHeight	TAF wind shear height forecast	Discrete_Ele	Dist	Instantaneous	None	TAF_HMDB	TDL_TAF_DEC_4.1	0
25792	\N	ShearDirection	TAF forecast of wind direction at the wind shear height	Discrete_Ele	Dir	Instantaneous	None	TAF_HMDB	TDL_TAF_DEC_4.1	0
25793	\N	ShearSpeed	TAF forecast of wind speed at the wind shear height	Contin_Int_Ele	Speed	Instantaneous	None	TAF_HMDB	TDL_TAF_DEC_4.1	0
25794	\N	WindGust	TAF wind gust forecast	Contin_Int_Ele	Speed	Matches_Valid_Period	Maximum	TAF_HMDB	TDL_TAF_DEC_4.1	0
25795	\N	Visibility	METAR observed vertical visibility	Discrete_Ele	Dist	Instantaneous	Minimum	MTR	TDL_MET_DEC_3.0	0
25796	\N	Pressure	METAR observed MSL pressure	Contin_Real_Ele	Pres	Instantaneous	None	MTR	TDL_MET_DEC_3.0	0
25797	\N	Altimeter	METAR observed altimeter setting	Contin_Real_Ele	Pres	Instantaneous	None	MTR	TDL_MET_DEC_3.0	0
25798	\N	PressureChange	METAR observed 3hr pressure change	Contin_Real_Ele	Pres	Matches_Valid_Period	None	MTR	TDL_MET_DEC_3.0	0
25799	\N	PressureTrend	METAR observed 3hr pressure tendency	Categorical_Ele	Tend	Matches_Valid_Period	None	MTR	TDL_MET_DEC_3.0	0
25800	\N	DewPoint	METAR observed dewpoint temperature	Contin_Real_Ele	Temp	Instantaneous	None	MTR	TDL_MET_DEC_3.0	0
25801	\N	Max6hr	METAR observed 6 hour maximum temperature	Contin_Real_Ele	Temp	Matches_Valid_Period	Maximum	MTR	TDL_MET_DEC_3.0	0
25802	\N	Min6hr	METAR observed 6 hour minimum temperature	Contin_Real_Ele	Temp	Matches_Valid_Period	Minimum	MTR	TDL_MET_DEC_3.0	0
25803	\N	Max24hr	METAR observed 24 hour maximum temperature	Contin_Real_Ele	Temp	Matches_Valid_Period	Maximum	MTR	TDL_MET_DEC_3.0	0
25804	\N	Min24hr	METAR observed 24 hour minimum temperature	Contin_Real_Ele	Temp	Matches_Valid_Period	Minimum	MTR	TDL_MET_DEC_3.0	0
25805	\N	WindGust	METAR observed maximum wind gust	Contin_Real_Ele	Speed	Matches_Valid_Period	Maximum	MTR	TDL_MET_DEC_3.0	0
25806	\N	CloudType	METAR observed cloud type	Boolean_Element	Cloud Type	Instantaneous	None	MTR	TDL_MET_DEC_3.0	0
25808	\N	CloudType	TAF cloud type forecast	Boolean_Element	Cloud Type	Instantaneous	None	TAF_HMDB	TDL_TAF_DEC_4.1	0
25809	\N	MaxTemp	AEV local observed maximum temperature in degrees Fahrenheit	Contin_Int_Ele	Temp	Matches_Valid_Period	Maximum	AEV_DATA	AWIPS_VERIF	0
25810	\N	MinTemp	AEV local observed minimum temperature in degrees Fahrenheit	Contin_Int_Ele	Temp	Matches_Valid_Period	Minimum	AEV_DATA	AWIPS_VERIF	0
25811	\N	PrecipAmount12	AEV local observed 12 hour precipitation amount	Contin_Int_Ele	Depth	Matches_Valid_Period	None	AEV_DATA	AWIPS_VERIF	0
25812	\N	AEVPrecipTypes	AEV local observed precipitation type on the hour	Categorical_Ele	Precip Type	Instantaneous	None	AEV_DATA	AWIPS_VERIF	0
25813	\N	AEVPrecipTypes2	AEV local observed precipitation type in a 2 hour window	Categorical_Ele	Precip Type	Matches_Valid_Period	None	AEV_DATA	AWIPS_VERIF	0
25814	\N	SnowAmount12	AEV local observed 12 hour snow accumulation	Contin_Int_Ele	Depth	Matches_Valid_Period	None	AEV_DATA	AWIPS_VERIF	0
25815	\N	CeilingHeight	AEV local observed ceiling height	Discrete_Ele	Dist	Instantaneous	None	AEV_DATA	AWIPS_VERIF	0
25816	\N	Visibility	AEV local observed visibility	Discrete_Ele	Dist	Instantaneous	Minimum	AEV_DATA	AWIPS_VERIF	0
25817	\N	WindDirection	AEV local observed wind direction on the hour	Discrete_Ele	Dir	Instantaneous	None	AEV_DATA	AWIPS_VERIF	0
25818	\N	WindSpeed	AEV local observed wind speed on the hour	Contin_Int_Ele	Speed	Instantaneous	None	AEV_DATA	AWIPS_VERIF	0
25819	\N	PeakWindDirection	AEV local observed peak wind direction in a 6 hour window	Discrete_Ele	Dir	Matches_Valid_Period	None	AEV_DATA	AWIPS_VERIF	0
25820	\N	PeakWindSpeed	AEV local observed peak wind speed in a 6 hour window	Contin_Int_Ele	Speed	Matches_Valid_Period	Maximum	AEV_DATA	AWIPS_VERIF	0
25822	\N	SnowAmount	SCD observed snow amount	Contin_Real_Ele	Depth	Matches_Valid_Period	None	SCD	TDL_SCD_DEC_4.2	0
25824	\N	Dewpoint_in_tenths	METAR observed dewpoint temperature in tenths of degrees Celcius	Contin_Real_Ele	Temp	Instantaneous	None	MTR	TDL_MET_DEC_3.0	0
25825	\N	CloudAmount	AEV local observed cloud amount	Categorical_Ele	Sky Cover	Instantaneous	None	AEV_DATA	AWIPS_VERIF	0
25826	\N	PeakWindSpeed	METAR observed peak wind speed	Contin_Real_Ele	Speed	Instantaneous	None	MTR	TDL_MET_DEC_3.0	0
25827	\N	PeakWindDirection	METAR observed peak wind direction	Discrete_Ele	Dir	Instantaneous	None	MTR	TDL_MET_DEC_3.0	0
25828	\N	PeakWindTime	METAR observed peak wind time, hours and minutes	Contin_Real_Ele	Clock_Time	Instantaneous	None	MTR	TDL_MET_DEC_3.0	0
25829	\N	SunshineDuration	METAR observed minutes of sunshine	Contin_Real_Ele	Sunshine	Calendar_Day	Sum	MTR	TDL_MET_DEC_3.0	0
25539	3272	MaxTemp	AVN MOS daytime maximum temperature forecast in degrees Fahrenheit	Contin_Int_Ele	Temp	Matches_Valid_Period	Maximum	AEV_DATA	AWIPS_VERIF	0
25540	3273	MinTemp	AVN MOS nighttime minimum temperature forecast in degrees Fahrenheit	Contin_Int_Ele	Temp	Matches_Valid_Period	Minimum	AEV_DATA	AWIPS_VERIF	0
25541	15362	PoP	AVN MOS probability of precipitation >=.01"	Contin_Int_Ele	Prob	Matches_valid_Period	None	AEV_DATA	AWIPS_VERIF	0
25542	15451	PrecipType	AVN MOS Precipitation type categorical forecast	Categorical_Ele	Precip Type	Instantaneous	None	AEV_DATA	AWIPS_VERIF	0
25543	15453	SnowAccumulation	AVN MOS snow accumulation categorical forecast	Categorical_Ele	Depth	Matches_Valid_Period	None	AEV_DATA	AWIPS_VERIF	0
25544	5131	CloudAmount	AVN MOS sky cover categorical forecast	Categorical_Ele	Sky Cover	Instantaneous	None	AEV_DATA	AWIPS_VERIF	0
25545	15454	Ceiling	AVN MOS ceiling height categorical forecast	Categorical_Ele	Dist	Instantaneous	None	AEV_DATA	AWIPS_VERIF	0
25546	15455	Visibility	AVN MOS horizontal visibility categorical forecast	Categorical_Ele	Dist	Instantaneous	None	AEV_DATA	AWIPS_VERIF	0
25547	3016	WindSpeed	AVN MOS wind speed forecast in knots	Contin_Int_Ele	Speed	Instantaneous	None	AEV_DATA	AWIPS_VERIF	0
25548	2817	WindDirection	AVN MOS wind direction forecast in degrees from true north	Discrete_Ele	Dir	Instantaneous	None	AEV_DATA	AWIPS_VERIF	0
25639	3272	MaxTemp	MRF MOS daytime maximum temperature forecast in degrees Fahrenheit	Contin_Int_Ele	Temp	Matches_Valid_Period	Maximum	AEV_DATA	AWIPS_VERIF	0
25640	3273	MinTemp	MRF MOS nighttime minimum temperature forecast in degrees Fahrenheit	Contin_Int_Ele	Temp	Matches_Valid_Period	Minimum	AEV_DATA	AWIPS_VERIF	0
25641	15362	PoP	MRF MOS probability of precipitation >=.01"	Contin_Int_Ele	Prob	Matches_valid_Period	None	AEV_DATA	AWIPS_VERIF	0
25642	15451	PrecipType	MRF MOS Precipitation type categorical forecast	Categorical_Ele	Precip Type	Instantaneous	None	AEV_DATA	AWIPS_VERIF	0
25643	15453	SnowAccumulation	MRF MOS snow accumulation categorical forecast	Categorical_Ele	Depth	Matches_Valid_Period	None	AEV_DATA	AWIPS_VERIF	0
25644	5131	CloudAmount	MRF MOS sky cover categorical forecast	Categorical_Ele	Sky Cover	Instantaneous	None	AEV_DATA	AWIPS_VERIF	0
25645	15454	Ceiling	MRF MOS ceiling height categorical forecast	Categorical_Ele	Dist	Instantaneous	None	AEV_DATA	AWIPS_VERIF	0
25646	15455	Visibility	MRF MOS horizontal visibility categorical forecast	Categorical_Ele	Dist	Instantaneous	None	AEV_DATA	AWIPS_VERIF	0
25647	3016	WindSpeed	MRF MOS wind speed forecast in knots	Contin_Int_Ele	Speed	Instantaneous	None	AEV_DATA	AWIPS_VERIF	0
25648	2817	WindDirection	MRF MOS wind direction forecast in degrees from true north	Discrete_Ele	Dir	Instantaneous	None	AEV_DATA	AWIPS_VERIF	0
\.


