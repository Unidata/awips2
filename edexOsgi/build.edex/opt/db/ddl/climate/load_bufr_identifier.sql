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
-- Data for Name: bufr_identifier; Type: TABLE DATA; Schema: public; Owner: pguser
--

COPY bufr_identifier (bufr_descriptor, bufr_f, bufr_x, bufr_y, bufr_name) FROM stdin;
3272	0	12	200	Daytime_Max_Temperature
3273	0	12	201	Nighttime_Min_Temperature
15362	0	60	2	Prob. of Liquid equivalent precip >= 0.01 inch during the past 
15451	0	60	91	Conditional Precip. Type Fcst. (categories)
15453	0	60	93	Quant. Snow Amount Fcst. (categories)
5131	0	20	11	Cloud Amount
15454	0	60	94	Ceiling Height Fcst. (categories)
15455	0	60	95	Visibility Fcst. (categories)
3016	0	11	200	Inflated wind speed (MOS)
2817	0	11	1	Wind Direction
2818	0	11	2	Wind Speed
3073	0	12	1	Temperature/dry-bulb temperature
3083	0	12	11	Maximum temperature at height and over period specified
3084	0	12	12	Minimum temperature at height and over period specified
3347	0	13	19	Total precipitation past 1 hour
3348	0	13	20	Total precipitation past 3 hours
3349	0	13	21	Total precipitation past 6 hours
3351	0	13	23	Total precipitation past 24 hours
5123	0	20	3	Present Weather
5171	0	20	51	Amount of low clouds
5172	0	20	52	Amount of middle clouds
5173	0	20	53	Amount of high clouds
5133	0	20	13	Height of base of cloud
5121	0	20	1	Horizontal visibility
3341	0	13	13	Total snow depth
\.


