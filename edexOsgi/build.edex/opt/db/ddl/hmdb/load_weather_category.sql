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
-- Data for Name: weather_category; Type: TABLE DATA; Schema: public; Owner: pguser
--

COPY weather_category (weather_category, wx_cat_descrip) FROM stdin;
Precip	Precipitation without thunder
Thunder	Thunder or precipitation with thunder
Obscur	Obscuration to visibility at station
Severe	Severe weather at station
Unk_Precip	Unknown precipitation in the vicinity of the station
Obscr_Nearby	Obscurations to visibility in the vicinity of the station
Sev_Wx_Nearby	Severe weather in the vicinity of the station
\.


