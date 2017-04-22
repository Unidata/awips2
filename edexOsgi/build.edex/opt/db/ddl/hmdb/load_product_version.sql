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
-- Data for Name: product_version; Type: TABLE DATA; Schema: public; Owner: pguser
--

COPY product_version (prod_version, product_name, version_number, version_descrip, effective_date, supersede_date, current_flag) FROM stdin;
110	MTR	0	Original AWIPS version.	\N	\N	Y
120	METAR_NETCDF	0	Original AWIPS version.	\N	\N	Y
140	METAR_HMDB	0	Original AWIPS version.	\N	\N	Y
210	SCP	0	Original AWIPS version.	\N	\N	Y
310	SDO	0	Original AWIPS version.	\N	\N	Y
410	SCD	0	Original AWIPS version.	\N	\N	Y
520	BUOY_NETCDF	0	Original AWIPS version.	\N	\N	Y
10110	TAF	0	Original AWIPS version.	\N	\N	Y
10120	TAF_HMDB	0	Original AWIPS version.	\N	\N	Y
10210	MOS_NGM_BUFR	0	Original AWIPS version.	\N	\N	Y
20110	AEV_DATA	0	Original AWIPS version.	\N	2002-06-20	N
20210	IFP_STA_DFM	4	Original AWIPS version.	\N	2001-04-09	N
20310	AEV_STA_DFM	0	Original AWIPS version.	\N	\N	Y
20211	IFP_STA_DFM	5	First IFPS Version	2001-04-09	2002-11-05	N
10220	MOS_AVN_BUFR	0	Original 5.2.1 version.	\N	\N	Y
10230	MOS_MRF_BUFR	0	Original 5.2.1 version.	\N	\N	Y
20111	AEV_DATA	1	AWIPS 5.2.1 version.	\N	\N	Y
20212	IFP_STA_DFM	10	IFPS-10 on Linux	2002-11-05	\N	Y
\.


