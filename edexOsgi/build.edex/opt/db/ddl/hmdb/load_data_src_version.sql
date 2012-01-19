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
-- Data for Name: data_src_version; Type: TABLE DATA; Schema: public; Owner: pguser
--

COPY data_src_version (source_version_id, source_process, version_number, effective_date, supersede_date) FROM stdin;
100	LAMP_FILE_SYSTEM	0	1998-06-05	\N
200	WCCF	0	1998-06-05	\N
300	TDL_TAF_DEC_4.1	0	1998-06-05	\N
400	TDL_MET_DEC_3.0	0	1998-06-05	\N
500	AWIPS_VERIF	0	1998-06-05	\N
600	TDL_SCD_DEC_4.2	0	1998-06-05	\N
700	NETCDF_BUFR_MOS	0	\N	\N
\.


