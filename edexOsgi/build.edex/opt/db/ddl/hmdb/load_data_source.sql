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
-- Data for Name: data_source; Type: TABLE DATA; Schema: public; Owner: pguser
--

COPY data_source (source_process, src_process_id, src_descrip) FROM stdin;
LAMP_FILE_SYSTEM	1	NGM MOS data stored in the TDL file system
WCCF	2	Constructs the MEF_CCF forecast from data derived from AWIPS
TDL_TAF_DEC_4.1	3	Parses and decodes a terminal forecast
TDL_MET_DEC_3.0	4	Parses and decodes a METAR report
AWIPS_VERIF	5	Porduces a matchup of the forecasts and their verifying observations
TDL_SCD_DEC_4.2	6	Parses and decodes a SCD report
NETCDF_BUFR_MOS	7	Decoded BUFR MOS data stored in netCDF files
\.


