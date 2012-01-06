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
-- Data for Name: time_zone; Type: TABLE DATA; Schema: public; Owner: pguser
--

COPY time_zone (time_zone_id, time_zone_name, time_zone_descrip, local_minus_utc, daylight_saving) FROM stdin;
UT	UTC	Universal Coordinated Time	00:00:00	N
ES	EST	Eastern Standard Time	-05:00:00	N
ED	EDT	Eastern Daylight Time	-04:00:00	Y
CS	CST	Central Standard Time	-06:00:00	N
CD	CDT	Central Daylight Time	-05:00:00	Y
MS	MST	Mountain Standard Time	-07:00:00	N
MD	MDT	Mountain Daylight Time	-06:00:00	Y
PS	PST	Pacific Standard Time	-08:00:00	N
PD	PDT	Pacific Daylight Time	-07:00:00	Y
AS	AST	Alaska Standard Time	-09:00:00	N
AD	ADT	Alaska Daylight Time	-08:00:00	Y
HS	HST	Hawaii Standard Time	-10:00:00	N
HD	HDT	Hawaii Daylight Time	-09:00:00	Y
GS	GST	Guam Standard Time	00:00:11	N
GD	GDT	Guam Daylight Time	\N	Y
\.


