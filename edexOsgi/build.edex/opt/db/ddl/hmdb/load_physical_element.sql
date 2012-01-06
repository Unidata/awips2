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
-- Data for Name: physical_element; Type: TABLE DATA; Schema: public; Owner: pguser
--

COPY physical_element (phys_element_id, phys_element_name, phys_element_descr, phys_ele_subtype, units_class, relationships) FROM stdin;
Temp	Temperature	\N	Measurable	temperature	\N
Prob	Probability	\N	Measurable	probability	\N
Precip Type	Precipitation Type	\N	Phenomenological	dimensionless	\N
Depth	Depth	\N	Measurable	length	\N
Sky Cover	Sky cover	\N	Measurable	probability	\N
Dist	Distance	\N	Measurable	length	\N
Speed	Speed	\N	Measurable	speed	\N
Dir	Direction	\N	Measurable	angle	\N
Weather	Weather	 	Phenomenological	dimensionless	\N
Tend	Tendency	\N	Measurable	pressure	\N
Pres	Pressure	\N	Measurable	pressure	\N
Cloud Type	Cloud Type	\N	Phenomenological	dimensionless	\N
Sunshine	Duration of Sunshine	\N	Measurable	time	0
Clock_Time	Clock time, hours and minutes	24-hour clock time in hhmm format	Measurable	time_of_day	0
\.


