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
-- Data for Name: categorical_ele; Type: TABLE DATA; Schema: public; Owner: pguser
--

COPY categorical_ele (element_id, num_ranges, max_cat_value, min_cat_value, misg_cat_value, text_flag, range_value_units, scale_factor) FROM stdin;
25743	5	5	0	-9999	N	in	0.1
25744	4	4	1	-9999	Y	tenths_of_sky	1
25745	7	7	1	-9999	N	ft	1
25746	5	5	1	-9999	N	mi	0.125
25782	4	3	0	-9999	Y	tenths_of_sky	1
25771	7	11	0	-9999	Y	eighths_of_sky	1
25790	6	5	0	-9999	Y	eighths_of_sky	1
25799	9	8	0	-9999	N	mb	0.0099999998
25742	3	3	1	-9999	Y	none	0
25780	3	3	1	-9999	Y	none	0
25812	7	123	1	-9999	Y	none	0
25813	7	123	1	-9999	Y	none	0
25825	4	4	1	-9999	Y	tenths_of_sky	1
25542	3	3	1	-9999	Y	none	0
25642	3	3	1	-9999	Y	none	0
25543	5	5	0	-9999	N	in	0.1
25643	5	5	0	-9999	N	in	0.1
25544	4	4	1	-9999	Y	tenths_of_sky	1
25545	7	7	1	-9999	N	ft	1
25546	5	5	1	-9999	N	mi	0.125
\.


