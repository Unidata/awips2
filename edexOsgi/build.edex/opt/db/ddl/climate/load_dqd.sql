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
-- Data for Name: dqd; Type: TABLE DATA; Schema: public; Owner: pguser
--

COPY dqd (dqd, dqd_description, dqd_method, dqd_hierarchy) FROM stdin;
Z	Preliminary	No QC Applied	1
C	Coarse Pass	Passed Stage 1	2
X	Erroneous	Failed Stage 1	0
I	Incomplete	Partial Integration	1
S	Single-Site Pass	Passed Stages 1-2	3
V	Spatial Pass	Passed Stages 1-3	4
Q	Questionable	Passed 1 Failed 2or3	2
K	Kalman Pass	Passed Stages 1-4	5
k	Kalman Fail	Passed 1-3, Failed 4	4
G	Subjective Good	In ACCEPT list	6
B	Subjective Bad	In REJECT list	0
F	Flagged Externally	Flagged by Sensor	0
D	Decoder Error	Error in decoding	0
R	Erroneous	Failed Stage 1	0
W	Wrong/Replaced	Edited or Replaced	6
E	Estimated	Estimated Value	3
\.


