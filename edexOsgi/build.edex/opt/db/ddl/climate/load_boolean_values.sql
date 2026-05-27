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
-- Data for Name: boolean_values; Type: TABLE DATA; Schema: public; Owner: pguser
--

COPY boolean_values (element_id, element_value, weather_type, varchar_of_wx, intensity_of_wx, wx_full_string, wx_type_substr, varchar_wx_substr, ints_wx_substr, weather_category, precip_state, occurring_flag, misg_indicator) FROM stdin;
25767	1	Drizzle	\N	Light	-DZ	DZ	\N	-	Precip	R	\N	\N
25767	2	Rain	\N	Light	-RA	RA	\N	-	Precip	R	\N	\N
25767	3	Rain	Shower              	Light	-SHRA	RA	SH   	-	Precip	R	\N	\N
25767	4	Snow	\N	Light	-SN	SN	\N	-	Precip	S	\N	\N
25767	5	Snow Grains	\N	Light	-SG	SG	\N	-	Precip	S	\N	\N
25767	6	Ice Pellets	\N	Light	-PL	PL	\N	-	Precip	S	\N	\N
25767	7	Snow	Shower              	Light	-SHSN	SN	SH   	-	Precip	S	\N	\N
25767	8	Ice Pellets	Shower              	Light	-SHPL	PL	SH   	-	Precip	S	\N	\N
25767	9	Drizzle	Freezing            	Light	-FZDZ	DZ	FZ   	-	Precip	Z	\N	\N
25767	10	Rain	Freezing            	Light	-FZRA	RA	FZ   	-	Precip	Z	\N	\N
25767	11	Drizzle	\N	Moderate	DZ	DZ	\N	\N	Precip	R	\N	\N
25767	12	Rain	\N	Moderate	RA	RA	\N	\N	Precip	R	\N	\N
25767	13	Rain	Shower              	Moderate	SHRA	RA	SH   	\N	Precip	R	\N	\N
25767	14	Snow	\N	Moderate	SN	SN	\N	\N	Precip	S	\N	\N
25767	15	Snow Grains	\N	Moderate	SG	SG	\N	\N	Precip	S	\N	\N
25767	16	Ice Pellets	\N	Moderate	PL	PL	\N	\N	Precip	S	\N	\N
25767	17	Snow	Shower              	Moderate	SHSN	SN	SH   	\N	Precip	S	\N	\N
25767	18	Ice Pellet	Shower              	Moderate	SHPL	PL	SH   	\N	Precip	S	\N	\N
25767	19	Drizzle	Freezing            	Moderate	FZDZ	DZ	FZ   	\N	Precip	Z	\N	\N
25767	20	Rain	Freezing            	Moderate	FZRA	RA	FZ   	\N	Precip	Z	\N	\N
25767	21	Drizzle	\N	Heavy	+DZ	DZ	\N	+	Precip	R	\N	\N
25767	22	Rain	\N	Heavy	+RA	RA	\N	+	Precip	R	\N	\N
25767	23	Rain	Shower              	Heavy	+SHRA	RA	SH   	+	Precip	R	\N	\N
25767	24	Snow	\N	Heavy	+SN	SN	\N	+	Precip	S	\N	\N
25767	25	Snow Grains	\N	Heavy	+SG	SG	\N	+	Precip	S	\N	\N
25767	26	Ice Pellets	\N	Heavy	+PL	PL	\N	+	Precip	S	\N	\N
25767	27	Snow	Shower              	Heavy	+SHSN	SN	SH   	+	Precip	S	\N	\N
25767	28	Ice Pellets	Shower              	Heavy	+SHPL	PL	SH   	+	Precip	S	\N	\N
25767	29	Drizzle	Freezing            	Heavy	+FZDZ	DZ	FZ   	+	Precip	Z	\N	\N
25767	30	Rain	Freezing            	Heavy	+FZRA	RA	FZ   	+	Precip	Z	\N	\N
25767	31	Unknown Precip	\N	\N	UP	UP	\N	\N	Precip	\N	\N	\N
25767	32	Ice Crystals	\N	\N	IC	IC	\N	\N	Precip	S	\N	\N
25767	33	Hail	\N	\N	GR	GR	\N	\N	Precip	S	\N	\N
25767	34	Small Hail	\N	\N	GS	GS	\N	\N	Precip	S	\N	\N
25767	35	Hail	Shower              	\N	SHGR	GR	SH   	\N	Precip	S	\N	\N
25767	36	Small Hail	Shower              	\N	SHGS	GS	SH   	\N	Precip	S	\N	\N
25767	37	Ice Pellets	\N	Light	-PL	PL	\N	-	Precip	S	\N	\N
25767	38	Ice Pellets	\N	Moderate	PL	PL	\N	\N	Precip	S	\N	\N
25767	39	Ice Pellets	\N	Heavy	+PL	PL	\N	+	Precip	S	\N	\N
25767	40	Ice Pellets	Shower              	Light	-SHPL	PL	SH   	-	Precip	S	\N	\N
25767	41	Ice Pellets	Shower              	Moderate	SHPL	PL	SH   	\N	Precip	S	\N	\N
25767	42	Ice Pellets	Shower              	Heavy	+SHPL	PL	SH   	+	Precip	S	\N	\N
25767	43	Ice Pellets	Thunderstorm        	Light	-TSPL	PL	TS   	-	Thunder	S	\N	\N
25767	44	Ice Pellets	Thunderstorm        	Moderate	TSPL	PL	TS   	\N	Thunder	S	\N	\N
25767	45	Ice Pellets	Thunderstorm        	Heavy	+TSPL	PL	TS   	+	Thunder	S	\N	\N
25767	101	Rain	Thunderstorm        	Light	-TSRA	RA	TS   	-	Thunder	R	\N	\N
25767	102	Snow	Thunderstorm        	Light	-TSSN	SN	TS   	-	Thunder	R	\N	\N
25767	103	Ice Pellets	Thunderstorm        	Light	-TSPL	PL	TS   	-	Thunder	S	\N	\N
25767	104	Rain	Thunderstorm        	Moderate	TSRA	RA	TS   	\N	Thunder	R	\N	\N
25767	105	Snow	Thunderstorm        	Moderate	TSSN	SN	TS   	\N	Thunder	R	\N	\N
25767	106	Ice Pellets	Thunderstorm        	Moderate	TSPL	PL	TS   	\N	Thunder	R	\N	\N
25767	107	Rain	Thunderstorm        	Heavy	+TSRA	RA	TS   	+	Thunder	R	\N	\N
25767	108	Snow	Thunderstorm        	Heavy	+TSSN	SN	TS   	+	Thunder	S	\N	\N
25767	109	Ice Pellets	Thunderstorm        	Moderate	+TSPL	PL	TS   	\N	Thunder	S	\N	\N
25767	110	Thunderstorm	\N	\N	TS	TS	\N	\N	Thunder	\N	\N	\N
25767	111	Hail	Thunderstorm        	Moderate	TSGR	GR	TS   	\N	Thunder	S	\N	\N
25767	112	Small Hail	Thunderstorm        	Moderate	TSGS	GS	TS   	\N	Thunder	S	\N	\N
25767	201	Fog	\N	\N	FG	FG	\N	\N	Obscur	\N	\N	\N
25767	202	Mist	\N	\N	BR	BR	\N	\N	Obscur	\N	\N	\N
25767	203	Smoke	\N	\N	FU	FU	\N	\N	Obscur	\N	\N	\N
25767	204	Volcanic Ash	\N	\N	VA	VA	\N	\N	Obscur	\N	\N	\N
25767	205	Dust	\N	\N	DU	DU	\N	\N	Obscur	\N	\N	\N
25767	206	Sand	\N	\N	SA	SA	\N	\N	Obscur	\N	\N	\N
25767	207	Haze	\N	\N	HZ	HZ	\N	\N	Obscur	\N	\N	\N
25767	208	Fog	Shallow             	\N	MIFG	FG	MI   	\N	Obscur	\N	\N	\N
25767	209	Fog	Partial             	\N	PRFG	FG	PR   	\N	Obscur	\N	\N	\N
25767	210	Fog	Patches             	\N	BCFG	FG	BC   	\N	Obscur	\N	\N	\N
25767	211	Fog	Freezing            	\N	FZFG	FG	FZ   	\N	Obscur	\N	\N	\N
25767	212	Dust	Low Drifting        	\N	DRDU	DU	DR   	\N	Obscur	\N	\N	\N
25767	213	Dust	Blowing             	\N	BLDU	DU	BL   	\N	Obscur	\N	\N	\N
25767	214	Sand	Low Drifting        	\N	DRSA	SA	DR   	\N	Obscur	\N	\N	\N
25767	215	Sand	Blowing             	\N	BLSA	SA	BL   	\N	Obscur	\N	\N	\N
25767	216	Snow	Drifting            	\N	DRSN	SN	DR   	\N	Obscur	\N	\N	\N
25767	217	Snow	Blowing             	\N	BLSN	SN	BL   	\N	Obscur	\N	\N	\N
25767	218	Spray	Blowing             	\N	BLPY	PY	BL   	\N	Obscur	\N	\N	\N
25767	301	Dust Whirl	\N	\N	PO	PO	\N	\N	Severe	\N	\N	\N
25767	302	Squall	\N	\N	SQ	SQ	\N	\N	Severe	\N	\N	\N
25767	303	Funnel Cloud	\N	\N	FC	FC	\N	\N	Severe	\N	\N	\N
25767	304	Sandstorm	\N	Moderate	SS	SS	\N	\N	Severe	\N	\N	\N
25767	305	Duststorm	\N	Moderate	DS	DS	\N	\N	Severe	\N	\N	\N
25767	306	Tornado	\N	\N	FC+	FC+	\N	\N	Severe	\N	\N	\N
25767	307	Sandstorm	\N	Heavy	SS+	SS	\N	+	Severe	\N	\N	\N
25767	308	Duststorm	\N	Heavy	DS+	DS	\N	+	Severe	\N	\N	\N
25767	401	Shower	\N	Vicinity	VCSH	SH	\N	VC	Unk_Precip	\N	\N	\N
25767	402	Thunderstorm	\N	Vicinity	VCTS	TS	\N	VC	Unk_Precip	\N	\N	\N
25767	501	Dust	Blowing             	Vicinity	VCBLDU	DU	BL   	VC	Obscr_Nearby	\N	\N	\N
25767	502	Snow	Bowing              	Vicinity	VCBLSN	SN	BL   	VC	Obscr_Nearby	\N	\N	\N
25767	503	Sand	Blowing             	Vicinity	VCBLSA	SA	BL   	VC	Obscr_Nearby	\N	\N	\N
25767	504	Fog	\N	Vicinity	VCFG	FG	\N	VC	Obscr_Nearby	\N	\N	\N
25767	601	Dust/Sand Swirl	\N	Vicinity	VCPO	PO	\N	VC	Sev_Wx_Nearby	\N	\N	\N
25767	602	Sandstorm	\N	Vicinity	VCSS	SS	\N	VC	Sev_Wx_Nearby	\N	\N	\N
25767	603	Duststorm	\N	Vicinity	VCDS	DS	\N	VC	Sev_Wx_Nearby	\N	\N	\N
25807	1001	Drizzle	\N	Light	-DZ	DZ	\N	-	Precip	R	\N	\N
25807	1002	Rain	\N	Light	-RA	RA	\N	-	Precip	R	\N	\N
25807	1003	Rain	Shower              	Light	-SHRA	RA	SH   	-	Precip	R	\N	\N
25807	1004	Snow	\N	Light	-SN	SN	\N	-	Precip	S	\N	\N
25807	1005	Snow Grains	\N	Light	-SG	SG	\N	-	Precip	S	\N	\N
25807	1006	Ice Pellets	\N	Light	-PL	PL	\N	-	Precip	S	\N	\N
25807	1007	Snow	Shower              	Light	-SHSN	SN	SH   	-	Precip	S	\N	\N
25807	1008	Ice Pellets	Shower              	Light	-SHPL	PL	SH   	-	Precip	S	\N	\N
25807	1009	Drizzle	Freezing            	Light	-FZDZ	DZ	FZ   	-	Precip	Z	\N	\N
25807	1010	Rain	Freezing            	Light	-FZRA	RA	FZ   	-	Precip	Z	\N	\N
25807	1011	Drizzle	\N	Moderate	DZ	DZ	\N	\N	Precip	R	\N	\N
25807	1012	Rain	\N	Moderate	RA	RA	\N	\N	Precip	R	\N	\N
25807	1013	Rain	Shower              	Moderate	SHRA	RA	SH   	\N	Precip	R	\N	\N
25807	1014	Snow	\N	Moderate	SN	SN	\N	\N	Precip	S	\N	\N
25807	1015	Snow Grains	\N	Moderate	SG	SG	\N	\N	Precip	S	\N	\N
25807	1016	Ice Pellets	\N	Moderate	PL	PL	\N	\N	Precip	S	\N	\N
25807	1017	Snow	Shower              	Moderate	SHSN	SN	SH   	\N	Precip	S	\N	\N
25807	1018	Ice Pellet	Shower              	Moderate	SHPL	PL	SH   	\N	Precip	S	\N	\N
25807	1019	Drizzle	Freezing            	Moderate	FZDZ	DZ	FZ   	\N	Precip	Z	\N	\N
25807	1020	Rain	Freezing            	Moderate	FZRA	RA	FZ   	\N	Precip	Z	\N	\N
25807	1021	Drizzle	\N	Heavy	+DZ	DZ	\N	+	Precip	R	\N	\N
25807	1022	Rain	\N	Heavy	+RA	RA	\N	+	Precip	R	\N	\N
25807	1023	Rain	Shower              	Heavy	+SHRA	RA	SH   	+	Precip	R	\N	\N
25807	1024	Snow	\N	Heavy	+SN	SN	\N	+	Precip	S	\N	\N
25807	1025	Snow Grains	\N	Heavy	+SG	SG	\N	+	Precip	S	\N	\N
25807	1026	Ice Pellets	\N	Heavy	+PL	PL	\N	+	Precip	S	\N	\N
25807	1027	Snow	Shower              	Heavy	+SHSN	SN	SH   	+	Precip	S	\N	\N
25807	1028	Ice Pellets	Shower              	Heavy	+SHPL	PL	SH   	+	Precip	S	\N	\N
25807	1029	Drizzle	Freezing            	Heavy	+FZDZ	DZ	FZ   	+	Precip	Z	\N	\N
25807	1030	Rain	Freezing            	Heavy	+FZRA	RA	FZ   	+	Precip	Z	\N	\N
25807	1032	Ice Crystals	\N	\N	IC	IC	\N	\N	Precip	S	\N	\N
25807	1033	Hail	\N	\N	GR	GR	\N	\N	Precip	S	\N	\N
25807	1034	Small Hail	\N	\N	GS	GS	\N	\N	Precip	S	\N	\N
25807	1035	Hail	Shower              	\N	SHGR	GR	SH   	\N	Precip	S	\N	\N
25807	1036	Small Hail	Shower              	\N	SHGS	GS	SH   	\N	Precip	S	\N	\N
25807	1037	Ice Pellets	Shower              	Light	-SHPE	PE	SH   	-	Precip	S	\N	\N
25807	1038	Ice Pellets	Shower              	Moderate	SHPE	PE	SH   	\N	Precip	S	\N	\N
25807	1039	Ice Pellets	Shower              	Heavy	+SHPE	PE	SH   	+	Precip	S	\N	\N
25807	1101	Rain	Thunderstorm        	Light	-TSRA	RA	TS   	-	Thunder	R	\N	\N
25807	1102	Snow	Thunderstorm        	Light	-TSSN	SN	TS   	-	Thunder	R	\N	\N
25807	1103	Ice Pellets	Thunderstorm        	Light	-TSPL	PL	TS   	-	Thunder	S	\N	\N
25807	1104	Rain	Thunderstorm        	Moderate	TSRA	RA	TS   	\N	Thunder	R	\N	\N
25807	1105	Snow	Thunderstorm        	Moderate	TSSN	SN	TS   	\N	Thunder	R	\N	\N
25807	1106	Ice Pellets	Thunderstorm        	Moderate	TSPL	PL	TS   	\N	Thunder	R	\N	\N
25807	1107	Rain	Thunderstorm        	Heavy	+TSRA	RA	TS   	+	Thunder	R	\N	\N
25807	1108	Snow	Thunderstorm        	Heavy	+TSSN	SN	TS   	+	Thunder	S	\N	\N
25807	1109	Ice Pellets	Thunderstorm        	Heavy	+TSPL	PL	TS   	+	Thunder	S	\N	\N
25807	1110	Thunderstorm	\N	\N	TS	TS	\N	\N	Thunder	\N	\N	\N
25807	1111	Hail	Thunderstorm        	Moderate	TSGR	GR	TS   	\N	Thunder	S	\N	\N
25807	1112	Small Hail	Thunderstorm        	Moderate	TSGS	GS	TS   	\N	Thunder	S	\N	\N
25807	1113	Ice Pellets	Thunderstorm        	Light	-TSPE	PE	TS   	-	Thunder	S	\N	\N
25807	1114	Ice Pellets	Thunderstorm        	Moderate	TSPE	PE	TS   	\N	Thunder	S	\N	\N
25807	1115	Ice Pellets	Thunderstorm        	Heavy	+TSPE	PE	TS   	+	Thunder	S	\N	\N
25807	1201	Fog	\N	\N	FG	FG	\N	\N	Obscur	\N	\N	\N
25807	1202	Mist	\N	\N	BR	BR	\N	\N	Obscur	\N	\N	\N
25807	1203	Smoke	\N	\N	FU	FU	\N	\N	Obscur	\N	\N	\N
25807	1204	Volcanic Ash	\N	\N	VA	VA	\N	\N	Obscur	\N	\N	\N
25807	1205	Dust	\N	\N	DU	DU	\N	\N	Obscur	\N	\N	\N
25807	1206	Sand	\N	\N	SA	SA	\N	\N	Obscur	\N	\N	\N
25807	1207	Haze	\N	\N	HZ	HZ	\N	\N	Obscur	\N	\N	\N
25807	1208	Fog	Shallow             	\N	MIFG	FG	MI   	\N	Obscur	\N	\N	\N
25807	1209	Fog	Partial             	\N	PRFG	FG	PR   	\N	Obscur	\N	\N	\N
25807	1210	Fog	Patches             	\N	BCFG	FG	BC   	\N	Obscur	\N	\N	\N
25807	1211	Fog	Freezing            	\N	FZFG	FG	FZ   	\N	Obscur	\N	\N	\N
25807	1212	Dust	Low Drifting        	\N	DRDU	DU	DR   	\N	Obscur	\N	\N	\N
25807	1213	Dust	Blowing             	\N	BLDU	DU	BL   	\N	Obscur	\N	\N	\N
25807	1214	Sand	Low Drifting        	\N	DRSA	SA	DR   	\N	Obscur	\N	\N	\N
25807	1215	Sand	Blowing             	\N	BLSA	SA	BL   	\N	Obscur	\N	\N	\N
25807	1216	Snow	Drifting            	\N	DRSN	SN	DR   	\N	Obscur	\N	\N	\N
25807	1217	Snow	Blowing             	\N	BLSN	SN	BL   	\N	Obscur	\N	\N	\N
25807	1218	Spray	Blowing             	\N	BLPY	PY	BL   	\N	Obscur	\N	\N	\N
25807	1301	Dust Whirl	\N	\N	PO	PO	\N	\N	Severe	\N	\N	\N
25807	1302	Squall	\N	\N	SQ	SQ	\N	\N	Severe	\N	\N	\N
25807	1303	Funnel Cloud	\N	\N	FC	FC	\N	\N	Severe	\N	\N	\N
25807	1304	Sandstorm	\N	Moderate	SS	SS	\N	\N	Severe	\N	\N	\N
25807	1305	Duststorm	\N	Moderate	DS	DS	\N	\N	Severe	\N	\N	\N
25807	1306	Tornado	\N	\N	+FC	+FC	\N	\N	Severe	\N	\N	\N
25807	1307	Sandstorm	\N	Heavy	+SS	SS	\N	+	Severe	\N	\N	\N
25807	1308	Duststorm	\N	Heavy	+DS	DS	\N	+	Severe	\N	\N	\N
25807	1401	Shower	\N	Vicinity	VCSH	SH	\N	VC	Unk_Precip	\N	\N	\N
25807	1402	Thunderstorm	\N	Vicinity	VCTS	TS	\N	VC	Unk_Precip	\N	\N	\N
25807	1504	Fog	\N	Vicinity	VCFG	FG	\N	VC	Obscr_Nearby	\N	\N	'
25807	1601	No Significant Wx	                    	\N	NSW	NSW	\N	\N	\N	\N	\N	\N
\.


