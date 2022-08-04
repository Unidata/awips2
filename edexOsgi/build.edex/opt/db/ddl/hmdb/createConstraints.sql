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
-- Name: boolean_values_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY boolean_values
    ADD CONSTRAINT boolean_values_pk PRIMARY KEY (element_id, element_value);


--
-- Name: bufr_identifier_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY bufr_identifier
    ADD CONSTRAINT bufr_identifier_pk PRIMARY KEY (bufr_descriptor);


--
-- Name: cat_values_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY cat_values
    ADD CONSTRAINT cat_values_pk PRIMARY KEY (element_id, category_value);


--
-- Name: categorical_ele_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY categorical_ele
    ADD CONSTRAINT categorical_ele_pk PRIMARY KEY (element_id);


--
-- Name: cli_asos_daily_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY cli_asos_daily
    ADD CONSTRAINT cli_asos_daily_pk PRIMARY KEY (station_code, day_of_year);


--
-- Name: cli_asos_monthly_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY cli_asos_monthly
    ADD CONSTRAINT cli_asos_monthly_pk PRIMARY KEY (station_code, "month");


--
-- Name: cli_sta_setup_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY cli_sta_setup
    ADD CONSTRAINT cli_sta_setup_pk PRIMARY KEY (station_id);


--
-- Name: climate_day_config_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY climate_day_config
    ADD CONSTRAINT climate_day_config_pk PRIMARY KEY (prod_id, time_of_day);


--
-- Name: climate_period_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY climate_period
    ADD CONSTRAINT climate_period_pk PRIMARY KEY (station_id);


--
-- Name: contin_int_ele_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY contin_int_ele
    ADD CONSTRAINT contin_int_ele_pk PRIMARY KEY (element_id);


--
-- Name: contin_real_ele_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY contin_real_ele
    ADD CONSTRAINT contin_real_ele_pk PRIMARY KEY (element_id);


--
-- Name: coordinates_2d_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY coordinates_2d
    ADD CONSTRAINT coordinates_2d_pk PRIMARY KEY (coord_type);


--
-- Name: daily_climate_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY daily_climate
    ADD CONSTRAINT daily_climate_pk PRIMARY KEY (station_id, date);


--
-- Name: data_source_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY data_source
    ADD CONSTRAINT data_source_pk PRIMARY KEY (source_process);


--
-- Name: data_src_version_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY data_src_version
    ADD CONSTRAINT data_src_version_pk PRIMARY KEY (source_version_id);


--
-- Name: day_climate_extreme_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY day_climate_extreme
    ADD CONSTRAINT day_climate_extreme_pkey PRIMARY KEY (station_code, day_of_year);


--
-- Name: day_climate_norm_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY day_climate_norm
    ADD CONSTRAINT day_climate_norm_pk PRIMARY KEY (station_id, day_of_year);


--
-- Name: defined_values_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY defined_values
    ADD CONSTRAINT defined_values_pk PRIMARY KEY (element_id, defined_value);


--
-- Name: discrete_ele_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY discrete_ele
    ADD CONSTRAINT discrete_ele_pk PRIMARY KEY (element_id);


--
-- Name: discrete_values_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY discrete_values
    ADD CONSTRAINT discrete_values_pk PRIMARY KEY (element_id, discrete_value);


--
-- Name: dqd_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY dqd
    ADD CONSTRAINT dqd_pk PRIMARY KEY (dqd);


--
-- Name: ele_src_version_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY ele_src_version
    ADD CONSTRAINT ele_src_version_pk PRIMARY KEY (ele_src_version);


--
-- Name: element_relat_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY element_relat
    ADD CONSTRAINT element_relat_pk PRIMARY KEY (ele_relat_id);


--
-- Name: forecast_backup_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY forecast_backup
    ADD CONSTRAINT forecast_backup_pk PRIMARY KEY (backup_status);


--
-- Name: fss_categ_multi_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY fss_categ_multi
    ADD CONSTRAINT fss_categ_multi_pk PRIMARY KEY (fss_rpt_instance, element_id, wx_ele_number);


--
-- Name: fss_categ_single_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY fss_categ_single
    ADD CONSTRAINT fss_categ_single_pk PRIMARY KEY (fss_rpt_instance, element_id);


--
-- Name: fss_cloud_layer_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY fss_cloud_layer
    ADD CONSTRAINT fss_cloud_layer_pk PRIMARY KEY (fss_rpt_instance, element_id, layer_number);


--
-- Name: fss_contin_real_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY fss_contin_real
    ADD CONSTRAINT fss_contin_real_pk PRIMARY KEY (fss_rpt_instance, element_id);


--
-- Name: fss_report_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY fss_report
    ADD CONSTRAINT fss_report_pk PRIMARY KEY (fss_rpt_instance);


--
-- Name: fss_wx_period_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY fss_wx_period
    ADD CONSTRAINT fss_wx_period_pk PRIMARY KEY (fss_rpt_instance, element_id, element_value, element_num);


--
-- Name: hydromet_element_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY hydromet_element
    ADD CONSTRAINT hydromet_element_pk PRIMARY KEY (element_id);


--
-- Name: issuance_type_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY issuance_type
    ADD CONSTRAINT issuance_type_pk PRIMARY KEY (issuance_type_id);


--
-- Name: issuing_office_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY issuing_office
    ADD CONSTRAINT issuing_office_pk PRIMARY KEY (office_id);


--
-- Name: map_proj_coords_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY map_proj_coords
    ADD CONSTRAINT map_proj_coords_pk PRIMARY KEY (map_proj_id);


--
-- Name: mon_climate_norm_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY mon_climate_norm
    ADD CONSTRAINT mon_climate_norm_pk PRIMARY KEY (station_id, month_of_year, period_type);


--
-- Name: mtr_status_pkey; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY mtr_status
    ADD CONSTRAINT mtr_status_pkey PRIMARY KEY (process_dtime);


--
-- Name: phys_ele_relat_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY phys_ele_relat
    ADD CONSTRAINT phys_ele_relat_pk PRIMARY KEY (ele_relat_id, phys_element_id);


--
-- Name: phys_units_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY physical_units
    ADD CONSTRAINT phys_units_pk PRIMARY KEY (physical_units);


--
-- Name: physical_element_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY physical_element
    ADD CONSTRAINT physical_element_pk PRIMARY KEY (phys_element_id);


--
-- Name: product_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY product
    ADD CONSTRAINT product_pk PRIMARY KEY (product_name);


--
-- Name: product_version_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY product_version
    ADD CONSTRAINT product_version_pk PRIMARY KEY (prod_version);


--
-- Name: relat_type_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY relat_type
    ADD CONSTRAINT relat_type_pk PRIMARY KEY (ele_relat_type);


--
-- Name: sta_agency_codes_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY sta_agency_codes
    ADD CONSTRAINT sta_agency_codes_pk PRIMARY KEY (station_id, naming_agency, agency_subsystem);


--
-- Name: station_location_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY station_location
    ADD CONSTRAINT station_location_pk PRIMARY KEY (station_id);


--
-- Name: time_zone_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY time_zone
    ADD CONSTRAINT time_zone_pk PRIMARY KEY (time_zone_id);


--
-- Name: units_class_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY units_class
    ADD CONSTRAINT units_class_pk PRIMARY KEY (units_class);


--
-- Name: units_conversion_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY units_conversion
    ADD CONSTRAINT units_conversion_pk PRIMARY KEY (convert_from, convert_to, units_class);


--
-- Name: units_system_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY units_system
    ADD CONSTRAINT units_system_pk PRIMARY KEY (units_system);


--
-- Name: units_translations_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY units_translations
    ADD CONSTRAINT units_translations_pk PRIMARY KEY (alien_units_desig, naming_agency, agency_subsystem);


--
-- Name: weather_category_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY weather_category
    ADD CONSTRAINT weather_category_pk PRIMARY KEY (weather_category);


--
-- Name: wmo_state_region_pk; Type: CONSTRAINT; Schema: public; Owner: pguser; Tablespace: 
--

ALTER TABLE ONLY wmo_state_region
    ADD CONSTRAINT wmo_state_region_pk PRIMARY KEY (state, wmo_header_region);


--
-- Name: datesta; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX datesta ON rpt USING btree (icao_loc_id, date);


--
-- Name: get1bysub; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX get1bysub ON fss_report USING btree (nominal_dtime, station_id, report_subtype);


--
-- Name: ix_prdlist; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX ix_prdlist ON prod_list USING btree (issuing_office);


--
-- Name: justdate; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX justdate ON rpt USING btree (date);


--
-- Name: mtrstatusind; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX mtrstatusind ON mtr_status USING btree (process_dtime);


--
-- Name: p110_76; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p110_76 ON bufr_identifier USING btree (bufr_descriptor);


--
-- Name: p111_77; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p111_77 ON dqd USING btree (dqd);


--
-- Name: p112_79; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p112_79 ON data_source USING btree (source_process);


--
-- Name: p113_81; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p113_81 ON forecast_backup USING btree (backup_status);


--
-- Name: p114_82; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p114_82 ON relat_type USING btree (ele_relat_type);


--
-- Name: p115_83; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p115_83 ON time_zone USING btree (time_zone_id);


--
-- Name: p116_84; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p116_84 ON units_system USING btree (units_system);


--
-- Name: p117_85; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p117_85 ON data_src_version USING btree (source_version_id);


--
-- Name: p123_100; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p123_100 ON element_relat USING btree (ele_relat_id);


--
-- Name: p124_101; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p124_101 ON coordinates_2d USING btree (coord_type);


--
-- Name: p125_105; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p125_105 ON contin_real_ele USING btree (element_id);


--
-- Name: p126_107; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p126_107 ON contin_int_ele USING btree (element_id);


--
-- Name: p127_109; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p127_109 ON categorical_ele USING btree (element_id);


--
-- Name: p128_111; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p128_111 ON cat_values USING btree (element_id, category_value);


--
-- Name: p129_114; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p129_114 ON phys_ele_relat USING btree (ele_relat_id, phys_element_id);


--
-- Name: p130_115; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p130_115 ON defined_values USING btree (element_id, defined_value);


--
-- Name: p132_118; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p132_118 ON sta_agency_codes USING btree (station_id, naming_agency, agency_subsystem);


--
-- Name: p133_120; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p133_120 ON fss_contin_real USING btree (fss_rpt_instance, element_id);


--
-- Name: p134_122; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p134_122 ON fss_categ_single USING btree (fss_rpt_instance, element_id);


--
-- Name: p135_124; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p135_124 ON fss_categ_multi USING btree (fss_rpt_instance, element_id, wx_ele_number);


--
-- Name: p136_126; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p136_126 ON fss_cloud_layer USING btree (fss_rpt_instance, element_id, layer_number);


--
-- Name: p142_140; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p142_140 ON discrete_values USING btree (element_id, discrete_value);


--
-- Name: p143_141; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p143_141 ON discrete_ele USING btree (element_id);


--
-- Name: p144_143; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p144_143 ON weather_category USING btree (weather_category);


--
-- Name: p145_144; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p145_144 ON boolean_values USING btree (element_id, element_value);


--
-- Name: p146_148; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p146_148 ON units_class USING btree (units_class);


--
-- Name: p147_149; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p147_149 ON issuance_type USING btree (issuance_type_id);


--
-- Name: p148_151; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p148_151 ON physical_units USING btree (physical_units);


--
-- Name: p149_154; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p149_154 ON physical_element USING btree (phys_element_id);


--
-- Name: p150_156; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p150_156 ON product_version USING btree (prod_version);


--
-- Name: p151_159; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p151_159 ON ele_src_version USING btree (ele_src_version);


--
-- Name: p155_164; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p155_164 ON issuing_office USING btree (office_id);


--
-- Name: p156_165; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p156_165 ON map_proj_coords USING btree (map_proj_id);


--
-- Name: p157_167; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p157_167 ON station_location USING btree (station_id);


--
-- Name: p164_188; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p164_188 ON product USING btree (product_name);


--
-- Name: p165_190; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p165_190 ON units_conversion USING btree (convert_from, convert_to, units_class);


--
-- Name: p166_196; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p166_196 ON units_translations USING btree (alien_units_desig, naming_agency, agency_subsystem);


--
-- Name: p172_215; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p172_215 ON hydromet_element USING btree (element_id);


--
-- Name: p177_228; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p177_228 ON climate_period USING btree (station_id);


--
-- Name: p179_230; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p179_230 ON climate_day_config USING btree (prod_id, time_of_day);


--
-- Name: p181_236; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p181_236 ON wmo_state_region USING btree (state, wmo_header_region);


--
-- Name: p182_239; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p182_239 ON cli_sta_setup USING btree (station_id);


--
-- Name: p183_241; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p183_241 ON fss_report USING btree (fss_rpt_instance);


--
-- Name: p186_246; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p186_246 ON day_climate_norm USING btree (station_id, day_of_year);


--
-- Name: p189_250; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p189_250 ON cli_asos_monthly USING btree (station_code, "month");


--
-- Name: p189_288; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p189_288 ON cli_asos_daily USING btree (station_code, day_of_year);


--
-- Name: p190_251; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p190_251 ON daily_climate USING btree (station_id, date);


--
-- Name: p195_386; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p195_386 ON mon_climate_norm USING btree (station_id, month_of_year, period_type);


--
-- Name: p196_387; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX p196_387 ON fss_wx_period USING btree (fss_rpt_instance, element_id, element_value, element_num);


--
-- Name: r117_263; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r117_263 ON data_src_version USING btree (source_process);


--
-- Name: r123_271; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r123_271 ON element_relat USING btree (ele_relat_type);


--
-- Name: r124_272; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r124_272 ON coordinates_2d USING btree (map_proj_id);


--
-- Name: r124_273; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r124_273 ON coordinates_2d USING btree (first_coord_unit);


--
-- Name: r124_274; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r124_274 ON coordinates_2d USING btree (second_coord_unit);


--
-- Name: r125_276; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r125_276 ON contin_real_ele USING btree (data_value_units);


--
-- Name: r126_278; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r126_278 ON contin_int_ele USING btree (data_value_units);


--
-- Name: r127_280; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r127_280 ON categorical_ele USING btree (range_value_units);


--
-- Name: r128_281; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r128_281 ON cat_values USING btree (element_id);


--
-- Name: r129_282; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r129_282 ON phys_ele_relat USING btree (ele_relat_id);


--
-- Name: r129_283; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r129_283 ON phys_ele_relat USING btree (phys_element_id);


--
-- Name: r130_284; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r130_284 ON defined_values USING btree (element_id);


--
-- Name: r132_289; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r132_289 ON sta_agency_codes USING btree (station_id);


--
-- Name: r133_290; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r133_290 ON fss_contin_real USING btree (fss_rpt_instance);


--
-- Name: r133_291; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r133_291 ON fss_contin_real USING btree (element_id);


--
-- Name: r134_292; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r134_292 ON fss_categ_single USING btree (fss_rpt_instance);


--
-- Name: r134_293; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r134_293 ON fss_categ_single USING btree (element_id);


--
-- Name: r135_294; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r135_294 ON fss_categ_multi USING btree (element_id);


--
-- Name: r135_295; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r135_295 ON fss_categ_multi USING btree (fss_rpt_instance);


--
-- Name: r136_296; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r136_296 ON fss_cloud_layer USING btree (fss_rpt_instance);


--
-- Name: r136_297; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r136_297 ON fss_cloud_layer USING btree (element_id);


--
-- Name: r142_306; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r142_306 ON discrete_values USING btree (element_id);


--
-- Name: r143_308; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r143_308 ON discrete_ele USING btree (data_value_units);


--
-- Name: r145_309; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r145_309 ON boolean_values USING btree (element_id);


--
-- Name: r145_310; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r145_310 ON boolean_values USING btree (weather_category);


--
-- Name: r148_311; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r148_311 ON physical_units USING btree (units_system);


--
-- Name: r148_312; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r148_312 ON physical_units USING btree (units_class);


--
-- Name: r149_313; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r149_313 ON physical_element USING btree (units_class);


--
-- Name: r150_314; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r150_314 ON product_version USING btree (product_name);


--
-- Name: r151_315; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r151_315 ON ele_src_version USING btree (prod_version);


--
-- Name: r151_316; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r151_316 ON ele_src_version USING btree (element_id);


--
-- Name: r151_317; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r151_317 ON ele_src_version USING btree (source_version_id);


--
-- Name: r155_323; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r155_323 ON issuing_office USING btree (elevation_units);


--
-- Name: r155_324; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r155_324 ON issuing_office USING btree (coord_type);


--
-- Name: r155_325; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r155_325 ON issuing_office USING btree (station_id);


--
-- Name: r156_326; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r156_326 ON map_proj_coords USING btree (delta_xy_units);


--
-- Name: r156_327; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r156_327 ON map_proj_coords USING btree (radius_units);


--
-- Name: r157_328; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r157_328 ON station_location USING btree (coord_type);


--
-- Name: r157_329; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r157_329 ON station_location USING btree (elevation_units);


--
-- Name: r165_349; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r165_349 ON units_conversion USING btree (convert_from);


--
-- Name: r165_350; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r165_350 ON units_conversion USING btree (convert_to);


--
-- Name: r165_351; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r165_351 ON units_conversion USING btree (units_class);


--
-- Name: r166_352; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r166_352 ON units_translations USING btree (awips_units_desig);


--
-- Name: r172_361; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r172_361 ON hydromet_element USING btree (bufr_descriptor);


--
-- Name: r172_362; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r172_362 ON hydromet_element USING btree (product_name);


--
-- Name: r172_363; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r172_363 ON hydromet_element USING btree (source_process);


--
-- Name: r172_364; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r172_364 ON hydromet_element USING btree (phys_element_id);


--
-- Name: r182_375; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r182_375 ON cli_sta_setup USING btree (office_id);


--
-- Name: r183_376; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r183_376 ON fss_report USING btree (report_type);


--
-- Name: r183_377; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r183_377 ON fss_report USING btree (time_zone_id);


--
-- Name: r183_378; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r183_378 ON fss_report USING btree (prod_version);


--
-- Name: r183_379; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r183_379 ON fss_report USING btree (source_version_id);


--
-- Name: r183_380; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r183_380 ON fss_report USING btree (station_id);


--
-- Name: r186_383; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r186_383 ON day_climate_norm USING btree (station_id);


--
-- Name: r190_385; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r190_385 ON daily_climate USING btree (station_id);


--
-- Name: r196_392; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r196_392 ON fss_wx_period USING btree (fss_rpt_instance);


--
-- Name: r196_393; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE INDEX r196_393 ON fss_wx_period USING btree (element_id, element_value);


--
-- Name: sta_icao_ix; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX sta_icao_ix ON station_location USING btree (station_code);


--
-- Name: u112_78; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX u112_78 ON data_source USING btree (src_process_id);


--
-- Name: u148_150; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX u148_150 ON physical_units USING btree (units_id);


--
-- Name: u164_187; Type: INDEX; Schema: public; Owner: pguser; Tablespace: 
--

CREATE UNIQUE INDEX u164_187 ON product USING btree (product_id);


--
-- Name: boolean_val_elemid_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY boolean_values
    ADD CONSTRAINT boolean_val_elemid_fk FOREIGN KEY (element_id) REFERENCES hydromet_element(element_id) MATCH FULL;


--
-- Name: boolean_val_wxcateg_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY boolean_values
    ADD CONSTRAINT boolean_val_wxcateg_fk FOREIGN KEY (weather_category) REFERENCES weather_category(weather_category) MATCH FULL;


--
-- Name: cat_values_elemid_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY cat_values
    ADD CONSTRAINT cat_values_elemid_fk FOREIGN KEY (element_id) REFERENCES hydromet_element(element_id) MATCH FULL;


--
-- Name: categorical_elemid_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY categorical_ele
    ADD CONSTRAINT categorical_elemid_fk FOREIGN KEY (element_id) REFERENCES hydromet_element(element_id) MATCH FULL;


--
-- Name: categorical_rangeval_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY categorical_ele
    ADD CONSTRAINT categorical_rangeval_fk FOREIGN KEY (range_value_units) REFERENCES physical_units(physical_units) MATCH FULL;


--
-- Name: cli_sta_setup_offid_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY cli_sta_setup
    ADD CONSTRAINT cli_sta_setup_offid_fk FOREIGN KEY (office_id) REFERENCES issuing_office(office_id) MATCH FULL;


--
-- Name: cli_sta_setup_staid_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY cli_sta_setup
    ADD CONSTRAINT cli_sta_setup_staid_fk FOREIGN KEY (station_id) REFERENCES station_location(station_id) MATCH FULL;


--
-- Name: clim_prd_staid_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY climate_period
    ADD CONSTRAINT clim_prd_staid_fk FOREIGN KEY (station_id) REFERENCES station_location(station_id) MATCH FULL;


--
-- Name: contin_int_dataval_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY contin_int_ele
    ADD CONSTRAINT contin_int_dataval_fk FOREIGN KEY (data_value_units) REFERENCES physical_units(physical_units) MATCH FULL;


--
-- Name: contin_int_elemid_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY contin_int_ele
    ADD CONSTRAINT contin_int_elemid_fk FOREIGN KEY (element_id) REFERENCES hydromet_element(element_id) MATCH FULL;


--
-- Name: contin_real_dataval_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY contin_real_ele
    ADD CONSTRAINT contin_real_dataval_fk FOREIGN KEY (data_value_units) REFERENCES physical_units(physical_units) MATCH FULL;


--
-- Name: contin_real_elemid_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY contin_real_ele
    ADD CONSTRAINT contin_real_elemid_fk FOREIGN KEY (element_id) REFERENCES hydromet_element(element_id) MATCH FULL;


--
-- Name: coordinates_firstcoord_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY coordinates_2d
    ADD CONSTRAINT coordinates_firstcoord_fk FOREIGN KEY (first_coord_unit) REFERENCES physical_units(physical_units) MATCH FULL;


--
-- Name: coordinates_mapproj_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY coordinates_2d
    ADD CONSTRAINT coordinates_mapproj_fk FOREIGN KEY (map_proj_id) REFERENCES map_proj_coords(map_proj_id) MATCH FULL;


--
-- Name: coordinates_secondcoord_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY coordinates_2d
    ADD CONSTRAINT coordinates_secondcoord_fk FOREIGN KEY (second_coord_unit) REFERENCES physical_units(physical_units) MATCH FULL;


--
-- Name: daily_clim_staid_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY daily_climate
    ADD CONSTRAINT daily_clim_staid_fk FOREIGN KEY (station_id) REFERENCES station_location(station_id) MATCH FULL;


--
-- Name: data_src_proc_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY data_src_version
    ADD CONSTRAINT data_src_proc_fk FOREIGN KEY (source_process) REFERENCES data_source(source_process) MATCH FULL;


--
-- Name: day_climate_norm_staid_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY day_climate_norm
    ADD CONSTRAINT day_climate_norm_staid_fk FOREIGN KEY (station_id) REFERENCES station_location(station_id) MATCH FULL;


--
-- Name: defined_values_elemid_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY defined_values
    ADD CONSTRAINT defined_values_elemid_fk FOREIGN KEY (element_id) REFERENCES hydromet_element(element_id) MATCH FULL;


--
-- Name: discrete_ele_dataval_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY discrete_ele
    ADD CONSTRAINT discrete_ele_dataval_fk FOREIGN KEY (data_value_units) REFERENCES physical_units(physical_units) MATCH FULL;


--
-- Name: discrete_ele_elemid_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY discrete_ele
    ADD CONSTRAINT discrete_ele_elemid_fk FOREIGN KEY (element_id) REFERENCES hydromet_element(element_id) MATCH FULL;


--
-- Name: discrete_val_elemid_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY discrete_values
    ADD CONSTRAINT discrete_val_elemid_fk FOREIGN KEY (element_id) REFERENCES hydromet_element(element_id) MATCH FULL;


--
-- Name: ele_src_ver_elemid_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY ele_src_version
    ADD CONSTRAINT ele_src_ver_elemid_fk FOREIGN KEY (element_id) REFERENCES hydromet_element(element_id) MATCH FULL;


--
-- Name: ele_src_ver_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY ele_src_version
    ADD CONSTRAINT ele_src_ver_fk FOREIGN KEY (prod_version) REFERENCES product_version(prod_version) MATCH FULL;


--
-- Name: ele_src_ver_verid_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY ele_src_version
    ADD CONSTRAINT ele_src_ver_verid_fk FOREIGN KEY (source_version_id) REFERENCES data_src_version(source_version_id) MATCH FULL;


--
-- Name: elem_relat_type_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY element_relat
    ADD CONSTRAINT elem_relat_type_fk FOREIGN KEY (ele_relat_type) REFERENCES relat_type(ele_relat_type) MATCH FULL;


--
-- Name: fss_categ_multi_elemid_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY fss_categ_multi
    ADD CONSTRAINT fss_categ_multi_elemid_fk FOREIGN KEY (element_id) REFERENCES hydromet_element(element_id) MATCH FULL;


--
-- Name: fss_categ_multi_rptinst_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY fss_categ_multi
    ADD CONSTRAINT fss_categ_multi_rptinst_fk FOREIGN KEY (fss_rpt_instance) REFERENCES fss_report(fss_rpt_instance) ON DELETE CASCADE;


--
-- Name: fss_categ_single_elemid_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY fss_categ_single
    ADD CONSTRAINT fss_categ_single_elemid_fk FOREIGN KEY (element_id) REFERENCES hydromet_element(element_id) MATCH FULL;


--
-- Name: fss_categ_single_rptinst_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY fss_categ_single
    ADD CONSTRAINT fss_categ_single_rptinst_fk FOREIGN KEY (fss_rpt_instance) REFERENCES fss_report(fss_rpt_instance) ON DELETE CASCADE;


--
-- Name: fss_cloud_elemid_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY fss_cloud_layer
    ADD CONSTRAINT fss_cloud_elemid_fk FOREIGN KEY (element_id) REFERENCES hydromet_element(element_id) MATCH FULL;


--
-- Name: fss_cloud_rptinst_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY fss_cloud_layer
    ADD CONSTRAINT fss_cloud_rptinst_fk FOREIGN KEY (fss_rpt_instance) REFERENCES fss_report(fss_rpt_instance) ON DELETE CASCADE;


--
-- Name: fss_contin_elemid_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY fss_contin_real
    ADD CONSTRAINT fss_contin_elemid_fk FOREIGN KEY (element_id) REFERENCES hydromet_element(element_id) MATCH FULL;


--
-- Name: fss_contin_rptinst_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY fss_contin_real
    ADD CONSTRAINT fss_contin_rptinst_fk FOREIGN KEY (fss_rpt_instance) REFERENCES fss_report(fss_rpt_instance) ON DELETE CASCADE;


--
-- Name: fss_report_prodver_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY fss_report
    ADD CONSTRAINT fss_report_prodver_fk FOREIGN KEY (prod_version) REFERENCES product_version(prod_version) MATCH FULL;


--
-- Name: fss_report_rpttype_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY fss_report
    ADD CONSTRAINT fss_report_rpttype_fk FOREIGN KEY (report_type) REFERENCES product(product_name) MATCH FULL;


--
-- Name: fss_report_src_verid_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY fss_report
    ADD CONSTRAINT fss_report_src_verid_fk FOREIGN KEY (source_version_id) REFERENCES data_src_version(source_version_id) MATCH FULL;


--
-- Name: fss_report_staid_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY fss_report
    ADD CONSTRAINT fss_report_staid_fk FOREIGN KEY (station_id) REFERENCES station_location(station_id) MATCH FULL;


--
-- Name: fss_report_tmzid_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY fss_report
    ADD CONSTRAINT fss_report_tmzid_fk FOREIGN KEY (time_zone_id) REFERENCES time_zone(time_zone_id) MATCH FULL;


--
-- Name: fss_wx_rptinst_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY fss_wx_period
    ADD CONSTRAINT fss_wx_rptinst_fk FOREIGN KEY (fss_rpt_instance) REFERENCES fss_report(fss_rpt_instance) ON DELETE CASCADE;


--
-- Name: hydro_elem_bufrdesc_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY hydromet_element
    ADD CONSTRAINT hydro_elem_bufrdesc_fk FOREIGN KEY (bufr_descriptor) REFERENCES bufr_identifier(bufr_descriptor) MATCH FULL;


--
-- Name: hydro_elem_physelemid_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY hydromet_element
    ADD CONSTRAINT hydro_elem_physelemid_fk FOREIGN KEY (phys_element_id) REFERENCES physical_element(phys_element_id) MATCH FULL;


--
-- Name: hydro_elem_prodname_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY hydromet_element
    ADD CONSTRAINT hydro_elem_prodname_fk FOREIGN KEY (product_name) REFERENCES product(product_name) MATCH FULL;


--
-- Name: hydro_elem_srcproc_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY hydromet_element
    ADD CONSTRAINT hydro_elem_srcproc_fk FOREIGN KEY (source_process) REFERENCES data_source(source_process) MATCH FULL;


--
-- Name: issuing_off_crdtype_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY issuing_office
    ADD CONSTRAINT issuing_off_crdtype_fk FOREIGN KEY (coord_type) REFERENCES coordinates_2d(coord_type) MATCH FULL;


--
-- Name: issuing_off_elevunits_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY issuing_office
    ADD CONSTRAINT issuing_off_elevunits_fk FOREIGN KEY (elevation_units) REFERENCES physical_units(physical_units) MATCH FULL;


--
-- Name: issuing_off_staid_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY issuing_office
    ADD CONSTRAINT issuing_off_staid_fk FOREIGN KEY (station_id) REFERENCES station_location(station_id) MATCH FULL;


--
-- Name: map_proj_coords_delta_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY map_proj_coords
    ADD CONSTRAINT map_proj_coords_delta_fk FOREIGN KEY (delta_xy_units) REFERENCES physical_units(physical_units) MATCH FULL;


--
-- Name: map_proj_coords_radius_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY map_proj_coords
    ADD CONSTRAINT map_proj_coords_radius_fk FOREIGN KEY (radius_units) REFERENCES physical_units(physical_units) MATCH FULL;


--
-- Name: phys_elem_elemid_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY phys_ele_relat
    ADD CONSTRAINT phys_elem_elemid_fk FOREIGN KEY (phys_element_id) REFERENCES physical_element(phys_element_id) MATCH FULL;


--
-- Name: phys_elem_relatid_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY phys_ele_relat
    ADD CONSTRAINT phys_elem_relatid_fk FOREIGN KEY (ele_relat_id) REFERENCES element_relat(ele_relat_id) MATCH FULL;


--
-- Name: physical_elem_class_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY physical_element
    ADD CONSTRAINT physical_elem_class_fk FOREIGN KEY (units_class) REFERENCES units_class(units_class) MATCH FULL;


--
-- Name: physical_units_class_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY physical_units
    ADD CONSTRAINT physical_units_class_fk FOREIGN KEY (units_class) REFERENCES units_class(units_class) MATCH FULL;


--
-- Name: physical_units_sys_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY physical_units
    ADD CONSTRAINT physical_units_sys_fk FOREIGN KEY (units_system) REFERENCES units_system(units_system) MATCH FULL;


--
-- Name: prod_ver_name_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY product_version
    ADD CONSTRAINT prod_ver_name_fk FOREIGN KEY (product_name) REFERENCES product(product_name) MATCH FULL;


--
-- Name: sta_agency_staid_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY sta_agency_codes
    ADD CONSTRAINT sta_agency_staid_fk FOREIGN KEY (station_id) REFERENCES station_location(station_id) MATCH FULL;


--
-- Name: station_loc_crdtype_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY station_location
    ADD CONSTRAINT station_loc_crdtype_fk FOREIGN KEY (coord_type) REFERENCES coordinates_2d(coord_type) MATCH FULL;


--
-- Name: station_loc_elevunits_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY station_location
    ADD CONSTRAINT station_loc_elevunits_fk FOREIGN KEY (elevation_units) REFERENCES physical_units(physical_units) MATCH FULL;


--
-- Name: units_conv_class_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY units_conversion
    ADD CONSTRAINT units_conv_class_fk FOREIGN KEY (units_class) REFERENCES units_class(units_class) MATCH FULL;


--
-- Name: units_conv_cnvrtfrm_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY units_conversion
    ADD CONSTRAINT units_conv_cnvrtfrm_fk FOREIGN KEY (convert_from) REFERENCES physical_units(physical_units) MATCH FULL;


--
-- Name: units_conv_cnvrtto_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY units_conversion
    ADD CONSTRAINT units_conv_cnvrtto_fk FOREIGN KEY (convert_to) REFERENCES physical_units(physical_units) MATCH FULL;


--
-- Name: units_trans_desig_fk; Type: FK CONSTRAINT; Schema: public; Owner: pguser
--

ALTER TABLE ONLY units_translations
    ADD CONSTRAINT units_trans_desig_fk FOREIGN KEY (awips_units_desig) REFERENCES physical_units(physical_units) MATCH FULL;


--
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- Name: boolean_values; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE boolean_values FROM PUBLIC;
REVOKE ALL ON TABLE boolean_values FROM pguser;
GRANT ALL ON TABLE boolean_values TO pguser;
GRANT SELECT ON TABLE boolean_values TO PUBLIC;


--
-- Name: bufr_identifier; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE bufr_identifier FROM PUBLIC;
REVOKE ALL ON TABLE bufr_identifier FROM pguser;
GRANT ALL ON TABLE bufr_identifier TO pguser;
GRANT SELECT ON TABLE bufr_identifier TO PUBLIC;


--
-- Name: cat_values; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE cat_values FROM PUBLIC;
REVOKE ALL ON TABLE cat_values FROM pguser;
GRANT ALL ON TABLE cat_values TO pguser;
GRANT SELECT ON TABLE cat_values TO PUBLIC;


--
-- Name: categorical_ele; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE categorical_ele FROM PUBLIC;
REVOKE ALL ON TABLE categorical_ele FROM pguser;
GRANT ALL ON TABLE categorical_ele TO pguser;
GRANT SELECT ON TABLE categorical_ele TO PUBLIC;


--
-- Name: cli_asos_daily; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE cli_asos_daily FROM PUBLIC;
REVOKE ALL ON TABLE cli_asos_daily FROM pguser;
GRANT ALL ON TABLE cli_asos_daily TO pguser;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE cli_asos_daily TO PUBLIC;


--
-- Name: cli_asos_monthly; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE cli_asos_monthly FROM PUBLIC;
REVOKE ALL ON TABLE cli_asos_monthly FROM pguser;
GRANT ALL ON TABLE cli_asos_monthly TO pguser;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE cli_asos_monthly TO PUBLIC;


--
-- Name: cli_freezedates; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE cli_freezedates FROM PUBLIC;
REVOKE ALL ON TABLE cli_freezedates FROM pguser;
GRANT ALL ON TABLE cli_freezedates TO pguser;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE cli_freezedates TO PUBLIC;


--
-- Name: cli_mon_season_yr; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE cli_mon_season_yr FROM PUBLIC;
REVOKE ALL ON TABLE cli_mon_season_yr FROM pguser;
GRANT ALL ON TABLE cli_mon_season_yr TO pguser;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE cli_mon_season_yr TO PUBLIC;


--
-- Name: cli_sta_setup; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE cli_sta_setup FROM PUBLIC;
REVOKE ALL ON TABLE cli_sta_setup FROM pguser;
GRANT ALL ON TABLE cli_sta_setup TO pguser;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE cli_sta_setup TO PUBLIC;


--
-- Name: climate_day_config; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE climate_day_config FROM PUBLIC;
REVOKE ALL ON TABLE climate_day_config FROM pguser;
GRANT ALL ON TABLE climate_day_config TO pguser;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE climate_day_config TO PUBLIC;


--
-- Name: climate_period; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE climate_period FROM PUBLIC;
REVOKE ALL ON TABLE climate_period FROM pguser;
GRANT ALL ON TABLE climate_period TO pguser;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE climate_period TO PUBLIC;


--
-- Name: climo_dates; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE climo_dates FROM PUBLIC;
REVOKE ALL ON TABLE climo_dates FROM pguser;
GRANT ALL ON TABLE climo_dates TO pguser;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE climo_dates TO PUBLIC;


--
-- Name: contin_int_ele; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE contin_int_ele FROM PUBLIC;
REVOKE ALL ON TABLE contin_int_ele FROM pguser;
GRANT ALL ON TABLE contin_int_ele TO pguser;
GRANT SELECT ON TABLE contin_int_ele TO PUBLIC;


--
-- Name: contin_real_ele; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE contin_real_ele FROM PUBLIC;
REVOKE ALL ON TABLE contin_real_ele FROM pguser;
GRANT ALL ON TABLE contin_real_ele TO pguser;
GRANT SELECT ON TABLE contin_real_ele TO PUBLIC;


--
-- Name: coordinates_2d; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE coordinates_2d FROM PUBLIC;
REVOKE ALL ON TABLE coordinates_2d FROM pguser;
GRANT ALL ON TABLE coordinates_2d TO pguser;
GRANT SELECT ON TABLE coordinates_2d TO PUBLIC;


--
-- Name: daily_climate; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE daily_climate FROM PUBLIC;
REVOKE ALL ON TABLE daily_climate FROM pguser;
GRANT ALL ON TABLE daily_climate TO pguser;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE daily_climate TO PUBLIC;


--
-- Name: data_source; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE data_source FROM PUBLIC;
REVOKE ALL ON TABLE data_source FROM pguser;
GRANT ALL ON TABLE data_source TO pguser;
GRANT SELECT ON TABLE data_source TO PUBLIC;


--
-- Name: data_src_version; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE data_src_version FROM PUBLIC;
REVOKE ALL ON TABLE data_src_version FROM pguser;
GRANT ALL ON TABLE data_src_version TO pguser;
GRANT SELECT ON TABLE data_src_version TO PUBLIC;


--
-- Name: day_climate_extreme; Type: ACL; Schema: public; Owner: postgres
--

REVOKE ALL ON TABLE day_climate_extreme FROM PUBLIC;
REVOKE ALL ON TABLE day_climate_extreme FROM postgres;
GRANT ALL ON TABLE day_climate_extreme TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE day_climate_extreme TO PUBLIC;


--
-- Name: day_climate_norm; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE day_climate_norm FROM PUBLIC;
REVOKE ALL ON TABLE day_climate_norm FROM pguser;
GRANT ALL ON TABLE day_climate_norm TO pguser;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE day_climate_norm TO PUBLIC;


--
-- Name: defined_values; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE defined_values FROM PUBLIC;
REVOKE ALL ON TABLE defined_values FROM pguser;
GRANT ALL ON TABLE defined_values TO pguser;
GRANT SELECT ON TABLE defined_values TO PUBLIC;


--
-- Name: discrete_ele; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE discrete_ele FROM PUBLIC;
REVOKE ALL ON TABLE discrete_ele FROM pguser;
GRANT ALL ON TABLE discrete_ele TO pguser;
GRANT SELECT ON TABLE discrete_ele TO PUBLIC;


--
-- Name: discrete_values; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE discrete_values FROM PUBLIC;
REVOKE ALL ON TABLE discrete_values FROM pguser;
GRANT ALL ON TABLE discrete_values TO pguser;
GRANT SELECT ON TABLE discrete_values TO PUBLIC;


--
-- Name: dqd; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE dqd FROM PUBLIC;
REVOKE ALL ON TABLE dqd FROM pguser;
GRANT ALL ON TABLE dqd TO pguser;
GRANT SELECT ON TABLE dqd TO PUBLIC;


--
-- Name: ele_src_version; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE ele_src_version FROM PUBLIC;
REVOKE ALL ON TABLE ele_src_version FROM pguser;
GRANT ALL ON TABLE ele_src_version TO pguser;
GRANT SELECT ON TABLE ele_src_version TO PUBLIC;


--
-- Name: element_relat; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE element_relat FROM PUBLIC;
REVOKE ALL ON TABLE element_relat FROM pguser;
GRANT ALL ON TABLE element_relat TO pguser;
GRANT SELECT ON TABLE element_relat TO PUBLIC;


--
-- Name: forecast_backup; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE forecast_backup FROM PUBLIC;
REVOKE ALL ON TABLE forecast_backup FROM pguser;
GRANT ALL ON TABLE forecast_backup TO pguser;
GRANT SELECT ON TABLE forecast_backup TO PUBLIC;


--
-- Name: fss_categ_multi; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE fss_categ_multi FROM PUBLIC;
REVOKE ALL ON TABLE fss_categ_multi FROM pguser;
GRANT ALL ON TABLE fss_categ_multi TO pguser;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE fss_categ_multi TO PUBLIC;


--
-- Name: fss_categ_single; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE fss_categ_single FROM PUBLIC;
REVOKE ALL ON TABLE fss_categ_single FROM pguser;
GRANT ALL ON TABLE fss_categ_single TO pguser;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE fss_categ_single TO PUBLIC;


--
-- Name: fss_cloud_layer; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE fss_cloud_layer FROM PUBLIC;
REVOKE ALL ON TABLE fss_cloud_layer FROM pguser;
GRANT ALL ON TABLE fss_cloud_layer TO pguser;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE fss_cloud_layer TO PUBLIC;


--
-- Name: fss_contin_real; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE fss_contin_real FROM PUBLIC;
REVOKE ALL ON TABLE fss_contin_real FROM pguser;
GRANT ALL ON TABLE fss_contin_real TO pguser;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE fss_contin_real TO PUBLIC;


--
-- Name: fss_report; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE fss_report FROM PUBLIC;
REVOKE ALL ON TABLE fss_report FROM pguser;
GRANT ALL ON TABLE fss_report TO pguser;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE fss_report TO PUBLIC;


--
-- Name: fss_wx_period; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE fss_wx_period FROM PUBLIC;
REVOKE ALL ON TABLE fss_wx_period FROM pguser;
GRANT ALL ON TABLE fss_wx_period TO pguser;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE fss_wx_period TO PUBLIC;


--
-- Name: hydromet_element; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE hydromet_element FROM PUBLIC;
REVOKE ALL ON TABLE hydromet_element FROM pguser;
GRANT ALL ON TABLE hydromet_element TO pguser;
GRANT SELECT ON TABLE hydromet_element TO PUBLIC;


--
-- Name: issuance_type; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE issuance_type FROM PUBLIC;
REVOKE ALL ON TABLE issuance_type FROM pguser;
GRANT ALL ON TABLE issuance_type TO pguser;
GRANT SELECT ON TABLE issuance_type TO PUBLIC;


--
-- Name: issuing_office; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE issuing_office FROM PUBLIC;
REVOKE ALL ON TABLE issuing_office FROM pguser;
GRANT ALL ON TABLE issuing_office TO pguser;
GRANT SELECT ON TABLE issuing_office TO PUBLIC;


--
-- Name: station_location; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE station_location FROM PUBLIC;
REVOKE ALL ON TABLE station_location FROM pguser;
GRANT ALL ON TABLE station_location TO pguser;
GRANT SELECT ON TABLE station_location TO PUBLIC;


--
-- Name: local_stations; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE local_stations FROM PUBLIC;
REVOKE ALL ON TABLE local_stations FROM pguser;
GRANT ALL ON TABLE local_stations TO pguser;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE local_stations TO PUBLIC;


--
-- Name: map_proj_coords; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE map_proj_coords FROM PUBLIC;
REVOKE ALL ON TABLE map_proj_coords FROM pguser;
GRANT ALL ON TABLE map_proj_coords TO pguser;
GRANT SELECT ON TABLE map_proj_coords TO PUBLIC;


--
-- Name: mon_climate_norm; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE mon_climate_norm FROM PUBLIC;
REVOKE ALL ON TABLE mon_climate_norm FROM pguser;
GRANT ALL ON TABLE mon_climate_norm TO pguser;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE mon_climate_norm TO PUBLIC;


--
-- Name: mtr_status; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE mtr_status FROM PUBLIC;
REVOKE ALL ON TABLE mtr_status FROM pguser;
GRANT ALL ON TABLE mtr_status TO pguser;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE mtr_status TO PUBLIC;


--
-- Name: phys_ele_relat; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE phys_ele_relat FROM PUBLIC;
REVOKE ALL ON TABLE phys_ele_relat FROM pguser;
GRANT ALL ON TABLE phys_ele_relat TO pguser;
GRANT SELECT ON TABLE phys_ele_relat TO PUBLIC;


--
-- Name: physical_element; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE physical_element FROM PUBLIC;
REVOKE ALL ON TABLE physical_element FROM pguser;
GRANT ALL ON TABLE physical_element TO pguser;
GRANT SELECT ON TABLE physical_element TO PUBLIC;


--
-- Name: physical_units; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE physical_units FROM PUBLIC;
REVOKE ALL ON TABLE physical_units FROM pguser;
GRANT ALL ON TABLE physical_units TO pguser;
GRANT SELECT ON TABLE physical_units TO PUBLIC;


--
-- Name: prod_list; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE prod_list FROM PUBLIC;
REVOKE ALL ON TABLE prod_list FROM pguser;
GRANT ALL ON TABLE prod_list TO pguser;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE prod_list TO PUBLIC;


--
-- Name: product_version; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE product_version FROM PUBLIC;
REVOKE ALL ON TABLE product_version FROM pguser;
GRANT ALL ON TABLE product_version TO pguser;
GRANT SELECT ON TABLE product_version TO PUBLIC;


--
-- Name: relat_type; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE relat_type FROM PUBLIC;
REVOKE ALL ON TABLE relat_type FROM pguser;
GRANT ALL ON TABLE relat_type TO pguser;
GRANT SELECT ON TABLE relat_type TO PUBLIC;


--
-- Name: rpt; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE rpt FROM PUBLIC;
REVOKE ALL ON TABLE rpt FROM pguser;
GRANT ALL ON TABLE rpt TO pguser;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE rpt TO PUBLIC;


--
-- Name: sta_agency_codes; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE sta_agency_codes FROM PUBLIC;
REVOKE ALL ON TABLE sta_agency_codes FROM pguser;
GRANT ALL ON TABLE sta_agency_codes TO pguser;
GRANT SELECT,INSERT,UPDATE ON TABLE sta_agency_codes TO PUBLIC;


--
-- Name: time_zone; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE time_zone FROM PUBLIC;
REVOKE ALL ON TABLE time_zone FROM pguser;
GRANT ALL ON TABLE time_zone TO pguser;
GRANT SELECT ON TABLE time_zone TO PUBLIC;


--
-- Name: units_class; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE units_class FROM PUBLIC;
REVOKE ALL ON TABLE units_class FROM pguser;
GRANT ALL ON TABLE units_class TO pguser;
GRANT SELECT ON TABLE units_class TO PUBLIC;


--
-- Name: units_conversion; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE units_conversion FROM PUBLIC;
REVOKE ALL ON TABLE units_conversion FROM pguser;
GRANT ALL ON TABLE units_conversion TO pguser;
GRANT SELECT ON TABLE units_conversion TO PUBLIC;


--
-- Name: units_system; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE units_system FROM PUBLIC;
REVOKE ALL ON TABLE units_system FROM pguser;
GRANT ALL ON TABLE units_system TO pguser;
GRANT SELECT ON TABLE units_system TO PUBLIC;


--
-- Name: units_translations; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE units_translations FROM PUBLIC;
REVOKE ALL ON TABLE units_translations FROM pguser;
GRANT ALL ON TABLE units_translations TO pguser;
GRANT SELECT ON TABLE units_translations TO PUBLIC;


--
-- Name: weather_category; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE weather_category FROM PUBLIC;
REVOKE ALL ON TABLE weather_category FROM pguser;
GRANT ALL ON TABLE weather_category TO pguser;
GRANT SELECT ON TABLE weather_category TO PUBLIC;


--
-- Name: wmo_state_region; Type: ACL; Schema: public; Owner: pguser
--

REVOKE ALL ON TABLE wmo_state_region FROM PUBLIC;
REVOKE ALL ON TABLE wmo_state_region FROM pguser;
GRANT ALL ON TABLE wmo_state_region TO pguser;
GRANT SELECT ON TABLE wmo_state_region TO PUBLIC;


--
-- PostgreSQL database dump complete
--
