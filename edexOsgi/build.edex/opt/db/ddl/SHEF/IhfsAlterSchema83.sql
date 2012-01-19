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
\c hd_ob83krf
ALTER FUNCTION public.plpgsql_call_handler() OWNER TO postgres;

--
-- TOC entry 2280 (class 2606 OID 19556)
-- Dependencies: 1788 1788 1788 1788 1788 1788
-- Name: adjfactor_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY adjustfactor
    ADD CONSTRAINT adjfactor_pk PRIMARY KEY (lid, pe, dur, ts, extremum);


--
-- TOC entry 2282 (class 2606 OID 19558)
-- Dependencies: 1789 1789
-- Name: admin_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY "admin"
    ADD CONSTRAINT admin_pk PRIMARY KEY (hsa);


--
-- TOC entry 2286 (class 2606 OID 19560)
-- Dependencies: 1792 1792 1792 1792 1792 1792 1792
-- Name: agri_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY agricultural
    ADD CONSTRAINT agri_pk PRIMARY KEY (lid, pe, dur, ts, extremum, obstime);


--
-- TOC entry 2288 (class 2606 OID 19562)
-- Dependencies: 1793 1793 1793 1793 1793 1793 1793 1793 1793 1793 1793
-- Name: alertalarmval_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY alertalarmval
    ADD CONSTRAINT alertalarmval_pk PRIMARY KEY (lid, pe, dur, ts, extremum, probability, validtime, basistime, aa_categ, aa_check);


--
-- TOC entry 2290 (class 2606 OID 19564)
-- Dependencies: 1794 1794 1794 1794 1794 1794 1794 1794 1794
-- Name: arealfcst_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY arealfcst
    ADD CONSTRAINT arealfcst_pk PRIMARY KEY (lid, pe, dur, ts, extremum, probability, validtime, basistime);


--
-- TOC entry 2293 (class 2606 OID 19566)
-- Dependencies: 1795 1795 1795 1795 1795 1795 1795
-- Name: arealobs_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY arealobs
    ADD CONSTRAINT arealobs_pk PRIMARY KEY (lid, pe, dur, ts, extremum, obstime);


--
-- TOC entry 2296 (class 2606 OID 19568)
-- Dependencies: 1796 1796 1796
-- Name: benchmark_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY benchmark
    ADD CONSTRAINT benchmark_pk PRIMARY KEY (lid, bnum);


--
-- TOC entry 2298 (class 2606 OID 19570)
-- Dependencies: 1798 1798
-- Name: colorname_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY colorname
    ADD CONSTRAINT colorname_pk PRIMARY KEY (color_name);


--
-- TOC entry 2300 (class 2606 OID 19572)
-- Dependencies: 1799 1799 1799 1799
-- Name: coloroverlay_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY coloroverlay
    ADD CONSTRAINT coloroverlay_pk PRIMARY KEY (userid, application_name, overlay_type);


--
-- TOC entry 2302 (class 2606 OID 19574)
-- Dependencies: 1800 1800 1800 1800 1800 1800 1800
-- Name: colorvalue_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY colorvalue
    ADD CONSTRAINT colorvalue_pk PRIMARY KEY (userid, application_name, color_use_name, duration, threshold_value, threshold_unit);


--
-- TOC entry 2304 (class 2606 OID 19576)
-- Dependencies: 1801 1801 1801 1801 1801 1801 1801 1801 1801
-- Name: commentvalue_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY commentvalue
    ADD CONSTRAINT commentvalue_pk PRIMARY KEY (lid, pe, dur, ts, extremum, probability, validtime, basistime);


--
-- TOC entry 2306 (class 2606 OID 19578)
-- Dependencies: 1802 1802 1802
-- Name: contact_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY contacts
    ADD CONSTRAINT contact_pk PRIMARY KEY (lid, contact);


--
-- TOC entry 2308 (class 2606 OID 19580)
-- Dependencies: 1803 1803 1803 1803 1803 1803 1803 1803 1803
-- Name: continvalue_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY contingencyvalue
    ADD CONSTRAINT continvalue_pk PRIMARY KEY (lid, pe, dur, ts, extremum, probability, validtime, basistime);


--
-- TOC entry 2310 (class 2606 OID 19582)
-- Dependencies: 1804 1804
-- Name: coopcomms_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY coopcomms
    ADD CONSTRAINT coopcomms_pk PRIMARY KEY (comm);


--
-- TOC entry 2312 (class 2606 OID 19584)
-- Dependencies: 1805 1805
-- Name: cooprecip_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY cooprecip
    ADD CONSTRAINT cooprecip_pk PRIMARY KEY (recip);


--
-- TOC entry 2314 (class 2606 OID 19586)
-- Dependencies: 1806 1806
-- Name: coopspons_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY coopspons
    ADD CONSTRAINT coopspons_pk PRIMARY KEY (spons);


--
-- TOC entry 2316 (class 2606 OID 19588)
-- Dependencies: 1807 1807 1807
-- Name: counties_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY counties
    ADD CONSTRAINT counties_pk PRIMARY KEY (county, state);


--
-- TOC entry 2318 (class 2606 OID 19590)
-- Dependencies: 1808 1808 1808 1808
-- Name: countynum_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY countynum
    ADD CONSTRAINT countynum_pk PRIMARY KEY (lid, state, county);


--
-- TOC entry 2320 (class 2606 OID 19592)
-- Dependencies: 1810 1810 1810 1810
-- Name: countytransmit_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY countytransmit
    ADD CONSTRAINT countytransmit_pk PRIMARY KEY (call_sign, county, state);


--
-- TOC entry 2324 (class 2606 OID 19594)
-- Dependencies: 1811 1811 1811 1811
-- Name: crest_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY crest
    ADD CONSTRAINT crest_pk PRIMARY KEY (lid, datcrst, timcrst);


--
-- TOC entry 2326 (class 2606 OID 19596)
-- Dependencies: 1812 1812 1812 1812 1812
-- Name: curpc_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY curpc
    ADD CONSTRAINT curpc_pk PRIMARY KEY (lid, ts, extremum, obstime);


--
-- TOC entry 2329 (class 2606 OID 19598)
-- Dependencies: 1813 1813 1813 1813 1813 1813
-- Name: curpp_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY curpp
    ADD CONSTRAINT curpp_pk PRIMARY KEY (lid, dur, ts, extremum, obstime);


--
-- TOC entry 2332 (class 2606 OID 19600)
-- Dependencies: 1814 1814 1814 1814
-- Name: dailypp_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY dailypp
    ADD CONSTRAINT dailypp_pk PRIMARY KEY (lid, ts, obstime);


--
-- TOC entry 2334 (class 2606 OID 19602)
-- Dependencies: 1815 1815
-- Name: damtypes_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY damtypes
    ADD CONSTRAINT damtypes_pk PRIMARY KEY ("type");


--
-- TOC entry 2336 (class 2606 OID 19604)
-- Dependencies: 1816 1816 1816 1816
-- Name: datalimits_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY datalimits
    ADD CONSTRAINT datalimits_pk PRIMARY KEY (pe, dur, monthdaystart);


--
-- TOC entry 2338 (class 2606 OID 19606)
-- Dependencies: 1817 1817 1817
-- Name: datum_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY datum
    ADD CONSTRAINT datum_pk PRIMARY KEY (lid, ddate);


--
-- TOC entry 2340 (class 2606 OID 19608)
-- Dependencies: 1818 1818
-- Name: dcp_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY dcp
    ADD CONSTRAINT dcp_pk PRIMARY KEY (lid);


--
-- TOC entry 2342 (class 2606 OID 19610)
-- Dependencies: 1819 1819
-- Name: dcpowner_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY dcpowner
    ADD CONSTRAINT dcpowner_pk PRIMARY KEY ("owner");


--
-- TOC entry 2344 (class 2606 OID 19612)
-- Dependencies: 1820 1820
-- Name: def_issuecrit_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY definingissuecriteria
    ADD CONSTRAINT def_issuecrit_pk PRIMARY KEY (def_issue_crit);


--
-- TOC entry 2346 (class 2606 OID 19614)
-- Dependencies: 1821 1821
-- Name: descrip_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY descrip
    ADD CONSTRAINT descrip_pk PRIMARY KEY (lid);


--
-- TOC entry 2348 (class 2606 OID 19616)
-- Dependencies: 1822 1822 1822
-- Name: dhradapt_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY dhradapt
    ADD CONSTRAINT dhradapt_pk PRIMARY KEY (radid, obstime);


--
-- TOC entry 2350 (class 2606 OID 19618)
-- Dependencies: 1823 1823 1823
-- Name: dhrradar_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY dhrradar
    ADD CONSTRAINT dhrradar_pk PRIMARY KEY (radid, obstime);


--
-- TOC entry 2352 (class 2606 OID 19620)
-- Dependencies: 1824 1824 1824 1824 1824 1824 1824
-- Name: discharge_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY discharge
    ADD CONSTRAINT discharge_pk PRIMARY KEY (lid, pe, dur, ts, extremum, obstime);


--
-- TOC entry 2355 (class 2606 OID 19622)
-- Dependencies: 1825 1825 1825
-- Name: dpaadapt_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY dpaadapt
    ADD CONSTRAINT dpaadapt_pk PRIMARY KEY (radid, obstime);


--
-- TOC entry 2357 (class 2606 OID 19624)
-- Dependencies: 1826 1826 1826
-- Name: dparadar_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY dparadar
    ADD CONSTRAINT dparadar_pk PRIMARY KEY (radid, obstime);


--
-- TOC entry 2359 (class 2606 OID 19626)
-- Dependencies: 1827 1827 1827
-- Name: dspadapt_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY dspadapt
    ADD CONSTRAINT dspadapt_pk PRIMARY KEY (radid, obstime);


--
-- TOC entry 2361 (class 2606 OID 19628)
-- Dependencies: 1828 1828 1828
-- Name: dspradar_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY dspradar
    ADD CONSTRAINT dspradar_pk PRIMARY KEY (radid, obstime);


--
-- TOC entry 2363 (class 2606 OID 19630)
-- Dependencies: 1829 1829 1829
-- Name: eligzon_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY eligzon
    ADD CONSTRAINT eligzon_pk PRIMARY KEY (state, zonenum);


--
-- TOC entry 2365 (class 2606 OID 19632)
-- Dependencies: 1830 1830 1830 1830 1830 1830 1830
-- Name: evaporation_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY evaporation
    ADD CONSTRAINT evaporation_pk PRIMARY KEY (lid, pe, dur, ts, extremum, obstime);


--
-- TOC entry 2374 (class 2606 OID 19634)
-- Dependencies: 1834 1834
-- Name: fcst_horizon_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY fcsthorizon
    ADD CONSTRAINT fcst_horizon_pk PRIMARY KEY (fcst_horizon);


--
-- TOC entry 2369 (class 2606 OID 19636)
-- Dependencies: 1832 1832
-- Name: fcst_method_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY fcstgenmethod
    ADD CONSTRAINT fcst_method_pk PRIMARY KEY (fcst_gen_method);


--
-- TOC entry 2367 (class 2606 OID 19638)
-- Dependencies: 1831 1831 1831 1831 1831 1831 1831 1831 1831
-- Name: fcstdischarge_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY fcstdischarge
    ADD CONSTRAINT fcstdischarge_pk PRIMARY KEY (lid, pe, dur, ts, extremum, probability, validtime, basistime);


--
-- TOC entry 2371 (class 2606 OID 19640)
-- Dependencies: 1833 1833 1833 1833 1833 1833 1833 1833 1833
-- Name: fcstheight_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY fcstheight
    ADD CONSTRAINT fcstheight_pk PRIMARY KEY (lid, pe, dur, ts, extremum, probability, validtime, basistime);


--
-- TOC entry 2376 (class 2606 OID 19642)
-- Dependencies: 1835 1835 1835 1835 1835 1835 1835 1835 1835
-- Name: fcstother_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY fcstother
    ADD CONSTRAINT fcstother_pk PRIMARY KEY (lid, pe, dur, ts, extremum, probability, validtime, basistime);


--
-- TOC entry 2378 (class 2606 OID 19644)
-- Dependencies: 1836 1836 1836 1836 1836 1836 1836 1836 1836
-- Name: fcstprecip_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY fcstprecip
    ADD CONSTRAINT fcstprecip_pk PRIMARY KEY (lid, pe, dur, ts, extremum, probability, validtime, basistime);


--
-- TOC entry 2380 (class 2606 OID 19646)
-- Dependencies: 1837 1837 1837 1837 1837 1837 1837 1837 1837
-- Name: fcstptdeterm_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY fcstptdeterm
    ADD CONSTRAINT fcstptdeterm_pk PRIMARY KEY (lid, snow_method, hydrol_method, reservoir_model, upstream_seg, hydraul_method, def_issue_crit, hours_qpf);


--
-- TOC entry 2382 (class 2606 OID 19648)
-- Dependencies: 1838 1838 1838 1838 1838 1838 1838 1838 1838
-- Name: fcstptesp_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY fcstptesp
    ADD CONSTRAINT fcstptesp_pk PRIMARY KEY (lid, snow_method, hydrol_method, reservoir_model, upstream_seg, hydraul_method, flowtype, fcsttype);


--
-- TOC entry 2384 (class 2606 OID 19650)
-- Dependencies: 1839 1839
-- Name: fcstptservice_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY fcstptservice
    ADD CONSTRAINT fcstptservice_pk PRIMARY KEY (lid);


--
-- TOC entry 2386 (class 2606 OID 19652)
-- Dependencies: 1840 1840 1840 1840 1840 1840 1840
-- Name: fcstptwatsup_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY fcstptwatsup
    ADD CONSTRAINT fcstptwatsup_pk PRIMARY KEY (lid, watsup_method, watsup_coord_agency, frequpd_normal, period_req, watsup_crit);


--
-- TOC entry 2388 (class 2606 OID 19654)
-- Dependencies: 1841 1841 1841 1841 1841 1841 1841 1841 1841
-- Name: fcsttemperature_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY fcsttemperature
    ADD CONSTRAINT fcsttemperature_pk PRIMARY KEY (lid, pe, dur, ts, extremum, probability, validtime, basistime);


--
-- TOC entry 2390 (class 2606 OID 19656)
-- Dependencies: 1842 1842
-- Name: fcsttype_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY fcsttype
    ADD CONSTRAINT fcsttype_pk PRIMARY KEY (fcsttype);


--
-- TOC entry 2392 (class 2606 OID 19658)
-- Dependencies: 1843 1843 1843 1843 1843 1843 1843
-- Name: fishcount_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY fishcount
    ADD CONSTRAINT fishcount_pk PRIMARY KEY (lid, pe, dur, ts, extremum, obstime);


--
-- TOC entry 2394 (class 2606 OID 19660)
-- Dependencies: 1844 1844 1844
-- Name: flood_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY flood
    ADD CONSTRAINT flood_pk PRIMARY KEY (lid, stage);


--
-- TOC entry 2396 (class 2606 OID 19662)
-- Dependencies: 1845 1845
-- Name: floodcat_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY floodcat
    ADD CONSTRAINT floodcat_pk PRIMARY KEY (lid);


--
-- TOC entry 2398 (class 2606 OID 19664)
-- Dependencies: 1846 1846 1846 1846 1846 1846
-- Name: floodstmt_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY floodstmt
    ADD CONSTRAINT floodstmt_pk PRIMARY KEY (lid, impact_value, rf, datestart, dateend);


--
-- TOC entry 2400 (class 2606 OID 19666)
-- Dependencies: 1847 1847 1847
-- Name: floodts_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY floodts
    ADD CONSTRAINT floodts_pk PRIMARY KEY (lid, obstime);


--
-- TOC entry 2402 (class 2606 OID 19668)
-- Dependencies: 1848 1848
-- Name: flowtype_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY flowtype
    ADD CONSTRAINT flowtype_pk PRIMARY KEY (flowtype);


--
-- TOC entry 2410 (class 2606 OID 19670)
-- Dependencies: 1853 1853 1853
-- Name: fpprevprod_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY fpprevprod
    ADD CONSTRAINT fpprevprod_pk PRIMARY KEY (lid, producttime);


--
-- TOC entry 2412 (class 2606 OID 19672)
-- Dependencies: 1854 1854 1854
-- Name: fpprevprodpractice_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY fpprevprodpractice
    ADD CONSTRAINT fpprevprodpractice_pk PRIMARY KEY (lid, producttime);


--
-- TOC entry 2414 (class 2606 OID 19674)
-- Dependencies: 1855 1855
-- Name: freq_upd_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY frequencyupdate
    ADD CONSTRAINT freq_upd_pk PRIMARY KEY (frequency_update);


--
-- TOC entry 2416 (class 2606 OID 19676)
-- Dependencies: 1856 1856 1856 1856
-- Name: gage_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY gage
    ADD CONSTRAINT gage_pk PRIMARY KEY (lid, gbegin, "type");


--
-- TOC entry 2418 (class 2606 OID 19678)
-- Dependencies: 1857 1857
-- Name: gagemaint_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY gagemaint
    ADD CONSTRAINT gagemaint_pk PRIMARY KEY (maint);


--
-- TOC entry 2420 (class 2606 OID 19680)
-- Dependencies: 1858 1858
-- Name: gageowner_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY gageowner
    ADD CONSTRAINT gageowner_pk PRIMARY KEY ("owner");


--
-- TOC entry 2422 (class 2606 OID 19682)
-- Dependencies: 1859 1859
-- Name: gagetype_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY gagetype
    ADD CONSTRAINT gagetype_pk PRIMARY KEY ("type");


--
-- TOC entry 2424 (class 2606 OID 19684)
-- Dependencies: 1860 1860 1860 1860 1860 1860 1860
-- Name: gatedam_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY gatedam
    ADD CONSTRAINT gatedam_pk PRIMARY KEY (lid, pe, dur, ts, extremum, obstime);


--
-- TOC entry 2426 (class 2606 OID 19686)
-- Dependencies: 1861 1861
-- Name: geoarea_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY geoarea
    ADD CONSTRAINT geoarea_pk PRIMARY KEY (area_id);


--
-- TOC entry 2428 (class 2606 OID 19688)
-- Dependencies: 1862 1862 1862 1862 1862 1862 1862
-- Name: ground_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY ground
    ADD CONSTRAINT ground_pk PRIMARY KEY (lid, pe, dur, ts, extremum, obstime);


--
-- TOC entry 2430 (class 2606 OID 19690)
-- Dependencies: 1863 1863 1863 1863 1863 1863 1863
-- Name: height_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY height
    ADD CONSTRAINT height_pk PRIMARY KEY (lid, pe, dur, ts, extremum, obstime);


--
-- TOC entry 2433 (class 2606 OID 19692)
-- Dependencies: 1864 1864 1864 1864
-- Name: hgstation_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY hgstation
    ADD CONSTRAINT hgstation_pk PRIMARY KEY (lid, pe, ts);


--
-- TOC entry 2435 (class 2606 OID 19694)
-- Dependencies: 1865 1865 1865 1865
-- Name: hourlypc_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY hourlypc
    ADD CONSTRAINT hourlypc_pk PRIMARY KEY (lid, ts, obsdate);


--
-- TOC entry 2438 (class 2606 OID 19696)
-- Dependencies: 1866 1866 1866 1866
-- Name: hourlypp_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY hourlypp
    ADD CONSTRAINT hourlypp_pk PRIMARY KEY (lid, ts, obsdate);


--
-- TOC entry 2441 (class 2606 OID 19698)
-- Dependencies: 1867 1867
-- Name: hsa_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY hsa
    ADD CONSTRAINT hsa_pk PRIMARY KEY (hsa);


--
-- TOC entry 2445 (class 2606 OID 19700)
-- Dependencies: 1871 1871
-- Name: hydrologicmethod_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY hydrologicmethod
    ADD CONSTRAINT hydrologicmethod_pk PRIMARY KEY (hydrol_method);


--
-- TOC entry 2447 (class 2606 OID 19702)
-- Dependencies: 1872 1872 1872 1872 1872 1872 1872
-- Name: ice_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY ice
    ADD CONSTRAINT ice_pk PRIMARY KEY (lid, pe, dur, ts, extremum, obstime);


--
-- TOC entry 2449 (class 2606 OID 19704)
-- Dependencies: 1873 1873 1873 1873 1873 1873
-- Name: ingestfilter_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY ingestfilter
    ADD CONSTRAINT ingestfilter_pk PRIMARY KEY (lid, pe, dur, ts, extremum);


--
-- TOC entry 2451 (class 2606 OID 19706)
-- Dependencies: 1874 1874 1874 1874 1874 1874 1874
-- Name: lake_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY lake
    ADD CONSTRAINT lake_pk PRIMARY KEY (lid, pe, dur, ts, extremum, obstime);


--
-- TOC entry 2453 (class 2606 OID 19708)
-- Dependencies: 1876 1876 1876 1876
-- Name: lightning_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY lightning
    ADD CONSTRAINT lightning_pk PRIMARY KEY (x_hgrid, y_hgrid, obstime);


--
-- TOC entry 2455 (class 2606 OID 19710)
-- Dependencies: 1877 1877 1877 1877
-- Name: linesgs_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY linesegs
    ADD CONSTRAINT linesgs_pk PRIMARY KEY (area_id, hrap_row, hrap_beg_col);


--
-- TOC entry 2457 (class 2606 OID 19712)
-- Dependencies: 1878 1878
-- Name: loc_area_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY locarea
    ADD CONSTRAINT loc_area_pk PRIMARY KEY (lid);


--
-- TOC entry 2404 (class 2606 OID 19714)
-- Dependencies: 1849 1849
-- Name: location_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY "location"
    ADD CONSTRAINT location_pk PRIMARY KEY (lid);


--
-- TOC entry 2459 (class 2606 OID 19716)
-- Dependencies: 1880 1880 1880 1880 1880
-- Name: locdatalim_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY locdatalimits
    ADD CONSTRAINT locdatalim_pk PRIMARY KEY (lid, pe, dur, monthdaystart);


--
-- TOC entry 2284 (class 2606 OID 19718)
-- Dependencies: 1790 1790 1790 1790
-- Name: locextagncy_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY locextagency
    ADD CONSTRAINT locextagncy_pk PRIMARY KEY (lid, agency_code, office);


--
-- TOC entry 2461 (class 2606 OID 19720)
-- Dependencies: 1881 1881 1881
-- Name: locimage_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY locimage
    ADD CONSTRAINT locimage_pk PRIMARY KEY (lid, imageid);


--
-- TOC entry 2465 (class 2606 OID 19722)
-- Dependencies: 1888 1888 1888
-- Name: lowwater_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY lowwater
    ADD CONSTRAINT lowwater_pk PRIMARY KEY (lid, lwdat);


--
-- TOC entry 2467 (class 2606 OID 19724)
-- Dependencies: 1889 1889 1889 1889 1889
-- Name: lwstmt_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY lwstmt
    ADD CONSTRAINT lwstmt_pk PRIMARY KEY (lid, pe, lower_value, criteria_rank);


--
-- TOC entry 2469 (class 2606 OID 19726)
-- Dependencies: 1890 1890
-- Name: mbfcstpt_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY mbfcstptlist
    ADD CONSTRAINT mbfcstpt_pk PRIMARY KEY (lid);


--
-- TOC entry 2471 (class 2606 OID 19728)
-- Dependencies: 1894 1894 1894 1894 1894 1894 1894
-- Name: moisture_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY moisture
    ADD CONSTRAINT moisture_pk PRIMARY KEY (lid, pe, dur, ts, extremum, obstime);


--
-- TOC entry 2473 (class 2606 OID 19730)
-- Dependencies: 1895 1895 1895 1895 1895 1895 1895
-- Name: monthly_values_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY monthlyvalues
    ADD CONSTRAINT monthly_values_pk PRIMARY KEY (lid, pe, dur, ts, extremum, adjustment);


--
-- TOC entry 2475 (class 2606 OID 19732)
-- Dependencies: 1896 1896 1896 1896
-- Name: mpe_gage_qc_pkey; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY mpe_gage_qc
    ADD CONSTRAINT mpe_gage_qc_pkey PRIMARY KEY (lid, pe, ts);


--
-- TOC entry 2477 (class 2606 OID 19734)
-- Dependencies: 1897 1897
-- Name: network_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY network
    ADD CONSTRAINT network_pk PRIMARY KEY (network);


--
-- TOC entry 2463 (class 2606 OID 19736)
-- Dependencies: 1884 1884
-- Name: nwrtransmitter_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY nwrtransmitter
    ADD CONSTRAINT nwrtransmitter_pk PRIMARY KEY (call_sign);


--
-- TOC entry 2479 (class 2606 OID 19738)
-- Dependencies: 1898 1898
-- Name: observer_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY observer
    ADD CONSTRAINT observer_pk PRIMARY KEY (lid);


--
-- TOC entry 2481 (class 2606 OID 19740)
-- Dependencies: 1899 1899 1899 1899
-- Name: offnotes_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY officenotes
    ADD CONSTRAINT offnotes_pk PRIMARY KEY (topic, id, postingtime);


--
-- TOC entry 2483 (class 2606 OID 19742)
-- Dependencies: 1900 1900 1900 1900
-- Name: ofsdatatrans_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY ofsdatatrans
    ADD CONSTRAINT ofsdatatrans_pk PRIMARY KEY (pe, dur, extremum);


--
-- TOC entry 2485 (class 2606 OID 19744)
-- Dependencies: 1901 1901 1901 1901
-- Name: ofsstntrans_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY ofsstntrans
    ADD CONSTRAINT ofsstntrans_pk PRIMARY KEY (lid, ofs_data_type, shef_source_code);


--
-- TOC entry 2487 (class 2606 OID 19746)
-- Dependencies: 1902 1902 1902 1902 1902 1902 1902 1902 1902 1902
-- Name: pairedvalue_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY pairedvalue
    ADD CONSTRAINT pairedvalue_pk PRIMARY KEY (lid, pe, dur, ts, extremum, probability, validtime, basistime, ref_value);


--
-- TOC entry 2489 (class 2606 OID 19748)
-- Dependencies: 1903 1903 1903
-- Name: perflog_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY perflog
    ADD CONSTRAINT perflog_pk PRIMARY KEY (process, start_time);


--
-- TOC entry 2536 (class 2606 OID 19750)
-- Dependencies: 1927 1927
-- Name: periodreq_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY requiredperiod
    ADD CONSTRAINT periodreq_pk PRIMARY KEY (period_req);


--
-- TOC entry 2491 (class 2606 OID 19752)
-- Dependencies: 1904 1904
-- Name: pointdata_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY pointdatapresets
    ADD CONSTRAINT pointdata_pk PRIMARY KEY (preset_id);


--
-- TOC entry 2493 (class 2606 OID 19754)
-- Dependencies: 1905 1905
-- Name: post_processor_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY postprocessor
    ADD CONSTRAINT post_processor_pk PRIMARY KEY (post_processor);


--
-- TOC entry 2495 (class 2606 OID 19756)
-- Dependencies: 1906 1906 1906 1906 1906 1906 1906
-- Name: power_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY power
    ADD CONSTRAINT power_pk PRIMARY KEY (lid, pe, dur, ts, extremum, obstime);


--
-- TOC entry 2497 (class 2606 OID 19758)
-- Dependencies: 1907 1907 1907 1907 1907 1907 1907
-- Name: pressure_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY pressure
    ADD CONSTRAINT pressure_pk PRIMARY KEY (lid, pe, dur, ts, extremum, obstime);


--
-- TOC entry 2501 (class 2606 OID 19760)
-- Dependencies: 1910 1910 1910 1910 1910 1910 1910
-- Name: procvalue_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY procvalue
    ADD CONSTRAINT procvalue_pk PRIMARY KEY (lid, pe, dur, ts, extremum, obstime);


--
-- TOC entry 2503 (class 2606 OID 19762)
-- Dependencies: 1911 1911 1911 1911 1911
-- Name: productlink_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY productlink
    ADD CONSTRAINT productlink_pk PRIMARY KEY (lid, product_id, producttime, postingtime);


--
-- TOC entry 2505 (class 2606 OID 19764)
-- Dependencies: 1912 1912
-- Name: proximity_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY proximity
    ADD CONSTRAINT proximity_pk PRIMARY KEY (proximity);


--
-- TOC entry 2507 (class 2606 OID 19766)
-- Dependencies: 1913 1913 1913
-- Name: pseudogagval_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY pseudogageval
    ADD CONSTRAINT pseudogagval_pk PRIMARY KEY (pseudo_gage_id, obstime);


--
-- TOC entry 2509 (class 2606 OID 19768)
-- Dependencies: 1914 1914 1914 1914
-- Name: pub_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY pub
    ADD CONSTRAINT pub_pk PRIMARY KEY (lid, pbegin, ppub);


--
-- TOC entry 2511 (class 2606 OID 19770)
-- Dependencies: 1915 1915
-- Name: purgedyndata_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY purgedyndata
    ADD CONSTRAINT purgedyndata_pk PRIMARY KEY (table_name);


--
-- TOC entry 2513 (class 2606 OID 19772)
-- Dependencies: 1917 1917
-- Name: radarloc_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY radarloc
    ADD CONSTRAINT radarloc_pk PRIMARY KEY (radid);


--
-- TOC entry 2515 (class 2606 OID 19774)
-- Dependencies: 1918 1918
-- Name: radarresp_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY radarresp
    ADD CONSTRAINT radarresp_pk PRIMARY KEY (radid);


--
-- TOC entry 2517 (class 2606 OID 19776)
-- Dependencies: 1919 1919 1919 1919 1919 1919 1919
-- Name: radiation_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY radiation
    ADD CONSTRAINT radiation_pk PRIMARY KEY (lid, pe, dur, ts, extremum, obstime);


--
-- TOC entry 2519 (class 2606 OID 19778)
-- Dependencies: 1920 1920 1920
-- Name: rating_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY rating
    ADD CONSTRAINT rating_pk PRIMARY KEY (lid, stage);


--
-- TOC entry 2521 (class 2606 OID 19780)
-- Dependencies: 1921 1921 1921
-- Name: ratingshift_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY ratingshift
    ADD CONSTRAINT ratingshift_pk PRIMARY KEY (lid, date);


--
-- TOC entry 2523 (class 2606 OID 19782)
-- Dependencies: 1922 1922 1922 1922 1922
-- Name: rawpc_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY rawpc
    ADD CONSTRAINT rawpc_pk PRIMARY KEY (lid, ts, extremum, obstime);


--
-- TOC entry 2527 (class 2606 OID 19784)
-- Dependencies: 1923 1923 1923 1923 1923 1923 1923
-- Name: rawpother_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY rawpother
    ADD CONSTRAINT rawpother_pk PRIMARY KEY (lid, pe, dur, ts, extremum, obstime);


--
-- TOC entry 2530 (class 2606 OID 19786)
-- Dependencies: 1924 1924 1924 1924 1924 1924
-- Name: rawpp_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY rawpp
    ADD CONSTRAINT rawpp_pk PRIMARY KEY (lid, dur, ts, extremum, obstime);


--
-- TOC entry 2532 (class 2606 OID 19788)
-- Dependencies: 1925 1925 1925
-- Name: refer_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY refer
    ADD CONSTRAINT refer_pk PRIMARY KEY (lid, reference);


--
-- TOC entry 2534 (class 2606 OID 19790)
-- Dependencies: 1926 1926 1926 1926 1926 1926 1926 1926 1926 1926
-- Name: rejecteddata_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY rejecteddata
    ADD CONSTRAINT rejecteddata_pk PRIMARY KEY (lid, pe, dur, ts, extremum, probability, validtime, basistime, postingtime);


--
-- TOC entry 2542 (class 2606 OID 19792)
-- Dependencies: 1930 1930
-- Name: res_model_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY reservoirmodel
    ADD CONSTRAINT res_model_pk PRIMARY KEY (reservoir_model);


--
-- TOC entry 2538 (class 2606 OID 19794)
-- Dependencies: 1928 1928 1928
-- Name: rescap_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY rescap
    ADD CONSTRAINT rescap_pk PRIMARY KEY (lid, elev);


--
-- TOC entry 2540 (class 2606 OID 19796)
-- Dependencies: 1929 1929
-- Name: reservoir_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY reservoir
    ADD CONSTRAINT reservoir_pk PRIMARY KEY (lid);


--
-- TOC entry 2544 (class 2606 OID 19798)
-- Dependencies: 1931 1931
-- Name: resowner_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY resowner
    ADD CONSTRAINT resowner_pk PRIMARY KEY ("owner");


--
-- TOC entry 2546 (class 2606 OID 19800)
-- Dependencies: 1932 1932
-- Name: rfc_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY rfc
    ADD CONSTRAINT rfc_pk PRIMARY KEY (rfc);


--
-- TOC entry 2548 (class 2606 OID 19802)
-- Dependencies: 1933 1933
-- Name: rivergroup_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY rivermongroup
    ADD CONSTRAINT rivergroup_pk PRIMARY KEY (group_id);


--
-- TOC entry 2550 (class 2606 OID 19804)
-- Dependencies: 1934 1934
-- Name: riverlocation_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY rivermonlocation
    ADD CONSTRAINT riverlocation_pk PRIMARY KEY (lid);


--
-- TOC entry 2406 (class 2606 OID 19806)
-- Dependencies: 1850 1850
-- Name: riverstat_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY riverstat
    ADD CONSTRAINT riverstat_pk PRIMARY KEY (lid);


--
-- TOC entry 2552 (class 2606 OID 19808)
-- Dependencies: 1935 1935 1935 1935
-- Name: riverstatus_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY riverstatus
    ADD CONSTRAINT riverstatus_pk PRIMARY KEY (lid, pe, ts);


--
-- TOC entry 2554 (class 2606 OID 19810)
-- Dependencies: 1936 1936
-- Name: routingmethod_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY routingmethod
    ADD CONSTRAINT routingmethod_pk PRIMARY KEY (hydraul_method);


--
-- TOC entry 2556 (class 2606 OID 19812)
-- Dependencies: 1937 1937
-- Name: rpffcstgroup_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY rpffcstgroup
    ADD CONSTRAINT rpffcstgroup_pk PRIMARY KEY (group_id);


--
-- TOC entry 2408 (class 2606 OID 19814)
-- Dependencies: 1851 1851
-- Name: rpffcstpoint_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY rpffcstpoint
    ADD CONSTRAINT rpffcstpoint_pk PRIMARY KEY (lid);


--
-- TOC entry 2558 (class 2606 OID 19816)
-- Dependencies: 1939 1939 1939 1939 1939
-- Name: rwbiasdyn_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY rwbiasdyn
    ADD CONSTRAINT rwbiasdyn_pk PRIMARY KEY (radid, office_id, obstime, memspan_ind);


--
-- TOC entry 2560 (class 2606 OID 19818)
-- Dependencies: 1940 1940
-- Name: rwbiasstat_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY rwbiasstat
    ADD CONSTRAINT rwbiasstat_pk PRIMARY KEY (office_id);


--
-- TOC entry 2562 (class 2606 OID 19820)
-- Dependencies: 1942 1942
-- Name: rwprefs_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY rwprefs
    ADD CONSTRAINT rwprefs_pk PRIMARY KEY (userid);


--
-- TOC entry 2564 (class 2606 OID 19822)
-- Dependencies: 1943 1943 1943
-- Name: rwradarresult_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY rwradarresult
    ADD CONSTRAINT rwradarresult_pk PRIMARY KEY (radid, obstime);


--
-- TOC entry 2566 (class 2606 OID 19824)
-- Dependencies: 1944 1944 1944
-- Name: rwresult_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY rwresult
    ADD CONSTRAINT rwresult_pk PRIMARY KEY (rfc, obstime);


--
-- TOC entry 2568 (class 2606 OID 19826)
-- Dependencies: 1946 1946
-- Name: s3postanalprefs_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY s3postanalprefs
    ADD CONSTRAINT s3postanalprefs_pk PRIMARY KEY (userid);


--
-- TOC entry 2571 (class 2606 OID 19828)
-- Dependencies: 1947 1947 1947 1947
-- Name: sac_sma_params_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY sacsmaparams
    ADD CONSTRAINT sac_sma_params_pk PRIMARY KEY (basin_id, source, validtime);


--
-- TOC entry 2573 (class 2606 OID 19830)
-- Dependencies: 1948 1948 1948 1948
-- Name: sac_sma_state_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY sacsmastate
    ADD CONSTRAINT sac_sma_state_pk PRIMARY KEY (basin_id, source, validtime);


--
-- TOC entry 2576 (class 2606 OID 19832)
-- Dependencies: 1950 1950
-- Name: servtype_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY servicetype
    ADD CONSTRAINT servtype_pk PRIMARY KEY (service_type);


--
-- TOC entry 2578 (class 2606 OID 19834)
-- Dependencies: 1951 1951
-- Name: shefdur_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY shefdur
    ADD CONSTRAINT shefdur_pk PRIMARY KEY (dur);


--
-- TOC entry 2580 (class 2606 OID 19836)
-- Dependencies: 1952 1952
-- Name: shefex_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY shefex
    ADD CONSTRAINT shefex_pk PRIMARY KEY (extremum);


--
-- TOC entry 2582 (class 2606 OID 19838)
-- Dependencies: 1953 1953
-- Name: shefpe_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY shefpe
    ADD CONSTRAINT shefpe_pk PRIMARY KEY (pe);


--
-- TOC entry 2584 (class 2606 OID 19840)
-- Dependencies: 1954 1954 1954
-- Name: shefpetrans_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY shefpetrans
    ADD CONSTRAINT shefpetrans_pk PRIMARY KEY (pe, coded_value);


--
-- TOC entry 2586 (class 2606 OID 19842)
-- Dependencies: 1955 1955
-- Name: shefprob_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY shefprob
    ADD CONSTRAINT shefprob_pk PRIMARY KEY (probcode);


--
-- TOC entry 2588 (class 2606 OID 19844)
-- Dependencies: 1956 1956
-- Name: shefqc_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY shefqc
    ADD CONSTRAINT shefqc_pk PRIMARY KEY (shef_qual_code);


--
-- TOC entry 2590 (class 2606 OID 19846)
-- Dependencies: 1957 1957
-- Name: shefts_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY shefts
    ADD CONSTRAINT shefts_pk PRIMARY KEY (ts);


--
-- TOC entry 2592 (class 2606 OID 19848)
-- Dependencies: 1958 1958 1958 1958 1958 1958 1958
-- Name: snow_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY snow
    ADD CONSTRAINT snow_pk PRIMARY KEY (lid, pe, dur, ts, extremum, obstime);


--
-- TOC entry 2594 (class 2606 OID 19850)
-- Dependencies: 1959 1959
-- Name: snowmethod_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY snowmethod
    ADD CONSTRAINT snowmethod_pk PRIMARY KEY (snow_method);


--
-- TOC entry 2596 (class 2606 OID 19852)
-- Dependencies: 1960 1960
-- Name: sshp_config_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY sshpconfig
    ADD CONSTRAINT sshp_config_pk PRIMARY KEY (lid);


--
-- TOC entry 2598 (class 2606 OID 19854)
-- Dependencies: 1961 1961
-- Name: state_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY state
    ADD CONSTRAINT state_pk PRIMARY KEY (state);


--
-- TOC entry 2443 (class 2606 OID 19856)
-- Dependencies: 1868 1868
-- Name: stnclass_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY stnclass
    ADD CONSTRAINT stnclass_pk PRIMARY KEY (lid);


--
-- TOC entry 2600 (class 2606 OID 19858)
-- Dependencies: 1966 1966
-- Name: telem_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY telem
    ADD CONSTRAINT telem_pk PRIMARY KEY (lid);


--
-- TOC entry 2602 (class 2606 OID 19860)
-- Dependencies: 1967 1967
-- Name: telmowner_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY telmowner
    ADD CONSTRAINT telmowner_pk PRIMARY KEY ("owner");


--
-- TOC entry 2604 (class 2606 OID 19862)
-- Dependencies: 1968 1968
-- Name: telmpayor_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY telmpayor
    ADD CONSTRAINT telmpayor_pk PRIMARY KEY (payor);


--
-- TOC entry 2606 (class 2606 OID 19864)
-- Dependencies: 1969 1969
-- Name: telmtype_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY telmtype
    ADD CONSTRAINT telmtype_pk PRIMARY KEY ("type");


--
-- TOC entry 2609 (class 2606 OID 19866)
-- Dependencies: 1970 1970 1970 1970 1970 1970 1970
-- Name: temperature_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY temperature
    ADD CONSTRAINT temperature_pk PRIMARY KEY (lid, pe, dur, ts, extremum, obstime);


--
-- TOC entry 2499 (class 2606 OID 19868)
-- Dependencies: 1908 1908 1908 1908
-- Name: textproduct_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY textproduct
    ADD CONSTRAINT textproduct_pk PRIMARY KEY (product_id, producttime, postingtime);


--
-- TOC entry 2611 (class 2606 OID 19870)
-- Dependencies: 1971 1971
-- Name: timezone_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY timezone
    ADD CONSTRAINT timezone_pk PRIMARY KEY (tzone);


--
-- TOC entry 2614 (class 2606 OID 19872)
-- Dependencies: 1972 1972 1972 1972 1972 1972
-- Name: unitgraph_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY unitgraph
    ADD CONSTRAINT unitgraph_pk PRIMARY KEY (lid, area_id, model, dur, ordinal);


--
-- TOC entry 2616 (class 2606 OID 19874)
-- Dependencies: 1973 1973
-- Name: unkstn_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY unkstn
    ADD CONSTRAINT unkstn_pk PRIMARY KEY (lid);


--
-- TOC entry 2618 (class 2606 OID 19876)
-- Dependencies: 1975 1975
-- Name: userprefs_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY userprefs
    ADD CONSTRAINT userprefs_pk PRIMARY KEY (userid);


--
-- TOC entry 2620 (class 2606 OID 19878)
-- Dependencies: 1977 1977
-- Name: vtecaction_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY vtecaction
    ADD CONSTRAINT vtecaction_pk PRIMARY KEY ("action");


--
-- TOC entry 2622 (class 2606 OID 19880)
-- Dependencies: 1978 1978
-- Name: vteccause_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY vteccause
    ADD CONSTRAINT vteccause_pk PRIMARY KEY (immed_cause);


--
-- TOC entry 2624 (class 2606 OID 19882)
-- Dependencies: 1979 1979 1979 1979
-- Name: vtecevent_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY vtecevent
    ADD CONSTRAINT vtecevent_pk PRIMARY KEY (geoid, product_id, producttime);


--
-- TOC entry 2626 (class 2606 OID 19884)
-- Dependencies: 1980 1980
-- Name: vtecphenom_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY vtecphenom
    ADD CONSTRAINT vtecphenom_pk PRIMARY KEY (phenom);


--
-- TOC entry 2628 (class 2606 OID 19886)
-- Dependencies: 1981 1981 1981 1981
-- Name: vtecpractice_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY vtecpractice
    ADD CONSTRAINT vtecpractice_pk PRIMARY KEY (geoid, product_id, producttime);


--
-- TOC entry 2630 (class 2606 OID 19888)
-- Dependencies: 1982 1982
-- Name: vtecrecord_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY vtecrecord
    ADD CONSTRAINT vtecrecord_pk PRIMARY KEY (record);


--
-- TOC entry 2632 (class 2606 OID 19890)
-- Dependencies: 1983 1983
-- Name: vtecsever_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY vtecsever
    ADD CONSTRAINT vtecsever_pk PRIMARY KEY (severity);


--
-- TOC entry 2634 (class 2606 OID 19892)
-- Dependencies: 1984 1984
-- Name: vtecsignif_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY vtecsignif
    ADD CONSTRAINT vtecsignif_pk PRIMARY KEY (signif);


--
-- TOC entry 2636 (class 2606 OID 19894)
-- Dependencies: 1985 1985 1985 1985 1985 1985 1985
-- Name: waterqual_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY waterquality
    ADD CONSTRAINT waterqual_pk PRIMARY KEY (lid, pe, dur, ts, extremum, obstime);


--
-- TOC entry 2638 (class 2606 OID 19896)
-- Dependencies: 1986 1986
-- Name: watsup_agency_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY watsupcoordagency
    ADD CONSTRAINT watsup_agency_pk PRIMARY KEY (watsup_coord_agency);


--
-- TOC entry 2640 (class 2606 OID 19898)
-- Dependencies: 1987 1987
-- Name: watsup_crit_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY watsupcriterion
    ADD CONSTRAINT watsup_crit_pk PRIMARY KEY (watsup_criterion);


--
-- TOC entry 2642 (class 2606 OID 19900)
-- Dependencies: 1988 1988
-- Name: watsup_method_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY watsupmethod
    ADD CONSTRAINT watsup_method_pk PRIMARY KEY (watsup_method);


--
-- TOC entry 2644 (class 2606 OID 19902)
-- Dependencies: 1989 1989
-- Name: watsup_resp_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY watsuprespagency
    ADD CONSTRAINT watsup_resp_pk PRIMARY KEY (watsup_resp_agency);


--
-- TOC entry 2646 (class 2606 OID 19904)
-- Dependencies: 1990 1990 1990 1990 1990 1990 1990
-- Name: weather_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY weather
    ADD CONSTRAINT weather_pk PRIMARY KEY (lid, pe, dur, ts, extremum, obstime);


--
-- TOC entry 2648 (class 2606 OID 19906)
-- Dependencies: 1991 1991
-- Name: wfo_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY wfo
    ADD CONSTRAINT wfo_pk PRIMARY KEY (wfo);


--
-- TOC entry 2650 (class 2606 OID 19908)
-- Dependencies: 1992 1992 1992 1992 1992 1992 1992
-- Name: wind_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY wind
    ADD CONSTRAINT wind_pk PRIMARY KEY (lid, pe, dur, ts, extremum, obstime);


--
-- TOC entry 2652 (class 2606 OID 19910)
-- Dependencies: 1993 1993 1993 1993 1993 1993 1993
-- Name: yunique_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY yunique
    ADD CONSTRAINT yunique_pk PRIMARY KEY (lid, pe, dur, ts, extremum, obstime);


--
-- TOC entry 2654 (class 2606 OID 19912)
-- Dependencies: 1994 1994 1994 1994
-- Name: zonenum_pk; Type: CONSTRAINT; Schema: public; Owner: awips; Tablespace: 
--

ALTER TABLE ONLY zonenum
    ADD CONSTRAINT zonenum_pk PRIMARY KEY (lid, state, zonenum);


--
-- TOC entry 2291 (class 1259 OID 19913)
-- Dependencies: 1794
-- Name: arealfcst_time_ind; Type: INDEX; Schema: public; Owner: awips; Tablespace: 
--

CREATE INDEX arealfcst_time_ind ON arealfcst USING btree (basistime);


--
-- TOC entry 2294 (class 1259 OID 19914)
-- Dependencies: 1795
-- Name: arealobs_time_ind; Type: INDEX; Schema: public; Owner: awips; Tablespace: 
--

CREATE INDEX arealobs_time_ind ON arealobs USING btree (obstime);


--
-- TOC entry 2321 (class 1259 OID 19915)
-- Dependencies: 1811 1811 1811
-- Name: crest_ind1; Type: INDEX; Schema: public; Owner: awips; Tablespace: 
--

CREATE INDEX crest_ind1 ON crest USING btree (lid, stage, datcrst);


--
-- TOC entry 2322 (class 1259 OID 19916)
-- Dependencies: 1811 1811
-- Name: crest_ind2; Type: INDEX; Schema: public; Owner: awips; Tablespace: 
--

CREATE INDEX crest_ind2 ON crest USING btree (lid, stage);


--
-- TOC entry 2327 (class 1259 OID 19917)
-- Dependencies: 1812
-- Name: curpc_time_ind; Type: INDEX; Schema: public; Owner: awips; Tablespace: 
--

CREATE INDEX curpc_time_ind ON curpc USING btree (obstime);


--
-- TOC entry 2330 (class 1259 OID 19918)
-- Dependencies: 1813
-- Name: curpptime_ind; Type: INDEX; Schema: public; Owner: awips; Tablespace: 
--

CREATE INDEX curpptime_ind ON curpp USING btree (obstime);


--
-- TOC entry 2353 (class 1259 OID 19919)
-- Dependencies: 1824
-- Name: discharge_time_ind; Type: INDEX; Schema: public; Owner: awips; Tablespace: 
--

CREATE INDEX discharge_time_ind ON discharge USING btree (obstime);


--
-- TOC entry 2372 (class 1259 OID 19920)
-- Dependencies: 1833
-- Name: fheight_time_ind; Type: INDEX; Schema: public; Owner: awips; Tablespace: 
--

CREATE INDEX fheight_time_ind ON fcstheight USING btree (basistime);


--
-- TOC entry 2431 (class 1259 OID 19921)
-- Dependencies: 1863
-- Name: height_time_ind; Type: INDEX; Schema: public; Owner: awips; Tablespace: 
--

CREATE INDEX height_time_ind ON height USING btree (obstime);


--
-- TOC entry 2436 (class 1259 OID 19922)
-- Dependencies: 1865
-- Name: hourlypc_time_ind; Type: INDEX; Schema: public; Owner: awips; Tablespace: 
--

CREATE INDEX hourlypc_time_ind ON hourlypc USING btree (obsdate);


--
-- TOC entry 2439 (class 1259 OID 19923)
-- Dependencies: 1866
-- Name: hourlypp_time_ind; Type: INDEX; Schema: public; Owner: awips; Tablespace: 
--

CREATE INDEX hourlypp_time_ind ON hourlypp USING btree (obsdate);


--
-- TOC entry 2524 (class 1259 OID 19924)
-- Dependencies: 1922
-- Name: rawpc_time_ind; Type: INDEX; Schema: public; Owner: awips; Tablespace: 
--

CREATE INDEX rawpc_time_ind ON rawpc USING btree (obstime);


--
-- TOC entry 2525 (class 1259 OID 19925)
-- Dependencies: 1923
-- Name: rawpother_ind; Type: INDEX; Schema: public; Owner: awips; Tablespace: 
--

CREATE INDEX rawpother_ind ON rawpother USING btree (obstime);


--
-- TOC entry 2528 (class 1259 OID 19926)
-- Dependencies: 1924
-- Name: rawpp_ind; Type: INDEX; Schema: public; Owner: awips; Tablespace: 
--

CREATE INDEX rawpp_ind ON rawpp USING btree (obstime);


--
-- TOC entry 2569 (class 1259 OID 19927)
-- Dependencies: 1947
-- Name: sac_params_timeind; Type: INDEX; Schema: public; Owner: awips; Tablespace: 
--

CREATE INDEX sac_params_timeind ON sacsmaparams USING btree (validtime);


--
-- TOC entry 2574 (class 1259 OID 19928)
-- Dependencies: 1948
-- Name: sac_state_time_ind; Type: INDEX; Schema: public; Owner: awips; Tablespace: 
--

CREATE INDEX sac_state_time_ind ON sacsmastate USING btree (validtime);


--
-- TOC entry 2607 (class 1259 OID 19929)
-- Dependencies: 1970
-- Name: temp_time_ind; Type: INDEX; Schema: public; Owner: awips; Tablespace: 
--

CREATE INDEX temp_time_ind ON temperature USING btree (obstime);


--
-- TOC entry 2612 (class 1259 OID 19930)
-- Dependencies: 1972 1972 1972 1972
-- Name: unitgraph_idx; Type: INDEX; Schema: public; Owner: awips; Tablespace: 
--

CREATE INDEX unitgraph_idx ON unitgraph USING btree (lid, area_id, model, dur);


--
-- TOC entry 2778 (class 2620 OID 19931)
-- Dependencies: 1824 33
-- Name: obs_discharge_ins; Type: TRIGGER; Schema: public; Owner: awips
--

CREATE TRIGGER obs_discharge_ins
    AFTER INSERT ON discharge
    FOR EACH ROW
    EXECUTE PROCEDURE obs_river_ins();




--
-- TOC entry 2779 (class 2620 OID 19932)
-- Dependencies: 34 1824
-- Name: obs_discharge_upd; Type: TRIGGER; Schema: public; Owner: awips
--

CREATE TRIGGER obs_discharge_upd
    AFTER UPDATE ON discharge
    FOR EACH ROW
    EXECUTE PROCEDURE obs_river_upd();




--
-- TOC entry 2780 (class 2620 OID 19933)
-- Dependencies: 1863 33
-- Name: obs_height_ins; Type: TRIGGER; Schema: public; Owner: awips
--

CREATE TRIGGER obs_height_ins
    AFTER INSERT ON height
    FOR EACH ROW
    EXECUTE PROCEDURE obs_river_ins();




--
-- TOC entry 2781 (class 2620 OID 19934)
-- Dependencies: 34 1863
-- Name: obs_height_upd; Type: TRIGGER; Schema: public; Owner: awips
--

CREATE TRIGGER obs_height_upd
    AFTER UPDATE ON height
    FOR EACH ROW
    EXECUTE PROCEDURE obs_river_upd();




--
-- TOC entry 2782 (class 2620 OID 19935)
-- Dependencies: 1922 29
-- Name: obs_rawpc_ins; Type: TRIGGER; Schema: public; Owner: awips
--

CREATE TRIGGER obs_rawpc_ins
    AFTER INSERT ON rawpc
    FOR EACH ROW
    EXECUTE PROCEDURE obs_rawpc_ins();




--
-- TOC entry 2783 (class 2620 OID 19936)
-- Dependencies: 30 1922
-- Name: obs_rawpc_upd; Type: TRIGGER; Schema: public; Owner: awips
--

CREATE TRIGGER obs_rawpc_upd
    AFTER UPDATE ON rawpc
    FOR EACH ROW
    EXECUTE PROCEDURE obs_rawpc_upd();




--
-- TOC entry 2784 (class 2620 OID 19937)
-- Dependencies: 1924 31
-- Name: obs_rawpp_ins; Type: TRIGGER; Schema: public; Owner: awips
--

CREATE TRIGGER obs_rawpp_ins
    AFTER INSERT ON rawpp
    FOR EACH ROW
    EXECUTE PROCEDURE obs_rawpp_ins();




--
-- TOC entry 2785 (class 2620 OID 19938)
-- Dependencies: 1924 32
-- Name: obs_rawpp_upd; Type: TRIGGER; Schema: public; Owner: awips
--

CREATE TRIGGER obs_rawpp_upd
    AFTER UPDATE ON rawpp
    FOR EACH ROW
    EXECUTE PROCEDURE obs_rawpp_upd();




--
-- TOC entry 2655 (class 2606 OID 19939)
-- Dependencies: 1951 2577 1788
-- Name: adjfactor_dur_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY adjustfactor
    ADD CONSTRAINT adjfactor_dur_fk FOREIGN KEY (dur) REFERENCES shefdur(dur) MATCH FULL;


--
-- TOC entry 2656 (class 2606 OID 19944)
-- Dependencies: 2579 1788 1952
-- Name: adjfactor_ex_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY adjustfactor
    ADD CONSTRAINT adjfactor_ex_fk FOREIGN KEY (extremum) REFERENCES shefex(extremum) MATCH FULL;


--
-- TOC entry 2657 (class 2606 OID 19949)
-- Dependencies: 1953 1788 2581
-- Name: adjfactor_pe_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY adjustfactor
    ADD CONSTRAINT adjfactor_pe_fk FOREIGN KEY (pe) REFERENCES shefpe(pe) MATCH FULL;


--
-- TOC entry 2658 (class 2606 OID 19954)
-- Dependencies: 1957 2589 1788
-- Name: adjfactor_ts_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY adjustfactor
    ADD CONSTRAINT adjfactor_ts_fk FOREIGN KEY (ts) REFERENCES shefts(ts) MATCH FULL;


--
-- TOC entry 2660 (class 2606 OID 19959)
-- Dependencies: 1796 2405 1850
-- Name: benchmark_rvr_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY benchmark
    ADD CONSTRAINT benchmark_rvr_fk FOREIGN KEY (lid) REFERENCES riverstat(lid) MATCH FULL;


--
-- TOC entry 2661 (class 2606 OID 19964)
-- Dependencies: 1799 2297 1798
-- Name: colorovr_name_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY coloroverlay
    ADD CONSTRAINT colorovr_name_fk FOREIGN KEY (color_name) REFERENCES colorname(color_name) MATCH FULL;


--
-- TOC entry 2662 (class 2606 OID 19969)
-- Dependencies: 1798 2297 1800
-- Name: colorval_name_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY colorvalue
    ADD CONSTRAINT colorval_name_fk FOREIGN KEY (color_name) REFERENCES colorname(color_name) MATCH FULL;


--
-- TOC entry 2663 (class 2606 OID 19974)
-- Dependencies: 2403 1802 1849
-- Name: contact_loc_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY contacts
    ADD CONSTRAINT contact_loc_fk FOREIGN KEY (lid) REFERENCES location(lid) MATCH FULL;


--
-- TOC entry 2664 (class 2606 OID 19979)
-- Dependencies: 1991 1807 2647
-- Name: counties_primbk_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY counties
    ADD CONSTRAINT counties_primbk_fk FOREIGN KEY (primary_back) REFERENCES wfo(wfo) MATCH FULL;


--
-- TOC entry 2665 (class 2606 OID 19984)
-- Dependencies: 1991 1807 2647
-- Name: counties_secbk_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY counties
    ADD CONSTRAINT counties_secbk_fk FOREIGN KEY (secondary_back) REFERENCES wfo(wfo) MATCH FULL;


--
-- TOC entry 2666 (class 2606 OID 19989)
-- Dependencies: 1807 2597 1961
-- Name: counties_state_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY counties
    ADD CONSTRAINT counties_state_fk FOREIGN KEY (state) REFERENCES state(state) MATCH FULL;


--
-- TOC entry 2667 (class 2606 OID 19994)
-- Dependencies: 2647 1991 1807
-- Name: counties_wfo_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY counties
    ADD CONSTRAINT counties_wfo_fk FOREIGN KEY (wfo) REFERENCES wfo(wfo) MATCH FULL;


--
-- TOC entry 2671 (class 2606 OID 19999)
-- Dependencies: 2462 1884 1810
-- Name: county_transmit_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY countytransmit
    ADD CONSTRAINT county_transmit_fk FOREIGN KEY (call_sign) REFERENCES nwrtransmitter(call_sign) MATCH FULL;


--
-- TOC entry 2669 (class 2606 OID 20004)
-- Dependencies: 1807 1808 1808 2315 1807
-- Name: countynum_cnt_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY countynum
    ADD CONSTRAINT countynum_cnt_fk FOREIGN KEY (county, state) REFERENCES counties(county, state) MATCH FULL;


--
-- TOC entry 2668 (class 2606 OID 20009)
-- Dependencies: 2403 1808 1849
-- Name: countynum_loc_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY countynum
    ADD CONSTRAINT countynum_loc_fk FOREIGN KEY (lid) REFERENCES location(lid) MATCH FULL;


--
-- TOC entry 2672 (class 2606 OID 20014)
-- Dependencies: 1811 2405 1850
-- Name: crest_rvr_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY crest
    ADD CONSTRAINT crest_rvr_fk FOREIGN KEY (lid) REFERENCES riverstat(lid) MATCH FULL;


--
-- TOC entry 2674 (class 2606 OID 20019)
-- Dependencies: 1816 1951 2577
-- Name: datalimits_dur_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY datalimits
    ADD CONSTRAINT datalimits_dur_fk FOREIGN KEY (dur) REFERENCES shefdur(dur) MATCH FULL;


--
-- TOC entry 2673 (class 2606 OID 20024)
-- Dependencies: 1816 2581 1953
-- Name: datalimits_pe_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY datalimits
    ADD CONSTRAINT datalimits_pe_fk FOREIGN KEY (pe) REFERENCES shefpe(pe) MATCH FULL;


--
-- TOC entry 2675 (class 2606 OID 20029)
-- Dependencies: 1850 2405 1817
-- Name: datum_rvr_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY datum
    ADD CONSTRAINT datum_rvr_fk FOREIGN KEY (lid) REFERENCES riverstat(lid) MATCH FULL;


--
-- TOC entry 2677 (class 2606 OID 20034)
-- Dependencies: 1818 2341 1819
-- Name: dcp_dcpowner_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY dcp
    ADD CONSTRAINT dcp_dcpowner_fk FOREIGN KEY (owner) REFERENCES dcpowner(owner) MATCH FULL;


--
-- TOC entry 2676 (class 2606 OID 20039)
-- Dependencies: 2403 1849 1818
-- Name: dcp_loc_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY dcp
    ADD CONSTRAINT dcp_loc_fk FOREIGN KEY (lid) REFERENCES location(lid) MATCH FULL;


--
-- TOC entry 2679 (class 2606 OID 20044)
-- Dependencies: 1912 2504 1821
-- Name: descrip_prox_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY descrip
    ADD CONSTRAINT descrip_prox_fk FOREIGN KEY (proximity) REFERENCES proximity(proximity) MATCH FULL;


--
-- TOC entry 2678 (class 2606 OID 20049)
-- Dependencies: 1850 2405 1821
-- Name: descrip_rvr_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY descrip
    ADD CONSTRAINT descrip_rvr_fk FOREIGN KEY (lid) REFERENCES riverstat(lid) MATCH FULL;


--
-- TOC entry 2680 (class 2606 OID 20054)
-- Dependencies: 1822 2512 1917
-- Name: dhradapt_rad_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY dhradapt
    ADD CONSTRAINT dhradapt_rad_fk FOREIGN KEY (radid) REFERENCES radarloc(radid) MATCH FULL;


--
-- TOC entry 2681 (class 2606 OID 20059)
-- Dependencies: 2512 1823 1917
-- Name: dhrradar_radloc_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY dhrradar
    ADD CONSTRAINT dhrradar_radloc_fk FOREIGN KEY (radid) REFERENCES radarloc(radid) MATCH FULL;


--
-- TOC entry 2682 (class 2606 OID 20064)
-- Dependencies: 1917 1825 2512
-- Name: dpaadapt_rad_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY dpaadapt
    ADD CONSTRAINT dpaadapt_rad_fk FOREIGN KEY (radid) REFERENCES radarloc(radid) MATCH FULL;


--
-- TOC entry 2683 (class 2606 OID 20069)
-- Dependencies: 1917 1826 2512
-- Name: dparadar_radloc_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY dparadar
    ADD CONSTRAINT dparadar_radloc_fk FOREIGN KEY (radid) REFERENCES radarloc(radid) MATCH FULL;


--
-- TOC entry 2684 (class 2606 OID 20074)
-- Dependencies: 1917 2512 1827
-- Name: dspadapt_rad_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY dspadapt
    ADD CONSTRAINT dspadapt_rad_fk FOREIGN KEY (radid) REFERENCES radarloc(radid) MATCH FULL;


--
-- TOC entry 2685 (class 2606 OID 20079)
-- Dependencies: 2512 1917 1828
-- Name: dspradar_radloc_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY dspradar
    ADD CONSTRAINT dspradar_radloc_fk FOREIGN KEY (radid) REFERENCES radarloc(radid) MATCH FULL;


--
-- TOC entry 2686 (class 2606 OID 20084)
-- Dependencies: 1961 2597 1829
-- Name: eligzon_state_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY eligzon
    ADD CONSTRAINT eligzon_state_fk FOREIGN KEY (state) REFERENCES state(state) MATCH FULL;


--
-- TOC entry 2713 (class 2606 OID 20089)
-- Dependencies: 1839 1850 2405
-- Name: fcstptser_riverst_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY fcstptservice
    ADD CONSTRAINT fcstptser_riverst_fk FOREIGN KEY (lid) REFERENCES riverstat(lid) MATCH FULL;


--
-- TOC entry 2712 (class 2606 OID 20094)
-- Dependencies: 1839 2575 1950
-- Name: fcstptser_servtype_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY fcstptservice
    ADD CONSTRAINT fcstptser_servtype_fk FOREIGN KEY (service_type) REFERENCES servicetype(service_type) MATCH FULL;


--
-- TOC entry 2721 (class 2606 OID 20099)
-- Dependencies: 1850 2405 1844
-- Name: flood_rvr_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY flood
    ADD CONSTRAINT flood_rvr_fk FOREIGN KEY (lid) REFERENCES riverstat(lid) MATCH FULL;


--
-- TOC entry 2722 (class 2606 OID 20104)
-- Dependencies: 2405 1845 1850
-- Name: floodcat_rvr_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY floodcat
    ADD CONSTRAINT floodcat_rvr_fk FOREIGN KEY (lid) REFERENCES riverstat(lid) MATCH FULL;


--
-- TOC entry 2723 (class 2606 OID 20109)
-- Dependencies: 1846 1850 2405
-- Name: floodstmt_rvr_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY floodstmt
    ADD CONSTRAINT floodstmt_rvr_fk FOREIGN KEY (lid) REFERENCES riverstat(lid) MATCH FULL;


--
-- TOC entry 2687 (class 2606 OID 20114)
-- Dependencies: 2373 1837 1834
-- Name: fpdeterm_fcsthoriz_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY fcstptdeterm
    ADD CONSTRAINT fpdeterm_fcsthoriz_fk FOREIGN KEY (fcst_horizon) REFERENCES fcsthorizon(fcst_horizon) MATCH FULL;


--
-- TOC entry 2688 (class 2606 OID 20119)
-- Dependencies: 1832 1837 2368
-- Name: fpdeterm_fcstmethod_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY fcstptdeterm
    ADD CONSTRAINT fpdeterm_fcstmethod_fk FOREIGN KEY (fcst_gen_method) REFERENCES fcstgenmethod(fcst_gen_method) MATCH FULL;


--
-- TOC entry 2689 (class 2606 OID 20124)
-- Dependencies: 2383 1837 1839
-- Name: fpdeterm_fpserv_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY fcstptdeterm
    ADD CONSTRAINT fpdeterm_fpserv_fk FOREIGN KEY (lid) REFERENCES fcstptservice(lid) MATCH FULL;


--
-- TOC entry 2690 (class 2606 OID 20129)
-- Dependencies: 1837 1855 2413
-- Name: fpdeterm_frupdrght_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY fcstptdeterm
    ADD CONSTRAINT fpdeterm_frupdrght_fk FOREIGN KEY (frequpd_drought) REFERENCES frequencyupdate(frequency_update) MATCH FULL;


--
-- TOC entry 2691 (class 2606 OID 20134)
-- Dependencies: 2413 1837 1855
-- Name: fpdeterm_frupflood_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY fcstptdeterm
    ADD CONSTRAINT fpdeterm_frupflood_fk FOREIGN KEY (frequpd_flood) REFERENCES frequencyupdate(frequency_update) MATCH FULL;


--
-- TOC entry 2692 (class 2606 OID 20139)
-- Dependencies: 1855 1837 2413
-- Name: fpdeterm_frupnorm_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY fcstptdeterm
    ADD CONSTRAINT fpdeterm_frupnorm_fk FOREIGN KEY (frequpd_normal) REFERENCES frequencyupdate(frequency_update) MATCH FULL;


--
-- TOC entry 2693 (class 2606 OID 20144)
-- Dependencies: 1871 1837 2444
-- Name: fpdeterm_hydrolcomp_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY fcstptdeterm
    ADD CONSTRAINT fpdeterm_hydrolcomp_fk FOREIGN KEY (hydrol_method) REFERENCES hydrologicmethod(hydrol_method) MATCH FULL;


--
-- TOC entry 2694 (class 2606 OID 20149)
-- Dependencies: 1820 2343 1837
-- Name: fpdeterm_issuecrit_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY fcstptdeterm
    ADD CONSTRAINT fpdeterm_issuecrit_fk FOREIGN KEY (def_issue_crit) REFERENCES definingissuecriteria(def_issue_crit) MATCH FULL;


--
-- TOC entry 2695 (class 2606 OID 20154)
-- Dependencies: 1837 1930 2541
-- Name: fpdeterm_resmodel_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY fcstptdeterm
    ADD CONSTRAINT fpdeterm_resmodel_fk FOREIGN KEY (reservoir_model) REFERENCES reservoirmodel(reservoir_model) MATCH FULL;


--
-- TOC entry 2696 (class 2606 OID 20159)
-- Dependencies: 2553 1837 1936
-- Name: fpdeterm_routingcomp_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY fcstptdeterm
    ADD CONSTRAINT fpdeterm_routingcomp_fk FOREIGN KEY (hydraul_method) REFERENCES routingmethod(hydraul_method) MATCH FULL;


--
-- TOC entry 2697 (class 2606 OID 20164)
-- Dependencies: 1959 1837 2593
-- Name: fpdeterm_snowmeth_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY fcstptdeterm
    ADD CONSTRAINT fpdeterm_snowmeth_fk FOREIGN KEY (snow_method) REFERENCES snowmethod(snow_method) MATCH FULL;


--
-- TOC entry 2698 (class 2606 OID 20169)
-- Dependencies: 1849 1837 2403
-- Name: fpdeterm_upseg_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY fcstptdeterm
    ADD CONSTRAINT fpdeterm_upseg_fk FOREIGN KEY (upstream_seg) REFERENCES location(lid) MATCH FULL;


--
-- TOC entry 2699 (class 2606 OID 20174)
-- Dependencies: 2373 1838 1834
-- Name: fpesp_fcsthoriz_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY fcstptesp
    ADD CONSTRAINT fpesp_fcsthoriz_fk FOREIGN KEY (fcst_horizon) REFERENCES fcsthorizon(fcst_horizon) MATCH FULL;


--
-- TOC entry 2700 (class 2606 OID 20179)
-- Dependencies: 2389 1838 1842
-- Name: fpesp_fcsttype_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY fcstptesp
    ADD CONSTRAINT fpesp_fcsttype_fk FOREIGN KEY (fcsttype) REFERENCES fcsttype(fcsttype) MATCH FULL;


--
-- TOC entry 2701 (class 2606 OID 20184)
-- Dependencies: 2401 1848 1838
-- Name: fpesp_flowtype_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY fcstptesp
    ADD CONSTRAINT fpesp_flowtype_fk FOREIGN KEY (flowtype) REFERENCES flowtype(flowtype) MATCH FULL;


--
-- TOC entry 2702 (class 2606 OID 20189)
-- Dependencies: 2383 1838 1839
-- Name: fpesp_fpserv_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY fcstptesp
    ADD CONSTRAINT fpesp_fpserv_fk FOREIGN KEY (lid) REFERENCES fcstptservice(lid) MATCH FULL;


--
-- TOC entry 2703 (class 2606 OID 20194)
-- Dependencies: 1855 2413 1838
-- Name: fpesp_frupdrght_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY fcstptesp
    ADD CONSTRAINT fpesp_frupdrght_fk FOREIGN KEY (frequpd_drought) REFERENCES frequencyupdate(frequency_update) MATCH FULL;


--
-- TOC entry 2704 (class 2606 OID 20199)
-- Dependencies: 2413 1838 1855
-- Name: fpesp_frupflood_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY fcstptesp
    ADD CONSTRAINT fpesp_frupflood_fk FOREIGN KEY (frequpd_flood) REFERENCES frequencyupdate(frequency_update) MATCH FULL;


--
-- TOC entry 2705 (class 2606 OID 20204)
-- Dependencies: 1838 1855 2413
-- Name: fpesp_frupnorm_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY fcstptesp
    ADD CONSTRAINT fpesp_frupnorm_fk FOREIGN KEY (frequpd_normal) REFERENCES frequencyupdate(frequency_update) MATCH FULL;


--
-- TOC entry 2706 (class 2606 OID 20209)
-- Dependencies: 1871 2444 1838
-- Name: fpesp_hydrolcomp_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY fcstptesp
    ADD CONSTRAINT fpesp_hydrolcomp_fk FOREIGN KEY (hydrol_method) REFERENCES hydrologicmethod(hydrol_method) MATCH FULL;


--
-- TOC entry 2707 (class 2606 OID 20214)
-- Dependencies: 1838 2492 1905
-- Name: fpesp_postproc_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY fcstptesp
    ADD CONSTRAINT fpesp_postproc_fk FOREIGN KEY (post_processor) REFERENCES postprocessor(post_processor) MATCH FULL;


--
-- TOC entry 2708 (class 2606 OID 20219)
-- Dependencies: 1838 2541 1930
-- Name: fpesp_resmodel_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY fcstptesp
    ADD CONSTRAINT fpesp_resmodel_fk FOREIGN KEY (reservoir_model) REFERENCES reservoirmodel(reservoir_model) MATCH FULL;


--
-- TOC entry 2709 (class 2606 OID 20224)
-- Dependencies: 1838 2553 1936
-- Name: fpesp_routingcomp_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY fcstptesp
    ADD CONSTRAINT fpesp_routingcomp_fk FOREIGN KEY (hydraul_method) REFERENCES routingmethod(hydraul_method) MATCH FULL;


--
-- TOC entry 2710 (class 2606 OID 20229)
-- Dependencies: 2593 1838 1959
-- Name: fpesp_snowmeth_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY fcstptesp
    ADD CONSTRAINT fpesp_snowmeth_fk FOREIGN KEY (snow_method) REFERENCES snowmethod(snow_method) MATCH FULL;


--
-- TOC entry 2711 (class 2606 OID 20234)
-- Dependencies: 1838 1849 2403
-- Name: fpesp_upseg_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY fcstptesp
    ADD CONSTRAINT fpesp_upseg_fk FOREIGN KEY (upstream_seg) REFERENCES location(lid) MATCH FULL;


--
-- TOC entry 2714 (class 2606 OID 20239)
-- Dependencies: 1986 2637 1840
-- Name: fpwatsup_agency_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY fcstptwatsup
    ADD CONSTRAINT fpwatsup_agency_fk FOREIGN KEY (watsup_coord_agency) REFERENCES watsupcoordagency(watsup_coord_agency) MATCH FULL;


--
-- TOC entry 2715 (class 2606 OID 20244)
-- Dependencies: 1840 2639 1987
-- Name: fpwatsup_crit_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY fcstptwatsup
    ADD CONSTRAINT fpwatsup_crit_fk FOREIGN KEY (watsup_crit) REFERENCES watsupcriterion(watsup_criterion) MATCH FULL;


--
-- TOC entry 2716 (class 2606 OID 20249)
-- Dependencies: 1840 2383 1839
-- Name: fpwatsup_fpserv_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY fcstptwatsup
    ADD CONSTRAINT fpwatsup_fpserv_fk FOREIGN KEY (lid) REFERENCES fcstptservice(lid) MATCH FULL;


--
-- TOC entry 2717 (class 2606 OID 20254)
-- Dependencies: 2413 1840 1855
-- Name: fpwatsup_frequpd_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY fcstptwatsup
    ADD CONSTRAINT fpwatsup_frequpd_fk FOREIGN KEY (frequpd_normal) REFERENCES frequencyupdate(frequency_update) MATCH FULL;


--
-- TOC entry 2718 (class 2606 OID 20259)
-- Dependencies: 1840 1988 2641
-- Name: fpwatsup_meth_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY fcstptwatsup
    ADD CONSTRAINT fpwatsup_meth_fk FOREIGN KEY (watsup_method) REFERENCES watsupmethod(watsup_method) MATCH FULL;


--
-- TOC entry 2719 (class 2606 OID 20264)
-- Dependencies: 1927 2535 1840
-- Name: fpwatsup_periodreq_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY fcstptwatsup
    ADD CONSTRAINT fpwatsup_periodreq_fk FOREIGN KEY (period_req) REFERENCES requiredperiod(period_req) MATCH FULL;


--
-- TOC entry 2720 (class 2606 OID 20269)
-- Dependencies: 1840 2643 1989
-- Name: fpwatsup_resp_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY fcstptwatsup
    ADD CONSTRAINT fpwatsup_resp_fk FOREIGN KEY (watsup_resp_agency) REFERENCES watsuprespagency(watsup_resp_agency) MATCH FULL;


--
-- TOC entry 2735 (class 2606 OID 20274)
-- Dependencies: 1857 1856 2417
-- Name: gage_gagemaint_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY gage
    ADD CONSTRAINT gage_gagemaint_fk FOREIGN KEY (maint) REFERENCES gagemaint(maint) MATCH FULL;


--
-- TOC entry 2736 (class 2606 OID 20279)
-- Dependencies: 1856 2419 1858
-- Name: gage_gageowner_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY gage
    ADD CONSTRAINT gage_gageowner_fk FOREIGN KEY (owner) REFERENCES gageowner(owner) MATCH FULL;


--
-- TOC entry 2737 (class 2606 OID 20284)
-- Dependencies: 2421 1856 1859
-- Name: gage_gagetype_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY gage
    ADD CONSTRAINT gage_gagetype_fk FOREIGN KEY (type) REFERENCES gagetype(type) MATCH FULL;


--
-- TOC entry 2738 (class 2606 OID 20289)
-- Dependencies: 1849 1856 2403
-- Name: gage_loc_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY gage
    ADD CONSTRAINT gage_loc_fk FOREIGN KEY (lid) REFERENCES location(lid) MATCH FULL;


--
-- TOC entry 2739 (class 2606 OID 20294)
-- Dependencies: 1850 2405 1864
-- Name: hgstation_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY hgstation
    ADD CONSTRAINT hgstation_fk FOREIGN KEY (lid) REFERENCES riverstat(lid) MATCH FULL;


--
-- TOC entry 2741 (class 2606 OID 20299)
-- Dependencies: 1877 2425 1861
-- Name: linesgs_garea_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY linesegs
    ADD CONSTRAINT linesgs_garea_fk FOREIGN KEY (area_id) REFERENCES geoarea(area_id) MATCH FULL;


--
-- TOC entry 2724 (class 2606 OID 20304)
-- Dependencies: 1807 1849 1849 1807 2315
-- Name: location_cnty_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY "location"
    ADD CONSTRAINT location_cnty_fk FOREIGN KEY (county, state) REFERENCES counties(county, state) MATCH FULL;


--
-- TOC entry 2725 (class 2606 OID 20309)
-- Dependencies: 1849 2440 1867
-- Name: location_hsa_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY "location"
    ADD CONSTRAINT location_hsa_fk FOREIGN KEY (hsa) REFERENCES hsa(hsa) MATCH FULL;


--
-- TOC entry 2726 (class 2606 OID 20314)
-- Dependencies: 2476 1897 1849
-- Name: location_net_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY "location"
    ADD CONSTRAINT location_net_fk FOREIGN KEY (network) REFERENCES network(network) MATCH FULL;


--
-- TOC entry 2727 (class 2606 OID 20319)
-- Dependencies: 1849 2545 1932
-- Name: location_rfc_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY "location"
    ADD CONSTRAINT location_rfc_fk FOREIGN KEY (rfc) REFERENCES rfc(rfc) MATCH FULL;


--
-- TOC entry 2728 (class 2606 OID 20324)
-- Dependencies: 2610 1849 1971
-- Name: location_tz_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY "location"
    ADD CONSTRAINT location_tz_fk FOREIGN KEY (tzone) REFERENCES timezone(tzone) MATCH FULL;


--
-- TOC entry 2729 (class 2606 OID 20329)
-- Dependencies: 1991 2647 1849
-- Name: location_wfo_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY "location"
    ADD CONSTRAINT location_wfo_fk FOREIGN KEY (wfo) REFERENCES wfo(wfo) MATCH FULL;


--
-- TOC entry 2743 (class 2606 OID 20334)
-- Dependencies: 2577 1880 1951
-- Name: locdatalim_dur_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY locdatalimits
    ADD CONSTRAINT locdatalim_dur_fk FOREIGN KEY (dur) REFERENCES shefdur(dur) MATCH FULL;


--
-- TOC entry 2742 (class 2606 OID 20339)
-- Dependencies: 2581 1880 1953
-- Name: locdatalim_pe_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY locdatalimits
    ADD CONSTRAINT locdatalim_pe_fk FOREIGN KEY (pe) REFERENCES shefpe(pe) MATCH FULL;


--
-- TOC entry 2659 (class 2606 OID 20344)
-- Dependencies: 1790 1849 2403
-- Name: locextagncy_loc_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY locextagency
    ADD CONSTRAINT locextagncy_loc_fk FOREIGN KEY (lid) REFERENCES location(lid) MATCH FULL;


--
-- TOC entry 2744 (class 2606 OID 20349)
-- Dependencies: 1849 1881 2403
-- Name: locimage_loc_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY locimage
    ADD CONSTRAINT locimage_loc_fk FOREIGN KEY (lid) REFERENCES location(lid) MATCH FULL;


--
-- TOC entry 2747 (class 2606 OID 20354)
-- Dependencies: 2405 1850 1888
-- Name: lowwater_rvr_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY lowwater
    ADD CONSTRAINT lowwater_rvr_fk FOREIGN KEY (lid) REFERENCES riverstat(lid) MATCH FULL;


--
-- TOC entry 2748 (class 2606 OID 20359)
-- Dependencies: 2309 1898 1804
-- Name: observer_comm_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY observer
    ADD CONSTRAINT observer_comm_fk FOREIGN KEY (comm) REFERENCES coopcomms(comm) MATCH FULL;


--
-- TOC entry 2749 (class 2606 OID 20364)
-- Dependencies: 2403 1898 1849
-- Name: observer_loc_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY observer
    ADD CONSTRAINT observer_loc_fk FOREIGN KEY (lid) REFERENCES location(lid) MATCH FULL;


--
-- TOC entry 2750 (class 2606 OID 20369)
-- Dependencies: 1805 2311 1898
-- Name: observer_recip_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY observer
    ADD CONSTRAINT observer_recip_fk FOREIGN KEY (recip) REFERENCES cooprecip(recip) MATCH FULL;


--
-- TOC entry 2751 (class 2606 OID 20374)
-- Dependencies: 1806 1898 2313
-- Name: observer_spons_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY observer
    ADD CONSTRAINT observer_spons_fk FOREIGN KEY (spons) REFERENCES coopspons(spons) MATCH FULL;


--
-- TOC entry 2752 (class 2606 OID 20379)
-- Dependencies: 1961 2597 1898
-- Name: observer_state_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY observer
    ADD CONSTRAINT observer_state_fk FOREIGN KEY (state) REFERENCES state(state) MATCH FULL;


--
-- TOC entry 2755 (class 2606 OID 20384)
-- Dependencies: 1900 2577 1951
-- Name: ofsdatatran_dur_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY ofsdatatrans
    ADD CONSTRAINT ofsdatatran_dur_fk FOREIGN KEY (dur) REFERENCES shefdur(dur) MATCH FULL;


--
-- TOC entry 2754 (class 2606 OID 20389)
-- Dependencies: 1952 1900 2579
-- Name: ofsdatatran_ext_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY ofsdatatrans
    ADD CONSTRAINT ofsdatatran_ext_fk FOREIGN KEY (extremum) REFERENCES shefex(extremum) MATCH FULL;


--
-- TOC entry 2753 (class 2606 OID 20394)
-- Dependencies: 2581 1900 1953
-- Name: ofsdatatrans_pe_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY ofsdatatrans
    ADD CONSTRAINT ofsdatatrans_pe_fk FOREIGN KEY (pe) REFERENCES shefpe(pe) MATCH FULL;


--
-- TOC entry 2756 (class 2606 OID 20399)
-- Dependencies: 1901 2403 1849
-- Name: ofsstntrans_loc_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY ofsstntrans
    ADD CONSTRAINT ofsstntrans_loc_fk FOREIGN KEY (lid) REFERENCES location(lid) MATCH FULL;


--
-- TOC entry 2757 (class 2606 OID 20404)
-- Dependencies: 1914 1850 2405
-- Name: pub_rvr_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY pub
    ADD CONSTRAINT pub_rvr_fk FOREIGN KEY (lid) REFERENCES riverstat(lid) MATCH FULL;


--
-- TOC entry 2758 (class 2606 OID 20409)
-- Dependencies: 1850 2405 1920
-- Name: rating_rvr_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY rating
    ADD CONSTRAINT rating_rvr_fk FOREIGN KEY (lid) REFERENCES riverstat(lid) MATCH FULL;


--
-- TOC entry 2760 (class 2606 OID 20414)
-- Dependencies: 1850 1925 2405
-- Name: refer_rvr_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY refer
    ADD CONSTRAINT refer_rvr_fk FOREIGN KEY (lid) REFERENCES riverstat(lid) MATCH FULL;


--
-- TOC entry 2761 (class 2606 OID 20419)
-- Dependencies: 1928 2539 1929
-- Name: rescap_res_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY rescap
    ADD CONSTRAINT rescap_res_fk FOREIGN KEY (lid) REFERENCES reservoir(lid) MATCH FULL;


--
-- TOC entry 2764 (class 2606 OID 20424)
-- Dependencies: 2333 1815 1929
-- Name: reservoir_dtype_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY reservoir
    ADD CONSTRAINT reservoir_dtype_fk FOREIGN KEY (type) REFERENCES damtypes(type) MATCH FULL;


--
-- TOC entry 2763 (class 2606 OID 20429)
-- Dependencies: 2403 1849 1929
-- Name: reservoir_loc_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY reservoir
    ADD CONSTRAINT reservoir_loc_fk FOREIGN KEY (lid) REFERENCES location(lid) MATCH FULL;


--
-- TOC entry 2762 (class 2606 OID 20434)
-- Dependencies: 1931 1929 2543
-- Name: reservoir_rownr_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY reservoir
    ADD CONSTRAINT reservoir_rownr_fk FOREIGN KEY (owner) REFERENCES resowner(owner) MATCH FULL;


--
-- TOC entry 2730 (class 2606 OID 20439)
-- Dependencies: 2403 1850 1849
-- Name: riverstat_loc_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY riverstat
    ADD CONSTRAINT riverstat_loc_fk FOREIGN KEY (lid) REFERENCES location(lid) MATCH FULL;


--
-- TOC entry 2731 (class 2606 OID 20444)
-- Dependencies: 1851 1937 2555
-- Name: rpffcstpnt_grp_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY rpffcstpoint
    ADD CONSTRAINT rpffcstpnt_grp_fk FOREIGN KEY (group_id) REFERENCES rpffcstgroup(group_id) MATCH FULL;


--
-- TOC entry 2732 (class 2606 OID 20449)
-- Dependencies: 1851 1867 2440
-- Name: rpffcstpnt_pbck_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY rpffcstpoint
    ADD CONSTRAINT rpffcstpnt_pbck_fk FOREIGN KEY (primary_back) REFERENCES hsa(hsa) MATCH FULL;


--
-- TOC entry 2733 (class 2606 OID 20454)
-- Dependencies: 2405 1850 1851
-- Name: rpffcstpnt_rvr_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY rpffcstpoint
    ADD CONSTRAINT rpffcstpnt_rvr_fk FOREIGN KEY (lid) REFERENCES riverstat(lid) MATCH FULL;


--
-- TOC entry 2734 (class 2606 OID 20459)
-- Dependencies: 1867 1851 2440
-- Name: rpffcstpnt_sbck_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY rpffcstpoint
    ADD CONSTRAINT rpffcstpnt_sbck_fk FOREIGN KEY (secondary_back) REFERENCES hsa(hsa) MATCH FULL;


--
-- TOC entry 2759 (class 2606 OID 20464)
-- Dependencies: 2405 1850 1921
-- Name: rshift_rvr_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY ratingshift
    ADD CONSTRAINT rshift_rvr_fk FOREIGN KEY (lid) REFERENCES riverstat(lid) MATCH FULL;


--
-- TOC entry 2740 (class 2606 OID 20469)
-- Dependencies: 2403 1849 1868
-- Name: stnclass_loc_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY stnclass
    ADD CONSTRAINT stnclass_loc_fk FOREIGN KEY (lid) REFERENCES location(lid) MATCH FULL;


--
-- TOC entry 2768 (class 2606 OID 20474)
-- Dependencies: 1849 2403 1966
-- Name: telem_loc_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY telem
    ADD CONSTRAINT telem_loc_fk FOREIGN KEY (lid) REFERENCES location(lid) MATCH FULL;


--
-- TOC entry 2767 (class 2606 OID 20479)
-- Dependencies: 1966 2601 1967
-- Name: telem_towner_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY telem
    ADD CONSTRAINT telem_towner_fk FOREIGN KEY (owner) REFERENCES telmowner(owner) MATCH FULL;


--
-- TOC entry 2766 (class 2606 OID 20484)
-- Dependencies: 1966 2603 1968
-- Name: telem_tpayor_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY telem
    ADD CONSTRAINT telem_tpayor_fk FOREIGN KEY (payor) REFERENCES telmpayor(payor) MATCH FULL;


--
-- TOC entry 2765 (class 2606 OID 20489)
-- Dependencies: 1966 2605 1969
-- Name: telem_ttype_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY telem
    ADD CONSTRAINT telem_ttype_fk FOREIGN KEY (type) REFERENCES telmtype(type) MATCH FULL;


--
-- TOC entry 2670 (class 2606 OID 20494)
-- Dependencies: 1807 1810 1810 2315 1807
-- Name: transmit_county_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY countytransmit
    ADD CONSTRAINT transmit_county_fk FOREIGN KEY (county, state) REFERENCES counties(county, state) MATCH FULL;


--
-- TOC entry 2746 (class 2606 OID 20499)
-- Dependencies: 1884 2597 1961
-- Name: transmit_state_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY nwrtransmitter
    ADD CONSTRAINT transmit_state_fk FOREIGN KEY (state) REFERENCES state(state) MATCH FULL;


--
-- TOC entry 2745 (class 2606 OID 20504)
-- Dependencies: 1884 1991 2647
-- Name: transmit_wfo_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY nwrtransmitter
    ADD CONSTRAINT transmit_wfo_fk FOREIGN KEY (wfo) REFERENCES wfo(wfo) MATCH FULL;


--
-- TOC entry 2769 (class 2606 OID 20509)
-- Dependencies: 2405 1972 1850
-- Name: unitgraph_rvr_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY unitgraph
    ADD CONSTRAINT unitgraph_rvr_fk FOREIGN KEY (lid) REFERENCES riverstat(lid) MATCH FULL;


--
-- TOC entry 2770 (class 2606 OID 20514)
-- Dependencies: 1979 1977 2619
-- Name: vtecevent_act_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY vtecevent
    ADD CONSTRAINT vtecevent_act_fk FOREIGN KEY (action) REFERENCES vtecaction(action) MATCH FULL;


--
-- TOC entry 2771 (class 2606 OID 20519)
-- Dependencies: 1979 2621 1978
-- Name: vtecevent_cau_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY vtecevent
    ADD CONSTRAINT vtecevent_cau_fk FOREIGN KEY (immed_cause) REFERENCES vteccause(immed_cause) MATCH FULL;


--
-- TOC entry 2772 (class 2606 OID 20524)
-- Dependencies: 1979 1980 2625
-- Name: vtecevent_phen_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY vtecevent
    ADD CONSTRAINT vtecevent_phen_fk FOREIGN KEY (phenom) REFERENCES vtecphenom(phenom) MATCH FULL;


--
-- TOC entry 2773 (class 2606 OID 20529)
-- Dependencies: 1979 2629 1982
-- Name: vtecevent_rec_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY vtecevent
    ADD CONSTRAINT vtecevent_rec_fk FOREIGN KEY (record) REFERENCES vtecrecord(record) MATCH FULL;


--
-- TOC entry 2774 (class 2606 OID 20534)
-- Dependencies: 1979 2631 1983
-- Name: vtecevent_sev_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY vtecevent
    ADD CONSTRAINT vtecevent_sev_fk FOREIGN KEY (severity) REFERENCES vtecsever(severity) MATCH FULL;


--
-- TOC entry 2775 (class 2606 OID 20539)
-- Dependencies: 1984 1979 2633
-- Name: vtecevent_sig_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY vtecevent
    ADD CONSTRAINT vtecevent_sig_fk FOREIGN KEY (signif) REFERENCES vtecsignif(signif) MATCH FULL;


--
-- TOC entry 2777 (class 2606 OID 20544)
-- Dependencies: 1829 1994 1994 1829 2362
-- Name: zonenum_elig_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY zonenum
    ADD CONSTRAINT zonenum_elig_fk FOREIGN KEY (state, zonenum) REFERENCES eligzon(state, zonenum) MATCH FULL;


--
-- TOC entry 2776 (class 2606 OID 20549)
-- Dependencies: 2403 1849 1994
-- Name: zonenum_loc_fk; Type: FK CONSTRAINT; Schema: public; Owner: awips
--

ALTER TABLE ONLY zonenum
    ADD CONSTRAINT zonenum_loc_fk FOREIGN KEY (lid) REFERENCES location(lid) MATCH FULL;


--
-- TOC entry 2789 (class 0 OID 0)
-- Dependencies: 6
-- Name: public; Type: ACL; Schema: -; Owner: awips
--



GRANT ALL ON SCHEMA public TO awips;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;
