/************************************************************************
 * USCORE.H								*
 *									*
 * This header file defines all of the C and FORTRAN routines that are  *
 * called by C and therefore need an underscore appended to the name.   *
 *									*
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/GSC		12/00	Combined undscr.h and uscore.h          *
 * S. Jacobs/NCEP	12/00	Added im_nexz & im_nzhd			*
 * F. J. Yen/NCEP	12/00	Added sv_dcod				*
 * F. J. Yen/NCEP	02/01	Added vopen, vclosp, vendd		*
 * S. Jacobs/NCEP	 3/01	Added ngd_nlst				*
 * D.W.Plummer/NCEP	 4/01	Added cv_rduc				*
 * D.W.Plummer/NCEP	 4/01	Added clo_init,_bstype,_bsarea,_bgnext	*
 * A. Hardy/GSC		 6/01   Added gh_ftbl, gh_gclr, gh_rtbl         *
 * A. Hardy/GSC		 6/01   Added gg_wcur                           *
 * A. Hardy/GSC		 7/01   Added im_chtb                           *
 * S. Jacobs/NCEP	 7/01   Added ngd_rsfl                          *
 * S. Jacobs/NCEP	 7/01   Added gg_qsrd, ti_itoj, ti_jtoi,        *
 *                                      prnlon, pr_mskn                 *
 * M. Li/SAIC		08/01   Added cpcg_srch                         *
 * T. Piper/SAIC	09/01   Removed duplicate entries               *
 * A. Hardy/SAIC	10/01   Added ti_yy24                           *
 * A. Hardy/SAIC	01/02   Added pr_hgfm and pr_knms               *
 * D.W.Plummer/NCEP	01/02	Added clo_bstag				*
 * T. Piper/SAIC	04/02	Added cfl_perms				*
 * R. Tian/SAIC		05/02	Added im_rfax				*
 * D. Kidwell/NCEP	 6/02	Added gsgtgn, vsgtgn                    *
 * M. Li/SAIC		 8/02	Added ws_dcod				*
 * A. Hardy/NCEP	 8/02   Added gg_csig, cs_dcod			*
 * D.W.Plummer/NCEP	 8/02	Added clo_bgcent			*
 * M. Li/SAIC		 9/02	Added ctb_pf*				*
 * T. Lee/SAIC		 9/02	Added si_dcod				*
 * J. Wu/SAIC	 	 9/02	add gplbnd_				*
 * F. J. Yen/NCEP	 9/02	Added rd_dcod_				*
 * D. Kidwell/NCEP	 9/02	Added tf_dcod_                          *
 * A. Hardy/NCEP	10/02   Added wo_dcod				*
 * T. Piper/SAIC	10/02	Added gb_read, nd_ftim, nd_lamb		*
 * A. Hardy/NCEP	10/02   Added wc_dcod				*
 * D.W.Plummer/NCEP	01/03	Added cgr_segdist and cgr_qclose	*
 * D.W.Plummer/NCEP	 3/03	Added im_gvtota and im_gtmp		*
 * T. Piper/SAIC	04/03	Replaced nd_lamb with nd_gds		*
 * K. Brill/HPC		 5/03	Added dg_intl_				*
 * m.gamazaychikov/SAIC  5/03	added ctb_g2read, ctb_g2gnam, ctb_g2gnum*
 * M. Li/SAIC		 6/03	Added gg_wwfo				*
 * T. Piper/SAIC	 7/03	Added ip_svar				*
 * D.W.Plummer/NCEP	 7/03	Added ggsort				*
 * A. Hardy/NCEP	 7/03   Added ctb_rdwou				*
 * A. Hardy/NCEP	 8/03   Added css_date				*
 * A. Hardy/NCEP	 8/03	Added fl_fpth, gg_wcvf, ggwwcr		*
 * D.W.Plummer/NCEP	 8/03	Added cgr_dang and cgr_range		*
 * D.W.Plummer/NCEP	 9/03	Added gqgprj				*
 * m.gamazaychikov/SAIC	10/03   Added gh_wwat, gh_wwug		        *
 * m.gamazaychikov/SAIC	10/03   Removed gg_atcf			        *
 * M. Li/SAIC		10/03	Added bfeinp, ip_dynm, ip_exit, ip_idnt	*
 * m.gamazaychikov/SAIC	10/03   Removed dcavcd, dcmmdc, and dcadcd      *
 *				Replaced dcavnm with dcgfs		*
 *				Replaced dcmrfm with dcgfsx		*
 * M. Li/SAIC           11/03   Added im_cbar                           *
 * D. Kidwell/NCEP	11/03	Added clo_findstn                       *
 * A. Hardy/NCEP	12/03	Added wsfill				*
 * J. Wu/SAIC           01/04   Added er_gnumerr & er_gerrmsg           *
 * A. Hardy/NCEP	 1/04	Added gg_wcck, gg_wwtp, gg_wusc		*
 * B. Yin/SAIC	        02/04   Added gh_bksq, gh_bkrd, gh_bkus         *
 * T. Piper/SAIC	03/04	Removed smgtim				*
 * B. Yin/SAIC  	03/04	Added css_gtim				*
 * T. Piper/SAIC	03/04	Removed cfdate and ss_gtim		*
 * m.gamazaychikov/SAIC	04/04	Added clo_finddesc, gh_bkpl, gh_bktb, 	*
 *				      gh_bkrv				*
 * D.W.Plummer/NCEP	 6/04	Added im_dump and im_icmn		*
 * M. Li/SAIC		 6/04	Added v2uinp				*
 * T. Piper/SAIC	 6/04	Added ctb_rdcpf				*
 * M. Li/SAIC		 8/04	Added cvg_setfilter & cvg_getElFilter	*
 * B. Yin/SAIC  	 8/04	Added gh_wwbk_ and gh_wwtx_		*
 * M. Li/SAIC           10/04   Replaced ctb_rdwou with ctb_rdprf       *
 * m.gamazaychikov/SAIC 11/04   Added gg_wfps                           *
 * B. Yin/SAIC          12/04   Added ti_c2i                            *
 * S. Gilbert/NCEP  	 1/05	Added many for nagrib2                  *
 * D. Kidwell/NCEP	 2/05	Added gh_wwbl                           *
 * S. Jacobs/NCEP	 2/05	Added gh_fstn				*
 * H. Zeng/SAIC		03/05	Added in_catminp&in_catmmap		*
 * C. Bailey/HPC        03/05   Added xgsave, ggsave                    *
 * A. Hardy/NCEP	 4/05	Added cvg_rebun,gg_cwcp,gg_wcpb,wbc_wcp *
 * T. Piper/SAIC	04/05	Added sn_clos, sn_rstr, sn_sstn, sn_stim*
 * J. Wu/SAIC		04/05	Added oa_bndset, oa_bndchk & oa_bndinb	*
 * F. J. Yen/NCEP	 4/05	Added wp_dcod and gg_wcp		*
 * R. Tian/SAIC		04/05	Added oabsdr				*
 * F. J. Yen/NCEP        4/05   Added gg_wlso                           *
 * H. Zeng/SAIC         04/05   Added in_discrete & in_dlines           *
 * M. Li/SAIC		05/05	Added ggrdinfo and oacinp		*
 * D.W.Plummer/NCEP	05/05	Added ggoadl				*
 * S. Gilbert/NCEP  	05/05	Added dg_nfil, dg_ndtm, dg_grid, tg_dual*
 * R. Tian/SAIC         07/05   Added cgr_qrol_                         *
 * B. Yin/SAIC          07/05   Added ti_dst                            *
 * H. Zeng/SAIC         07/05   Added fa_dcod                           *
 * M. Li/SAIC           09/05   Removed gh_bkrd and gh_bkus             *
 * D.W.Plummer/NCEP     09/05   Add dg_nend_                            *
 * S. Gilbert/NCEP      08/05   Added gd_open,gd_opnf, tg_ctoi, lv_ccrd *
 * S. Gilbert/NCEP      09/05   Added tg_diff                           *
 * m.gamazaychikov/SAIC 10/05   Added ip_log                            *
 * S. Gilbert/NCEP      11/05   Added gh_0bkp                           *
 * B. Yin/SAIC          12/05   Added clo_snapPt                        *
 * m.gamazaychikov/SAIC 12/05   Added gg_rdvgf                          *
 * T. Piper/SAIC	01/06	Added cst_wrap				*
 * S. Gilbert/NCEP      02/06   Added gh_advn                           *
 * S. Danz/AWC          03/06   Added cgr_inpolywn, cgr_csegint,        *
 *                              cgr_bisectpt, cgr_objint, cgr_objinpoly,*
 *                              cgr_concave, ces_gtrtbl                 *
 * B. Yin/SAIC          04/06   Added vg2frzl                           *
 * M. Li/SAIC		04/06	Added in_disqleft			*
 * R. Tian/SAIC		06/06	Added DG and PD defines			*
 * T. Piper/SAIC	06/06	Added ig_ascii and ig_dcod		*
 * J. Wu/SAIC		06/06	Added cvg_rdfilter			*
 * T. Piper/SAIC	07/06	Added gg_rwav				*
 * T. Piper/SAIC	07/06	Added gg_wsrd				*
 * R. Tian/SAIC		08/06	Added inc_outt				*
 * D.W.Plummer/NCEP	09/06	Added some DM library functions		*
 * R. Tian/SAIC		09/06	Added gr_sscl,in_scal,ip_resp,tm_accp	*
 * R. Tian/SAIC		09/06	Added tm_str				*
 * T. Piper/SAIC	09/06	Added im_qimg, im_rtbl, im_smgd		*
 * R. Tian/SAIC		10/06	Removed na_*, added gd_cref, gg_proj,	*
 *                              gr_algn, gr_mbn2, gr_suba		*
 * S. Gilbert/NCEP      10/06   Added gr_vnav                           *
 * S. Jacobs/NCEP	12/06	Added cst_zpad				*
 * F. J. Yen		 1/07	Added vfasaw				*
 * L. Hinson/AWC        01/07   Added inc_pgfatxt                       *
 * B. Yin/SAIC          02/07   Added ces_rtbl                          *
 * m.gamazaychikov/SAIC 06/07   Add gh_wtcb, gh_wtce, gh_wtct           *
 * S. Gilbert/NCEP      06/07   Added dg_kxky                           *
 * S. Gilbert/NCEP      07/07   Added gdpltc and gdpltcf		*
 * S. Gilbert/NCEP      07/07   Added gclgrnf, gcboxx(f) and gstrml(f)	*
 * S. Gilbert/NCEP      07/07   Added gdpdta(f), and gdpduv(f)		*
 * S. Gilbert/NCEP      07/07   Added gdtdta(f)				*
 * S. Gilbert/NCEP      08/07   Added gdxdta(f), gdxgts(f) and gdxdvv(f)*
 * S. Gilbert/NCEP      08/07   Added gdtxrd(f) and gdtxrv(f)		*
 * S. Gilbert/NCEP      10/07   Added gh_bkcal and gh_bkloc		*
 * S. Jacobs/NCEP       10/07   Added ctb_hfread, ctb_hfgetfont         *
 * M. Li/SAIC		11/07	Add gdzdat, gdzdatf, gdzdsp, st_lstf    *
 *                              gr_intp					*
 * M. Li/SAIC		11/07	Add gdedat,gdedatf,gdedsp,gdegdt,gdeghd *
 * T. Piper/SAIC	12/07	Removed rscale, rscint & rslwid; they	*
 *				do not exist!				*
 * E. Safford/SAIC	12/07 	add new wrapper functions		*
 * T. Piper/SAIC        01/08   Added GD_INIT				*
 * T. Piper/SAIC	02/08	Added GF driver functions		*
 * T. Piper/SAIC	03/08	Added new UNISYS navigation routines	*
 * m.gamazaychikov/SAIC 03/08   Add ta_dcod                             *
 * G. Hull/SAIC 	03/08	Added new vi_drivc, vi_drivf for gdvint *
 * B. Hebbard/SAIC      03/08   Added new gdstaa, gdstaaf for gdstat    *
 * T. Lee/SAIC		04/08	Added cgr_reduceptsw, poly_cvgfw,	*
 *				  poly_clipw, poly_get_vertex, 		*
 *				  poly_get_contour			*
 * m.gamazaychikov/SAIC	09/08	Removed gh_fstn, added clo_findstnw, 	*
 * 				  clo_finddescw				*
 ***********************************************************************/
#ifdef UNDERSCORE

#define aclear		aclear_
#define aclosp		aclosp_
#define aegrp		aegrp_
#define aendd		aendd_
#define af_dcod		af_dcod_
#define afill		afill_
#define ainita		ainita_
#define aline		aline_
#define am_dcod		am_dcod_
#define aqdatt		aqdatt_

#define ascolr		ascolr_
#define asdatt		asdatt_
#define asgrp		asgrp_
#define astext		astext_
#define asymb		asymb_
#define atext		atext_
#define awrfil		awrfil_

#define bfeinp		bfeinp_	

#define cbf_clos	cbf_clos_
#define cbf_open	cbf_open_
#define cbf_read	cbf_read_

#define ccheck		ccheck_

#define cds_gfill	cds_gfill_
#define cds_init	cds_init_
#define cds_ress	cds_ress_
#define cds_rtbl	cds_rtbl_
#define cds_scal	cds_scal_
#define cds_scol	cds_scol_
#define cds_sfill	cds_sfill_

#define cendmq		cendmq_

#define cfl_perms	cfl_perms_
#define cfl_scnd	cfl_scnd_
#define cfl_mnam	cfl_mnam_

#define cgetmq		cgetmq_

#define cgr_dang	cgr_dang_
#define cgr_inpoly	cgr_inpoly_
#define cgr_polyint	cgr_polyint_
#define cgr_range	cgr_range_
#define cgr_segdist	cgr_segdist_
#define cgr_segint	cgr_segint_
#define cgr_qclose	cgr_qclose_
#define cgr_rolseg	cgr_rolseg_
#define cgr_qrol        cgr_qrol_
#define cgr_inpolywn    cgr_inpolywn_
#define cgr_csegint     cgr_csegint_
#define cgr_bisectpt    cgr_bisectpt_
#define cgr_objint      cgr_objint_
#define cgr_objinpoly   cgr_objinpoly_
#define cgr_concave     cgr_concave_
#define cgr_reduceptsw	cgr_reduceptsw_

#define ces_gtrtbl     ces_gtrtbl_
#define ces_rtbl       ces_rtbl_

#define ciproc		ciproc_

#define clo_bgcent	clo_bgcent_
#define clo_bgnext	clo_bgnext_
#define clo_bsarea	clo_bsarea_
#define clo_bstag	clo_bstag_
#define clo_bstype	clo_bstype_
#define clo_cmpdir	clo_cmpdir_
#define clo_cmpwds	clo_cmpwds_
#define clo_compass	clo_compass_
#define clo_dddec	clo_dddec_
#define clo_direct	clo_direct_
#define clo_dist	clo_dist_
#define clo_dltln	clo_dltln_
#define clo_finddesc	clo_finddesc_
#define clo_findstn	clo_findstn_
#define clo_finddescw	clo_finddescw_
#define clo_findstnw	clo_findstnw_
#define clo_init	clo_init_
#define clo_snapPt      clo_snapPt_

#define cpcg_srch	cpcg_srch_

#define crecv		crecv_

#define crg_init	crg_init_

#define cs_dcod		cs_dcod_

#define csend		csend_
#define csleep		csleep_
#define csproc		csproc_

#define css_date	css_date_
#define css_gtim	css_gtim_

#define	cst_wrap	cst_wrap_
#define	cst_zpad	cst_zpad_

#define ctb_dtcat	ctb_dtcat_
#define ctb_dtdump	ctb_dtdump_
#define ctb_dtget	ctb_dtget_
#define ctb_dtpath	ctb_dtpath_
#define ctb_dtrd	ctb_dtrd_
#define ctb_dttime	ctb_dttime_
#define ctb_dttmpl	ctb_dttmpl_
#define ctb_g2gnam	ctb_g2gnam_
#define ctb_g2gnum	ctb_g2gnum_
#define ctb_g2read	ctb_g2read_
#define ctb_hfread	ctb_hfread_
#define ctb_hfgetfont	ctb_hfgetfont_
#define ctb_pfread	ctb_pfread_
#define ctb_pfstr	ctb_pfstr_
#define ctb_plget	ctb_plget_
#define ctb_rdcpf	ctb_rdcpf_
#define ctb_rdprf	ctb_rdprf_

#define cv_mdfy		cv_mdfy_
#define cv_prmt		cv_prmt_
#define cv_rduc		cv_rduc_

#define	cvg_getElFilter cvg_getElFilter_
#define	cvg_setfilter	cvg_setfilter_
#define	cvg_rebun	cvg_rebun_	
#define	cvg_rdfilter	cvg_rdfilter_	

#define dc_dcls		dc_dcls_
#define dc_dlog		dc_dlog_
#define dc_dopn		dc_dopn_
#define dc_exit		dc_exit_
#define dc_gbul		dc_gbul_
#define dc_ghdr		dc_ghdr_
#define dc_init		dc_init_
#define dc_sgnl		dc_sgnl_
#define dc_wbuf		dc_wbuf_
#define dc_wlog		dc_wlog_

#define dcgfs		dcgfs_
#define dcffcd		dcffcd_
#define dcgfsx		dcgfsx_
#define dcnmdc		dcnmdc_
#define dcudcd		dcudcd_

#define dfrnt		dfrnt_

#define dgc_cxgp	dgc_cxgp_
#define dgc_fixa	dgc_fixa_
#define dgc_flno	dgc_flno_
#define dgc_glev	dgc_glev_
#define dgc_grid	dgc_grid_
#define dgc_inxt	dgc_inxt_
#define dgc_ndtm	dgc_ndtm_
#define dgc_nfil	dgc_nfil_
#define dgc_nrdt	dgc_nrdt_
#define dgc_ntim	dgc_ntim_
#define dgc_nwdt	dgc_nwdt_
#define dgc_qdtm	dgc_qdtm_
#define dgc_qgrd	dgc_qgrd_
#define dgc_qtms	dgc_qtms_
#define dgc_subg	dgc_subg_
#define dgc_vecr	dgc_vecr_
#define dgc_vect	dgc_vect_
#define dg_clal		dg_clal_
#define dg_clos		dg_clos_
#define dg_fall         dg_fall_
#define dg_grel		dg_grel_
#define dg_hilo		dg_hilo_
#define dg_igrg		dg_igrg_
#define dg_intl		dg_intl_
#define dg_kxky		dg_kxky_
#define dg_nend         dg_nend_
#define dg_nrel		dg_nrel_
#define dg_oang		dg_oang_
#define dg_orgn		dg_orgn_
#define dg_qkxy		dg_qkxy_
#define dg_qref		dg_qref_
#define dg_rstm		dg_rstm_

#define dm_cnct		dm_cnct_
#define db_init		db_init_
#define db_open		db_open_
#define db_rdtr		db_rdtr_
#define db_rdtrgrid	db_rdtrgrid_
#define db_gsathdr	db_gsathdr_
#define db_getstinfo	db_getstinfo_
#define db_getcycle	db_getcycle_
#define db_getparm	db_getparm_
#define db_isdbfile	db_isdbfile_

#define db_wsetnavtime	db_wsetnavtime_
#define db_setnavtime	db_setnavtime_
#define db_getnavtime	db_getnavtime_

#define db_seta2dtinfo	db_seta2dtinfo_
#define db_wsetevtname	db_wsetevtname_
#define db_setevtname	db_setevtname_
#define db_getevtname	db_getevtname_
#define db_setensmbrs   db_setensmbrs_
#define db_getensmbrs   db_getensmbrs_
#define db_setsubgnav   db_setsubgnav_ 
//#define db_ggemstns	db_ggemstns_
//#define db_ggemparms	db_ggemparms_
//#define db_getgemsfdata	db_getgemsfdata_
//#define db_ggemsfdata	db_ggemsfdata_
/************************************************************************
*/
#define db_getduri	db_getduri_
#define db_getgnav	db_getgnav_
#define db_retsubgcrs   db_retsubgcrs_
#define db_gtimgrid	db_gtimgrid_
#define db_msgcave	db_msgcave_
#define db_gtim		db_gtim_
#define db_sqparms	db_sqparms_
#define dm_rint		dm_rint_
#define dm_pkgd		dm_pkgd_
#define dm_wpkg		dm_wpkg_
#define dm_rpkgc	dm_rpkgc_
#define dm_pkgdc	dm_pkgdc_
#define dm_wpkgc	dm_wpkgc_

#define dp_ugrb		dp_ugrb_
#define dp_unmc		dp_unmc_
#define dp_udif		dp_udif_

#define er_gerrmsg	er_gerrmsg_
#define er_gnumerr	er_gnumerr_
#define er_init		er_init_
#define er_lmsg		er_lmsg_
#define er_mmsg		er_mmsg_
#define er_smsg		er_smsg_
#define er_stat		er_stat_
#define er_wbuf		er_wbuf_
#define er_wmsg		er_wmsg_
                                                                                
#define fa_dcod         fa_dcod_

#define fl_clos		fl_clos_
#define fl_fpth		fl_fpth_
#define fl_tbop		fl_tbop_

#define garc		garc_
#define garrw		garrw_
#define gazdrm		gazdrm_

#define gb_bds		gb_bds_
#define gb_bms		gb_bms_
#define gb_btoi		gb_btoi_
#define gb_clos		gb_clos_
#define gb_diag		gb_diag_
#define gb_ends		gb_ends_
#define gb_gaus		gb_gaus_
#define gb_gbdh		gb_gbdh_
#define gb_gds		gb_gds_
#define gb_ges		gb_ges_
#define gb_ggds		gb_ggds_
#define gb_gids		gb_gids_
#define gb_gmtx		gb_gmtx_
#define gb_gpbd		gb_gpbd_
#define gb_gpds		gb_gpds_
#define gb_gsec		gb_gsec_
#define gb_gspk		gb_gspk_
#define gb_gubd		gb_gubd_
#define gb_ids		gb_ids_
#define gb_lamb		gb_lamb_
#define gb_ltln		gb_ltln_
#define gb_merc		gb_merc_
#define gb_next		gb_next_
#define gb_open		gb_open_
#define gb_pds		gb_pds_
#define gb_polr		gb_polr_
#define gb_read		gb_read_
#define gb_scan		gb_scan_
#define gb_scpk		gb_scpk_
#define gb_sphr		gb_sphr_
#define gb_sspk		gb_sspk_
#define gb_unpk		gb_unpk_

#define gbarb		gbarb_
#define gcboxx		gcboxx_
#define gcboxxf		gcboxxf_
#define gcfill		gcfill_
#define gcfillf		gcfillf_
#define gcircl		gcircl_
#define gclear		gclear_
#define gclgrn		gclgrn_
#define gclgrnf		gclgrnf_
#define gclpnl		gclpnl_
#define gcmbo		gcmbo_
#define gctyp		gctyp_
#define gstrml		gstrml_
#define gstrmlf		gstrmlf_

#define gd_clos		gd_clos_
#define gd_cref		gd_cref_
#define gd_geni		gd_geni_
#define gd_gcyc		gd_gcyc_
#define gd_init		gd_init_
#define gd_open         gd_open_
#define gd_opnf         gd_opnf_
#define gd_wpgd		gd_wpgd_
#define gdc_open        gdc_open_
#define gdc_gcyc        gdc_gcyc_
#define gd_gtmf         gd_gtmf_
#define gdc_gtmf        gdc_gtmf_

#define gdarr		gdarr_
#define gdedat          gdedat_
#define gdedatf         gdedatf_
#define gdedsp          gdedsp_
#define gdegdt          gdegdt_
#define gdeghd          gdeghd_
#define gdpdta		gdpdta_
#define gdpdtaf		gdpdtaf_
#define gdpduv		gdpduv_
#define gdpduvf		gdpduvf_
#define gdpltb		gdpltb_
#define gdpltc		gdpltc_
#define gdpltcf		gdpltcf_
#define gdstaa          gdstaa_
#define gdstaaf         gdstaaf_
#define gdpstp		gdpstp_
#define gdpstt		gdpstt_
#define gdptmc		gdptmc_
#define gdrazm		gdrazm_
#define gdtdta		gdtdta_
#define gdtdtaf		gdtdtaf_
#define gdtxrd          gdtxrd_
#define gdtxrdf         gdtxrdf_
#define gdtxrv          gdtxrv_
#define gdtxrvf         gdtxrvf_
#define gdxdta          gdxdta_
#define gdxdtaf         gdxdtaf_
#define gdxdvv          gdxdvv_
#define gdxdvvf         gdxdvvf_
#define gdxgts          gdxgts_
#define gdxgtsf         gdxgtsf_
	
#define gdzdsp		gdzdsp_ 
#define gdzdat		gdzdat_ 
#define gdzdatf	        gdzdatf_ 

#define genanm		genanm_
#define gendp		gendp_
#define geplot		geplot_
#define getmap		getmap_

#define gfill		gfill_
#define gfrnt		gfrnt_

#define gg_airm		gg_airm_
#define gg_asrd         gg_asrd_
#define gg_cbar		gg_cbar_
#define gg_csig		gg_csig_
#define gg_cwcp		gg_cwcp_
#define gg_dltn		gg_dltn_
#define gg_dvgf		gg_dvgf_
#define gg_init		gg_init_
#define gg_isig		gg_isig_
#define gg_ltln		gg_ltln_
#define gg_ltng		gg_ltng_
#define gg_map		gg_map_
#define gg_maps		gg_maps_
#define gg_motf		gg_motf_
#define gg_ncon		gg_ncon_
#define gg_panl		gg_panl_
#define gg_proj		gg_proj_
#define gg_qsrd		gg_qsrd_
#define gg_rdvgf	gg_rdvgf_
#define gg_sdev		gg_sdev_
#define gg_splt		gg_splt_
#define gg_rwav		gg_rwav_
#define gg_sttl		gg_sttl_
#define gg_warn		gg_warn_
#define gg_wcck		gg_wcck_
#define gg_wcp		gg_wcp_
#define gg_wcpb		gg_wcpb_
#define gg_wcur		gg_wcur_
#define gg_wcvf		gg_wcvf_
#define gg_wfps		gg_wfps_
#define gg_wlbl		gg_wlbl_
#define gg_wlso         gg_wlso_
#define	gg_wsrd		gg_wsrd_
#define gg_wstr		gg_wstr_
#define gg_wtch		gg_wtch_
#define gg_wusc		gg_wusc_
#define gg_wwcr		gg_wwcr_
#define gg_wwfo		gg_wwfo_
#define gg_wwtp		gg_wwtp_
#define gg_zare		gg_zare_

#define ggapsm		ggapsm_
#define ggdriv		ggdriv_
#define ggoadl		ggoadl_
#define ggrdinfo	ggrdinfo_
#define ggsave		ggsave_
#define ggsort		ggsort_
#define ggtpnt		ggtpnt_
#define ghash		ghash_

#define gh_0bkp		gh_0bkp_
#define gh_advn		gh_advn_
#define gh_bkcal	gh_bkcal_
#define gh_bkloc	gh_bkloc_
#define gh_bkpl		gh_bkpl_
#define gh_bkrv		gh_bkrv_
#define gh_bksq		gh_bksq_
#define gh_bktb		gh_bktb_
#define gh_ftbl		gh_ftbl_
#define gh_gclr		gh_gclr_
#define gh_rtbl		gh_rtbl_
#define gh_wtcb         gh_wtcb_
#define gh_wtce         gh_wtce_
#define gh_wtct         gh_wtct_
#define gh_wwat		gh_wwat_
#define gh_wwbk		gh_wwbk_
#define gh_wwbl		gh_wwbl_
#define gh_wwtx		gh_wwtx_
#define gh_wwug		gh_wwug_

#define gicng		gicng_
#define ginitp		ginitp_
#define gline		gline_
#define glogo		glogo_
#define gmark		gmark_
#define gmesg		gmesg_

#define gp_azdr		gp_azdr_
#define gp_draz		gp_draz_

#define gptnd		gptnd_
#define gpwth		gpwth_
#define gplbnd		gplbnd_

#define gqarrw		gqarrw_
#define gqbarb		gqbarb_
#define gqbnd		gqbnd_
#define gqclr2		gqclr2_
#define gqcmbo		gqcmbo_
#define gqcolr		gqcolr_
#define gqcomp		gqcomp_
#define gqctyp		gqctyp_
#define gqcvsc		gqcvsc_
#define gqdarr		gqdarr_
#define gqdatt		gqdatt_
#define gqdev		gqdev_
#define gqfill		gqfill_
#define gqfrnt		gqfrnt_
#define gqgprj		gqgprj_
#define gqhash		gqhash_
#define gqicng		gqicng_
#define gqline		gqline_
#define gqmprj		gqmprj_
#define gqmrkr		gqmrkr_
#define gqptnd		gqptnd_
#define gqpwth		gqpwth_
#define gqsizd		gqsizd_
#define gqsky		gqsky_
#define gqsmth		gqsmth_
#define gqspcl		gqspcl_
#define gqspln		gqspln_
#define gqtext		gqtext_
#define gqturb		gqturb_
#define gqwthr		gqwthr_

#define grecv		grecv_
#define groam		groam_

#define gr_acol		gr_acol_
#define gr_algn		gr_algn_
#define gr_cnav		gr_cnav_
#define gr_dorg		gr_dorg_
#define gr_gtim		gr_gtim_
#define gr_intp		gr_intp_
#define gr_levl		gr_levl_
#define gr_ltln		gr_ltln_
#define gr_mbn2		gr_mbn2_
#define gr_mnav		gr_mnav_
#define gr_pack		gr_pack_
#define gr_plin		gr_plin_
#define gr_ploc		gr_ploc_
#define gr_setr		gr_setr_
#define gr_snav		gr_snav_
#define gr_sscl		gr_sscl_
#define gr_stat		gr_stat_
#define gr_suba		gr_suba_
#define gr_rban		gr_rban_
#define gr_rnav		gr_rnav_
#define gr_vnav		gr_vnav_
#define gr_wgb2		gr_wgb2_

#define grc_rnav         grc_rnav_

#define gsarrw		gsarrw_
#define gsatim		gsatim_
#define gsbarb		gsbarb_
#define gsbrgb		gsbrgb_
#define gsclr2		gsclr2_
#define gscmbo		gscmbo_
#define gscolr		gscolr_
#define gscrgb		gscrgb_
#define gsctyp		gsctyp_
#define gsdarr		gsdarr_
#define gsdeva		gsdeva_
#define gsend		gsend_
#define gsfill		gsfill_
#define gsfrnt		gsfrnt_
#define gsgprj		gsgprj_
#define gsgraf		gsgraf_
#define gsgtgn		gsgtgn_
#define gshash		gshash_
#define gsicmn		gsicmn_
#define gsicng		gsicng_
#define gsky		gsky_
#define gsline		gsline_
#define gslwin		gslwin_
#define gsmfil		gsmfil_
#define gsmprj		gsmprj_
#define gsmrkr		gsmrkr_
#define gspcl		gspcl_
#define gspln		gspln_
#define gsplot		gsplot_
#define gsptnd		gsptnd_
#define gspwth		gspwth_
#define gsroam		gsroam_
#define gssky		gssky_
#define gssmth		gssmth_
#define gsspcl		gsspcl_
#define gsspln		gsspln_
#define gstanm		gstanm_
#define gstext		gstext_
#define gsturb		gsturb_
#define gstxsy		gstxsy_
#define gsview		gsview_
#define gswthr		gswthr_

#define gtext		gtext_
#define gtextc		gtextc_
#define gtrans		gtrans_
#define gturb		gturb_
#define gtxsy		gtxsy_
#define gwthr		gwthr_

#define hc_dcod		hc_dcod_

#define hscolr		hscolr_

#define ifrnt		ifrnt_
#define	ig_ascii	ig_ascii_
#define ig_dcod		ig_dcod_

#define im_btot		im_btot_
#define im_cbar		im_cbar_
#define im_chtb		im_chtb_
#define im_drop		im_drop_
#define im_dump		im_dump_
#define im_gpix		im_gpix_
#define im_gtmp		im_gtmp_
#define im_gvtota	im_gvtota_
#define im_icmn		im_icmn_
#define im_lutf		im_lutf_
#define im_nexz		im_nexz_
#define im_nzhd		im_nzhd_
#define im_qchn		im_qchn_
#define im_qimg		im_qimg_
#define im_qlut		im_qlut_
#define im_qsiz		im_qsiz_
#define im_qtim		im_qtim_
#define im_rcdf		im_rcdf_
#define im_rgin		im_rgin_
#define	im_rtbl		im_rtbl_
#define im_sbgn		im_sbgn_
#define im_simg		im_simg_
#define	im_smgd		im_smgd_
#define im_wgin		im_wgin_
#define im_rfax		im_rfax_
#define im_ttob		im_ttob_
#define lc_km_to_latlon lc_km_to_latlon_
#define lc_latlon_to_km lc_latlon_to_km_
#define prnlon		prnlon_
#define psn_km_to_latlon_alaska psn_km_to_latlon_alaska_
#define psn_km_to_latlon_hawaii psn_km_to_latlon_hawaii_

#define in_bdta		in_bdta_
#define in_cint		in_cint_
#define in_line		in_line_
#define in_outt		in_outt_
#define in_text		in_text_
#define in_txtn		in_txtn_
#define in_catminp	in_catminp_
#define in_catmmap	in_catmmap_
#define in_discrete	in_discrete_
#define in_discmap	in_discmap_
#define in_discq	in_discq_
#define in_discqleft	in_discqleft_
#define in_dlines	in_dlines_
#define in_dlinq	in_dlinq_
#define inc_outt	inc_outt_
#define inc_pgfatxt	inc_pgfatxt_
#define in_scal		in_scal_

#define ip_dynm		ip_dynm_
#define ip_exit		ip_exit_
#define ip_help		ip_help_
#define ip_idnt		ip_idnt_
#define ip_init		ip_init_
#define ip_putv		ip_putv_
#define ip_str		ip_str_
#define ip_strp		ip_strp_
#define ip_svar		ip_svar_
#define ip_log          ip_log_
#define ip_resp		ip_resp_

#define is_dcod		is_dcod_

#define ispln		ispln_

#define lc_gare		lc_gare_

#define ls_dcod		ls_dcod_

#define lv_cord		lv_cord_
#define lv_ccrd		lv_ccrd_
#define lv_sort		lv_sort_

#define ma_dcod		ma_dcod_

#define map_draw	map_draw_
#define map_init	map_init_
#define map_mark	map_mark_

#define mcirc		mcirc_
#define mclear		mclear_
#define mclose		mclose_
#define mdots		mdots_
#define mfill		mfill_
#define mheader		mheader_
#define minit		minit_
#define minita		minita_
#define minitd		minitd_
#define mline		mline_
#define mmesg		mmesg_
#define mopen		mopen_

#define mqcomp		mqcomp_
#define mqdatt		mqdatt_

#define mrflhd		mrflhd_
#define mrfrhd		mrfrhd_

#define msclrtb		msclrtb_
#define mscolr		mscolr_
#define msdatt		msdatt_
#define msfill		msfill_
#define msflclr		msflclr_
#define msflnm		msflnm_
#define msfont		msfont_
#define mslnclr		mslnclr_
#define mslnwid		mslnwid_
#define mslwid		mslwid_
#define mstext		mstext_

#define mt_dcod		mt_dcod_

#define mtext		mtext_
#define mtextc		mtextc_
#define mtitle		mtitle_

#define mv_btoi		mv_btoi_
#define mv_ev32		mv_ev32_
#define mv_itob		mv_itob_
#define mv_swp2		mv_swp2_
#define mv_swp4		mv_swp4_
#define mv_ve32		mv_ve32_

#define mwflhd		mwflhd_
#define mwfrhd		mwfrhd_

#define nc_dcod		nc_dcod_

#define nd_ftim		nd_ftim_
#define nd_gds		nd_gds_
#define ngd_dspl	ngd_dspl_
#define ngd_nlst	ngd_nlst_
#define ngd_rsfl	ngd_rsfl_
#define ngd_tlst	ngd_tlst_

#define nms_dspl	nms_dspl_

#define nsf_dspl	nsf_dspl_
#define nsf_tlst	nsf_tlst_
#define nsfc_tlst	nsfc_tlst_

#define nsn_dspl	nsn_dspl_
#define nsn_tlst	nsn_tlst_

#define nmap_sfmap	nmap_sfmap_
#define nmap_snmap	nmap_snmap_

#define nmp_dspl	nmp_dspl_

#define nsfopn		nsfopn_

#define oa_bndchk	oa_bndchk_	
#define oa_bndinb	oa_bndinb_	
#define oa_bndset	oa_bndset_
#define oacinp		oacinp_

#define initpc		initpc_
#define pcirc 		pcirc_
#define pclear		pclear_
#define pclosp		pclosp_
#define pcvtclr		pcvtclr_

#define pdots		pdots_
#define pendd		pendd_
#define pfill		pfill_
#define pfontn		pfontn_
#define pinita		pinita_
#define pinitclr	pinitclr_
#define pinitd		pinitd_
#define pline		pline_

#define pqclrs		pqclrs_
#define pqcomp		pqcomp_
#define pqdatt		pqdatt_

#define pr_hgmf		pr_hgmf_
#define pr_hgfm		pr_hgfm_
#define pr_knmh		pr_knmh_
#define pr_knms		pr_knms_
#define pr_mskn		pr_mskn_
#define pr_tmkc		pr_tmkc_

#define psatim		psatim_
#define pscint		pscint_
#define pscnam		pscnam_
#define pscolr		pscolr_
#define pscrgb		pscrgb_
#define psdatt		psdatt_
#define psfill		psfill_
#define psicmn		psicmn_
#define psltyp		psltyp_
#define pslwid		pslwid_
#define psopen		psopen_
#define pstext		pstext_

#define ptext		ptext_
#define ptextc		ptextc_

#define pupdclr		pupdclr_

#define rclear		rclear_
#define rclosp		rclosp_
#define rdots		rdots_

#define rd_dcod		rd_dcod_

#define rendd		rendd_
#define rfill		rfill_
#define rfontn		rfontn_
#define rinita		rinita_
#define rinitd		rinitd_
#define rline		rline_
#define rrdfont		rrdfont_

#define rscolr		rscolr_
#define rscrgb		rscrgb_
#define rsdatt		rsdatt_
#define rsdump		rsdump_
#define rsfill		rsfill_
#define rsltyp		rsltyp_
#define rsopen		rsopen_
#define rspan		rspan_
#define rstext		rstext_

#define rtext		rtext_

#define sc_dcod		sc_dcod_

#define sf_atim		sf_atim_
#define sf_clos		sf_clos_
#define sf_crcn		sf_crcn_
#define sf_rdat		sf_rdat_
#define sf_rspc		sf_rspc_
#define sf_rstr		sf_rstr_
#define sf_snxt		sf_snxt_
#define sf_sstn		sf_sstn_
#define sf_stim		sf_stim_
#define sf_uare		sf_uare_
#define sf_wbox		sf_wbox_
#define sf_wdat		sf_wdat_

#define sfc_acny	sfc_acny_
#define sfc_gtim	sfc_gtim_
#define sfc_opnf	sfc_opnf_

#define si_dcod		si_dcod_

#define sn_clos		sn_clos_
#define sn_rstr		sn_rstr_
#define sn_sstn		sn_sstn_
#define sn_stim		sn_stim_

#define ss_vers		ss_vers_

#define st_atoe		st_atoe_
#define st_find		st_find_
#define st_gtst		st_gtst_
#define st_lstf		st_lstf_
#define st_lstr		st_lstr_
#define st_null		st_null_
#define st_numb		st_numb_
#define st_opcl		st_opcl_
#define st_clst		st_clst_

#define sv_dcod		sv_dcod_

#define ta_dcod         ta_dcod_

#define tb_fgeo		tb_fgeo_
#define tb_grnv		tb_grnv_
#define tb_idst		tb_idst_
#define tb_nids		tb_nids_
#define tb_parm		tb_parm_

#define tclear		tclear_
#define tclosp		tclosp_
#define tdots		tdots_
#define tendd		tendd_
#define tfill		tfill_

#define tf_dcod		tf_dcod_

#define tg_crinc	tg_crinc_
#define tg_ctoi		tg_ctoi_
#define tg_diff		tg_diff_
#define tg_dual		tg_dual_
#define tg_itoc		tg_itoc_
#define tg_vald		tg_vald_

#define ti_addd		ti_addd_
#define ti_addm		ti_addm_
#define ti_c2i		ti_c2i_
#define ti_ccnt		ti_ccnt_
#define ti_cdtm		ti_cdtm_
#define ti_ctoi		ti_ctoi_
#define ti_daym		ti_daym_
#define ti_dayw		ti_dayw_
#define ti_difd		ti_difd_
#define ti_diff		ti_diff_
#define ti_dst          ti_dst_
#define ti_dtm4		ti_dtm4_
#define ti_itoc		ti_itoc_
#define ti_itoj		ti_itoj_
#define ti_jtoi		ti_jtoi_
#define ti_mdif		ti_mdif_
#define ti_stan		ti_stan_
#define ti_subd		ti_subd_
#define ti_subm		ti_subm_
#define ti_yy24		ti_yy24_
#define ti_tzdf		ti_tzdf_

#define tinita		tinita_
#define tline		tline_

#define tsatim		tsatim_
#define tscolr		tscolr_
#define tsdatt		tsdatt_
#define tsfill		tsfill_
#define tsicmn		tsicmn_
#define tsopen		tsopen_
#define tspan		tspan_

#define uclear		uclear_
#define uclosp		uclosp_
#define udmbnd 		udmbnd_
#define uegrp		uegrp_
#define uendd		uendd_
#define ufill		ufill_
#define ufscan		ufscan_
#define uinita		uinita_
#define uldblk		uldblk_
#define uline		uline_
#define umkdta		umkdta_
#define umkttl		umkttl_
#define uopen		uopen_
#define updgrf 		updgrf_
#define uqdatt		uqdatt_

#define uscolr		uscolr_
#define usdatt		usdatt_
#define usgrp		usgrp_
#define ustext		ustext_
#define uswap		uswap_
#define usymb		usymb_

#define utext		utext_
#define utf_plot	utf_plot_
#define uwrbuf		uwrbuf_
#define uwrdta		uwrdta_

#define vi_drivc        vi_drivc_
#define vi_drivf        vi_drivf_
#define vi_gindx        vi_gindx_

#define v2uinp		v2uinp_
#define vcirc		vcirc_
#define vclosp		vclosp_
#define vegrp		vegrp_
#define vendd		vendd_
#define vfill		vfill_
#define vfrnt		vfrnt_
#define vg2frzl		vg2frzl_
#define vhash		vhash_
#define vinita		vinita_
#define vline		vline_
#define vmark		vmark_
#define vopen		vopen_

#define vscirc		vscirc_
#define vsclr2		vsclr2_
#define vscolr		vscolr_
#define vsdash		vsdash_
#define vsdatt		vsdatt_
#define vsfill		vsfill_
#define vsfrnt		vsfrnt_
#define vsgrp		vsgrp_
#define vsgtgn		vsgtgn_
#define vshash		vshash_
#define vsline		vsline_
#define vsmark		vsmark_
#define vspln		vspln_
#define vssmth		vssmth_
#define vsspln		vsspln_
#define vssymb		vssymb_
#define vstext		vstext_
#define vstxsy		vstxsy_
#define vswind		vswind_
#define vsymb		vsymb_

#define vtext		vtext_
#define vtextc		vtextc_
#define vtxsy		vtxsy_

#define vwind		vwind_

#define wbcolr		wbcolr_
#define wclear		wclear_
#define wclosp		wclosp_
#define wfill		wfill_
#define winit		winit_
#define wline		wline_

#define wc_dcod		wc_dcod_

#define wn_dcod		wn_dcod_

#define wo_dcod		wo_dcod_

#define wp_dcod		wp_dcod_

#define ws_dcod		ws_dcod_

#define wsatim		wsatim_
#define wsbrgb		wsbrgb_
#define wscolb		wscolb_
#define wscolr		wscolr_
#define wsicmn		wsicmn_
#define wsfill		wsfill_

#define ww_crnr		ww_crnr_
#define ww_dcod		ww_dcod_
#define ww_plot		ww_plot_

#define wbc_dsts	wbc_dsts_
#define wbc_wcp		wbc_wcp_

/*
 *  GF driver functions
 */
#define gfclosp		gfclosp_
#define gfdatt		gfdatt_
#define gfendd		gfendd_
#define gfflsh		gfflsh_
#define gfinita		gfinita_
#define gfselwin	gfselwin_

/*
 *  XW driver functions
 */
#define xcaloc		xcaloc_
#define xcamgr		xcamgr_
#define xcirc		xcirc_
#define xclear		xclear_
#define xclosp		xclosp_
#define xclosw		xclosw_
#define xclpnl		xclpnl_
#define xcsdat		xcsdat_

#define xdot		xdot_
#define xdwtbl		xdwtbl_
#define xenanm		xenanm_
#define xendd		xendd_
#define xfill		xfill_
#define xgbank		xgbank_
#define xgsave          xgsave_
#define xgsdat		xgsdat_
#define xgtpnt		xgtpnt_
#define xinita		xinita_
#define xinitclr	xinitclr_
#define xinitd		xinitd_
#define xline		xline_
#define xloopc		xloopc_
#define xmotifw		xmotifw_

#define xopenw		xopenw_
#define xpoint		xpoint_

#define xqclrs		xqclrs_
#define xqcomp		xqcomp_
#define xqdatt		xqdatt_

#define xresize		xresize_
#define xroam		xroam_

#define xsatim		xsatim_
#define xsatpx		xsatpx_
#define xsattbl		xsattbl_
#define xscint		xscint_
#define xscnam		xscnam_
#define xscolr		xscolr_
#define xscrgb		xscrgb_
#define xsdatt		xsdatt_
#define xselwin		xselwin_
#define xsfill		xsfill_
#define xsicmn		xsicmn_
#define xslutf		xslutf_
#define xslwid		xslwid_
#define xslwin		xslwin_
#define xsplot		xsplot_
#define xsroam		xsroam_
#define xstanm		xstanm_
#define xstext		xstext_

#define xtext		xtext_
#define xtextc		xtextc_

#define xupdclr		xupdclr_

#define xxevnt		xxevnt_
#define xxflsh		xxflsh_
#define oabsdr		oabsdr_

#define pd_sped		pd_sped_
#define pd_drct		pd_drct_
#define pd_mskn		pd_mskn_
#define pd_tmck		pd_tmck_
#define pd_tmpk		pd_tmpk_

#define poly_cvgfw	poly_cvgfw_
#define poly_clipw	poly_clipw_
#define poly_get_vertex	poly_get_vertex_
#define poly_get_contour poly_get_contour_

#define tm_accp		tm_accp_
#define tm_str		tm_str_

#define vfasaw		vfasaw_

/*
 *  nwx specific (for now) functions
 */
#define wwcrnr_calc	wwcrnr_calc_ 
#define wgem_er_wmsg	wgem_er_wmsg_
#define wgem_gclear	wgem_gclear_
#define wgem_geplot	wgem_geplot_
#define wgem_gg_ltln	wgem_gg_ltln_
#define wgem_gg_map	wgem_gg_map_
#define wgem_gg_maps	wgem_gg_maps_
#define wgem_gg_panl	wgem_gg_panl_
#define wgem_gg_wstr	wgem_gg_wstr_
#define wgem_gmark	wgem_gmark_
#define wgem_gmesg	wgem_gmesg_
#define wgem_gsatim	wgem_gsatim_
#define wgem_gscolr	wgem_gscolr_
#define wgem_gsmrkr	wgem_gsmrkr_
#define wgem_gsmfil	wgem_gsmfil_
#define wgem_gtext	wgem_gtext_
#define wgem_in_text	wgem_in_text_
#endif
