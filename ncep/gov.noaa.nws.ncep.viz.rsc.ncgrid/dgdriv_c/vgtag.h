
/************************************************************************
 * vgtag.h								*
 *									*
 * This header file contains the predefined tags used for encoding and	*
 * decoding VG elements.						*
 *									*
 **									*
 * J. Wu/SAIC	 	10/04	rename from cvggfa.h			*
 * B. Yin/SAIC		11/04	removed unnecessary tags		*
 * B. Yin/SAIC		11/04	added status/tag/r2sig/points tags 	*
 * E. Safford/SAIC	11/04	add GFA_SNAPSHOTS and GFA_CATEGORY	*
 * B. Yin/SAIC		06/05	change smear to user-smear&system_smear *
 * E. Safford/SAIC	07/05   rm GFA_SEQUENCE                         *
 * H. Zeng/SAIC		09/05	rm TAG_GFA_R2SIGMET			*
 * J. Wu/SAIC		10/05	change outlook to user- & system_outlook*
 * B. Yin/SAIC		11/05	rm GFA_CATEGORY				*
 * E. Safford/SAIC	12/05	add GFA_INIT				*
 * B. Yin/SAIC		04/06	add TAG_GFA_FZL_TOP & TAG_GFA_FZL_BOTTOM*
 * B. Yin/SAIC		07/06	add all GFA hazards & linelm, lintyp    *
 * S. Danz/AWC		10/06	add arrow endpoint			*
 * B. Yin/SAIC		01/07	add TAG_GFA_FZLRANGE   			*
 * L. Hinson/AWC        01/07   add TAG_GFA_TXTCLR, TAG_GFA_TXTSZ,      *
 *                              TAG_GFA_TXTFN, TAG_GFA_TXTHW,           *
 *                              TAG_GFA_TXTWDTH, TAG_GFA_TXTALGN        *
 *                              TAG_GFA_TXTLYT                          *
 * E. Safford/SAIC	04/07	add GFA_FBBA_AIRMET & OUTLOOK		*
 * L. Hinson/AWC        06/07   add TAG_GFA_ARROWSZ                     *
 * B. Yin/SAIC		12/07	add TAG_GFA_OVERRIDE_TM			*
 * J. Wu/SAIC           06/08   add hazard TS - thunderstorm            *
 * B. Yin/SAIC          06/08   add hazard MVFR                         *
 * B. Yin/SAIC          07/08   add 20 KT surface wind hazard           *
 * L. Hinson/AWC        06/08   add TAG_GFA_CYCLE                       *
 ***********************************************************************/
#ifndef VGTAG_H
#define VGTAG_H

#define GFA_HAZARD_IFR		( 1 )
#define GFA_HAZARD_MT_OBSC	( 2 )
#define GFA_HAZARD_ICE		( 3 )
#define GFA_HAZARD_TURB		( 4 )
#define GFA_HAZARD_TURB_HI	( 5 )
#define GFA_HAZARD_TURB_LO	( 6 )
#define GFA_HAZARD_SFC_WND	( 7 )
#define GFA_HAZARD_SIGWX	( 8 )
#define GFA_HAZARD_CIG_CLD	( 9 )
#define GFA_HAZARD_TCU_CLD	(10 )
#define GFA_HAZARD_MTW		(11 )
#define GFA_HAZARD_CLD		(12 )
#define GFA_HAZARD_FZLVL_SFC	(13 )
#define GFA_HAZARD_FZLVL	(14 )
#define GFA_HAZARD_M_FZLVL	(15 )
#define GFA_HAZARD_LLWS		(16 )
#define GFA_HAZARD_TS           (17 )
#define GFA_HAZARD_MVFR         (18 )
#define GFA_HAZARD_SFC_WND20    (19 )

#define GFA_INIT		(-1 )
#define GFA_SNAPSHOT		( 0 )
#define GFA_USER_SMEAR		( 1 )
#define GFA_SYSTEM_SMEAR	( 2 )
#define GFA_USER_OUTLOOK	( 3 )
#define GFA_SYSTEM_OUTLOOK	( 4 )
#define GFA_FBBA_AIRMET		( 5 )
#define GFA_FBBA_OUTLOOK	( 6 )

#define TAG_GFA_LINELM		"<gfa_linelm>"
#define TAG_GFA_LINTYP		"<gfa_linetype>"
#define TAG_GFA_SUBTYPE		"<gfa_subType>"
#define TAG_GFA_AREATYPE	"<gfa_areaType>"
#define TAG_GFA_NPTS		"<gfa_npts>"
#define TAG_GFA_NBLOCKS		"<gfa_nblocks>"
#define TAG_GFA_FCSTHR		"<gfa_fcstHr>"
#define TAG_GFA_TOP		"<gfa_top>"
#define TAG_GFA_BOTTOM		"<gfa_bottom>"
#define TAG_GFA_LINEWIDTH	"<gfa_lineWidth>"
#define TAG_GFA_LAT		"<gfa_lat>"
#define TAG_GFA_LON		"<gfa_lon>"
#define TAG_GFA_STATUS		"<gfa_status>"
#define TAG_GFA_TAG		"<gfa_tag>"
#define TAG_GFA_POINTS		"<gfa_points>" 
#define TAG_GFA_SNAPSHOTHRS	"<gfa_smearHrs>"
#define TAG_GFA_FZL_TOP		"<gfa_fzlTop>"
#define TAG_GFA_FZL_BOTTOM	"<gfa_fzlBottom>"
#define TAG_GFA_ARROW_LAT	"<gfa_arrow_lat>"
#define TAG_GFA_ARROW_LON	"<gfa_arrow_lon>"
#define TAG_GFA_FZLRANGE	"<gfa_fzlRange>"
#define TAG_GFA_ARROWSZ         "<gfa_arrowSize>"
#define TAG_GFA_TXTCLR          "<gfa_txtColor>"
#define TAG_GFA_TXTSZ           "<gfa_txtSize>"
#define TAG_GFA_TXTFN           "<gfa_txtFont>"
#define TAG_GFA_TXTHW           "<gfa_txtHardware>"
#define TAG_GFA_TXTWDTH         "<gfa_txtWidth>"
#define TAG_GFA_TXTALGN         "<gfa_txtAlign>"
#define TAG_GFA_TXTLYT          "<gfa_txtLayout>"
#define TAG_GFA_AREAS		"<gfa_areas>"
#define TAG_GFA_REGION		"<gfa_region>"
#define TAG_GFA_STATESLIST	"<gfa_statesList>"
#define TAG_GFA_CONDSBEGIN	"<gfa_condsBegin>"
#define TAG_GFA_CONDSEND	"<gfa_condsEnd>"
#define TAG_GFA_OVERRIDE_TM	"<gfa_overrideIssueTm>"
#define TAG_GFA_CYCLE           "<gfa_cycle>"

/*
 *   In TAG_GFA_SUBTYPE    0 = snapshot 
 *                         1 = user drawn smear 
 *			   2 = system generated smear 
 *                         3 = user drawn outlook 
 *                         4 = system generated outlook 
 *                         5 = F/BB/A airmet
 *                         6 = F/BB/A outlook
 */

#endif




