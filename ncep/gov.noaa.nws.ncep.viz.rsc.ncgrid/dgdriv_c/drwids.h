/************************************************************************
 * drwids.h								*
 *									*
 * This file contains structure definitions used by GEMPAK to create	*
 * and manipulate XW drawing commands					*
 *									*
 **									*
 * E. Wehner/EAI	 4/97	Created					*
 * D. Keiser/GSC	 6/97	Add symbols				*
 * D. Keiser/GSC	 6/97	Added delete all function		*
 * E. Wehner/EAi	 6/97	Added wind barbs and arrows		*
 * D. Keiser/GSC	 6/97	Added flip function and volcano obj	*
 * E. Safford/GSC	 6/97	Added the first special text objects	*
 * E. Wehner/EAi	 7/97	Added function for rotation		*
 * E. Safford/GSC	 7/97	Added remaining special text objects	*
 * D.W.Plummer/NCEP	 7/97	Added dashed lines and filled arrow	*
 * E. Safford/GSC	11/97	Added modify				*
 * C. Lin/EAI		11/97	Added save_as and restore		*
 * E. Safford/GSC	01/98	Added undo				*
 * E. Safford/GSC	02/98	Added partial delete			*
 * E. Safford/GSC	03/98	Added mulit-select			*
 * W. Li/EAI		03/98	Added additional symbols		*
 * E. Safford/GSC	03/98	Remove unused funcs, add MAX #defines	*
 * E. Safford/GSC	04/98	change ALL to ANY			*
 * W. Li/EAI		04/98	Add OBJ_SPTEXTUD			*
 * E. Safford/GSC	04/98	Added ungroup				*
 * C. Lin/EAI		04/98	Added combo-symbol and new special lines*
 * S. Law/GSC		04/98	Added copy function			*
 * C. Lin/EAI		04/98	Added label function, outlook obj	*
 * W. Li/EAI		04/98	Added darr and hash in CLASS_WINDS	*
 * S. Law/GSC		05/98	Rearranged and reduced number of TEXT	*
 * F. J. Yen/NCEP	06/98	Added OBJ_QPF				*
 * F. J. Yen/NCEP	06/98	Repositioned dashed lines & renumbered	*
 * W. Li/EAI		07/98	Add new symbol(pwtstorm)		*
 * D.W.Plummer/NCEP	 8/98	Added OBJ_HCNTRK			*
 * D.W.Plummer/NCEP	 8/98	Added OBJ_GGCNTR			*
 * G. Krueger/EAI	08/98	Add STMCNTR, TRPDPRSN, and TRPCYCLN	*
 * S. Law/GSC		09/98	Added FUNC_DELPOINT			*
 * F. J. Yen/NCEP	09/98	Add OBJ_XRAINF, OBJ_SPLN20, & OBJ_SPLN21*
 * S. Law/GSC		09/98	Added FUNC_CONNECT			*
 * S. Law/GSC		09/98	Added FUNC_UNGRPALL			*
 * G. Krueger/EAI	10/98	Add OBJ_FLAME				*
 * W. Li/EAI		11/98	Added FUNC_NUMB_EDIT			*
 * S. Law/GSC		11/98	Made watch boxes a seperate class	*
 * E. Safford/GSC	12/98	rename NUMB_EDIT to INC_DEC		*
 * A. Hardy/GSC		12/98	Added CLASS_CIRCLE			*
 * S. Law/GSC		01/99	Changed OBJ_SHOWBOX to OBJ_WATCHFMT	*
 * G. Krueger/EAI	01/99	Add OBJ_XCROSS and OBJ_LOWX		*
 * F. J. Yen/NCEP	01/99	Added OBJ_WXD				*
 * E. Safford/GSC	01/99	added FUNC_SHOW_GRPS			*
 * S. Jacobs/NCEP	 2/99	Added OBJ_SQUALL			*
 * S. Jacobs/NCEP	 2/99	Changed all combo syms to OBJ_CSYMBnn	*
 * S. Jacobs/NCEP	 2/99	Added new combo syms & renumbered objs	*
 * S. Jacobs/NCEP	 3/99	Added special line 22			*
 * W. LI/EAI		05/99	add FUNC_DEL_OBJ			*
 * S. Law/GSC		05/99	Added CLASS_TRACKS and OBJ_TRKSTORM	*
 * G. Krueger/EAI	05/99	Add OBJ_CIRSOL				*
 * S. Law/GSC		07/99	Added CLASS_SIGMETS and OBJ_SIGINTL	*
 * S. Law/GSC		08/99	Added OBJ_SIGNCON, OBJ_SIGCONV,		*
 *				      OBJ_SIGOUTL, OBJ_SIGAIRM		*
 * G. Krueger/EAI	08/99	N & SH trop storm specials		*
 * T. Piper/GSC		12/99	Added OBJ_HAZE				*
 * E. Safford/GSC	12/99	Added OBJ_WATCHLN			*
 * S. Law/GSC		12/99	Added OBJ_WTCHSTAT			*
 * S. Law/GSC		12/99	Added OBJ_SIGCCF			*
 * S. Law/GSC		03/00	Added OBJ_CCFPRD			*
 * A. Hardy/GSC		05/00	Added OBJ_WTCHCNL;MAX_OBJECTS 160->170  *
 * H. Zeng/EAI		12/00	Added FUNC_REDO				*
 * S. Jacobs/NCEP	 3/01	Added OBJ_TRPTFNT			*
 * H. Zeng/EAI		05/01	Added FUNC_CHNG_GRPS			*
 * S. Jacobs/NCEP	 9/01	Added OBJ_NUCLEAR			*
 * J. Wu/SAIC		10/01	Added OBJ_SPLN23 - double line		*
 * M. Li/SAIC		10/01	Added CLASS_MARKER and OBJ_MARKnn	*
 * J. Wu/SAIC		10/01	Added OBJ_KINKLN1, 2 - kink arrow lines	*
 * M. Li/SAIC		10/01	Added OBJ_TEXTICNG - midlevel icing	*
 * E. Safford/SAIC	11/01	reorder all OBJ_*			*
 * H. Zeng/EAI		01/02	Added FUNC_LAYER			*
 * T. Lee/SAIC		04/02	Added FUNC_OPEN_PROD			*
 * J. Wu/SAIC		09/02	add CLASS_LIST				*
 * J. Wu/SAIC		10/02	add FUNC_EXTRAP				*
 * J. Wu/SAIC		11/02	add OBJ_LIST* & inc. MAX_OBJECTS to 2400*
 * J. Wu/SAIC		02/03	add OBJ_TEXTMCLOUD for midlevel cloud	*
 * m.gamazaychikov/SAIC 04/03	added special symbols 42 thru 49	*
 * m.gamazaychikov/SAIC 04/03	added combo symbol number 28		*
 * H. Zeng/XTRIA	07/03	added volcano and ash cloud elements	*
 * J. Wu/SAIC		08/03	add CLASS_MET & OBJ_JET for jet element	*
 * J. Wu/SAIC		10/03	increase MAX_CLASSES/MAX_OBJECTS for jet*
 * E. Safford/SAIC	11/03	add FUNC_SAVE_ALL			*
 * E. Safford/SAIC	11/03	add FUNC_SMEAR				*
 * A. Hardy/NCEP	12/03	added special symbol 50 		*
 * J. Wu/SAIC		02/04	add OBJ_AIRMET within CLASS_MET		*
 * J. Wu/SAIC		03/04	add OBJ_NCONSIG within CLASS_MET	*
 * H. Zeng/XTRIA	03/04	added FUNC_BLANK			*
 * J. Wu/SAIC		04/04	add FUNC_INTERP	(interpolation) 	*
 * A. Hardy/NCEP	04/04	added OBJ_LISTWBCMZ			*
 * J. Wu/SAIC		05/04	add OBJ_GFA 				*
 * J. Wu/SAIC		07/04	add FUNC_FILTER				*
 * J. Wu/SAIC		09/04	remove OBJ_AIRMET/OBJ_NCONSIG under MET	*
 * B. Yin/SAIC		12/04	add OBJ_AIRMET within products		*
 * J. Wu/SAIC		03/05	add FUNC_SHOW_NONGRP			*
 * S. Gilbert/NCEP	 6/05	Added OBJ_SPLN26 - ZZZZZ line		*
 * H. Zeng/SAIC		07/06	add FUNC_DISTANCE			*
 * E. Safford/SAIC	03/07	add FUNC_FROM, OBJ_AIRMET_P, OBJ_GFA_P	*
 * m.gamazaychikov/SAIC 05/07	add OBJ_TCE, OBJ_TCT, OBJ_TCB		*
 * E. Safford/SAIC	07/07	add FUNC_CYCLE				*
 * S. Jacobs/NCEP	04/08	Added OBJ_DSHLN10 - dotted line		*
 ***********************************************************************/

#ifndef _drwids_include
#define _drwids_include

/*
 *  Operations function codes
 */

#define MAX_OPERS		 41

#define FUNC_OPEN_VGF		  1
#define FUNC_CLOSVGF		  2
#define FUNC_SAVE_VGF		  3
#define FUNC_SELECT		  4
#define FUNC_DELETE		  5
#define FUNC_MOVE		  6
#define FUNC_REFRESH		  7
#define FUNC_DELALL		  8
#define FUNC_FLIP		  9
#define FUNC_GROUP		 10
#define FUNC_ROTATE		 11
#define FUNC_MODIFY		 12
#define FUNC_SAVE_AS		 13
#define FUNC_RESTORE		 14
#define FUNC_UNDO		 15
#define FUNC_PARTDELETE		 16
#define FUNC_MULTISEL		 17
#define FUNC_UNGROUP		 18
#define FUNC_COPY		 19
#define FUNC_LABEL		 20
#define FUNC_DELPOINT		 21
#define FUNC_CONNECT		 22
#define FUNC_UNGRPALL		 23
#define FUNC_INC_DEC		 24
#define FUNC_SHOW_GRPS		 25
#define FUNC_DEL_OBJ		 26
#define FUNC_REDO		 27
#define FUNC_CHNG_GRPS	   	 28
#define FUNC_LAYER		 29
#define FUNC_OPEN_PROD		 30
#define FUNC_EXTRAP		 31
#define FUNC_SAVE_ALL		 32
#define FUNC_SMEAR		 33
#define FUNC_BLANK		 34
#define FUNC_INTERP		 35
#define FUNC_FILTER		 36
#define FUNC_SHOW_NONGRP	 37
#define	FUNC_DISTANCE		 38
#define	FUNC_FROM		 39
#define	FUNC_CYCLE		 40

/* 
 * Classes of objects to operate on
 */

#define MAX_CLASSES		 17

#define CLASS_FRONTS		  1
#define CLASS_WATCHES		  2
#define CLASS_LINES		  3
#define CLASS_SYMBOLS		  4
#define CLASS_TEXT		  5
#define CLASS_WINDS		  6
#define CLASS_ANY		  7
#define CLASS_COMSYM		  8
#define CLASS_PRODUCTS		  9
#define CLASS_TRACKS		 10
#define CLASS_SIGMETS		 11
#define CLASS_CIRCLE		 12
#define CLASS_MARKER		 13
#define CLASS_LIST		 14
#define CLASS_MET		 15
#define CLASS_BLANK		 16

/* 
 * Specific objects to operate on 
 */
#define MAX_OBJECTS		2600

/* fronts */
#define OBJ_COLDFNT		 101
#define OBJ_WKCOLDFNT		 102
#define OBJ_DIFCOLDFNT		 103
#define OBJ_STATFNT		 104
#define OBJ_WKSTATFNT		 105
#define OBJ_DIFSTATFNT		 106
#define OBJ_DRYFNT		 107
#define OBJ_TROFFNT		 108
#define OBJ_WARMFNT		 109
#define OBJ_WKWARMFNT		 110
#define OBJ_DIFWARMFNT		 111
#define OBJ_OCCLFNT		 112
#define OBJ_WKOCCLFNT		 113
#define OBJ_DIFOCCLFNT		 114
#define OBJ_SQUALL		 115
#define OBJ_TRPTFNT		 116

/* lines */
#define OBJ_CNTR		 201
#define OBJ_DSHLINE		 202
#define OBJ_SPLN1		 203	/* ball-chain */
#define OBJ_SPLN2		 204	/* zig-zag */
#define OBJ_SPLN3		 205
#define OBJ_SPLN4		 206
#define OBJ_SPLN5		 207
#define OBJ_SPLN6		 208
#define OBJ_SPLN7		 209
#define OBJ_SPLN8		 210
#define OBJ_SPLN9		 211
#define OBJ_SPLN10		 212
#define OBJ_SPLN11		 213
#define OBJ_SPLN12		 214
#define OBJ_SPLN13		 215
#define OBJ_SPLN14		 216
#define OBJ_SPLN15		 217
#define OBJ_SPLN16		 218
#define OBJ_SPLN17		 219
#define OBJ_SPLN18		 220
#define OBJ_SPLN19		 221
#define OBJ_SPLN20		 222
#define OBJ_SPLN21		 223
#define OBJ_SPLN22		 224
#define OBJ_SPLN23		 225	/* double line */
#define OBJ_KINKLN1		 226	/* kink line with open head */
#define OBJ_KINKLN2		 227	/* kink line with filled head */
#define OBJ_SPLN26		 236	/* ZZZZZZ line */

/* more dash lines */
#define OBJ_DSHLN2		 228
#define OBJ_DSHLN3		 229
#define OBJ_DSHLN4		 230
#define OBJ_DSHLN5		 231
#define OBJ_DSHLN6		 232
#define OBJ_DSHLN7		 233
#define OBJ_DSHLN8		 234
#define OBJ_DSHLN9		 235
#define OBJ_DSHLN10		 237

/* products */
#define OBJ_WATCHFMT		 301
#define OBJ_OUTLOOK		 302
#define OBJ_SFCPRG		 303
#define OBJ_QPF			 304
#define OBJ_HCNTRK		 305
#define OBJ_GGCNTR		 306
#define OBJ_XRAINF		 307
#define OBJ_WXD			 308
#define OBJ_WTCHSTAT		 309
#define OBJ_CCFPRD		 310
#define OBJ_WTCHCNL		 311
#define OBJ_AIRMET		 312
#define OBJ_AIRMET_P		 313

/* text */
#define OBJ_TEXTGEN		 401	/* general text object */
#define OBJ_TEXTFZL		 402	/* freezing level */
#define OBJ_TEXTTURB		 403	/* turbulence */
#define OBJ_TEXTCLD		 404	/* cloud top */
#define OBJ_TEXTICNG		 405	/* midlevel icing */
#define OBJ_TEXTMCLOUD		 406	/* midlevel cloud */

/* vectors */
#define OBJ_WINDBARB		 501
#define OBJ_WINDARRW		 502
#define OBJ_WINDDARR		 503
#define OBJ_WINDHASH		 504

/* SIGMETs */
#define OBJ_SIGINTL		 601	/* international SIGMET */
#define OBJ_SIGNCON		 602	/* non-convective SIGMET */
#define OBJ_SIGCONV		 603	/* convective SIGMET */
#define OBJ_SIGOUTL		 604	/* convective outlook */
#define OBJ_SIGAIRM		 605	/* AIRMET */
#define OBJ_SIGCCF		 606	/* Collaborative Convective Forecast */
#define OBJ_SIGVOL		 607	/* volcano element */
#define OBJ_SIGVAC		 608	/* volcanic ash cloud element */

/* watches */
#define OBJ_WBCOUNTY		 701
#define OBJ_WBPARALL		 702
#define OBJ_WATCHLN 		 703

/* tracks */
#define OBJ_TRKSTORM		 801

/* circles */
#define OBJ_CIRSOL		 901

/* Markers */
#define OBJ_MARK1		1001
#define OBJ_MARK2		1002
#define OBJ_MARK3		1003
#define OBJ_MARK4		1004
#define OBJ_MARK5		1005
#define OBJ_MARK6		1006
#define OBJ_MARK7		1007
#define OBJ_MARK8		1008
#define OBJ_MARK9		1009
#define OBJ_MARK10		1010
#define OBJ_MARK11		1011
#define OBJ_MARK12		1012
#define OBJ_MARK13		1013
#define OBJ_MARK14		1014
#define OBJ_MARK15		1015
#define OBJ_MARK16		1016
#define OBJ_MARK17		1017
#define OBJ_MARK18		1018
#define OBJ_MARK19		1019
#define OBJ_MARK20		1020
#define OBJ_MARK21		1021
#define OBJ_MARK22		1022

/* Cloud Type Symbols */
#define OBJ_CLOUD01		1101
#define OBJ_CLOUD02		1102
#define OBJ_CLOUD03		1103
#define OBJ_CLOUD04		1104
#define OBJ_CLOUD05		1105
#define OBJ_CLOUD06		1106
#define OBJ_CLOUD07		1107
#define OBJ_CLOUD08		1108
#define OBJ_CLOUD09		1109
#define OBJ_CLOUD11		1111
#define OBJ_CLOUD12		1112
#define OBJ_CLOUD13		1113
#define OBJ_CLOUD14		1114
#define OBJ_CLOUD15		1115
#define OBJ_CLOUD16		1116
#define OBJ_CLOUD17		1117
#define OBJ_CLOUD18		1118
#define OBJ_CLOUD19		1119
#define OBJ_CLOUD21		1121
#define OBJ_CLOUD22		1122
#define OBJ_CLOUD23		1123
#define OBJ_CLOUD24		1124
#define OBJ_CLOUD25		1125
#define OBJ_CLOUD26		1126
#define OBJ_CLOUD27		1127
#define OBJ_CLOUD28		1128
#define OBJ_CLOUD29		1129

/* Past Weather Symbols */
#define OBJ_PSTWX03		1203
#define OBJ_PSTWX04		1204
#define OBJ_PSTWX05		1205
#define OBJ_PSTWX06		1206
#define OBJ_PSTWX07		1207
#define OBJ_PSTWX08		1208
#define OBJ_PSTWX09		1209

/* Pressure Tendency Symbols */
#define OBJ_PTEND00		1300
#define OBJ_PTEND01		1301
#define OBJ_PTEND02		1302
#define OBJ_PTEND03		1303
#define OBJ_PTEND04		1304
#define OBJ_PTEND05		1305
#define OBJ_PTEND06		1306
#define OBJ_PTEND07		1307
#define OBJ_PTEND08		1308

/* sky cover symbols */	
#define OBJ_SKY00		1400
#define OBJ_SKY01		1401
#define OBJ_SKY02		1402
#define OBJ_SKY03		1403
#define OBJ_SKY04		1404
#define OBJ_SKY05		1405
#define OBJ_SKY06		1406
#define OBJ_SKY07		1407
#define OBJ_SKY08		1408
#define OBJ_SKY09		1409
#define OBJ_SKY10		1410

/* Icing Symbols */
#define OBJ_ICE00		1500
#define OBJ_ICE01		1501
#define OBJ_ICE02		1502
#define OBJ_ICE03		1503
#define OBJ_ICE04		1504
#define OBJ_ICE05		1505
#define OBJ_ICE06		1506
#define OBJ_ICE07		1507
#define OBJ_ICE08		1508
#define OBJ_ICE09		1509
#define OBJ_ICE10		1510

/* Special Symbols */
#define OBJ_SPSYM00		1600
#define OBJ_SPSYM01		1601
#define OBJ_SPSYM02		1602
#define OBJ_SPSYM03		1603
#define OBJ_SPSYM04		1604
#define OBJ_SPSYM05		1605
#define OBJ_SPSYM06		1606
#define OBJ_SPSYM07		1607
#define OBJ_SPSYM08		1608
#define OBJ_SPSYM09		1609
#define OBJ_SPSYM10		1610
#define OBJ_SPSYM11		1611
#define OBJ_SPSYM12		1612
#define OBJ_SPSYM13		1613
#define OBJ_SPSYM14		1614
#define OBJ_SPSYM15		1615
#define OBJ_SPSYM16		1616
#define OBJ_SPSYM17		1617
#define OBJ_SPSYM18		1618
#define OBJ_SPSYM19		1619
#define OBJ_SPSYM20		1620
#define OBJ_SPSYM21		1621
#define OBJ_SPSYM22		1622
#define OBJ_SPSYM23		1623
#define OBJ_SPSYM24		1624
#define OBJ_SPSYM25		1625
#define OBJ_SPSYM26		1626
#define OBJ_SPSYM27		1627
#define OBJ_SPSYM28		1628
#define OBJ_SPSYM29		1629
#define OBJ_SPSYM30		1630
#define OBJ_SPSYM31		1631
#define OBJ_SPSYM32		1632
#define OBJ_SPSYM33		1633
#define OBJ_SPSYM34		1634
#define OBJ_SPSYM35		1635
#define OBJ_SPSYM36		1636
#define OBJ_SPSYM37		1637
#define OBJ_SPSYM38		1638
#define OBJ_SPSYM39		1639
#define OBJ_SPSYM40		1640
#define OBJ_SPSYM41		1641
#define OBJ_SPSYM42		1642
#define OBJ_SPSYM43		1643
#define OBJ_SPSYM44		1644
#define OBJ_SPSYM45		1645
#define OBJ_SPSYM46		1646
#define OBJ_SPSYM47		1647
#define OBJ_SPSYM48		1648
#define OBJ_SPSYM49		1649
#define OBJ_SPSYM50		1650


/* Turbulence Symbols */
#define OBJ_TURB00		1700
#define OBJ_TURB01		1701
#define OBJ_TURB02		1702
#define OBJ_TURB03		1703
#define OBJ_TURB04		1704
#define OBJ_TURB05		1705
#define OBJ_TURB06		1706
#define OBJ_TURB07		1707
#define OBJ_TURB08		1708

/* combo-symbols */
#define OBJ_CSYMB01		1801
#define OBJ_CSYMB02		1802
#define OBJ_CSYMB03		1803
#define OBJ_CSYMB04		1804
#define OBJ_CSYMB05		1805
#define OBJ_CSYMB06		1806
#define OBJ_CSYMB07		1807
#define OBJ_CSYMB08		1808
#define OBJ_CSYMB09		1809
#define OBJ_CSYMB10		1810
#define OBJ_CSYMB11		1811
#define OBJ_CSYMB12		1812
#define OBJ_CSYMB13		1813
#define OBJ_CSYMB14		1814
#define OBJ_CSYMB15		1815
#define OBJ_CSYMB16		1816
#define OBJ_CSYMB17		1817
#define OBJ_CSYMB18		1818
#define OBJ_CSYMB19		1819
#define OBJ_CSYMB20		1820
#define OBJ_CSYMB21		1821
#define OBJ_CSYMB22		1822
#define OBJ_CSYMB23		1823
#define OBJ_CSYMB24		1824
#define OBJ_CSYMB25		1825
#define OBJ_CSYMB26		1826
#define OBJ_CSYMB27		1827
#define OBJ_CSYMB28		1828

/* Weather Symbols */
#define OBJ_WXSYM000		1900
#define OBJ_WXSYM001		1901
#define OBJ_WXSYM002		1902
#define OBJ_WXSYM003		1903
#define OBJ_WXSYM004		1904
#define OBJ_WXSYM005		1905
#define OBJ_WXSYM006		1906
#define OBJ_WXSYM007		1907
#define OBJ_WXSYM008		1908
#define OBJ_WXSYM009		1909
#define OBJ_WXSYM010		1910
#define OBJ_WXSYM011		1911
#define OBJ_WXSYM012		1912
#define OBJ_WXSYM013		1913
#define OBJ_WXSYM014		1914
#define OBJ_WXSYM015		1915
#define OBJ_WXSYM016		1916
#define OBJ_WXSYM017		1917
#define OBJ_WXSYM018		1918
#define OBJ_WXSYM019		1919
#define OBJ_WXSYM020		1920
#define OBJ_WXSYM021		1921
#define OBJ_WXSYM022		1922
#define OBJ_WXSYM023		1923
#define OBJ_WXSYM024		1924
#define OBJ_WXSYM025		1925
#define OBJ_WXSYM026		1926
#define OBJ_WXSYM027		1927
#define OBJ_WXSYM028		1928
#define OBJ_WXSYM029		1929
#define OBJ_WXSYM030		1930
#define OBJ_WXSYM031		1931
#define OBJ_WXSYM032		1932
#define OBJ_WXSYM033		1933
#define OBJ_WXSYM034		1934
#define OBJ_WXSYM035		1935
#define OBJ_WXSYM036		1936
#define OBJ_WXSYM037		1937
#define OBJ_WXSYM038		1938
#define OBJ_WXSYM039		1939
#define OBJ_WXSYM040		1940
#define OBJ_WXSYM041		1941
#define OBJ_WXSYM042		1942
#define OBJ_WXSYM043		1943
#define OBJ_WXSYM044		1944
#define OBJ_WXSYM045		1945
#define OBJ_WXSYM046		1946
#define OBJ_WXSYM047		1947
#define OBJ_WXSYM048		1948
#define OBJ_WXSYM049		1949
#define OBJ_WXSYM050		1950
#define OBJ_WXSYM051		1951
#define OBJ_WXSYM052		1952
#define OBJ_WXSYM053		1953
#define OBJ_WXSYM054		1954
#define OBJ_WXSYM055		1955
#define OBJ_WXSYM056		1956
#define OBJ_WXSYM057		1957
#define OBJ_WXSYM058		1958
#define OBJ_WXSYM059		1959
#define OBJ_WXSYM060		1960
#define OBJ_WXSYM061		1961
#define OBJ_WXSYM062		1962
#define OBJ_WXSYM063		1963
#define OBJ_WXSYM064		1964
#define OBJ_WXSYM065		1965
#define OBJ_WXSYM066		1966
#define OBJ_WXSYM067		1967
#define OBJ_WXSYM068		1968
#define OBJ_WXSYM069		1969
#define OBJ_WXSYM070		1970
#define OBJ_WXSYM071		1971
#define OBJ_WXSYM072		1972
#define OBJ_WXSYM073		1973
#define OBJ_WXSYM074		1974
#define OBJ_WXSYM075		1975
#define OBJ_WXSYM076		1976
#define OBJ_WXSYM077		1977
#define OBJ_WXSYM078		1978
#define OBJ_WXSYM079		1979
#define OBJ_WXSYM080		1980
#define OBJ_WXSYM081		1981
#define OBJ_WXSYM082		1982
#define OBJ_WXSYM083		1983
#define OBJ_WXSYM084		1984
#define OBJ_WXSYM085		1985
#define OBJ_WXSYM086		1986
#define OBJ_WXSYM087		1987
#define OBJ_WXSYM088		1988
#define OBJ_WXSYM089		1989
#define OBJ_WXSYM090		1990
#define OBJ_WXSYM091		1991
#define OBJ_WXSYM092		1992
#define OBJ_WXSYM093		1993
#define OBJ_WXSYM094		1994
#define OBJ_WXSYM095		1995
#define OBJ_WXSYM096		1996
#define OBJ_WXSYM097		1997
#define OBJ_WXSYM098		1998
#define OBJ_WXSYM099		1999
#define OBJ_WXSYM103		2103
#define OBJ_WXSYM104		2104
#define OBJ_WXSYM105		2105
#define OBJ_WXSYM107		2107
#define OBJ_WXSYM201		2201
#define OBJ_WXSYM202		2202
#define OBJ_WXSYM203		2203

/* Lists */
#define OBJ_LISTCOUNTY		2301
#define OBJ_LISTZONE		2302
#define OBJ_LISTWFO		2303
#define OBJ_LISTSTATE		2304
#define OBJ_LISTWBCMZ		2305

/* CLASS_MET: Jets/GFAs */
#define OBJ_JET			2401
#define OBJ_TCA			2402	/* TCA */
#define OBJ_GFA			2403	/* GFA */
#define OBJ_GFA_P		2404	/* GFA Prime */
#define OBJ_TCE			2405	/* TCE */
#define OBJ_TCT			2406	/* TCT */
#define OBJ_TCB			2407	/* TCB */

/* BLANK */
#define OBJ_BLANK		2501

#endif
