/************************************************************************
 * xwprm.h								*
 *									*
 * This include file constants definition for XW driver.		*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI 	 	 4/98   extract from xwcmn.h			*
 * E. Safford/GSC	12/98	add 1 to MAX_PIXMAP for empty frame	*
 * S. Law/GSC		01/00	added ANIM_* defines			*
 * E. Safford/GSC	05/00	move MAX_LOOP in from xwcmn.h, increase	*
 *					MAX_PIXMAP to 88		*
 * R. Tian/SAIC		05/02	added FAX_COLORS			*
 * H. Zeng/XTRIA	04/03	modified MAX_PIXMAP value		*
 * T. Piper/SAIC	07/03	added color bank IDs			*
 * T. Piper/SAIC	01/04	moved color bank IDs to color.h		*
 * T. Piper/SAIC	01/04	removed default color #s, in color.h	*
 * H. Zeng/XTRIA	03/04	increase the value of MAX_LOOP		*
 * B. Yin/SAIC		05/04	increase the value of MAX_PIXMAP	*
 ***********************************************************************/

#ifndef XWPRM_H
#define XWPRM_H

#define MAX_WINDOW	5   /* max # of windows can be opened simultaneously */
#define MAX_LOOP	16  /* max # of loops in nmap2 */
#define MAX_PIXMAP      136 /* maximum # of pixmaps in one window */
#define DEFAULT_WNAME   "GEMPAK 5" /* default window name */
#define WNAME_LEN	73           /* maximum window name length */

/*
 * definition of animation commands.
 */
#define ANIM_HIDE_DATA	0
#define ANIM_LOOP_FORW	1
#define ANIM_STOP_LOOP	2
#define ANIM_STEP_FORW	3
#define ANIM_STEP_BACK	4
#define ANIM_LOOP_BACK	5
#define ANIM_ROCK_LOOP	6
#define ANIM_FRST_FRAM	7
#define ANIM_LAST_FRAM	8
#define ANIM_CURR_FRAM	9

#endif  /* XWPRM_H */
