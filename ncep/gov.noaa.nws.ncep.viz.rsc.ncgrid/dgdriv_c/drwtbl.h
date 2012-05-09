/************************************************************************
 * drwtbl.h                                                             *
 *                                                                      *
 * This file contains structure definitions used by GEMPAK to store     *
 * drawing settings for different types of elements			*
 *                                                                      *
 **                                                                     *
 * E. Wehner/EAi	 3/97	Created					*
 * D. Keiser/GSC	 4/97	Added new attributes 			*
 * D. Keiser/GSC	 7/97	Redesigned setting structure 		*
 *				increase MAX_TYPES			*
 ***********************************************************************/


#define MAX_TYPES		40

/*
 * Settings info - Settings information - colors, width, height, etc
 * for each drawable element
 */


typedef struct frnttyp
{
    int		size;
    int		strk;
    int		dir;
} FrntTyp;

typedef struct mrkrtyp
{
    int		type;
    int		mkhw;
    float	size;
    int		width;
} MrkrTyp;

typedef struct linetyp
{
    int		type;
    int		lthw;
    int		width;
    int		lwhw;
} LineTyp;

typedef struct texttyp
{
    int		txfn;
    int		txhw;
    float	size;
    int		width;
    float	rotat;
    int		xoff;
    int		yoff;
    int		alin;
} TextTyp;

typedef struct splntyp
{
    int		type;
    int		strk;
    int		dir;
    float	size;
    int		width;
} SplnTyp;

typedef struct symbtyp
{
    int		width;
    float	size;
} SymbTyp;

typedef struct windtyp
{
    int		type;
} WindTyp;

typedef struct setting
{
    char	gemtyp[8];
    char	vgtyp[10];
    int		maj_col;
    int		min_col;
    union
    {
	FrntTyp	frnt;
	MrkrTyp	mrkr;
	LineTyp	line;
	TextTyp	text;
	SplnTyp	spln;
	SymbTyp	symb;
	WindTyp	wind;
    } atttyp;
} setting_t[MAX_TYPES], sglset_t;
