/*
 *  The following code is used by the time series mods keyword functions.
 *
 *  Taken from the old mods_info.h - D. Page - 14 Oct. 1995
 */

#ifndef ChooseTStypestruct
#define ChooseTStypestruct


void     do_first();
void     do_last();
void     do_opType();


int     Do_First;
int     Do_Last;
int     Do_Before_operation;



static xs_menu_struct Choose_TStype_struct[] =
	{
	{"First" , do_first, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"Last" , do_last, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"Before Operation" , do_opType, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	};

static xs_menu_struct Choose_TStype_struct_LB[] =
	{
	{"Last" , do_last, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"Before Operation" , do_opType, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	};

static xs_menu_struct Choose_TStype_struct_BL[] =
	{
	{"Before Operation" , do_opType, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"Last" , do_last, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	};

static xs_menu_struct Choose_TStype_struct_FB[] =
	{
	{"First" , do_first, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	{"Before Operation" , do_opType, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	};

static xs_menu_struct Choose_TStype_struct_B[] =
	{
	{"Before Operation" , do_opType, NULL, TRUE, PUSH_BUTTON, NULL, 0, NULL},
	};

#define MAX_CHARS_IN_TSMOD_KEYWORD 17

#endif  /*    end definition of ChooseTStypestruct      */
