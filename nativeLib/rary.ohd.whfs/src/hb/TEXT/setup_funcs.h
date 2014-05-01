/*
	File:		setup_funcs.h
	Date:		12/13/1994
	Author:		Dale Shelton

	Purpose:	Define the function pointers
			for the setup_tree structure.
			Used to perform table specific
			actions.
*/


#ifndef setup_funcs_h
#define setup_funcs_h


void	coe_load(void);
void	cpm_load(void);
void	comms_load(void);
void	recip_load(void);
void	spons_load(void);
void	downer_load(void);
void	gmaint_load(void);
void	gowner_load(void);
void	gtype_load(void);
void	hsa_load(void);
void	net_load(void);
void	rowner_load(void);
void	rfc_load(void);
void	towner_load(void);
void	tpayor_load(void);
void	ttype_load(void);
void	waro_load(void);
void	wfo_load(void);
void	wsfo_load(void);
void	dtype_load(void);


#endif
