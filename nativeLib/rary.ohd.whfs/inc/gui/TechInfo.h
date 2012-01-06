
#ifndef techinfo_h
#define techinfo_h

/*	
	Data definitions.
*/
extern Widget		techDS;
extern Widget		techFM;
extern Widget		tchAppLbl;
extern Widget		tchAppInf;
extern Widget		tchVerLbl;
extern Widget		tchVerInf;
extern Widget		tchDatLbl;
extern Widget		tchDatInf;
extern Widget		techLbl;
extern Widget		nwsLbl;
extern Widget		strLbl;
extern Widget		zipLbl;
extern Widget		foneLbl;
extern Widget		techSP;
extern Widget		tchclsPB;


/*
	Function prototypes.
*/
void	techinfo_show(Widget w, char *app, char *ver, char *date);
void	techinfo_close();


#endif
