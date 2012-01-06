/*  nmap_dttm.h  */

#ifndef _DTTM_H
#define _DTTM_H


typedef struct {
        int  year;
        int  mon;
        int  day;
        int  hour;
        int  min;
}dttmi_t;

typedef struct {
        Widget  year;
        Widget  mon;
        Widget  day;
        Widget  hhmm;
}dtmbw_t;


#endif	/* _DTTM_H */
