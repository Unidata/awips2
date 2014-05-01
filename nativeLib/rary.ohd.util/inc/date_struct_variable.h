#ifndef DATE_STRUCT_VARIABLE_H
#define DATE_STRUCT_VARIABLE_H

typedef struct         {
			int   month,day,year,hour;
			char  cdate[11];
			char  ldate[20];
			char  lldate[20];
			char  dttm[22];    /* Informix datetime format */
		       } date_struct;


date_struct    date_st3;

#endif /* #ifndef DATE_STRUCT_VARIABLE_H */
