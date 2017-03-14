/* checkInputTSMsgAllow.c						*/
/*									*/
/* Scan ts array to find any input time series that allow missing data. */
/*									*/
/* Originally written by George Smith - HRL - 941120                    */

checkInputTSMsgAllow(

float   ts_float[],        /* time series floating data */
char    ts_char[][4],      /* time series character data */
char    oper_names_found[][8],    /* operation name */
char    oper_types_found[][8],    /* operation type */
int     oper_locp[],       /* location of the beginning of operations parameter array */
char    ts_id[][8],        /* time series id */
char    ts_datatype[][8],  /* time series data type array */
int     ts_delta_t[],      /* sample time interval array */
int     *number_in_list    /* operation and parameter array index pointer */
                    )
{
int     count;               /* counter */
int     include_time_series; /* include time series flag */
int     locts;               /* location of the time series array */
int     loc_datatype;        /* location of the data type in the time series array */
int     locd;                /* location of the data array */
int     ts_test;             /* integer ts dt for testing purposes */

static char    msng_allowed_types[9][5] = {
					   "DQIN",
					   "DQME",
					   "PELV",
					   "QIN ",
					   "RQIN",
					   "RQME",
					   "RQOT",
					   "STG ",
					   "TWEL"
					  };
locts = 1;

while (locts > 0)
 {
  if(ts_float[locts-1] >= 1.0)
    {                              /*  There are remaining t.s.      */
     locd = ts_float[locts-1 + 7]; /*  Location of data in d array   */
     if(ts_float[locts-1] < 3.0)
       {                        /*  This is an input or update    */
				/*  time series.                  */
	loc_datatype = locts-1 + 4;
	include_time_series = 0;
	for (count = 0; count < 9; count++)
	    {
	     if(strncmp(ts_char[loc_datatype],
			msng_allowed_types[count],
			4)
		== 0)
 	       {
		include_time_series = 1;
		if(*number_in_list > 0)       /* Check to be sure all   */
		                             /* t.s. have same delta_t */
                  {
		   ts_test = ts_float[locts-1 + 5];
		   if(ts_delta_t[0] != ts_test)
		      include_time_series = 0;   /* Do not include t.s. */
		  }                              /* if delta_t's differ */
		break;
	       }                       /*  end if(strncmp..  */
	    }                          /*  end for           */
	if(include_time_series == 1)
	  {
	   strncpy(ts_id[*number_in_list], ts_char[locts-1 + 2], 8);
	   memset(ts_datatype[*number_in_list], '\0', 8);
	   strncpy(ts_datatype[*number_in_list], ts_char[loc_datatype], 4);
	   ts_delta_t[*number_in_list] = ts_float[locts-1 + 5];
	/*
	 * Now copy t.s. info into oper_ part of mod_data structure so
	 * the mod_popup function will display the time series names and
	 * data types in the operations (sic) list.
	 */
	   strncpy(oper_names_found[*number_in_list], ts_id[*number_in_list], 8);
	   strncpy(oper_types_found[*number_in_list], ts_datatype[*number_in_list], 8);
	   oper_locp[*number_in_list] = ts_delta_t[*number_in_list];

	   (*number_in_list)++;
	  }                            /*  end if(include_..  */
       }                               /*  end if(ts[locts..  */
     locts = ts_float[locts-1 + 1];
    }                                  /*  end if(ts_float..  */
  else
    {
     locts = 0;
    }
 }                                     /*  end while          */

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Utility/RCS/checkInputTSMsgAllow.c,v $";
 static char rcs_id2[] = "$Id: checkInputTSMsgAllow.c,v 1.2 2002/02/11 19:50:37 dws Exp $";}
/*  ===================================================  */

}
