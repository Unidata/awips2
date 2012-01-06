#include "time_format_struct.h"

/*******************************************************************

   load_tim.c
   
   PURPOSE
   To serve as include file for definitions of the time
   value in the output.
   
   NOTES
   This approach, where a text version and an index value of each 
   format is maintained, allows the index to the format to be
   determined once and then used repeatedly as needed; as opposed to 
   simply storing the requested format as a string and matching
   it with the appropriate processing each time it is needed.
   The function that uses these formats is format_datetime().
   
   The variable names given below do follow a naming convention:
   M=month, D=day, Y=year, W=weekday, H=hour, C=character, A=abbreviated.
   A field which can be either numeric or character is numeric 
   unless the "C" qualifier is before it as in "CM".
   Also a field which can be abbreviated or not is assumed to be
   unabbreviated unless the "A" qualifier precedes it as in "CAM".
   The number of letters and their order is significant.

   The default format gives datetimes as: 2/14 22:00
********************************************************************/


/* define a list of values for the format names and indices */

const time_format_struct TIME_FORMAT_LIST[] = 
{
   { "T_HEADER",     T_HEADER     },
   { "T_MMDDYYYY",   T_MMDDYYYY   },
   { "T_MMDDYY",     T_MMDDYY     },
   { "T_MMDD",       T_MMDD       },
   { "T_MMDDS",      T_MMDDS      },
   { "T_MMDDXM",     T_MMDDXM     },
   { "T_AWXM",       T_AWXM       },
   { "T_WXM",        T_WXM        },
   { "T_HHXM",       T_HHXM       },
   { "T_CMDDYYYY",   T_CMDDYYYY   },
   { "T_CAMDDYYYY",  T_CAMDDYYYY  },
   { "T_WCMDDYYYY",  T_WCMDDYYYY  },
   { "T_WCAMDDYYYY", T_WCAMDDYYYY },
   { "T_AWCAMDDYYYY",T_AWCAMDDYYYY},
   { "T_CMDD",       T_CMDD       },
   { "T_CAMDD",      T_CAMDD      },
   { "T_WCMDD",      T_WCMDD      },
   { "T_WCAMDD",     T_WCAMDD     },
   { "T_AWCAMDD",    T_AWCAMDD    },
   { "T_WHH",        T_WHH        },
   { "T_AWHH",       T_AWHH       },
   { "T_AWH",        T_AWH        },
   { "T_HHW",        T_HHW        },
   { "T_HW",         T_HW         },
   { "T_HHAW",       T_HHAW       },
   { "T_HHMMDD",     T_HHMMDD     },
   { "T_W",          T_W          },
   { "T_PHRASE",     T_PHRASE     },
   { "T_AW",         T_AW         },
   { "T_AWHHNN",     T_AWHHNN     },
   { "T_DDHHNN",     T_DDHHNN     },
   { "T_USER",       T_USER       },
   { "T_DEFAULT",    T_DEFAULT    },
   { "T_WWA",        T_WWA        }
};

/* set the number of items in the time format list */

int NUM_OF_TIME_FORMATS = 
   (sizeof(TIME_FORMAT_LIST) / sizeof(time_format_struct));
