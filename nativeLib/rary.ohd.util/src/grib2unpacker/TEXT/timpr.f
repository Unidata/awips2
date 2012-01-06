      SUBROUTINE TIMPR(KFILDS,KFILDO,ITITLE) 
C 
C        JANUARY   1994   CHAMBERS, GLAHN   TDL  MOS-2000
C        SEPTEMBER 2002   GLAHN   ADDED COMMAS IN FORMAT 110
C 
C        PURPOSE 
C            A DATE/TIME STAMPING FUNCTION WHICH WILL WRITE THE DATE AND
C            TIME AND A USER MESSAGE OF UP TO 20 CHARACTERS ON UNIT 
C            NO. KFILDO.  UNDER THE D COMPILER OPTION, MESSAGE WILL 
C            ALSO BE WRITTEN TO THE CURRENT CONSOLE, UNIT NO. KFILDS
C            (ONLY) WHEN KFILDS NE KFILDO. 
C 
C        DATA SET USE 
C            KFILDS - UNIT NUMBER FOR OUTPUT TO CURRENT CONSOLE.  (OUTPUT) 
C            KFILDO - OUTPUT (PRINT) FILE UNIT NUMBER.  (OUTPUT) 
C 
C        VARIABLES 
C 
C            INPUT 
C              KFILDS = UNIT NUMBER FOR OUTPUT TO CURRENT CONSOLE. 
C              KFILDO = OUTPUT (PRINT) FILE UNIT NUMBER. 
C              ITITLE = USER MESSAGE.  (CHARACTER*20) 
C 
C            INTERNAL 
C              DTATME = DATE AND TIME FOR PRINTING.  (CHARACTER*24)
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            NONE. 
C 
      CHARACTER*24 DTATME
      CHARACTER*20 ITITLE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/grib2unpacker/RCS/timpr.f,v $
     . $',                                                             '
     .$Id: timpr.f,v 1.1 2004/09/16 16:51:50 dsa Exp $
     . $' /
C    ===================================================================
C
C 
      CALL FDATE(DTATME) 
D     IF(KFILDS.NE.KFILDO)WRITE(KFILDS,110)ITITLE,DTATME
      WRITE(KFILDO,110)ITITLE,DTATME
 110  FORMAT(' ',A20,'   DATE/TIME:  ',A24) 
      RETURN 
      END 
