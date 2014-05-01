      SUBROUTINE LOCPOS(KFILDO,LOC1,IPOS1,NBYTES,LBYPWD,
     1                  LOC,IPOS)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/grib2unpacker/RCS/locpos.f,v $
     . $',                                                             '
     .$Id: locpos.f,v 1.1 2004/09/16 16:51:50 dsa Exp $
     . $' /
C    ===================================================================
C
C
C        MAY     1999   GLAHN   TDL   GRIB2
C        JANUARY 2001   GLAHN   COMMENTS
C 
C        PURPOSE 
C            ACCEPTS A WORD LOCATION, LOC1, AND BIT POSITION
C            WITHIN THAT WORD, IPOS1, ADDS NBYTES, AND 
C            CALCULATES A NEW WORD LOCATION, LOC, AND BIT
C            POSITION, IPOS.  IT IS ASSUMED THAT IPOS1 POINTS
C            TO A BIT JUST AFTER A FULL BYTE.  CALUCLATIONS 
C            ARE IN BYTES RATHER THAN BITS TO PREVENT POSSIBLE
C            OVERFLOW.  USED WITH GRIB2 ROUTINE UNPK_GRIB2.  
C            FOR INSTANCE, IF LOC1 AND IPOS1 IS THE BEGINNING
C            OF A SECTION, AND THE SECTION IS NBYTES LONG, THEN
C            THE NEXT SECTION SHOULD START AT IPOS IN LOC.
C
C        DATA SET USE 
C           KFILDO - UNIT NUMBER FOR OUTPUT (PRINT) FILE. (OUTPUT) 
C
C        VARIABLES 
C              KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (INPUT)
C                LOC1 = WORD POSITION OF NEXT BYTE.  (INPUT)
C               IPOS1 = BIT POSITION IN LOC1.  (INPUT)
C              NBYTES = NUMBER OF BYTES TO ADD.  (INPUT)
C              LBYPWD = BYTES PER WORD FOR EITHER A 32- OR 64-BIT
C                       MACHINE.  (INPUT)
C                 LOC = WORD POSITION OF NEXT BYTE.  (OUTPUT)
C                IPOS = BIT POSITION OF BEGINNING OF NEXT BYTE.
C                       (OUTPUT)
C
C             LOCAL VARIABLES
C              KBYTES = THE NUMBER OF BYTES PRIOR TO BIT POS1 IN
C                       WORD LOC1.
C
C        NON SYSTEM SUBROUTINES CALLED 
C           NONE
C
      KBYTES=(LOC1-1)*LBYPWD+(IPOS1-1)/8
C        KBYTES IS THE NUMBER OF BYTES PRIOR TO BIT IPOS1 IN
C        WORD LOC1.
      KBYTES=KBYTES+NBYTES
      LOC=KBYTES/LBYPWD+1
      IPOS=(KBYTES-(LOC-1)*LBYPWD)*8+1
      RETURN
      END
