      SUBROUTINE UNPK_TRACE(KFILDO,JER,NDIER,KIER,NERR,ISEVERE)
C
C        JUNE    2000   LAWRENCE   ERROR POSTING AND TRACING FOR GRIB2
C        JANUARY 2001   GLAHN      COMMENTS; INSERTED KIER=KIER-1;
C                                  CHANGED IER( , ) TO JER( , )
C
C        PURPOSE 
C            INSERTS AN ERROR CODE AND ITS SEVERITY INTO JER( , ).
C            INSERTS 999 WHEN JER( , ) IS FULL.  IN THAT CASE,  
C            TWO ERROR CODES ARE LOST.  SINCE THE PROGRAM PROCEEDS,
C            MORE MAY BE LOST BEFORE PROGRAM COMPLETION.
C
C        DATA SET USE 
C           KFILDO - UNIT NUMBER FOR OUTPUT (PRINT) FILE. (OUTPUT) 
C
C        VARIABLES 
C             KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (INPUT)
C           JER(J,K) = ARRAY OF ERRORS (J=1,NDIER) (K=1,2)
C                      (INPUT/OUTPUT)
C              NDIER = THE DIMENSION OF JER( , ). THE MAXIMUM NUMBER
C                      OF ERRORS ALLOWED TO BE REPORTED IN JER( , ).
C                      (INPUT)
C               KIER = NUMBER OF VALUES IN JER( , ) UPON ENTRY AND
C                      EXIT.  (INPUT/OUTPUT)
C               NERR = VALUE TO INSERT INTO JER(KIER,1).  (INPUT)
C            ISEVERE = THE SEVERITY LEVEL OF THE DIAGNOSTIC TO INSERT
C                      INTO JER(KIER,2).  VALID SEVERITY LEVELS ARE:
C                      0 = NOT A PROBLEM
C                      1 = WARNING
C                      2 = FATAL
C                      (INPUT)
C
C        LOCAL VARIABLES
C           NONE
C
C        NON SYSTEM SUBROUTINES CALLED 
C           NONE
C
      DIMENSION JER(NDIER,2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/grib2unpacker/RCS/unpk_trace.f,v $
     . $',                                                             '
     .$Id: unpk_trace.f,v 1.1 2004/09/16 16:51:50 dsa Exp $
     . $' /
C    ===================================================================
C
C
      KIER=KIER+1
C
      IF(KIER.LE.NDIER)THEN
         JER(KIER,1)=NERR
         JER(KIER,2)=ISEVERE
      ELSE
         KIER=KIER-1
         JER(KIER,1)=999
         JER(KIER,2)=2
      ENDIF
C
      RETURN
      END
