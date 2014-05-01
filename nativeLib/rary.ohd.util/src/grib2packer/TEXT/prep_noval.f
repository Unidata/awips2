      SUBROUTINE PREP_NOVAL(IB,NXY,IBITMAP,IPKOPT,IER)
C
C        MAY      2000   LAWRENCE  ORIGINAL CODING
C        JANUARY  2001   GLAHN     COMMENTS
C        FEBRUARY 2002   GLAHN     IPKOPT SET = 2 WHEN A BIT MUST
C                                  BE PAKCED 
C        MARCH    2002   GLAHN     COMMENTS
C
C        PURPOSE
C            TO HANDLE THE SITUATION WHERE THERE ARE NO VALUES
C            IN THE GRID. THREE POSSIBILITIES ARE CHECKED BY THIS
C            ROUTINE:
C
C            1) THERE ARE NO VALUES IN THE GRID, BUT THERE IS A
C               BIT-MAP AND IT INDICATES THAT THERE SHOULD BE. THIS
C               IS TREATED AS AN ERROR.
C            2) THERE ARE NO VALUES IN THE GRID, AND THE BIT-MAP
C               INDICATES THAT THERE ARE NO VALUES IN THE GRID.
C               THE USER IS NOTIFIED AND THE FIELD IS
C               PACKED USING THE SIMPLE METHOD.
C            3) THERE ARE NO VALUES IN THE GRID, AND THERE IS NO
C               ACCOMPANYING BIT-MAP. THIS IS NOT CONSIDERED TO
C               BE A REASONABLE SITUATION. IT IS TREATED AS AN
C               ERROR.
C
C        DATA SET USE
C           KFILDO - UNIT NUMBER FOR OUTPUT (PRINT) FILE. (OUTPUT)
C
C        VARIABLES
C               IB(K) = THE BIT MAP WHEN ONE IS USED.  (INPUT/OUTPUT)
C                       IT CAN BE INPUT OR IN CAN BE CALCULATED IF
C                       THE SIMPLE METHOD IS USED (K=1,NXY).
C                       COMPLEX AND SPATIAL DIFFERENCING DO NOT
C                       USE A BIT MAP, BUT WILL ACCEPT ONE AND INSERT
C                       THE MISSING VALUES.
C                 NXY = DIMENSION OF IB( ).  (INPUT)
C             IBITMAP = 1 WHEN THERE IS A BITMAP IN IB( ).
C                       0 OTHERWISE.
C                       (INPUT)
C              IPKOPT = PACKING INDICATOR:
C                       0 = ERROR, DON'T PACK
C                       1 = PACK IA( ), SIMPLE
C                       2 = PACK IA( ) AND IB( ), SIMPLE
C                       3 = PACK COMPLEX OR SPATIAL DIFFERENCING
C                       4 = PACK COMPLEX.
C                       (OUTPUT)
C                 IER = RETURN STATUS CODES. VALUES CAN COME FROM
C                       SUBROUTINES; OTHERWISE:
C                       0 = GOOD RETURN.
C                     902 = THERE ARE NO "GOOD" VALUES IN THE GRID
C                           AND THE BIT-MAP INDICATES THAT.  NOT TREATED
C                           AS FATAL IN PREPR.
C                     903 = THERE ARE NO VALUES IN THE GRID AND THE
C                           BIT-MAP INDICATES THAT THERE SHOULD BE.
C                           TREATED AS FATAL IN PREPR.
C                     904 = THERE ARE NO VALUES IN THE GRID, AND THERE
C                           IS NO BIT-MAP.  TREATED AS FATAL IN PREPR.
C                    
C        LOCAL VARIABLES
C                     K = A LOOP INDEX VARIABLE.
C
C        NON SYSTEM SUBROUTINES CALLED
C           NONE
C
      DIMENSION IB(NXY)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/grib2packer/RCS/prep_noval.f,v $
     . $',                                                             '
     .$Id: prep_noval.f,v 1.1 2004/09/16 16:52:29 dsa Exp $
     . $' /
C    ===================================================================
C
C
C        THIS SECTION WHEN THERE ARE NO VALUES IN
C        THE DATA FIELD. THAT IS, NVAL = 0.
      IF(IBITMAP.EQ.1)THEN
C
         DO 10 K=1,NXY
C
            IF(IB(K).EQ.1)THEN
C                 THERE IS A BIT MAP.
C                 THERE ARE NO VALUES IN THE GRID
C                 AND THE BIT MAP INDICATES THERE
C                 SHOULD BE. DON'T PACK.
               IER=903
               IPKOPT=0
               GO TO 900
            ENDIF
C
 10      CONTINUE
C
C           THERE IS A BIT MAP.
C           THERE ARE NO VALUES IN THE GRID
C           AND THE BIT MAP INDICATES NO VALUES.
C           NOTIFY USER AND PACK.  EVEN THOUGH ALL VALUES
C           IN THE BIT MAP ARE ZERO, THE WMO STANDARDS
C           EVIDENTLY DO NOT ALLOW FOR NOT PACKING THE 
C           BIT MAP.
         IER=902
         IPKOPT=2
C
      ELSE
C           THERE ARE NO VALUES IN GRID AND NO
C           BIT MAP.  NOT A REASONABLE SITUATION.
C           DON'T PACK.
         IER=904
         IPKOPT=0
         GO TO 900
      ENDIF
C
 900  RETURN
      END
