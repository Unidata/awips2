      SUBROUTINE BOUSTRO_FLT(A,NX,NY)
C
C        APRIL   2000   LAWRENCE   CALLED BY PREPR
C        JANUARY 2001   GLAHN      COMMENTS; CHANGED ALGORITHM TO
C                                  TO USE 2-D ARRAY RATHER THAN 1-D
C
C        PURPOSE
C            SCANS AND REORDERS AN ARRAY OF DATA BOUSTROPHEDONICALLY.
C            BOUSTROPHEDONIC ORDERING IS A PROCESS IN WHICH
C            AN ARRAY OF DATA IS SCANNED STARTING FROM THE JY = 1
C            ROW AND WORKING UP TO THE JY = NY ROW. THE ROWS ARE
C            ALTERNATIVELY SCANNED FROM LEFT TO RIGHT AND FROM
C            RIGHT TO LEFT. THIS SCANNING METHOD IS ADVANTAGEOUS
C            WHEN USED TO PROCESS A FIELD OF DATA THAT WILL BE
C            PACKED WITH THE GRIB2 COMPLEX PACKING METHOD (WITH
C            OR WITHOUT SPATIAL DIFFERENCES).  IT GENERALLY
C            PRODUCES A DATA ARRAY WHICH CAN BE BETTER BROKEN
C            DOWN INTO GROUPS, A PROCESS WHICH IS CORE TO THE
C            COMPLEX PACKING METHOD.
C
C        DATA SET USE
C           NONE
C
C        VARIABLES
C            A(IX,JY) = CONTAINS THE DATA FIELD TO BE
C                       BOUSTROPHEDONICALLY REORDERED
C                       (IX=1,NX) (JY=1,NY).  (INPUT/OUTPUT)
C                  NX = THE NUMBER OF COLUMNS IN THE ARRAY.  (INPUT)
C                  NY = THE NUMBER OF ROWS IN THE ARRAY.  (INPUT)
C
C        LOCAL VARIABLES
C                NXP1 = NX + 1.  THIS IS DONE UP FRONT IN CASE SOME
C                       COMPILERS WON'T LIFT IT OUT OF THE LOOP.
C                TEMP = SAVES THE VALUE OF A( , ) CURRENTLY BEING SWAPPED
C                       SO THAT IT IS NOT OVERWRITTEN.
C
C         NON SYSTEM SUBROUTINES CALLED
C            NONE
C
      DIMENSION A(NX,NY)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/grib2packer/RCS/boustro_flt.f,v $
     . $',                                                             '
     .$Id: boustro_flt.f,v 1.1 2004/09/16 16:52:29 dsa Exp $
     . $' /
C    ===================================================================
C
C
      NXP1=NX+1
C
      DO 125 JY=1,NY
C
         IF(MOD(JY,2).EQ.0)THEN
C
            DO 123 IX=1,NX/2
               TEMP=A(IX,JY)
               A(IX,JY)=A(NXP1-IX,JY)
               A(NXP1-IX,JY)=TEMP
 123        CONTINUE
C
         ENDIF
C  
 125  CONTINUE
C
      RETURN
      END
