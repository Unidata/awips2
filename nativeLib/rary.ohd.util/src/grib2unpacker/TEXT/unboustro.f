      SUBROUTINE UNBOUSTRO(A,NX,NY)
C
C        APRIL    2000   LAWRENCE   CALLED BY UNPKSECDIF AND 
C                                   UNPKCMBM.
C
C        PURPOSE
C            TO SCAN AND REORDER AN ARRAY OF DATA BOUSTROPHEDONICALLY.
C            BOUSTROPHEDONIC ORDERING IS A PROCESS IN WHICH
C            AN ARRAY OF DATA IS SCANNED STARTING FROM THE LAST
C            ROW AND WORKING UP TO THE FIRST ROW. THE ROWS ARE
C            ALTERNATIVELY SCANNED FROM LEFT TO RIGHT AND FROM
C            RIGHT TO LEFT. THIS SCANNING METHOD IS ADVANTAGEOUS
C            WHEN USED TO PROCESS A FIELD OF DATA THAT WILL BE
C            PACKED WITH THE GRIB2 COMPLEX PACKING METHOD (WITH
C            OR WITHOUT SPATIAL DIFFERENCES). IT GENERALLY
C            PRODUCES A SMOOTHER DATA FIELD WHICH CAN BE MORE
C            EASILY BROKEN DOWN INTO GROUPS, A PROCESS WHICH
C            IS CORE TO THE COMPLEX PACKING METHOD.
C
C        DATA SET USE
C           NONE
C
C        VARIABLES
C                A(K) = CONTAINS THE DATA FIELD TO BE
C                       BOUSTROPHEDONICALLY REORDERED
C                       (K=1,NX*NY).  (INPUT/OUTPUT)
C                  NX = THE NUMBER OF COLUMNS IN THE PRODUCT.  (INPUT)
C                  NY = THE NUMBER OF ROWS IN THE PRODUCT.  (INPUT)
C
C        LOCAL VARIABLES
C                TEMP = SAVES THE VALUE OF A( ) CURRENTLY BEING SWAPPED
C                       SO THAT IT IS NOT OVERWRITTEN.
C                   K = LOOPING/INDEX VARIABLE.
C              NXDIV2 = THE NUMBER OF COLUMNS IN THE PRODUCT DIVIDED
C                       BY 2 AND TRUNCATED TO A WHOLE NUMBER. 
C              NXJYP1 = THE PRODUCT OF NX AND JY INCREMENTED BY 1.
C
C         NON SYSTEM SUBROUTINES CALLED
C            NONE
C
      DIMENSION A(NX*NY)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/grib2unpacker/RCS/unboustro.f,v $
     . $',                                                             '
     .$Id: unboustro.f,v 1.1 2004/09/16 16:51:50 dsa Exp $
     . $' /
C    ===================================================================
C
C
      K=0
      NXDIV2=NX/2
C
      DO 20 JY=1,NY
C
         IF(MOD(JY,2).EQ.0)THEN
C
            DO 10 IX=1,NXDIV2
               K=K+1
               NXJYP1=NX*JY+1
               TEMP=A(K)
               A(K)=A(NXJYP1-IX)
               A(NXJYP1-IX)=TEMP
 10         CONTINUE
C
            IF(MOD(NX,2).EQ.0)THEN
              K=K+NXDIV2
            ELSE
              K=K+NXDIV2+1
            ENDIF
C
         ELSE
            K=K+NX
         ENDIF
C
 20   CONTINUE
C
      RETURN
      END
