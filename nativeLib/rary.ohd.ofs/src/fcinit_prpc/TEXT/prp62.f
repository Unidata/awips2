C MEMBER PRP62
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 
C
C @PROCESS LVL(77)
C
      SUBROUTINE PRP62 (P)

C     THIS IS THE PRINT PARAMETER ROUTINE FOR ADJUST-H.

C     THIS ROUTINE ORIGINALLY WRITTEN BY
C        JOANNE R. SALERNO - NWRFC   FEB 1998      

C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012

C     POSITION     CONTENTS OF P ARRAY

C      1           VERSION NUMBER OF OPERATION
C      2-19        GENERAL NAME OR TITLE

C  INPUT
C     20-22        DW FCST  STAGE      - CSTAP (STGP)
C
C     TIDAL RANGE/SLICE DEFINITION
C     23-24        RANGE LIMIT SERIES IDENTIFIER   -  RANGE
C     25           RANGE LIMIT TYPE CODE           -  TIDB
C                  RANGE(1) R1 LOWER LIMIT
C                  RANGE(2) R2 LOWER LIMIT
C                  RANGE(3) R3 LOWER LIMIT
C                  RANGE(4) R4 LOWER LIMIT
C                  RANGE(5) R4 LOWER LIMIT
C                  RANGE(1) R1 LOWER LIMIT
C
C     OBSERVED PRTO3 AVERAGE  BALANCE TIME SERIES PER RANGE
C     26-27        RANGE1 AVE TIDE BALANCE TIME SERIES ID  -  SBALR1
C     28           RANGE1 AVE TIDE BALANCE TYPE CODE       -  TIDB
C     29-30        RANGE2 AVE TIDE BALANCE TIME SERIES ID  -  SBALR2
C     31           RANGE2 AVE TIDE BALANCE TYPE CODE       -  TIDB
C     32-33        RANGE3 AVE TIDE BALANCE TIME SERIES ID  -  SBALR3
C     34           RANGE3 AVE TIDE BALANCE TYPE CODE       -  TIDB
C     35-36        RANGE4 AVE TIDE BALANCE TIME SERIES ID  -  SBALR4
C     37           RANGE4 AVE TIDE BALANCE TYPE CODE       -  TIDB
C
C  OUTPUT
C     38-39        DWADJ  ADJUSTED CSTA TIME SERIES ID    -  DWADJ
C     40           DWADJ  ADJUSTED DATE TYPE CODE         -  STGA

C**********************************************************************

C     THE NUMBER OF ELEMENTS REQUIRED IN THE P ARRAY IS  40

C     THE NUMBER OF ELEMENTS REQUIRED IN THE C ARRAY IS   0

C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012

      DIMENSION P(*)

      INTEGER IVERSN

C     COMMON BLOCKS
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prp62.f,v $
     . $',                                                             '
     .$Id: prp62.f,v 1.2 1998/10/14 13:47:09 page Exp $
     . $' /
C    ===================================================================
C

C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012

C        ADJUST STAGE APPLY   - VERSION XXXX

C        INPUT TIME SERIES                    ID     CODE
C           DW OBS/FCST STAGE              XXXXXXXX  XXXX
C           RANGE LIMIT                    XXXXXXXX  XXXX
C           R1 AVE BALANCE                 XXXXXXXX  XXXX
C           R2 AVE BALANCE                 XXXXXXXX  XXXX
C           R3 AVE BALANCE                 XXXXXXXX  XXXX
C           R4 AVE BALANCE                 XXXXXXXX  XXXX

C        PRIMARY OUTPUT TIME SERIES
C           ADJUST STAGE                   XXXXXXXX  XXXX
 

C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012

C     CHECK TRACE LEVEL
      CALL FPRBUG ('PRP62   ',1,62,IBUG)

C     PRINT HEADING THEN TITLE INFORMATION FROM P ARRAY

      IVERSN = INT(P(1))
      WRITE(IPR,501) IVERSN,(P(I),I=2,19)
 501  FORMAT(//,10X,'ADJUST STAGE- VERSION ',I4,/,
     +          10X,18A4,/)


C     PRINT TIME SERIES INFORMATION FROM P ARRAY

      WRITE(IPR,504) (P(I),I=20,25),(P(I),I=26,40)
 504  FORMAT(/,10X,'INPUT TIME SERIES',18X,'ID',6X,'CODE',/,
     +     13X,'DW OBS/FCST  STAGE',12X,2A4,2X,A4,/,
     +     13X,'RANGE LIMIT       ',12X,2A4,2X,A4,/,
     +     13X,'AVE BALANCE RANGE1',12X,2A4,2X,A4,/,
     +     13X,'AVE BALANCE RANGE2',12X,2A4,2X,A4,/,
     +     13X,'AVE BALANCE RANGE3',12X,2A4,2X,A4,/,
     +     13X,'AVE BALANCE RANGE4',12X,2A4,2X,A4,//,
     +     10X,'PRIMARY OUTPUT TIME SERIES',/,
     +     13X,'ADJUSTED STAGE TS ',12X,2A4,2X,A4,/)


      IF (ITRACE.GE.1) WRITE(IODBUG,199)
 199  FORMAT('PRP62:  EXITED:')

      RETURN
      END
