C MEMBER PRP59
C-----------------------------------------------------------------------
C
C                             LAST UPDATE:
C
C @PROCESS LVL(77)
C
      SUBROUTINE PRP59 (P)

C     THIS IS THE PRINT PARAMETER ROUTINE FOR TIDEREV.

C     THIS ROUTINE ORIGINALLY WRITTEN BY
C        JOANNE R. SALERNO - NWRFC   OCT 1997

C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012

C     POSITION     CONTENTS OF P ARRAY

C      1           VERSION NUMBER OF OPERATION
C      2-19        GENERAL NAME OR TITLE

C     ASTORIA DWOPER OBSERVED STAGE
C     20-21       OBSERVED STAGE TIME SERIES IDENTIFIER
C     22          OBSERVED STAGE DATA TYPE CODE

C     ASTORIA NOS STAGE
C     23-24       NOS TIME SERIES IDENTIFIER
C     25          NOS DATA TYPE CODE

C     ASTORIA MAX/MIN OBS/FX BALANCE
C     26-27       OBS/FX MAX/MIN BALANCE TIME SERIES IDENTIFIER - TIDE1
C     28            "       "       "    DATA TYPE CODE
C     29-30       OBS/FX MAX/MIN BALANCE TIME SERIES IDENTIFIER - TIDE2
C     31            "       "       "    DATA TYPE CODE
C     32-33       OBS/FX MAX/MIN BALANCE TIME SERIES IDENTIFIER - TIDE3
C     34            "       "       "    DATA TYPE CODE
C     35-36       OBS/FX MAX/MIN BALANCE TIME SERIES IDENTIFIER - TIDE4
C     37            "       "       "    DATA TYPE CODE

C**********************************************************************

C     THE NUMBER OF ELEMENTS REQUIRED IN THE P ARRAY IS  37

C     THE NUMBER OF ELEMENTS REQUIRED IN THE C ARRAY IS   0

C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012

      DIMENSION P(*)

      INTEGER IVERSN,OPTION
      REAL PREAL(37)
      CHARACTER*4  PCHAR(37)
      EQUIVALENCE (PREAL(1),PCHAR(1))

C     COMMON BLOCKS

      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prp59.f,v $
     . $',                                                             '
     .$Id: prp59.f,v 1.1 1998/04/07 11:57:29 page Exp $
     . $' /
C    ===================================================================
C

C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012

C        TIDE BALANCE REVIEW -VERSION XXXX

C        INPUT TIME SERIES                    ID     CODE
C           DW OBSERVED STAGE              XXXXXXXX  XXXX
C           NOS STAGE                      XXXXXXXX  XXXX

C        PRIMARY OUTPUT TIME SERIES
C           TIDE1 BALANCE                  XXXXXXXX  XXXX
C           TIDE2 BALANCE                  XXXXXXXX  XXXX
C           TIDE3 BALANCE                  XXXXXXXX  XXXX
C           TIDE4 BALANCE                  XXXXXXXX  XXXX


C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012

C     CHECK TRACE LEVEL
      CALL FPRBUG ('PRP59   ',1,59,IBUG)

C     PRINT HEADING THEN TITLE INFORMATION FROM P ARRAY

      IVERSN = INT(P(1))
      DO 21 II=2,19
  21  PREAL(II) = P(II)
      WRITE(IPR,501) IVERSN,(PCHAR(II),II=2,19)
 501  FORMAT(//,10X,'TIDE BALANCE REVIEW -VERSION ',I4,/,
     +          10X,18A4,/)


C     PRINT TIME SERIES INFORMATION FROM P ARRAY

      II=0
      DO 22 JJ=20,37
      II=II+1
  22  PREAL(II) = P(JJ)
      WRITE(IPR,504) (PCHAR(II),II=1,18)
 504  FORMAT(/,10X,'INPUT TIME SERIES',18X,'ID',6X,'CODE',/,
     +     13X,'DWOPER OBSERVED STAGE',9X,2A4,2X,A4,/,
     +     13X,'NOS    FORECAST STAGE',9X,2A4,2X,A4,//,
     +     10X,'PRIMARY OUTPUT TIME SERIES',/,
     +     13X,'TIDE1 BALANCE',17X,2A4,2X,A4,/,
     +     13X,'TIDE2 BALANCE',17X,2A4,2X,A4,/,
     +     13X,'TIDE3 BALANCE',17X,2A4,2X,A4,/,
     +     13X,'TIDE4 BALANCE',17X,2A4,2X,A4,/)


      IF (ITRACE.GE.1) WRITE(IODBUG,199)
 199  FORMAT('PRP59:  EXITED:')

      RETURN
      END
