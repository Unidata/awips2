C MEMBER PRP61
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/28/1998 TLS
C
C @PROCESS LVL(77)
C
      SUBROUTINE PRP61 (P)

C     THIS IS THE PRINT PARAMETER ROUTINE FOR STAGEREV.

C     THIS ROUTINE ORIGINALLY WRITTEN BY
C        JOANNE R. SALERNO - NWRFC   OCT 1997      

C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012

C     POSITION     CONTENTS OF P ARRAY

C      1           VERSION NUMBER OF OPERATION
C      2-19        GENERAL NAME OR TITLE

C  INPUT
C     20           TIME INTERVAL FOR OBS/FCST         - ITIME
C     21-23        OBSERVED STAGE                     - CSTAO (STG)
C     24-26        DW FCST  STAGE                     - CSTAP (STGE)
C  OUTPUT
C     27           TIME INTERVAL FOR RANGE & BALANCE  -OTIME
C     TIDAL RANGE/SLICE DEFINITION
C     28-29        RANGE LIMIT SERIES IDENTIFIER     -  RANGE
C     30           RANGE LIMIT TYPE CODE             -  OUTTC
C                  RANGE(1) R1 LOWER LIMIT
C                  RANGE(2) R2 LOWER LIMIT
C                  RANGE(3) R3 LOWER LIMIT
C                  RANGE(4) R4 LOWER LIMIT
C                  RANGE(5) R4 UPPER LIMIT
C
C     OBSERVED PRTO3 AVERAGE  BALANCE TIME SERIES PER RANGE
C     31-32        RANGE1 AVE TIDE BALANCE TIME SERIES ID  -  SBALR1
C     33-34        RANGE2 AVE TIDE BALANCE TIME SERIES ID  -  SBALR2
C     35-36        RANGE3 AVE TIDE BALANCE TIME SERIES ID  -  SBALR3
C     37-38        RANGE4 AVE TIDE BALANCE TIME SERIES ID  -  SBALR4
C
C     39-40        RESERVED

C**********************************************************************

C     THE NUMBER OF ELEMENTS REQUIRED IN THE P ARRAY IS  40

C     THE NUMBER OF ELEMENTS REQUIRED IN THE C ARRAY IS   0

C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012

      DIMENSION P(*)

      INTEGER IVERSN
      INTEGER ITIME,OTIME

C     COMMON BLOCKS

      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prp61.f,v $
     . $',                                                             '
     .$Id: prp61.f,v 1.2 1998/10/14 13:46:46 page Exp $
     . $' /
C    ===================================================================
C

C
C

C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012

C        STAGE REVIEW    - VERSION XXXX

C        INPUT TIME INTERVAL               XX HOURS
C        INPUT TIME SERIES                    ID     CODE
C           OBSERVED    STAGE              XXXXXXXX  XXXX
C           DW OBS/FCST STAGE              XXXXXXXX  XXXX

C        OUTPUT TIME INTERVAL              XX HOURS
C        PRIMARY OUTPUT TIME SERIES
C           RANGE LIMIT                    XXXXXXXX  XXXX
C           R1 AVE BALANCE                 XXXXXXXX  XXXX
C           R2 AVE BALANCE                 XXXXXXXX  XXXX
C           R3 AVE BALANCE                 XXXXXXXX  XXXX
C           R4 AVE BALANCE                 XXXXXXXX  XXXX
 

C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012

C     CHECK TRACE LEVEL
      CALL FPRBUG ('PRP61   ',1,61,IBUG)

C     PRINT HEADING THEN TITLE INFORMATION FROM P ARRAY

      IVERSN = INT(P(1))
      WRITE(IPR,501) IVERSN,(P(I),I=2,19)
 501  FORMAT(//,10X,'STAGE REVIEW- VERSION ',I4,/,
     +          10X,18A4,/)


C     PRINT TIME SERIES INFORMATION FROM P ARRAY

      ITIME = INT(P(20))
      OTIME = INT(P(27))
      
      WRITE(IPR,504) ITIME,(P(I),I=21,26),OTIME,(P(I),I=28,38)
 504  FORMAT(/,10X,'INPUT TIME SERIES',18X,'ID',7X,'CODE',/,
     +     13X,'OBS/FCST TIME INTERVAL',8X,I2,/,
     +     13X,'OBSERVED        STAGE',9X,2A4,2X,A4,/,
     +     13X,'DW OBS/FCST     STAGE',9X,2A4,2X,A4,//,
     +     10X,'PRIMARY OUTPUT TIME SERIES',/,
     +     13X,'OUTPUT TIME INTERVAL',10X,I2,/,
     +     13X,'RANGE LIMIT       ',12X,2A4,2X,A4,/,
     +     13X,'AVE BALANCE RANGE1',12X,2A4/,
     +     13X,'AVE BALANCE RANGE2',12X,2A4/,
     +     13X,'AVE BALANCE RANGE3',12X,2A4/,
     +     13X,'AVE BALANCE RANGE4',12X,2A4/)


      IF (ITRACE.GE.1) WRITE(IODBUG,199)
 199  FORMAT('PRP61:  EXITED:')

      RETURN
      END
