C MODULE MPMAIN
C-----------------------------------------------------------------------
C
C  THIS MAIN PROGRAM IS USED TO DIMENSION THE ARRAYS USED IN THE MAP
c  PROGRAM.
C
C  EQUATION TO COMPUTE SIZE OF D ARRAY - D(NUM)
C
C       IBASE = M1*28 + M2*8 + M3*6 + M1*M2*2 + M1*M3 + M2*M6*12
C
C       IADD1 = M1*4 + M1*M2*5 + 6400
C       IADD2 = M1*817
C       IADD3 = M2*749
C
C       IF IADD1 GE IADD2 AND IADD1 GT IADD3,  NUM = IBASE + IADD1
C       IF IADD2 GE IADD1 AND IADD2 GT IADD3,  NUM = IBASE + IADD2
C       IF IADD3 GE IADD1 AND IADD3 GT IADD2,  NUM = IBASE + IADD3
C
C
C        M1 = MAXIMUM NUMBER OF STATIONS
C        M2 = MAXIMUM NUMBER OF BASINS
C        M3 = MAXIMUM NUMBER OF MONTHS
C        M4 = UNUSED (previously was MAXIMUM NUMBER OF SCRATCH DISK RECORDS)
C        M5 = LENGTH OF D ARRAY
C
C
      SUBROUTINE MAP_MAIN
C
C      DIMENSION    D(343600)
      DIMENSION    D(497200)
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
      COMMON /DIM/ M1,M2,M3,M4,M5,M6
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/map/RCS/mpmain.f,v $
     . $',                                                             '
     .$Id: mpmain.f,v 1.6 2004/08/10 14:54:58 dsa Exp $
     . $' /
C    ===================================================================
C
C
C     Subroutine ARGVER outputs the version/date info and exits the
C      program if the first command line argument is "-version"
C
      CALL ARGVER()
C
      CALL UPRIMO_MAP()
C
C  SET OPTIONS FOR UTILITY ROUTINE AND PRINT HEADER
      ISAV=ICDPUN
      ICDPUN=7
      CALL USETO1 ('NOOVERPRINT',IERR)
      CALL USETO1 ('NOPAGNUM',IERR)
      CALL UPAGE (LP)
      CALL USETO1 ('NOPAGHDR',IERR)
      ICDPUN=ISAV
C
C  SET MAXIMUM SIZES
      M1=200
      M2=50
C
C     Change MXYEAR from 50 to 100 for handling the same years of data
C     as PXPP program -- bug r23-45  guoxian zhou 08/11/03
C      M3=600
      M3=100*12
C
C     gzhou -- r23-45
C      M5=343600
      M5=497200
C
      ISTRCE=0
C
      CALL MPDIM (D)
C
      IUSTOP=0
      CALL USTOP (LP,IUSTOP)
C
      END
