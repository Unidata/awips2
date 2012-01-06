C MODULE FCONVT
C-----------------------------------------------------------------------
C   THIS SUBROUTINE PROVIDES THE STANDARD ENGLISH UNITS FOR THE
C   STANDARD METRIC UNITS AND THE NECESSARY CONVERSION FACTORS.
C     UNITSM - STANDARD METRIC UNITS
C     DIM    - DIMENSIONS OF THE UNITS
C     UNITSE - STANDARD ENGLISH UNITS
C     CFACT  - MULTIPLICATION CONVERSION FACTOR
C     CONST  - ADDITION CONSTANT
C     IER    - ERROR FLAG
C               0=NORMAL RETURN
C               1=UNITSM NOT FOUND IN LIST
C                 (UNITSE=UNITSM,CFACT=1.0,CONST=0.0)
C-----------------------------------------------------------------------
      SUBROUTINE FCONVT(UNITSM,DIM,UNITSE,CFACT,CONST,IER)

      CHARACTER*4    UNITSM,DIM,UNITSE
      CHARACTER*4    TMPSE,DIMLES
      CHARACTER*4    UNITS1(10),UNITS2(10)

      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_util/RCS/fconvt.f,v $
     . $',                                                             '
     .$Id: fconvt.f,v 1.3 2000/12/18 21:12:17 jgofus Exp $
     . $' /
C    ===================================================================
C
      DATA    DIMLES / 'DLES' /
      DATA    UNITS1 / 'CMSD','CMS ','MM  ','CM  ','M   ',
     $                 'KM  ','DEGC','M/S ','KM/H','MMHG'  /
      DATA    UNITS2 / 'CFSD','CFS ','IN  ','IN  ','FT  ',
     $                 'MI  ','DEGF','FT/S','MI/H','INHG'  /


      IF (ITRACE.GT.0) WRITE (IODBUG,'('' *** ENTER FCONVT'')')

      IER=0
      TMPSE=UNITSM
      CONST=0.0
      CFACT=1.0

C  IF THE UNITS ARE DIMENSIONLESS, NO NEED FOR CONVERSION
      IF (DIM .NE. DIMLES) THEN

        IF (UNITSM.EQ.UNITS1(1)) THEN
          TMPSE=UNITS2(1)
        ELSEIF (UNITSM.EQ.UNITS1(2)) THEN
          TMPSE=UNITS2(2)
        ELSEIF (UNITSM.EQ.UNITS1(3)) THEN
          TMPSE=UNITS2(3)
        ELSEIF (UNITSM.EQ.UNITS1(4)) THEN
          TMPSE=UNITS2(4)
        ELSEIF (UNITSM.EQ.UNITS1(5)) THEN
          TMPSE=UNITS2(5)
        ELSEIF (UNITSM.EQ.UNITS1(6)) THEN
          TMPSE=UNITS2(6)
        ELSEIF (UNITSM.EQ.UNITS1(7)) THEN
          TMPSE=UNITS2(7)
        ELSEIF (UNITSM.EQ.UNITS1(8)) THEN
          TMPSE=UNITS2(8)
        ELSEIF (UNITSM.EQ.UNITS1(9)) THEN
          TMPSE=UNITS2(9)
        ELSEIF (UNITSM.EQ.UNITS1(10)) THEN
          TMPSE=UNITS2(10)
        ELSE

C  UNITS NOT FOUND IN THE LIST OF STANDARD METRIC UNITS
          WRITE (IPR,60) UNITSM
   60     FORMAT ('0**WARNING** THE UNITS ',A4,
     $            ' WERE NOT FOUND IN THE',
     $            ' LIST OF STANDARD METRIC UNITS.')
          CALL WARN
          IER=1

        ENDIF

        IF (IER .EQ. 0) CALL UDUCNV (UNITSM,TMPSE,2,1,CFACT,CONST,IERR)

      ENDIF

      UNITSE = TMPSE

      IF (ITRACE.GT.0) WRITE (IODBUG,'('' *** EXIT FCONVT'')')

      RETURN
      END
