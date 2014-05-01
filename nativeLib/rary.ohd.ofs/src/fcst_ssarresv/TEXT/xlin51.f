C MEMBER XLIN51
C DESC LINEAR INTERPOLATION BETWEEN TWO NON-MISSING VALUES.
C
C@PROCESS LVL(77)
C
      SUBROUTINE XLIN51(IXTYPE,WK,LWKSP,QDUM,LQDUM,IST,IEND)
C
C--------------------------------------------------------------------
C  ROUTINE TO GENERATE INSTANTANEOUS DISCHARGE, MEAN DISCHARGE, AND 
C  STORAGE FOR MISSING OBSERVED DATA BY LINEAR INTERPOLATION
C  BETWEEN TWO NON-MISSING VALUES.
C
C--------------------------------------------------------------------
C  ORIGINALLY PROGRAMMED BY - KUANG HSU - HRL - OCTOBER 1994
C--------------------------------------------------------------------
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fctime'
C
      DIMENSION WK(*),QDUM(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ssarresv/RCS/xlin51.f,v $
     . $',                                                             '
     .$Id: xlin51.f,v 1.1 1996/03/21 13:46:24 page Exp $
     . $' /
C    ===================================================================
C
C
C
C  PRINT TRACE HERE IF REQUESTED
C
      IF (ITRACE .GE. 3) WRITE(IODBUG,600)
  600 FORMAT(' *** ENTER XLIN51 ***')
      ISUTYP = 2
      IF(IST.GT.1) GO TO 150    
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  FIRST THING WE DO IS TO LOOK FOR FIRST OBSERVATION OF RUN.
C
      DO 100 I=IST,IEND
C
C  SEE IF TIME-SERIES VALUE IS MISSING
C
      VAL = QDUM(LQDUM+I-1)
      JULI = I*MINODT + (IDA-1)*24
      IF (IFMSNG(VAL) .EQ. 1 .OR. IFGNOR(IXTYPE,JULI,ISUTYP) .EQ. 1)
     .  GO TO 100
C
C  WE'VE FOUND A DATA VALUE.
C
      JEND = I
      DO 50 J=1,JEND
      WK(LWKSP+J-1) = VAL
   50 CONTINUE
      VAL1 = VAL
      GO TO 155
  100 CONTINUE
C
C  IF WE'VE MADE IT TO HERE, THEN WE HAVE NO OBSERVATIONS AND WE'RE
C  NOT ABLE TO DO ANY ADJUSTING.
C
      GO TO 900
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  HERE WE GO FROM THE FIRST OBSERVATION OUT TO THE LAST PERIOD OF
C  THE OBSERVED PERIOD OF THE RUN LOOKING FOR MORE OBSERVATIONS
C
  150 CONTINUE
C
      JEND = IST-1
      VAL1 = WK(LWKSP+IST-2)
C
 155  DO 500 I=IST,IEND
C
C  SEE IF TIME-SERIES VALUE IS MISSING
C
      VAL = QDUM(LQDUM+I-1)
      JULI = I*MINODT + (IDA-1)*24
      IF (IFMSNG(VAL) .EQ. 1 .OR. IFGNOR(IXTYPE,JULI,ISUTYP) .EQ. 1)
     .  GO TO 500
C
C  WE'VE FOUND A DATA VALUE.
C
      JST = JEND+1
      JEND = I
      VAL2 = VAL
      VADD = (VAL2-VAL1)/(JEND-JST+1)
      VAL = VAL1
      DO 450 J=JST,JEND
      VAL = VAL+VADD
      WK(LWKSP+J-1) = VAL
  450 CONTINUE
      VAL1 = VAL2
  500 CONTINUE
C
 900  CONTINUE
      IF (ITRACE.GE.3) WRITE(IODBUG,699)
  699 FORMAT('   ***EXIT XLIN51 ***')
C
      RETURN
      END
