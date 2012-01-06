      SUBROUTINE sumdata(PRECIP24,PRECIP1,MX,MY,J,INFILE,ISTAT)
C
      INTEGER*2 PRECIP24(MX),PRECIP1(MX)
      LOGICAL EX
      CHARACTER*79 INFILE
C
C  =================================== RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/sumdata.f,v $
     . $',                                                             '
     .$Id: sumdata.f,v 1.1 1995/09/08 14:55:55 page Exp $
     . $' /
C  =====================================================================
C
C
      ISTAT = 0
      IF (J .EQ. 0) THEN
	 INQUIRE(FILE=INFILE,EXIST=EX)
	 IF (.NOT. EX) GO TO 99
	 OPEN(31,FILE=INFILE,STATUS='OLD',ERR=99,FORM='UNFORMATTED')
      ENDIF
      READ(31) PRECIP1
      IF (J .EQ. MY-1) CLOSE(31)
      DO 10, I=1,MX
	 IF (PRECIP1(I) .GE. 0.) THEN
	     PRECIP24(I) = PRECIP24(I)+PRECIP1(I)
	 ENDIF
 10   CONTINUE
      RETURN
 99   PRINT 90, INFILE
      ISTAT = -1
 90   FORMAT(3X,'*** FILE DOES NOT EXIST',/,3X,A79)
      RETURN
      END
C
C -------------------------------------------------------
C
      SUBROUTINE timedist(RATIO,IDAT,MX,MY,INFILE,FILE2)
C
      DIMENSION RATIO(MX,MY),IDAT(MX,MY)
      INTEGER*2 IDAT
      CHARACTER*79 INFILE, FILE2
      LOGICAL EX

	 INQUIRE(FILE=INFILE,EXIST=EX)
	 IF (EX) THEN
	 OPEN(40,FILE=INFILE,STATUS='OLD',ERR=100,
     &       FORM='UNFORMATTED')
	 READ(40) IDAT
	 DO 50, I=1,MY
	 DO 50, J=1,MX
	   IDAT(J,I) = IDAT(J,I) * RATIO(J,I)
  50     CONTINUE
	 CLOSE(40)
	 OPEN(41,FILE=INFILE,FORM='UNFORMATTED')
	 WRITE(41) IDAT
	 ENDIF
	 OPEN(42, FILE=FILE2, STATUS='UNKNOWN')
	 DO 75, I=1,MY
	    WRITE(42,4)(IDAT(J,I), J=1,MX)
  75     CONTINUE
 100  CONTINUE
      RETURN
  4   FORMAT(' ',335I6)
      END
