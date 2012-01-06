      SUBROUTINE readdata(ID,TYPE,DATE,IDAT,IFILE)
C
      DIMENSION IDAT(131,131)
      INTEGER*2 IDAT
      CHARACTER*40 NAME
      CHARACTER*3 ID
      CHARACTER*2 TYPE
      CHARACTER*8 DATE
      LOGICAL EX
C
C  =================================== RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ifp/src/IFP_Map/RCS/read_data.f,v $
     . $',                                                             '
     .$Id: read_data.f,v 1.1 1995/09/08 14:55:39 page Exp $
     . $' /
C  =====================================================================
C
C
      IFILE = 0
      NAME = '/home/precip/data/stageii/'//ID//TYPE//DATE//'z'
      INQUIRE(FILE=NAME,EXIST=EX)
      IF (.NOT. EX) GO TO 91
      OPEN(31,FILE=NAME,ERR=91,STATUS='OLD',FORM='UNFORMATTED')
      READ(31,ERR=91) IDAT
      CLOSE(31)
  10  CONTINUE
      RETURN
91    IFILE = 1
      PRINT 40, NAME
 40   FORMAT(' Unable to find/read ',A40)
      RETURN
      END
C
C
      SUBROUTINE writemosaic(MAXX,MAXY,IROW,RFC,DATE,MOSAIC)
C
      DIMENSION MOSAIC(MAXX)
      INTEGER*2 MOSAIC
      CHARACTER*3 RFC
      CHARACTER*8 DATE
      CHARACTER*39 FILENAME
      CHARACTER*36 MAPXNAME
C
      IF (IROW .EQ. 0) THEN
	FILENAME = '/home/precip/data/stageiii/'//RFC//DATE//'z'
	OPEN(33,FILE=FILENAME,FORM='UNFORMATTED',STATUS='UNKNOWN')
	MAPXNAME = '/home/precip/ftp/output/'//RFC//DATE//'z'
	OPEN(34,FILE=MAPXNAME,FORM='FORMATTED',STATUS='UNKNOWN')
      ENDIF
      WRITE(33) MOSAIC
      WRITE(34,4) (MOSAIC(J), J=1,MAXX)
      IF (IROW .EQ. MAXY-1) THEN
	CLOSE(34)
	CLOSE(33)
      ENDIF
  4   FORMAT(' ',335I6)
      RETURN
      END
C
C
      SUBROUTINE readoldmosaic(MX,MY,IROW,FNAME,IFILE,MOSAIC)
C
      DIMENSION MOSAIC(MX)
      INTEGER*2 MOSAIC
      CHARACTER*39 FNAME
      LOGICAL EX

      IFILE = 0
      IF (IROW .EQ. 0) THEN
	INQUIRE(FILE=FNAME,EXIST=EX)
	IF (.NOT. EX) GO TO 91
	OPEN(31,FILE=FNAME,STATUS='OLD',FORM='UNFORMATTED')
      ENDIF
      READ(31) MOSAIC
      IF (IROW .EQ. MY-1) CLOSE(31)
      RETURN
 91   IFILE = -1
      RETURN
      END
C
