C MODULE HINCNX
C-----------------------------------------------------------------------
C
      SUBROUTINE HINCNX
C
C  THIS ROUTINE INCREMENTS THE POINTER TO THE NEXT OPTION RECORD.
C
C  IF THE POINTER IS LRECOP A NEW RECORD IS READ AND THE POINTER IS
C  RESET TO 1.
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'hclcommon/hseg1'
      INCLUDE 'hclcommon/hunits'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hincnx.f,v $
     . $',                                                             '
     .$Id: hincnx.f,v 1.3 2001/06/13 13:43:55 dws Exp $
     . $' /
C    ===================================================================
C
C
C  CHECK IF NEED TO READ NEW RECORD
      IF (NXOPT.LT.LRECOP) GO TO 20
      
C  CHECK IF ALL OPTION RECORDS READ
      IF (NUMRED.EQ.MAXOPT) GO TO 30
C
C  READ NEXT RECORD
      NUMRED=NUMRED+1
      CALL UREADT (KHDFLT,NUMRED,IOPTRC,ISTAT)
      IF (ISTAT.NE.0) GO TO 40
      NXOPT=0
      IF (IHCLDB.GT.1) WRITE (LPD,10) NUMRED,IOPTRC
10    FORMAT (' IN HINCNX - NUMRED=',I4,' IOPTRC=',16(I5,1X))
C
C  INCREMENT POINTER TO NEXT OPTION RECORD
20    NXOPT=NXOPT+1
      GO TO 40
C
30    ISTAT=-1
C
40    RETURN
C
      END
