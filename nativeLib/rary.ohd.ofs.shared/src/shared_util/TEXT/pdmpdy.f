C MEMBER PDMPDY
C  (from old member PPPDMPDY)
C
      SUBROUTINE PDMPDY(TYPE,KZDA,MSNG,PNTR,LP,DATA,LD)
C..........................................
C     SUBROUTINE DUMPS THE POINTER AND DATA ARRAYS RETURNED
C      FROM RPDDLY FOR DEBUG PURPOSES.
C..........................................
C     WRITTEN BY--ERIC ANDERSON,HRL--FEBRUARY 1983
C..........................................
      INTEGER*2 MSNG,PNTR(1),DATA(1)
C
C     COMMON BLOCKS
      COMMON/PUDBUG/IOPDBG,IPTRCE,NDBUG,PDBUG(20),IPALL
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_util/RCS/pdmpdy.f,v $
     . $',                                                             '
     .$Id: pdmpdy.f,v 1.1 1995/09/17 19:24:22 dws Exp $
     . $' /
C    ===================================================================
C
C..........................................
C     CHECK TRACE LEVEL
      IF(IPTRCE.GE.3) WRITE(IOPDBG,900)
 900  FORMAT(1H0,17H** PDMPDY ENTERED)
C..........................................
C     PRINT VALUES
      WRITE(IOPDBG,901) TYPE,KZDA,LP,LD,MSNG
 901  FORMAT(1H0,50HDUMP OF POINTER AND DATA ARRAYS FROM RPDDLY--TYPE=,
     1A4,2X,4HDAY=,I5,/6X,19HLENGTH OF POINTERS=,I6,3X,
     215HLENGTH OF DATA=,I6,3X,13HMISSING DATA=,I5,/1X,
     37HELEMENT)
      IF(LP.EQ.0) GO TO 105
      I1=1
 100  I2=I1+19
      IF(I2.GT.LP)I2=LP
      WRITE(IOPDBG,902)I1,(PNTR(I),I=I1,I2)
 902  FORMAT(1H0,I5,2X,20I6)
      IF(I2.EQ.LP) GO TO 105
      I1=I2+1
      GO TO 100
  105 IF(LD.EQ.0) GO TO 99
      I1=1
 110  I2=I1+19
      IF(I2.GT.LD)I2=LD
      WRITE(IOPDBG,902)I1,(DATA(I),I=I1,I2)
      IF(I2.EQ.LD) GO TO 99
      I1=I2+1
      GO TO 110
C     PRINTOUT COMPLETE
C..........................................
  99  IF(IPTRCE.GE.3) WRITE(IOPDBG,903)
 903  FORMAT(1H0,14H** EXIT PDMPDY)
      RETURN
      END
