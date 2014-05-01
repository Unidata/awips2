C MEMBER PDUMPA
C  (from old member PPPDUMPA)
C
      SUBROUTINE PDUMPA(LARRAY,ARRAY,ATYPE,AID,IA)
C..........................................
C     SUBROUTINE PRINTS A GIVEN ARRAY IN REAL AND ALPHA FORM.
C..........................................
C     WRITTEN BY -- ERIC ANDERSON, HRL -- JANUARY 1983
C..........................................
      DIMENSION ARRAY(1),AID(2)
C
C     COMMON BLOCK
      COMMON/PUDBUG/IOPDBG,IPTRCE,NDBUG,PDBUG(20),IPALL
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_util/RCS/pdumpa.f,v $
     . $',                                                             '
     .$Id: pdumpa.f,v 1.1 1995/09/17 19:24:25 dws Exp $
     . $' /
C    ===================================================================
C
C..........................................
C     TRACE LEVEL=3
      IF(IPTRCE.GE.3) WRITE(IOPDBG,900)
  900 FORMAT(1H0,17H** PDUMPA ENTERED)
C..........................................
C     CHECK THAT LENGTH OF ARRAY IS GREATER THAN ZERO.
      IF(LARRAY.GT.0) GO TO 100
      WRITE(IOPDBG,901) ATYPE,AID
  901 FORMAT(1H0,14HPARAMETER TYPE,1X,A4,4H(ID=,2A4,
     1 29H) DOES NOT CONTAIN ANY VALUES)
      GO TO 99
C..........................................
C     PRINT CONTENTS OF ARRAY
  100 WRITE(IOPDBG,902)ATYPE,AID,LARRAY
  902 FORMAT(1H0,42HCONTENTS OF PARAMETER ARRAY IN REAL--TYPE=,A4,
     1 2X,3HID=,2A4,2X,7HLENGTH=,I5,/1X,7HELEMENT)
      I1=1
  105 I2=I1+11
      IF(I2.GT.LARRAY) I2=LARRAY
      WRITE(IOPDBG,903)I1,(ARRAY(I),I=I1,I2)
  903 FORMAT(1H0,I5,2X,12F10.4)
      IF(I2.EQ.LARRAY) GO TO 110
      I1=I2+1
      GO TO 105
  110 IF(IA.EQ.0) GO TO 99
      WRITE(IOPDBG,904)
  904 FORMAT(1H0,32HCONTENTS OF ABOVE ARRAY IN ALPHA,/1X,7HELEMENT)
      I1=1
  115 I2=I1+11
      IF(I2.GT.LARRAY) I2=LARRAY
      WRITE(IOPDBG,905)I1,(ARRAY(I),I=I1,I2)
  905 FORMAT(1H0,I5,2X,12(6X,A4))
      IF(I2.EQ.LARRAY) GO TO 99
      I1=I2+1
      GO TO 115
C     PRINT COMPLETE
C..........................................
   99 IF(IPTRCE.GE.3) WRITE(IOPDBG,906)
  906 FORMAT(1H0,14H** EXIT PDUMPA)
      RETURN
      END
