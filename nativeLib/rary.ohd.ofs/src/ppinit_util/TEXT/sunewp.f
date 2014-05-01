C MEMBER SUNEWP
C-----------------------------------------------------------------------
C
C  DESC ROUTINE TO CHECK FOR NEWPAGE OPTION
C
      SUBROUTINE SUNEWP (NFLD,ISTRT,CHK,LCHK,LLPAR,LRPAR,LENGTH,IRPFND,
     *   OPTN,NUMERR,NUMWRN,NEWPAG,ISTAT)
C
      REAL XYES/4HYES /,XNO/4HNO  /
      REAL*8 OPTN
C
      DIMENSION CHK(5)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_util/RCS/sunewp.f,v $
     . $',                                                             '
     .$Id: sunewp.f,v 1.1 1995/09/17 19:15:46 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) WRITE (IOSDBG,60)
      IF (ISTRCE.GT.0) CALL SULINE (IOSDBG,1)
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG(4HUTIL)
C
      IF (LDEBUG.GT.0) WRITE (IOSDBG,70) NFLD
      IF (LDEBUG.GT.0) CALL SULINE (IOSDBG,1)
C
      ISTAT=0
C
C  CHECK FOR LEFT PARENTHESES
      IF (LLPAR.GT.0) GO TO 10
         CHK(1)=XYES
         WRITE (LP,90) OPTN,CHK(1)
         CALL SUWRNS (LP,2,NUMWRN)
         GO TO 30
C
C  CHECK FOR RIGHT PARENTHESES
10    IF (LRPAR.GT.0) IRPFND=1
         IF (LRPAR.GT.0) GO TO 20
            WRITE (LP,80) NFLD
            CALL SULINE (LP,2)
            LRPAR=LENGTH+1
C
C  SET OPTION VALUE
20    CALL UFPACK (LCHK,CHK,ISTRT,LLPAR+1,LRPAR-1,IERR)
C
C  CHECK OPTION VALUE
      IF (CHK(1).EQ.XYES.OR.CHK(1).EQ.XNO) GO TO 30
         WRITE (LP,100) OPTN,CHK(1)
         CALL SUERRS (LP,2,NUMERR)
         GO TO 50
C
C  CHECK IF NEWPAGE INDICATOR TO BE RETURNED
30    IF (NEWPAG.EQ.-1) GO TO 40
C
C  START NEW PAGE
      IF (CHK(1).EQ.XYES) CALL SUPAGE
      GO TO 50
C
C  SET NEWPAGE INDICATOR
40    IF (CHK(1).EQ.XYES) NEWPAG=1
      IF (CHK(1).EQ.XNO) NEWPAG=0
C
50    IF (ISTRCE.GT.0) WRITE (IOSDBG,110)
      IF (ISTRCE.GT.0) CALL SULINE (IOSDBG,1)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
60    FORMAT (' *** ENTER SMSTAT')
70    FORMAT (' NFLD=',I2)
80    FORMAT ('0*** NOTE - RIGHT PARENTHESIS ASSUMED IN FIELD ',I2,
     *   '.')
90    FORMAT ('0*** WARNING - NO LEFT PARENTHESES FOUND. ',A8,
     *   ' OPTION SET TO ',A4,'.')
100   FORMAT ('0*** ERROR - INVALID ',A8,' OPTION : ',5A4)
110   FORMAT (' *** EXIT SMSTAT')
C
      END
