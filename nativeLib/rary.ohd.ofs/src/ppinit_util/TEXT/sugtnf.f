C MEMBER SUGTNF
C-----------------------------------------------------------------------
C
      SUBROUTINE SUGTNF (LARRAY,ARRAY,DISP,NUMERR,ISTAT)
C
C  DESC  ROUTINE TO READ NETWORK PARAMETERS AND LOAD COMMON BLOCK.
C
      REAL XNEW/4HNEW /
C
      REAL XOLD/4HOLD /
      DIMENSION ARRAY(1),UNUSED(10)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sntwfx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_util/RCS/sugtnf.f,v $
     . $',                                                             '
     .$Id: sugtnf.f,v 1.1 1995/09/17 19:15:35 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) WRITE (IOSDBG,50)
      IF (ISTRCE.GT.0) CALL SULINE (IOSDBG,1)
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG(4HUTIL)
C
      ISTAT=0
C
C  CHECK IF NETWORK FLAG COMMON BLOCK FILLED
      IF (INFFIL.GT.0) GO TO 45
C
C  READ NTWK PARAMETERS
      IPRERR=0
      CALL SRNTWK (LARRAY,ARRAY,IVNTWK,INWDTE,NNWFLG,INWFLG,UNUSED,
     *   IPRERR,IERR)
      IF (IERR.GT.0) GO TO 10
         IF (DISP.EQ.XOLD) GO TO 30
            WRITE (LP,60)
            CALL SULINE (LP,2)
C
C  INITIALIZE NTWK PARAMETER ARRAY
10    WRITE (LP,70)
      CALL SULINE (LP,2)
      DO 20 I=1,NNWFLG
         INWFLG(I)=0
20       CONTINUE
      UNSD=-999.
      WDISP=XNEW
      CALL SWNTWK (IVNTWK,UNSD,NNWFLG,INWFLG,INWDTE,
     *   LARRAY,ARRAY,WDISP,IERR)
      IRNTWK=IERR
      IF (IRNTWK.EQ.0) GO TO 30
         WRITE (LP,80)
         CALL SUERRS (LP,2,NUMERR)
         GO TO 40
C
30    IF (LDEBUG.GT.0)
     *   CALL SPNTWK (IVNTWK,INWDTE,NNWFLG,INWFLG,UNUSED,IERR)
C
40    IF (IRNTWK.GT.0) ISTAT=IERR
      IF (IERR.EQ.0) INFFIL=1
C
45    IF (ISTRCE.GT.0) WRITE (IOSDBG,90) ISTAT
      IF (ISTRCE.GT.0) CALL SULINE (IOSDBG,1)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
50    FORMAT (' *** ENTER SUGTNF')
60    FORMAT ('0*** NOTE - NTWK PARAMETERS ALREADY EXIST.')
70    FORMAT ('0*** NOTE - NTWK PARAMETERS NOT FOUND. PARAMETERS ',
     *   'WILL BE WRITTEN.')
80    FORMAT ('0*** ERROR - NTWK PARAMETERS NOT SUCCESSFULLY ',
     *   'WRITTEN.')
90    FORMAT (' *** EXIT SUGTNF : STATUS CODE=',I2)
C
      END
