C MODULE SUFPRT
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT ALL OPEN FILES.
C
      SUBROUTINE SUFPRT (NUNIT,ISTAT)
C
      CHARACTER*1 XNBR(10)/'0','1','2','3','4','5','6','7','8','9'/
      CHARACTER*1 FILUSE(10)
C
      INCLUDE 'uiox'
      INCLUDE 'ufiles'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_util/RCS/sufprt.f,v $
     . $',                                                             '
     .$Id: sufprt.f,v 1.2 2001/06/13 14:06:42 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SUFPRT'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('UTIL')
C
      IUNIT=NUNIT
      IF (IUNIT.EQ.0) IUNIT=LP
C
C  CHECK IF ANY FILES CURRENTLY OPEN
      DO 10 I=1,MFILES
         IF (IFILES(I).GT.0) GO TO 20
10       CONTINUE
      WRITE (IUNIT,70)
      CALL SULINE (IUNIT,2)
      GO TO 50
C
C  CHECK LINES LEFT ON PAGE
20    IF (IUNIT.EQ.LP.AND.ISLEFT(15).GT.0) CALL SUPAGE
C
      WRITE (IUNIT,80)
      CALL SULINE (IUNIT,2)
C
      ISTAT=0
C
      WRITE (IUNIT,90)
      CALL SULINE (IUNIT,1)
      WRITE (IUNIT,100) XNBR
      CALL SULINE (IUNIT,2)
C
      NPER=10
      NROW=0
      DO 40 I=1,MFILES,NPER
         DO 30 N=1,NPER
            FILUSE(N)=' '
            IPOS=I+N-2
            IF (IPOS.LE.0) GO TO 30
               IF (IFILES(IPOS).EQ.1) FILUSE(N)='X'
30          CONTINUE
         NROW=NROW+1
         NFILE1=NROW*NPER-NPER
         NFILE2=NFILE1+NPER-1
         IF (NFILE2.GT.MFILES) NFILE2=MFILES
         WRITE (IUNIT,110) NFILE1,NFILE2,FILUSE
         CALL SULINE (IUNIT,1)
40       CONTINUE
C
50    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SUFPRT'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
70    FORMAT ('0*** NOTE IN SUFPRT - NO FILES CURRENTLY OPEN.')
80    FORMAT ('0- FILES CURRENTLY OPEN -')
90    FORMAT (' ')
100   FORMAT (5X,'FILE RANGE',5X,10(A1,3X) /
     *   5X,10('-'),5X,9('----'),'-')
110   FORMAT (5X,I3.3,'-',I3.3,3X,5X,10(A1,3X))
C
      END
