C MODULE SBYNEG
C-----------------------------------------------------------------------
C
      SUBROUTINE SBYNEG (X,Y,XDEL,YDEL,IP,NBPTS,IY,IXB,IXE,MSEGS,IYP,
     *   LFACTR,ISTAT)
C
C  THIS ROUTINE COMPUTES THE GRID POINTS CORRESPONDING TO THE RIGHT
C  END OF THE ROWS OF POINTS DEFINING THE BASIN BOUNDARY.
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
C
      DIMENSION X(NBPTS),Y(NBPTS)
      DIMENSION IY(MSEGS),IXB(MSEGS),IXE(MSEGS)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sbyneg.f,v $
     . $',                                                             '
     .$Id: sbyneg.f,v 1.3 2001/06/13 08:38:01 mgm Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SBYNEG'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      LDEBUG=ISBUG('GRID')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'IYP=',IYP
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
      JP=IYP
      IYTMP=Y(IP)
      IJ=IP+1
      IF (IP.EQ.NBPTS) IJ=1
C
10    IF ((IYTMP/LFACTR)*LFACTR.EQ.IYTMP) GO TO 20
      IYTMP=IYTMP-1
      GO TO 10
C
20    IF (IYTMP.LE.Y(IJ)) GO TO 60
C
C  COMPUTE X INTERCEPT
      IXTMP=X(IP)+(IYTMP-Y(IP))*(XDEL/YDEL)
30    IF ((IXTMP/LFACTR)*LFACTR.EQ.IXTMP) GO TO 40
      IXTMP=IXTMP-1
      GO TO 30
C
40    IF (JP.GT.MSEGS) THEN
         WRITE (LP,110) MSEGS,JP
         CALL SUERRS (LP,2,-1)
         ISTAT=1
         GO TO 80
         ENDIF
C
      IY(JP)=IYTMP
      IXE(JP)=IXTMP
      JP=JP+1
      IYTMP=IYTMP-LFACTR
      GO TO 20
C
60    IYP=JP
C
      IF (LDEBUG.GT.0) THEN
         N=IYP-1
         DO 70 I=1,N
            WRITE (IOSDBG,'(1X,A,I4,4(1X,A,I4))')
     *         'I=',I,'IY(I)=',IY(I),'IXB(I)=',IXB(I),'IXE(I)=',IXE(I)
            CALL SULINE (IOSDBG,1)
70          CONTINUE
         ENDIF
C
80    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SBYNEG'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
110   FORMAT ('0*** ERROR - IN SBYNEG - THE DIMENSION OF THE ',
     *   'LINE SEGMENT ARRAYS (',I5,') ','IS TOO SMALL. ',
     *   I5,' WORDS ARE NEEDED.')
C
      END
