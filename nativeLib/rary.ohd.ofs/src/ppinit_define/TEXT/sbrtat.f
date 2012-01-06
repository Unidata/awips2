C MODULE SBRTAT
C-----------------------------------------------------------------------
C
      SUBROUTINE SBRTAT (X1,X2,X3,X4,Y1,Y2,Y3,Y4,DIFF,ISTAT)
C
C  THIS ROUTINE ROTATES THE AXES OF THE NWSRFS/HRAP GRID IF A BASIN
C  BOUNDARY SEGMENT IS TOO CLOSE VERTICAL.
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
C
      DIMENSION X(4),Y(4)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sbrtat.f,v $
     . $',                                                             '
     .$Id: sbrtat.f,v 1.4 2001/06/13 08:37:22 mgm Exp $
     . $' /
C    ===================================================================
C
      DATA DEGRAD/.01745329/
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SBRTAT'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      LDEBUG=ISBUG('GRID')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,60) X1,Y1,X2,Y2,X3,Y3,X4,Y4
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      MAXADJ=99
      NUMADJ=0
C
      PHI=5.*DEGRAD
C
10    X(1)=X1
      X(2)=X2
      X(3)=X3
      X(4)=X4
      Y(1)=Y1
      Y(2)=Y2
      Y(3)=Y3
      Y(4)=Y4
C
      DO 20 I=1,4
         R=(X(I)**2+Y(I)**2)**.5
         IF (R.LT.0.00001) GO TO 20
            PHI2=ACOS(X(I)/R)
            X(I)=R*COS(PHI2-PHI)
            Y(I)=R*SIN(PHI2-PHI)
20       CONTINUE
C
      IF (ABS(X(1)-X(2)).GT.DIFF.AND.ABS(X(3)-X(4)).GT.DIFF) GO TO 30
C
      PHI=PHI+(5.*DEGRAD)
      NUMADJ=NUMADJ+1
      IF (NUMADJ.GT.MAXADJ) THEN
         WRITE (LP,70) MAXADJ
         CALL SUERRS (LP,2,-1)
         ISTAT=1
         GO TO 40
         ENDIF
      GO TO 10
C
30    X1=X(1)
      X2=X(2)
      X3=X(3)
      X4=X(4)
      Y1=Y(1)
      Y2=Y(2)
      Y3=Y(3)
      Y4=Y(4)
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,60) X1,Y1,X2,Y2,X3,Y3,X4,Y4
         CALL SULINE (IOSDBG,1)
         ENDIF
C
40    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SBRTAT'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
60    FORMAT (' ',
     *   'X1=',F10.4,3X,'Y1=',F10.4,3X,
     *   'X2=',F10.4,3X,'Y2=',F10.4,3X,
     *   'X3=',F10.4,3X,'Y3=',F10.4,3X,
     *   'X4=',F10.4,3X,'Y4=',F10.4)
70    FORMAT ('0*** ERROR - IN SBRTAT - MAXIMUM NUMBER OF ADJUSTMENTS ',
     *   'TO LINE SEGMENT (',I4,') EXCEEDED.')
C
      END
