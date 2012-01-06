C MODULE SBAREA
C-----------------------------------------------------------------------
C
      SUBROUTINE SBAREA (X,Y,NBPTS,GRIDL,NGRID,AREA,ISTAT)
C
C  THIS ROUTINE COMPUTES THE AREA OF A BASIN USING THE USER SPECIFIED
C  POINTS AND COMPUTES THE NUMBER OF GRID POINTS IN THE BASIN.
C
      REAL*8 XY1,XY2,GRIDA
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
C
      DIMENSION X(NBPTS),Y(NBPTS)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sbarea.f,v $
     . $',                                                             '
     .$Id: sbarea.f,v 1.3 2001/06/13 12:38:51 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SBAREA'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      LDEBUG=ISBUG('GRID')
C
      IF (LDEBUG.GT.0) THEN
         DO 20 I=1,NBPTS
           WRITE (IOSDBG,'(1X,A,I4,2(1X,A,F7.2))')
     *        'I=',I,'X(I)=',X(I),'Y(I)=',Y(I)
            CALL SULINE (IOSDBG,1)
20          CONTINUE
         ENDIF
C
      ISTAT=0
C
      XY1=0.0
      XY2=0.0
C
      DO 40 I=1,NBPTS
         J=I+1
         IF (I.EQ.NBPTS) J=1
         XY1=XY1+(X(J)*Y(I))
40       CONTINUE
C
      DO 50 I=1,NBPTS
         J=I+1
         IF (I.EQ.NBPTS) J=1
         XY2=XY2+(X(I)*Y(J))
50       CONTINUE
C
      GRIDA=0.5*(XY1-XY2)
      IF (GRIDA.LE.0.0) THEN
         WRITE (LP,90)
         CALL SUERRS (LP,2,-1)
         ISTAT=1
         GO TO 70
         ENDIF
C
      NGRID=GRIDA
C
C  CONVERT FROM NUMBER OF GRID POINTS TO AREA (KM2)
      AREA=GRIDA*GRIDL**2
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NGRID=',NGRID,' AREA=',AREA,
     *      ' GRIDA=',GRIDA,' GRIDL=',GRIDL
         CALL SULINE (IOSDBG,1)
         ENDIF
C
70    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SBAREA'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
90    FORMAT ('0*** ERROR - IN SBAREA - AREA CALCULATION INDICATES ',
     *   'PROBLEM WITH USER SPECIFIED POINTS. CHECK THAT LAT/LON ',
     *   'PAIRS ARE IN ORDER.')
C
      END
