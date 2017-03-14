      SUBROUTINE ICIN55(JN,NB,NBT,YDI,QDI,NN,ICOND,K1,K2,K23)
C
C        THIS SUBROUTINE ADDS INITIAL CONDITIONS(YDI AND QDI) AT
C        X-SECTIONS WHICH HAVE BEEN ADDED BY LINEAR INTERPOLATION.
C        IT ALSO DEALS WITH INITIAL CONDITION
C
C        ICOND=0  INITIAL CONDITIONS WILL BE DETERMINED VIA BACKWATER
C                  COMPUTATIONS.
C        ICOND=1  INITIAL CONDITIONS WILL BE INTERPOLATED BETWEEN KNOW
C                  VALUES.  IF ZERO VALUES ARE READ IN BACKWATER
C                  COMPUTATIONS WILL BE DONE.
C
C HSD bug r25-54 10/2004 xfan
C     The initial conditions are not being properly computed.  
C     The model assumed that ydi values <=0 should be recomputed 
C     with backwater computations.  Since some of the tidal
C     values are negative, it assumes the backwater computations 
C     were necessary when they are not.
C
C        ICOND=2  INITIAL CONDITIONS ARE READ-IN BECAUSE OF TIDE

      COMMON/METR55/METRIC
      COMMON/M3255/IOBS,KTERM,KPL,JNK,TEH
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU

      DIMENSION NB(K1),NBT(K1),YDI(K2,K1),QDI(K2,K1),NN(K23,K1)
      CHARACTER*8 SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_fldwav/RCS/icin55.f,v $
     . $',                                                             '
     .$Id: icin55.f,v 1.6 2005/01/12 17:49:50 xfan Exp $
     . $' /
C    ===================================================================
C
      DATA SNAME/ 'ICIN55  '  /

      CALL FPRBUG(SNAME, 1, 55, IBUG)

      IF(ICOND.EQ.0) WRITE(IPR,10)
   10 FORMAT(//10X,'===== NOTE: INITIAL CONDITIONS AT INTERPOLATED CROSS
     . SECTIONS WILL BE DETERMINED VIA BACKWATER COMPUTATIONS.')
C HSD bug r25-54 10/2004
C     IF(ICOND.EQ.0) WRITE(IPR,20)
      IF(ICOND.EQ.1) WRITE(IPR,20)
C
   20 FORMAT(//10X,'===== NOTE: INITIAL CONDITIONS AT INTERPOLATED CROSS
     . SECTIONS WILL BE LINEARLY INTERPOLATED BETWEEN KNOWN VALUES.'/
     .22X,'IF ZERO VALUES ARE READ IN, BACKWATER COMPUTATIONS WILL BE DO
     .NE.')
C HSD bug r25-54 10/2004
      IF(ICOND.EQ.2) WRITE(IPR,25)
   25 FORMAT(//10X,'===== NOTE: ALL INITIAL CONDITIONS WERE READ-IN ...
     . NO INTERPOLATION IS NEEDED.')
C
      TOL=0.00001
      DO 300 J=1,JN
        N=NBT(J)
      ICKVAL=999999
      IF (N.GT.ICKVAL) THEN
         WRITE (IPR,998) 'N',N,ICKVAL
998   FORMAT ('0**ERROR** IN ICIN55 - VALUE OF VARIABLE ',A,' (',I10,
     *   ') IS GREATER THAN ',I6,'.')
         CALL ERROR
         GO TO 999
         ENDIF
        N2=NB(J)
      ICKVAL=-999999
      IF (N2.LT.ICKVAL) THEN
         WRITE (IPR,997) 'N2',N2,ICKVAL
997   FORMAT ('0**ERROR** IN ICIN55 - VALUE OF VARIABLE ',A,' (',I10,
     *   ') IS LESS THAN ',I7,'.')
         CALL ERROR
         GO TO 999
         ENDIF
      ICKVAL=999999
      IF (N2.GT.ICKVAL) THEN
         WRITE (IPR,998) 'N2',N2,ICKVAL
         CALL ERROR
         GO TO 999
         ENDIF

C HSD bug r25-54 10/2004
         IF(ICOND.EQ.2) GO TO 105
C

CC        IF(N.EQ.N2) GO TO 200
        I2=NN(N,J)
        Y2=YDI(N,J)
        Q2=QDI(N,J)

          write(iodbug,1)
 1        format(/5x,'qdi')
          write(iodbug,2) (qdi(i,j), i=1,n2)
 2        format(10f10.0)
          write(iodbug,3)
 3        format(/5x,'ydi')
          write(iodbug,4) (ydi(i,j), i=1,n2)
 4        format(10f10.3)

        I1=N+1
        IF(I1.GT.N2) I1=N2
        IF(YDI(N2,J).GT.0.AND.YDI(N,J).GT.0.AND.YDI(I1,J).GT.0.) 
     .       GO TO 105

        DO 100 I=1,N
          IE=N-I+1
          I1=NN(IE,J)
          Y1=YDI(IE,J)
          Q1=QDI(IE,J)

          IF(I1.EQ.IE) GO TO 30
C ......................................................................
C ==== PUT YDI & QDI IN THE PROPER NEW LOCATION & ZERO OUT THE OLD VALUE

          YDI(I1,J)=YDI(IE,J)
          QDI(I1,J)=QDI(IE,J)
          YDI(IE,J)=0.
          QDI(IE,J)=0.
   30    IF(I.EQ.1) GO TO 100
          IF(ICOND.EQ.0) GO TO 100
          IF(I1.EQ.I2) GO TO 100
C ......................................................................
C ==== INTERPOLATE TO GET THE REST OF YDI & QDI VALUES
C ==== NOTE: IF ZERO VALUES ARE READ IN, A BACKWATER WILL BE DONE LATER

          DY=(Y2-Y1)/(I2-I1)
          DQ=(Q2-Q1)/(I2-I1)
          DO 50 II=I1+1,I2
            IF(ABS(Y1).GT.TOL.AND.ABS(Y2).GT.TOL)YDI(II,J)=Y1+DY*(II-I1)
            IF(ABS(Q1).GT.TOL.AND.ABS(Q2).GT.TOL)QDI(II,J)=Q1+DQ*(II-I1)
   50     CONTINUE
C ......................................................................
   90     Y2=Y1
          Q2=Q1
          I2=I1
  100   CONTINUE
 105    IF (JNK.LE.4.OR.IBUG.EQ.0) GOTO 999
        WRITE(IODBUG,110) J
  110   FORMAT(/10X,'QDI(I,',I2,')')
        CALL WYQMET55(METRIC,1,N2,QDI(1,J),0,N2)
        WRITE(IODBUG,120) J
  120   FORMAT(/10X,'YDI(I,',I2,')')
        CALL WYQMET55(METRIC,1,N2,YDI(1,J),1,N2)
  300 CONTINUE

999   RETURN
      END








