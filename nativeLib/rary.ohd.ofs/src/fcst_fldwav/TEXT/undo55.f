      SUBROUTINE UNDO55(STT,LTSTT,ST1,LTST1,STN,LTSTN,QL,LTQL,GZ,GZ1,
     . GZN,NGAGE,LQ1,LQN,NQL,KD,KU,NB,X,JN,KPL,NU,IOBS,QLI,KSTG,
     . XNOS,TIDE,STE,LTSTE,K1,K2,K4,K10)
C
C           THIS SUBROUTINE CONVERTS ALL INPUT TIME SERIES BACK TO
C           THEIR ORIGINAL FORMS.
C
C           THIS SUBROUTINE WAS WRITTEN BY:
C           JANICE LEWIS      HRL   JANUARY,1999     VERSION NO. 1
C
      INCLUDE 'common/fcary'
      INCLUDE 'common/fdbug'

C
      DIMENSION STT(*),LTSTT(*),STN(*),ST1(*),LTST1(*),QL(*)
      DIMENSION LTQL(*),QLI(*),XNOS(*),TIDE(*),STE(*),LTSTE(*)
      DIMENSION LQ1(K10,K1),LQN(K10,K1),NQL(K1),KD(K1),KU(K1),NB(K1)
      DIMENSION X(K2,K1),NGAGE(K1),GZ(K4,K1),GZ1(K1),GZN(K1),SNAME(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/undo55.f,v $
     . $',                                                             '
     .$Id: undo55.f,v 1.2 2000/03/14 11:23:17 page Exp $
     . $' /
C    ===================================================================
C
C
C
      DATA SNAME/4HUNDO,4H55  /
C
      CALL FPRBUG(SNAME,1,55,IBUG)
C
C.......................................................................
C         CONVERT THE OBSERVED WATER SURFACE ELEVATIONS TO STAGES
C
      IF(IOBS.EQ.0.OR.KPL.EQ.2.OR.KSTG.EQ.1) GO TO 200
      DO 1999 J=1,JN
      NGAG=NGAGE(J)
      IF(NGAG.LE.0) GO TO 1999
      IF(IBUG.GE.1) WRITE(IODBUG,1) J
    1 FORMAT(/1H0,10X,30HOBSERVED STAGES FOR RIVER NO. ,I2/)
      IJ=LCAT21(1,J,NGAGE)-1
      DO 1996 I =1,NGAG
      LOST=LTSTT(I+IJ)-1
      if(ibug.ge.1) WRITE(IODBUG,10) (STT(K+LOST),K=1,NU)
      DO 1997 K =1,NU
      IK=IFMSNG(STT(K+LOST))
      IF(IK.NE.1) STT(K+LOST)=STT(K+LOST)-GZ(I,J)
 1997 CONTINUE
      IF(IBUG.EQ.1) THEN
        WRITE(IODBUG,10) (STT(K+LOST),K=1,NU)
   10   FORMAT(1H ,10X,10F10.2)
        WRITE(IODBUG,2)
    2   FORMAT(//)
      ENDIF
 1996 CONTINUE
   12 FORMAT(1H ,10X,10F10.0)
 1999 CONTINUE
C
C.......................................................................
C         CONVERT THE ADJUSTED WATER SURFACE ELEVATIONS TO STAGES

      IF(IOBS.EQ.1) GO TO 200
      DO 180 J=1,JN
        NGAG=NGAGE(J)
        IF(NGAG.LE.0) GO TO 180
        IF(IBUG.EQ.1) WRITE(IODBUG,3) J
    3   FORMAT(/1H0,10X,30HADJUSTED STAGES FOR RIVER NO. ,I2/)
        IJ=LCAT21(1,J,NGAGE)-1
        DO 175 I =1, NGAG
          LTSE=LTSTE(I+IJ)-1
          DO 170 K =1, NU
            IK=IFMSNG(STE(K+LTSE))
            IF(IK.EQ.1) GO TO 175
            STE(K+LTSE)=STE(K+LTSE)-GZ(I,J)
  170     CONTINUE
          IF(IBUG.EQ.1) WRITE(IODBUG,10) (STE(K+LTSE),K=1,NU)
          IF(IBUG.EQ.1) WRITE(IODBUG,2)
  175   CONTINUE
  180 CONTINUE

C.......................................................................
C         CONVERT THE UPSTREAM WATER SURFACE ELEVATIONS TO STAGES
C
  200 IF(NU.LE.0) GO TO 210
      DO 208 J=1,JN
      IF(KU(J).NE.1) GO TO 208
      LOS1=LTST1(J)-1
      DO 207 K=1,NU
  207 ST1(K+LOS1)=ST1(K+LOS1)-GZ1(J)
      IF(IBUG.EQ.1) WRITE(IODBUG,110) J
  110 FORMAT(1H0,10X,45HSTAGES AT THE UPSTREAM SECTION FOR RIVER NO. ,
     1 I2/)
      IF(IBUG.EQ.1) THEN
        WRITE(IODBUG,10) (ST1(K+LOS1),K=1,NU)
        WRITE(IODBUG,2)
      ENDIF
  208 CONTINUE
C
cc  209 IF (KD(1)-2) 210,213,213
C
C.......................................................................
C         CONVERT THE DOWNSTREAM WATER SURFACE ELEVATIONS TO STAGES
C
  210 DO 220 J=1,JN
      IF(KD(J).GT.1.OR.J.GT.1) GO TO 220
      LOSN=LTSTN-1
      DO 212 K=1,NU
        IK=IFMSNG(STT(K+LOSN))
        IF(IK.NE.1) STN(K+LOSN)=STN(K+LOSN)-GZN(J)
        IF(KD(1).EQ.0) THEN
          XNOS(K)=XNOS(K)-GZN(J)
          TIDE(K)=TIDE(K)-GZN(J)
        ENDIF
  212 CONTINUE
      IF(IBUG.EQ.1) THEN
        WRITE(IODBUG,310)
  310   FORMAT(1H0,10X,32HSTAGES AT THE DOWNSTREAM SECTION/)
        WRITE(IODBUG,10) (STN(K+LOSN),K=1,NU)
        IF(KD(1).EQ.0) THEN
          WRITE(IODBUG,312)
  312     FORMAT(1H0,10X,'NOS TIDE AT THE DOWNSTREAM SECTION'/)
          WRITE(IODBUG,10) (XNOS(K),K=1,NU)
          WRITE(IODBUG,314)
  314     FORMAT(1H0,10X,'ADJUSTED TIDE AT THE DOWNSTREAM SECTION'/)
          WRITE(IODBUG,10) (TIDE(K),K=1,NU)
        ENDIF
      ENDIF
C
  220 CONTINUE
C
C.......................................................................
C         CONVERT THE LATERAL FLOWS BACK TO UNITS OF CFS
C
      DO 41 J=1,JN
      IF(NQL(J).LE.0) GO TO 41
      NQ=NQL(J)
      LIJ=LCAT21(1,J,NQL)-1
      IF(IBUG.EQ.1) WRITE(IODBUG,410) J
  410 FORMAT(1H0,10X,29HLATERAL INFLOW FOR RIVER NO. ,I3/)
      DO 500 I = 1,NQ
        L1=LQ1(I,J)
        LN=LQN(I,J)
        DX=ABS(X(LN,J)-X(L1,J))*5280.
        LOQ=LTQL(I+LIJ)-1
        IF(IFILLC.EQ.0) QLI(I+LIJ)=QLI(I+LIJ)*DX
cc        QLI(I+LIJ)=QLI(I+LIJ)*DX
        DO 400 K=1,NU
          QL(K+LOQ)=QL(K+LOQ)*DX
  400   CONTINUE
        IF(IBUG.EQ.1) THEN
          WRITE(IODBUG,12) (QL(K+LOQ),K=1,NU)
          WRITE(IODBUG,2)
        ENDIF
  500 CONTINUE
      IF(IBUG.EQ.1) THEN
        WRITE(IODBUG,510) J
  510   FORMAT(/10X,37HINITIAL LATERAL INFLOW FOR RIVER NO. ,I2/)
        WRITE(IODBUG,12) (QLI(I+LIJ),I=1,NQ)
        WRITE(IODBUG,2)
      ENDIF
   41 CONTINUE
C.......................................................................
C
      IF(ITRACE.EQ.1) WRITE(IODBUG,9000) SNAME
 9000 FORMAT(1H0,2H**,1X,2A4,8H EXITED.)
C
      RETURN
      END

