      SUBROUTINE SLOPE55(J,N,XFACT,X,HS,KRCH,SLOP,K1,K2,K9)
      COMMON/M3255/IOBS,KTERM,KPL,JNK,TEH
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU

      DIMENSION X(K2,K1),HS(K9,K2,K1),KRCH(K2,K1),SLOP(K2,K1)

      CHARACTER*8 SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_fldwav/RCS/slope55.f,v $
     . $',                                                             '
     .$Id: slope55.f,v 1.2 2004/02/02 20:41:45 jgofus Exp $
     . $' /
C    ===================================================================
C
      DATA SNAME/'SLOPE55 '/

      CALL FPRBUG(SNAME, 1, 55, IBUG)

C      IF(IBUG.EQ.1) WRITE(IODBUG,1)
    1 FORMAT(1X,'** ENTER SLOPE **')
      NM=N-1
   58 FORMAT(/10X,'(SLOP(I,J),I=1,N) FOR RIVER NO. ',I3)
   59 FORMAT(8F10.6)
      DO 10 I=1,NM
      DX=ABS(X(I+1,J)-X(I,J))*XFACT
      IF(DX.LT.1.) DX=1.
      DH=HS(1,I,J)-HS(1,I+1,J)
      SLOP(I,J)=(HS(1,I,J)-HS(1,I+1,J))/DX
      IF(SLOP(I,J).LE.0.000001) SLOP(I,J)=0.000001
   10 CONTINUE
      DO 20 I=1,NM
      KR=KRCH(I,J)
      IF(KR.NE.-2.OR.KR.NE.-3) GO TO 30
      KRCH(I,J)=-KRCH(I,J)
   20 CONTINUE
      SLS=SLOP(1,J)
      DO 25 I=2,NM
      SAU=(SLS+SLOP(I,J))/2.
      SAD=(SLOP(I,J)+SLOP(I+1,J))/2.
      DXU=ABS(X(I-1,J)-X(I,J))
      DXD=ABS(X(I,J)-X(I+1,J))
      SLS=SLOP(I,J)
      SLOP(I,J)=(SAU*DXU+SAD*DXD)/(DXU+DXD)
   25 CONTINUE
   30 CONTINUE
      IF(SLOP(N,J).LE.0.000001) SLOP(N,J)=SLOP(NM,J)
      IF (JNK.LT.4) GO TO 999
      IF(IBUG.EQ.1) WRITE(IODBUG,58) J
      IF(IBUG.EQ.1) WRITE(IODBUG,59) (SLOP(I,J),I=1,N)
999   RETURN
      END
