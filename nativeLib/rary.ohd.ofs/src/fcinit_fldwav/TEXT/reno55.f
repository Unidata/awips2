      SUBROUTINE RENO55(NN,NJUN,NBT,KRCH,NUMLAD,LAD,NRCM1,NCM,LQ1,LQN,
     1 LQT,NQL,NGAGE,NGS,LROUT,KRTYP,KRT1,KRTN,MRV,MRU,NJUM,MPLOC,MPRV,
     2 K1,K2,K4,K5,K7,K10,K16,K23,K30)
C
C      THIS SUBROUTINE RENUMBERS LOCATION OF INTERNAL BOUNDARIES,
C      TRIBUTARY JUNCTIONS, LATERAL FLOW, LEVEE ETC. AFTER INTERPOLATION
C
C MR 1954 - 09/2004 FLDWAV Multi-Scenario Enhancement 
C  no changes were actually made in this module for this enhancement
C
      COMMON/M155/NU,JN,JJ,KIT,G,DT,TT,TIMF,F1
      COMMON/M3255/IOBS,KTERM,KPL,JNK,TEH
      COMMON/NETWK55/NET
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU

      INCLUDE 'common/fldmap55'

      DIMENSION NN(K23,K1),NJUN(K1),NBT(K1),MRV(K1)
      DIMENSION KRCH(K2,K1),NUMLAD(K1),LAD(K16,K1),MPRV(K30)
      DIMENSION NRCM1(K1),NCM(K7,K1),NJUM(K1),MRU(K1)
      DIMENSION LQT(K10,K1),LQ1(K10,K1),LQN(K10,K1)
      DIMENSION NQL(K1),NGAGE(K1),NGS(K4,K1),MPLOC(2,K30)
      DIMENSION LROUT(K1),KRTYP(K5,K1),KRT1(K5,K1),KRTN(K5,K1)
      CHARACTER*8  SNAME

C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_fldwav/RCS/reno55.f,v $
     . $',                                                             '
     .$Id: reno55.f,v 1.6 2004/10/27 16:15:15 jgofus Exp $
     . $' /
C    ===================================================================
C
      DATA SNAME/ 'RENO55  ' /

      CALL FPRBUG(SNAME, 1, 55, IBUG)
C
      DO 80 J=1,JN
      MJ=MRV(J)
      ICKVAL=999999
      IF (MJ.GT.ICKVAL) THEN
         WRITE (IPR,364) 'MJ',MJ,ICKVAL
364   FORMAT ('0**ERROR** IN RENO55 - VALUE OF VARIABLE ',A,' (',I10,
     *   ') IS GREATER THAN ',I6,'.')
         CALL ERROR
         GO TO 999
         ENDIF
      IF(MJ.GT.0) LTRB=NQL(MJ)
      LR=LROUT(J)
      DO 45 L=1,LR
      M=KRT1(L,J)
      KRT1(L,J)=NN(M,J)
      M=KRTN(L,J)
      KRTN(L,J)=NN(M,J)
   45 CONTINUE
      NM=NBT(J)-1
      LL=0
      DO 204 I=1,NM
      KRA=IABS(KRCH(I,J))
      IF(KRA.LT.10.OR.KRA.GT.40) GO TO 204
      LL=LL+1
      LAD(LL,J)=NN(I,J)
  204 CONTINUE
      NUMLAD(J)=LL
      NRCM=NRCM1(J)
      IF (NRCM.LE.0) GO TO 1995
      DO 1994 L = 1, NRCM
      M=NCM(L,J)
      ICKVAL=999999
      IF (M.GT.ICKVAL) THEN
         WRITE (IPR,364) 'M',M,ICKVAL
         CALL ERROR
         GO TO 999
         ENDIF
      NCM(L,J)=NN(M,J)
 1994 CONTINUE
 1995 NGAG = NGAGE(J)
C jgg      IF (NGAG.LE.0) GO TO 1997
      IF (NGAG.LE.0) GO TO 1999
      DO 1996 L = 1, NGAG
      M=NGS(L,J)
      NGS(L,J)=NN(M,J)
 1996 CONTINUE

c ... output time series
C jgg 1997 NOUT=NSTR(J)
C jgg      IF(NOUT.GT.0) THEN
C jgg        DO 1999 L=1,NOUT
C jgg          M=NST(L,J)
C jgg          NST(L,J)=NN(M,J)
 1999   CONTINUE
C jgg      ENDIF

      IF(NQL(J).EQ.0) GO TO 60
      NQLJ=NQL(J)
      DO 51 L=1,NQLJ
      LQT(L,J)=LQ1(L,J)
   51 CONTINUE
      DO 55 L=1,NQLJ
      M=LQT(L,J)
      LQ1(L,J)=NN(M,J)
      LQN(L,J)=NN(M+1,J)
   55 CONTINUE
C  ADD STARTING & ENDING REACH OF TRIBUTARY CONFLUENCE
   60 IF(J.GE.2 .AND. J.LE.(JN-NET)) THEN
        L=NJUN(J)
        NJUN(J)=NN(L,MJ)
        LTRB=LTRB+J-1
        LQ1(LTRB,MJ)=NJUN(J)
        LQN(LTRB,MJ)=NN(L+1,MJ)
      ENDIF
C NETWORK-RIVER PROPERTIES
      IF (J.GT.(JN-NET)) THEN
      MMJ=MRU(J)
        L1=NJUN(J)
      NJUN(J)=NN(L1,MJ)
      L2=NJUM(J)
      NJUM(J)=NN(L2,MMJ)
        ENDIF
   80 CONTINUE

c mapping locations
      IF(NMAP.GT.0) THEN
cc        print*, "before switch mploc1=",(mploc(1,l),l=1,nmap)
cc        print*, "              mploc2=",(mploc(2,l),l=1,nmap)
        DO 85 L=1,NMAP
          J=MPRV(L)
          L1=MPLOC(1,L)
          MPLOC(1,L)=NN(L1,J)
          L2=MPLOC(2,L)
          MPLOC(2,L)=NN(L2,J)
   85   CONTINUE
cc        print*, "after switch mploc1=",(mploc(1,l),l=1,nmap)
cc        print*, "             mploc2=",(mploc(2,l),l=1,nmap)
      ENDIF

      IF (JNK.LT.4) GO TO 999
      DO 90 J=1,JN
      IF(J.LE.(JN-NET) .AND. IBUG.EQ.1) WRITE(IODBUG,501) J
      IF(J.GT.(JN-NET) .AND. IBUG.EQ.1) WRITE(IODBUG,500) J
      LL=NUMLAD(J)
      IF(LL.GT.0 .AND.IBUG.EQ.1) WRITE(IODBUG,502) (LAD(L,J),L=1,LL)
      IF(J.GT.1 .AND.IBUG.EQ.1) WRITE(IODBUG,503) NJUN(J)
        IF(J.GT.(JN-NET) .AND. IBUG.EQ.1) WRITE(IODBUG,507) NJUM(J)
      NGAG=NGAGE(J)
      IF(NGAG.GT.0 .AND. IBUG.EQ.1) WRITE(IODBUG,504)(NGS(L,J),L=1,NGAG)
      NQLJ=NQL(J)
      IF(NQLJ.GT.0) THEN
        IF(IBUG.EQ.1) WRITE(IODBUG,505) (LQ1(L,J),L=1,NQLJ)
        IF(IBUG.EQ.1) WRITE(IODBUG,506) (LQN(L,J),L=1,NQLJ)
      END IF
      LR=LROUT(J)
      DO 160 L=1,LR
  160 IF(IBUG.EQ.1) WRITE(IODBUG,4099) L,KRTYP(L,J),KRT1(L,J),KRTN(L,J)
 4099 FORMAT(/5X,2HL=,I5,5X,'KRTYP=',I2,5X,'KRT1= ',I3,5X,'KRTN= ',I3)
   90 CONTINUE

      IF(NMAP.GT.0) THEN
        WRITE(IPR,510) (MPLOC(1,L),L=1,NMAP)
        WRITE(IPR,512) (MPLOC(2,L),L=1,NMAP)
      END IF

  500 FORMAT(/2X,'RIVER NO.',I3,5X,'(NETWORK RIVER)')
  501 FORMAT(/2X,'RIVER NO.',I3)
  502 FORMAT(5X,'LAD=  ',20I4,100(/10X,20I4))
  503 FORMAT(5X,'NJUN= ',20I4,100(/10X,20I4))
  504 FORMAT(5X,'NGS=  ',20I4,100(/10X,20I4))
  505 FORMAT(5X,'LQ1=  ',20I4,100(/10X,20I4))
  506 FORMAT(5X,'LQN=  ',20I4,100(/10X,20I4))
  507 FORMAT(5X,'NJUM= ',20I4,100(/10X,20I4))
  510 FORMAT(5X,'MPLOC1= ',20I4,100(/10X,20I4))
  512 FORMAT(5X,'MPLOC2= ',20I4,100(/10X,20I4))

C     IF(IBUG.EQ.1) WRITE(IODBUG,11111)
11111 FORMAT(1X,'** EXIT RENO **')
  999 RETURN
      END
