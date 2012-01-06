      SUBROUTINE RDRCH55(PO,NBT,KRCHT,NUMLAD,XT,DXM,NB,
     * NJFMT,NIFMT,NJTOT,NITOT,JN,NODESC,IERR,K1,K13,K18,K23)
      CHARACTER*80 DESC
      COMMON/FLP55/KFLP
      COMMON/LEV55/NLEV,DHLV,NPOND,DTHLV,IDTHLV
      COMMON/NPC55/NP,NPST,NPEND
      COMMON/MXVAL55/MXNB,MXNGAG,MXNCM1,MXNCML,MXNQL,MXINBD,MXRCH,
     .               MXMGAT,MXNXLV,MXROUT,MXNBT,MXNSTR,MXSLC
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU

      INCLUDE 'common/ofs55'

      DIMENSION NBT(K1),KRCHT(K13,K1),NUMLAD(K1),XT(K23,K1),DXM(K13,K1)
      DIMENSION NB(K1),NJFMT(K18),NIFMT(K18),NJTOT(K18),NITOT(K18),PO(*)
      CHARACTER*8 SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_fldwav/RCS/rdrch55.f,v $
     . $',                                                             '
     .$Id: rdrch55.f,v 1.4 2004/02/02 20:39:57 jgofus Exp $
     . $' /
C    ===================================================================
C
      DATA SNAME/ 'RDRCH55 ' /

      CALL FPRBUG(SNAME, 1, 55, IBUG)

      IERR=0

      IF(NODESC.EQ.0)THEN
      IF(IBUG.EQ.1) WRITE(IODBUG,18)
   18 FORMAT(//
     .10X,'XT   = LOCATION OF SECTION WHERE COMPUTATIONS ARE MADE'/
     .10X,'DXM  = MIN COMPUTATIONAL DISTANCE INTERVAL BET SECTIONS'/
     .10X,'KRCHT= PARAMETER FOR ROUTING METHOD OR INTERNAL BOUNDARY'/)
      ENDIF

CCC      MXRCHT=MXNBT
      MXNB=0

      DO 1500 J=1,JN
      N=NBT(J)
      NM=N-1
C......................................................................
C     XT   --  LOCATION OF SECTION WHERE COMPUTATIONS ARE MADE
C......................................................................
      IF(IBUG.EQ.1) WRITE(IODBUG,100) J,J
  100 FORMAT(/' XT(I,',I2,') I=1,NB(',I2,')')
      READ(IN,'(A)',END=1000) DESC
      READ(IN,*) (XT(I,J),I=1,N)
      IF(IBUG.EQ.1) WRITE(IODBUG,1005) (XT(I,J),I=1,N)
 1005 FORMAT(8F10.3)

C.......................................................................
C     DXM   --  MINIMUM COMPUTATIONAL DISTANCE INTERVAL BET SECTIONS
C.......................................................................
      IF(IBUG.EQ.1) WRITE(IODBUG,101) J,J
 101  FORMAT(/' DXM(I,',I2,') I=1,NB(',I2,')')
      READ(IN,'(A)',END=1000) DESC
      READ(IN,*) (DXM(I,J),I=1,NM)
      IF(IBUG.EQ.1) WRITE(IODBUG,1005) (DXM(I,J),I=1,NM)

C.......................................................................
C     KRCHT --  PARAMETER FOR ROUTING METHOD OR INTERNAL BOUNDARY
C.......................................................................
      IF(IBUG.EQ.1) WRITE(IODBUG,1020) J
 1020 FORMAT(/' KRCH(I,',I2,') I=1,NB-1')
      READ(IN,'(A)',END=1000) DESC
      READ(IN,*) (KRCHT(I,J),I=1,NM)
      IF(IBUG.EQ.1) WRITE(IODBUG,1025) (KRCHT(I,J),I=1,NM)
 1025 FORMAT(5X,8I5)
 1500 CONTINUE

C
C       DETERMINE THE FOLLOWING:
C         NN(I,J) -- NEW NUMBER OF EACH ORIGINAL X-SECTION
C         NB(J)   -- NEW TOTAL NUMBER OF X-XECTIONS INCLUDING
C                    INTERPOLATED ONES.
C         NLOCK   -- TOTAL NUM. OF DAM/LOCK TIME SERIES
      NLOCK=0
      NSECT=0
      DO 870 J=1,JN
      N=NBT(J)
      NM=N-1
      II=0
      LL=0
      DO 750 I=1,NM
C    DXM FOR DAM OR BRIDGE
      KRA=IABS(KRCHT(I,J))
      IF (KRA.EQ.28) NLOCK=NLOCK+1
      DXMX=DXM(I,J)
      IF (DXMX.LT.0.0001) DXMX=1000.0
      IF (KRA.GE.10) DXMX=1000.0
      IF(KRA.LT.10.OR.KRA.GT.40) GO TO 700
      LL=LL+1
  700 NX=ABS(XT(I+1,J)-XT(I,J))/DXMX+0.05
      IF(NX.EQ.0) NX=1
      II=II+NX
  750 CONTINUE
      NUMLAD(J)=LL
      NB(J)=II+1
      NSECT=NSECT+NB(J)
      IF(MXNB.LT.NB(J)) MXNB=NB(J)
      IF(MXINBD.LT.NUMLAD(J)) MXINBD=NUMLAD(J)
  870 CONTINUE
      PO(315)=NSECT+0.01
      PO(321)=NLOCK+0.01

      MXNB=MXNB+1
      MXNXLV=0
      IF(NLEV.EQ.0) GO TO 900
      DO 880 L=1,NLEV
      JFM=NJFMT(L)
      IFM=NIFMT(L)
      JTO=NJTOT(L)
      ITO=NITOT(L)
      NXT=0                                 
      DXMF=DXM(IFM,JFM)
      DXMT=DXM(ITO,JTO)
      IF (DXMF.LT.0.000001) DXMF=1000.0
      IF (DXMT.LT.0.000001) DXMT=1000.0
      NXF=ABS(XT(IFM+1,JFM)-XT(IFM,JFM))/DXMF+0.05
      IF(ITO.GT.0) NXT=ABS(XT(ITO+1,JTO)-XT(ITO,JTO))/DXMT+0.05
      NLV=NXF
      IF(NXT.GT.NXF) NLV=NXT
      IF(NLV.LT.1) NLV=1
      MXNXLV=MXNXLV+NLV
  880 CONTINUE 
  900 CONTINUE  
C      IF(IBUG.EQ.1) WRITE(IODBUG,11111)
11111 FORMAT(1X,'** EXIT RDRCH **')
      GO TO 9000
 1000 WRITE(IPR,1010)
 1010 FORMAT(/5X,'**ERROR** END OF FILE ENCOUNTERED WHILE READING INPUT
     *REACH INFO.'/)
      CALL ERROR
      IERR=1
 9000 RETURN
      END
