      SUBROUTINE  REDIC55(CO,ICO,NB,NQL,NUMLAD,YDI,QDI,QLI,PLTI,IWTI,
     . JN,NTQL,KRCHT,LAD,NLOCK,IERR,IDOS,K16,K13,K2,K1)

      COMMON/IONUM/IN,IPR,IPU
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      DIMENSION CO(1),ICO(1),NB(K1),NQL(K1),NUMLAD(K1),LAD(K16,K1)
      DIMENSION YDI(K2,K1),QDI(K2,K1),QLI(NTQL),PLTI(NLOCK),IWTI(NLOCK)
      DIMENSION KRCHT(K13,K1)
      CHARACTER*8  SNAME

C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_fldwav/RCS/redic55.f,v $
     . $',                                                             '
     .$Id: redic55.f,v 1.4 2002/02/11 13:53:16 michaelo Exp $
     . $' /
C    ===================================================================
C
      DATA SNAME / 'REDIC55 ' /

      CALL FPRBUG(SNAME,1,55,IBUG)
 
C.......................................................................
C    ENTER CARRYOVER INFORMATION
C   1   YDI   - INITIAL WSEL
C   2   QDI   - INITIAL DISCHARGES
C   3   QLI   - INITIAL LATERAL INFLOWS
C   4   PLTI  - INITIAL POOL ELEVATIONS
C   5   IWTI  - INITIAL GATE CONTROL SWITCHES
C.......................................................................

      IERR=0

      DO 25 J=1,JN
        N=NB(J)
C.......................................................................
C  YDI
        READ(IN,'(A)',END=1000) DESC
        READ(IN,*,END=1000) (YDI(I,J),I=1,N)
        IF(IBUG.EQ.1) THEN
          WRITE(IODBUG,10) J
   10     FORMAT(/10X,'INITIAL WSEL FOR RIVER NO. ',I5)
          WRITE(IODBUG,20) (YDI(I,J),I=1,N)
   20     FORMAT(10F10.2)
        ENDIF
   25 CONTINUE
C.......................................................................
C  QDI
      DO 50 J=1,JN
        N=NB(J)
        READ(IN,'(A)',END=1000) DESC
        READ(IN,*,END=1000) (QDI(I,J),I=1,N)
        IF(IBUG.EQ.1) THEN
          WRITE(IODBUG,30) J
   30     FORMAT(/10X,'INITIAL DISCHARGES FOR RIVER NO. ',I5)
          WRITE(IODBUG,20) (QDI(I,J),I=1,N)
   40     FORMAT(10F10.0)
        ENDIF
   50 CONTINUE

        IF(IDOS.LT.3) GO TO 500
C.......................................................................
C  QLI
      LQLI=0
      DO 75 J=1,JN
        IF(NQL(J).EQ.0) GO TO 75
        NQ=NQL(J)
        READ(IN,'(A)',END=1000) DESC
        READ(IN,*,END=1000) (QLI(LQLI+I),I=1,NQ)
        IF(IBUG.EQ.1) THEN
          WRITE(IODBUG,60) J
   60     FORMAT(/10X,'INITIAL LATERAL INFLOWS FOR RIVER NO. ',I5)
          WRITE(IODBUG,40) (QLI(LQLI+I),I=1,NQ)
        ENDIF
        LQLI=LQLI+NQ
   75 CONTINUE
C.......................................................................
C  PLTI
      IF(NLOCK.EQ.0) GO TO 500
      LPLT=0
      IP1=1
      DO 100 J=1,JN
        IF(NUMLAD(J).EQ.0) GO TO 100
        NUM=NUMLAD(J)
        NPL=0
cc        I1=LCAT21(1,J,IPO(LONLAD))
        DO 78 I=1,NUM
        LD=LAD(I,J)
        IF(KRCHT(LD,J).EQ.28) NPL=NPL+1
 78     CONTINUE
        IF(NPL.EQ.0) GO TO 100
        IP2=IP1+NPL-1
        READ(IN,'(A)',END=1000) DESC
        READ(IN,*,END=1000) (PLTI(LPLT+I),I=IP1,IP2)
        IF(IBUG.EQ.1) THEN
          WRITE(IODBUG,77) J
   77     FORMAT(/10X,'INITIAL POOL ELEVATIONS FOR RIVER NO. ',I5)
          WRITE(IODBUG,40) (PLTI(LPLT+I),I=IP1,IP2)
        ENDIF
        LPLT=LPLT+NPL
CMGM        IP1=IP2+1
        IP1=1
  100 CONTINUE
C.......................................................................
C  IWTI
      LIWT=0
      IP1=1
      DO 125 J=1,JN
        IF(NUMLAD(J).EQ.0) GO TO 125
        NUM=NUMLAD(J)
        NPL=0
        DO 72 I=1,NUM
        LD=LAD(I,J)
        IF(KRCHT(LD,J).EQ.28) NPL=NPL+1
 72   CONTINUE
        IF(NPL.EQ.0) GO TO 125
        IP2=IP1+NPL-1
        READ(IN,'(A)',END=1000) DESC
        READ(IN,*,END=1000) (IWTI(LIWT+I),I=IP1,IP2)
        IF(IBUG.EQ.1) THEN
          WRITE(IODBUG,70) J
   70     FORMAT(/10X,'INITIAL GATE CONTROL SWITCHES FOR RIVER NO. ',I5)
          WRITE(IODBUG,80) (IWTI(IWT+I),I=IP1,IP2)
   80     FORMAT(20I5)
        ENDIF
        LIWT=LIWT+NUM
CMGM        IP1=IP2+1
        IP1=1
  125 CONTINUE
  500 CONTINUE
      GO TO 9000

 1000 WRITE(IPR,1010)
 1010 FORMAT(/5X,'**ERROR** END OF FILE ENCOUNTERED WHILE READING INITIA
     .L CONDITIONS')
 5000 IERR=1
 9000 RETURN
      END
