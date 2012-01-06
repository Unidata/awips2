      SUBROUTINE BLINIT21(XNOS,STN,NU,LAG)

C    THIS SUBROUTINE INITIALIZES THE HI & LO INFO

C  DEFINITION OF VARIABLES
C    EHH,ELH,ELL,EHL         - ELEVATION CORR. TO TIDE HI-HI,LO-HI,
C                                LO-LO,HI-LO AT CURRENTLY
C    EHHO,ELHO,ELLO,EHLO     - ELEVATION CORR. TO TIDE HI-HI,LO-HI,
C                                LO-LO,HI-LO AT PREVIOUSLY
C    BHH,BLH,BLL,BHL         - (NOS-OBS) FOR CURRENT HH,LH,LL,HL
C    BHHO,BLHO,BLLO,BHLO     - (NOS-OBS) FOR PREVIOUS HH,LH,LL,HL
C    BLMX,BLMN               - MAX AND MIN (NOS-OBS)
C    ITHH,ITLH,ITLL,ITHL     - TIME STEP CORR TO CURRENT HH,LH,LL,HL
C    ITHHO,ITLHO,ITLLO,ITHLO - TIME STEP CORR TO PREVIOUS HH,LH,LL,HL
C    NU      - NO. OF TIME STEPS
C    LAG     - NO. OF LAG PERIODS BETWEEN OBS & NOS TIDE PEAKS & VALLEYS
C    STN(I)  - OBSERVED TIDE AT D/S BOUNDARY  T.S.
C    XNOS(I) - NOS TIDE T.S.
C
C THIS SUBROUTINE CALLS: CKHILO21,GETBAL21
 
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fnopr'
 
      COMMON/XHILO/EHH,ELH,ELL,EHL,EHHO,ELHO,ELLO,EHLO,
     .             BHH,BLH,BLL,BHL,BHHO,BLHO,BLLO,BHLO,BLMX,BLMN,
     .             ITHH,ITLH,ITLL,ITHL,ITHHO,ITLHO,ITLLO,ITHLO

C
      DIMENSION XNOS(*),STN(*)
      CHARACTER*8  SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_dwoper/RCS/blinit21.f,v $
     . $',                                                             '
     .$Id: blinit21.f,v 1.3 2000/09/27 16:09:30 page Exp $
     . $' /
C    ===================================================================
C

      DATA  SNAME / 'BLINIT21' /
C
C
      CALL FPRBUG(SNAME,1,21,IBUG)

C...FIND HI AND LO VALUES
      II=1
      CALL CKHILO21(XNOS,II,NU)

C...IMAX IS THE LAST TIME PD IN THE PREVIOUS CYCLE
      IMAX=ITHH
      IF(IMAX.LT.ITLL) IMAX=ITLL
      IF(IMAX.LT.ITLH) IMAX=ITLH
      IF(IMAX.LT.ITHL) IMAX=ITHL

      ITHH1=ITHH
      ITLL1=ITLL
      ITLH1=ITLH
      ITHL1=ITHL
      EHH1=EHH
      ELL1=ELL
      ELH1=ELH
      EHL1=EHL

      IMX=IMAX
      IF(STN(ITHH-LAG).LT.-900.) THEN
  100   II=IMX+1
        CALL CKHILO21(XNOS,II,NU)
        IMX=ITHH
        IF(IMX.LT.ITLL) IMX=ITLL
        IF(IMX.LT.ITLH) IMX=ITLH
        IF(IMX.LT.ITHL) IMX=ITHL
        IF(STN(ITHH-LAG).LT.-900.) THEN
          II1=II
          II=IMX+1
          IF(II.LT.NU.AND.II1.NE.II) GO TO 100
        ENDIF
      ENDIF
      IF(STN(ITHH-LAG).GT.-900.) THEN
        CALL GETBAL21(STN,ITHH,EHH,BHH,EHHO,BHHO,LAG)
        BHHO=BHH
        ITHH=ITHH1
        ITHHO=ITHH
        EHH=EHH1
      ELSE
        PRINT*, ' NO BALANCE FOUND ... PROGRAM TERMINATED'
        BHHO=0.
        BHH=0.
CC        STOP
      ENDIF
      IMX=IMAX
      IF(STN(ITLL-LAG).LT.-900.) THEN
  200   II=IMX+1
        CALL CKHILO21(XNOS,II,NU)
        IMX=ITHH
        IF(IMX.LT.ITLL) IMX=ITLL
        IF(IMX.LT.ITLH) IMX=ITLH
        IF(IMX.LT.ITHL) IMX=ITHL
        IF(STN(ITLL-LAG).LT.-900.) THEN
          II1=II
          II=IMX+1
          IF(II.LT.NU.AND.II1.NE.II) GO TO 200
        ENDIF
      ENDIF
      IF(STN(ITLL-LAG).GT.-900.) THEN
        CALL GETBAL21(STN,ITLL,ELL,BLL,ELLO,BLLO,LAG)
        BLLO=BLL
        ITLL=ITLL1
        ITLLO=ITLL
        ELL=ELL1
      ELSE
        PRINT*, ' NO BALANCE FOUND ... PROGRAM TERMINATED'
        BLLO=0.
        BLL=0.
CC        STOP
      ENDIF

      IMX=IMAX
      IF(STN(ITLH-LAG).LT.-900.) THEN
  300   II=IMX+1
        CALL CKHILO21(XNOS,II,NU)
        IMX=ITHH
        IF(IMX.LT.ITLL) IMX=ITLL
        IF(IMX.LT.ITLH) IMX=ITLH
        IF(IMX.LT.ITHL) IMX=ITHL
        IF(STN(ITLH-LAG).LT.-900.) THEN
          II1=II
          II=IMX+1
          IF(II.LT.NU.AND.II1.NE.II) GO TO 300
        ENDIF
      ENDIF
      IF(STN(ITLH-LAG).GT.-900.) THEN
        CALL GETBAL21(STN,ITLH,ELH,BLH,ELHO,BLHO,LAG)
        BLHO=BLH
        ITLH=ITLH1
        ITLHO=ITLH
        ELH=ELH1
      ELSE
        PRINT*, ' NO BALANCE FOUND ... PROGRAM TERMINATED'
        BLHO=0.
        BLH=0.
CC        STOP
      ENDIF

      IMX=IMAX
      IF(STN(ITHL-LAG).LT.-900.) THEN
  400   II=IMX+1
        CALL CKHILO21(XNOS,II,NU)
        IMX=ITHH
        IF(IMX.LT.ITLL) IMX=ITLL
        IF(IMX.LT.ITLH) IMX=ITLH
        IF(IMX.LT.ITHL) IMX=ITHL
        IF(STN(ITHL-LAG).LT.-900.) THEN
          II1=II
          II=IMX+1
          IF(II.LT.NU.AND.II1.NE.II) GO TO 400
        ENDIF
      ENDIF
      IF(STN(ITHL-LAG).GT.-900.) THEN
        CALL GETBAL21(STN,ITHL,EHL,BHL,EHLO,BHLO,LAG)
        BHLO=BHL
        ITHL=ITHL1
        ITHLO=ITHL
        EHL=EHL1
      ELSE
        PRINT*, ' NO BALANCE FOUND ... PROGRAM TERMINATED'
        BHLO=0.
        BHL=0.
CC        STOP
      ENDIF
      IF(ITRACE.EQ.1) WRITE(IODBUG,9000) SNAME
 9000 FORMAT(1H0,'** ',A,' EXITED.')
      RETURN
      END
C ----------------------------------------------------------------------
