      SUBROUTINE TDFILL21(TIDE,STN,XNOS,I,LAG,IMIN,NU)

C  THIS SUBROUTINE FILLS THE MISSING VALUES IN THE TIDE

      INCLUDE 'common/fdbug'
      COMMON/XHILO/EHH,ELH,ELL,EHL,EHHO,ELHO,ELLO,EHLO,
     .             BHH,BLH,BLL,BHL,BHHO,BLHO,BLLO,BHLO,BLMX,BLMN,
     .             ITHH,ITLH,ITLL,ITHL,ITHHO,ITLHO,ITLLO,ITHLO
C
      DIMENSION TIDE(*),STN(*),XNOS(*),ITOR(5),BOR(5)
      CHARACTER*8  SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_dwoper/RCS/tdfill21.f,v $
     . $',                                                             '
     .$Id: tdfill21.f,v 1.3 2001/06/13 19:15:18 dws Exp $
     . $' /
C    ===================================================================
C

      DATA  SNAME / 'TDFILL21' /
C
C
      CALL FPRBUG(SNAME,1,21,IBUG)

C  ORDER THE TIDE

      ITOR(1)=ITHHO
      BOR(1)=BHHO
      IF(ITOR(1).LT.ITLLO) THEN
        ITOR(1)=ITLLO
        BOR(1)=BLLO
      ENDIF
      IF(ITOR(1).LT.ITLHO) THEN
        ITOR(1)=ITLHO
        BOR(1)=BLHO
      ENDIF
      IF(ITOR(1).LT.ITHLO) THEN
        ITOR(1)=ITHLO
        BOR(1)=BHLO
      ENDIF

      ITOR(2)=ITHH
      BOR(2)=BHH
      IF(ITOR(2).GT.ITLL) THEN
        ITOR(3)=ITOR(2)
        BOR(3)=BOR(2)
        ITOR(2)=ITLL
        BOR(2)=BLL
      ELSE
        ITOR(3)=ITLL
        BOR(3)=BLL
      ENDIF

      IF(ITOR(2).GT.ITLH) THEN      
        DO 10 KK=1,2
          K=5-KK
          ITOR(K)=ITOR(K-1)
          BOR(K)=BOR(K-1)
   10   CONTINUE
        ITOR(2)=ITLH
        BOR(2)=BLH
      ELSEIF(ITOR(3).GT.ITLH) THEN
        ITOR(4)=ITOR(3)
        BOR(4)=BOR(3)
        ITOR(3)=ITLH
        BOR(3)=BLH
      ELSE
        ITOR(4)=ITLH
        BOR(4)=BLH
      ENDIF

      IF(ITOR(2).GT.ITHL) THEN
        DO 20 KK=1,3
          K=6-KK
          ITOR(K)=ITOR(K-1)
          BOR(K)=BOR(K-1)
   20   CONTINUE
        ITOR(2)=ITHL
        BOR(2)=BHL
      ELSEIF(ITOR(3).GT.ITHL) THEN      
        DO 30 KK=1,2
          K=6-KK
          ITOR(K)=ITOR(K-1)
          BOR(K)=BOR(K-1)
   30   CONTINUE
      ELSEIF(ITOR(4).GT.ITHL) THEN
        ITOR(5)=ITOR(4)
        BOR(5)=BOR(4)
        ITOR(4)=ITHL
        BOR(4)=BHL
      ELSE
        ITOR(5)=ITHL
        BOR(5)=BHL
      ENDIF

C...1st part of tide is incomplete
      IF(ITOR(2).LT.ITOR(1).AND.I.LE.ITOR(5)) THEN
        BOR(1)=BOR(5)
        ITOR(1)=ITOR(5)-6
      ENDIF    

C...last part of tide is incomplete
      IF(ITOR(1).EQ.ITOR(5)) THEN
        DO 40 KN=1,4
          ITOR(KN)=ITOR(KN+1)
          BOR(KN)=BOR(KN+1)
 40    CONTINUE
        ITOR(5)=NU
        BOR(5)=BOR(1)
      ENDIF

      IB=I+LAG
      DO 50 K=2,5
        IF(IB.GE.ITOR(K-1).AND.IB.LE.ITOR(K)) THEN
          CALL TDINTP21(BOR(K-1),BOR(K),ITOR(K-1),ITOR(K),IB,XNOS(IB),
     .       TIDE(I))
          GO TO 60
        ENDIF
   50 CONTINUE
      K=5
      IF(IB.GT.NU) IB=NU
      TIDE(I)=XNOS(IB)
   60 IF(IBUG.EQ.1) WRITE(IODBUG,100) ITOR(K-1),ITOR(K),I,IB,
     .   STN(I),XNOS(I),TIDE(I)
  100 FORMAT(10X,'  IT1  IT2    I   IB       OBS       NOS      TIDE'/
     . 10X,4I5,3F10.2)
      IF(ITRACE.EQ.1) WRITE(IODBUG,9000) SNAME
 9000 FORMAT(1H0,'** ',A,' EXITED.')
      RETURN
      END
C ----------------------------------------------------------------------
