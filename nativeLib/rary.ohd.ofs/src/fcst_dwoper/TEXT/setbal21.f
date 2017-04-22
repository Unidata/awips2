      SUBROUTINE SETBAL21(I,STN,XNOS,NU,IMIN,IMAX,IFUT,LAG)

C  THIS SUBROUTINE COMPUTED THE BALLANCES FOR THE TIDE

      INCLUDE 'common/fdbug'
      COMMON/XHILO/EHH,ELH,ELL,EHL,EHHO,ELHO,ELLO,EHLO,
     .             BHH,BLH,BLL,BHL,BHHO,BLHO,BLLO,BHLO,BLMX,BLMN,
     .             ITHH,ITLH,ITLL,ITHL,ITHHO,ITLHO,ITLLO,ITHLO
C
      DIMENSION STN(*),XNOS(*)
      CHARACTER*8  SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_dwoper/RCS/setbal21.f,v $
     . $',                                                             '
     .$Id: setbal21.f,v 1.2 2000/09/27 16:12:39 page Exp $
     . $' /
C    ===================================================================
C

      DATA  SNAME / 'SETBAL21' /
C
C
      CALL FPRBUG(SNAME,1,21,IBUG)

      IF(I.LT.IMAX) GO TO 500
      BHHO=BHH
      BLHO=BLH
      BHLO=BHL
      BLLO=BLL
      EHHO=EHH
      ELHO=ELH
      EHLO=EHL
      ELLO=ELL
      ITHHO=ITHH
      ITLHO=ITLH
      ITHLO=ITHL
      ITLLO=ITLL

      II=IMAX+1

      CALL CKHILO21(XNOS,II,NU)
  200 IF(IFUT.EQ.0) THEN
        CALL GETBAL21(STN,ITHH,EHH,BHH,EHHO,BHHO,LAG)
        CALL GETBAL21(STN,ITLL,ELL,BLL,ELLO,BLLO,LAG)
        CALL GETBAL21(STN,ITLH,ELH,BLH,ELHO,BLHO,LAG)
        CALL GETBAL21(STN,ITHL,EHL,BHL,EHLO,BHLO,LAG)
      ELSE
        BHH=BHH*.8
        BLL=BLL*.8
        BLH=BLH*.8
        BHL=BHL*.8
      ENDIF
      IMAX=ITHH
      IMIN=ITHH
      BLMX=BHH
      BLMN=BHH
      IF(ITLH.GT.IMAX) THEN
        IMAX=ITLH
        BLMX=BLH
      ENDIF
      IF(ITHL.GT.IMAX) THEN
        IMAX=ITHL
        BLMX=BHL
      ENDIF
      IF(ITLL.GT.IMAX) THEN
        IMAX=ITLL
        BLMX=BLL
      ENDIF
      IF(ITLH.LT.IMIN) THEN
        IMIN=ITLH
        BLMN=BLH
      ENDIF
      IF(ITHL.LT.IMIN) THEN
        IMIN=ITHL
        BLMN=BHL
      ENDIF
      IF(ITLL.LT.IMIN) THEN
        IMIN=ITLL
        BLMN=BLL
      ENDIF
 500  IF(ITRACE.EQ.1) WRITE(IODBUG,9000) SNAME
 9000 FORMAT(1H0,'** ',A,' EXITED.')
      RETURN
      END
C ----------------------------------------------------------------------
