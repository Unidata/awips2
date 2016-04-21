      SUBROUTINE BLNDTD21(XNOS,STN,TIDE,NU)
C  THIS SUBROUTINE BLENDS THE NOS TIDE (STID) AND OBSERVED TIDE (TID)
C  TO GET AN ADJUSTED TIDE (TIDE) AS THE DOWNSTREAM BOUNDARY FOR
C  THE COLUMBIA RIVER.

C THIS SUBROUTINE CALLS: TDINIT21,GETLAG21,SETBAL21,TDFILL21

C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fnopr'
      INCLUDE 'common/ionum'
      COMMON/XHILO/EHH,ELH,ELL,EHL,EHHO,ELHO,ELLO,EHLO,
     .             BHH,BLH,BLL,BHL,BHHO,BLHO,BLLO,BHLO,BLMX,BLMN,
     .             ITHH,ITLH,ITLL,ITHL,ITHHO,ITLHO,ITLLO,ITHLO
C
      DIMENSION XNOS(*),STN(*),TIDE(*)
      CHARACTER*8  SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_dwoper/RCS/blndtd21.f,v $
     . $',                                                             '
     .$Id: blndtd21.f,v 1.2 2000/09/27 16:10:45 page Exp $
     . $' /
C    ===================================================================
C

      DATA  SNAME / 'BLNDTS21' /
C
C
      CALL FPRBUG(SNAME,1,21,IBUG)

      CALL TDINIT21(STN,XNOS,NU,MISS,IEND,LAG)

      IF(MISS.EQ.0) THEN
C......... NO MISSING DATA ... USE OBSERVED TIDE
C.......................................................................
        DO 10 I=1,NU
          TIDE(I)=STN(I)
   10   CONTINUE
      ELSEIF(MISS.EQ.NU) THEN
C......... ALL MISSING DATA ... USE NOS TIDE WITH NO ADJUSTMENT
C.......................................................................
        DO 20 I=1,NU
          TIDE(I)=XNOS(I)
   20   CONTINUE
        WRITE(IPR,25)
   25   FORMAT(//5X,'**WARNING**  OBSERVED TIDE MISSING FOR THE ENTIRE R
     .UN PERIOD ... NOS TIDE WILL BE USED.')
        CALL WARN
      ELSE
C......... SOME MISSING DATA ... BLEND NOS & OBS TIDE HYDROGRAPHS
C.......................................................................
CC        CALL GETLAG21(STN,XNOS,NU,LAG)
        IMAX=0
cc        IMIN=0
        IMIN=ITHH
        IMIN=MIN(IMIN,ITLL)
        IMIN=MIN(IMIN,ITLH)
        IMIN=MIN(IMIN,ITHL)
        IFUT=0
        IMX=IMAX
        DO 40 I=1,NU
          IF(STN(I).LT.-900.) THEN
            IF(I.GT.IEND.AND.I.GT.IMX) THEN
              IFUT=1
              CALL SETBAL21(I,STN,XNOS,NU,IMIN,IMAX,IFUT,LAG)
            ENDIF
            CALL TDFILL21(TIDE,STN,XNOS,I,LAG,IMIN,NU)
          ELSE
            TIDE(I)=STN(I)
            CALL SETBAL21(I,STN,XNOS,NU,IMIN,IMAX,IFUT,LAG)
          ENDIF         
          IF(I.LE.IEND) IMX=IMAX
   40   CONTINUE
      ENDIF
      IF(NOPROT.EQ.0) WRITE(IPR,42)
   42 FORMAT(//30X,'TIDAL INFORMATION'/14X,'I',7X,'OBS',7X,'NOS',6X,
     . 'TIDE',3X,'BALANCE')
      DO 50 I=1,NU
        DIFF=TIDE(I)-XNOS(I)
        IF(NOPROT.EQ.0) WRITE(IPR,30) I,STN(I),XNOS(I),TIDE(I),DIFF
   30   FORMAT(10X,I5,7F10.2)
   50 CONTINUE
      IF(ITRACE.EQ.1) WRITE(IODBUG,9000) SNAME
 9000 FORMAT(1H0,'** ',A,' EXITED.')
      RETURN
      END
C ----------------------------------------------------------------------
