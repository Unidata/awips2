C MEMBER PRC14
C  (from old member FCPRC14)
C.......................................................................
      SUBROUTINE PRC14(PADJ,CADJ)
C
C     SUBROUTINE PRINTS THE CARRYOVER STORED IN THE C (CADJ) ARRAY
C     FOR THE ADJUST OPERATION.
C
C.......................................................................
C     PROGRAMMED BY KAY KROUSE     FEBRUARY 1980
C.......................................................................
      DIMENSION PADJ(1),CADJ(1),PC14(2)
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fconit'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prc14.f,v $
     . $',                                                             '
     .$Id: prc14.f,v 1.1 1995/09/17 18:49:37 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA PC14/4HPRC1,4H4   /
C.......................................................................
C     CHECK TRACE LEVEL
      CALL FPRBUG(PC14,1,14,IBUG)
C.......................................................................
      NCO=PADJ(18)
      ICRY=PADJ(19)
      IF(IVALUE.EQ.0)GO TO 5
      IF(ICRY.EQ.0)GO TO 15
  5   IOI=PADJ(7)
      IOM=PADJ(8)
      MM=NCO-3
      WRITE(IPR,902)
 902  FORMAT(1H0,10X,20H**CARRYOVER VALUES**)
      IF(IOI.EQ.0)GO TO 10
      DELQ=CADJ(MM+1)
      NBLND=CADJ(MM+2)
      ABSQ=CADJ(MM+3)
      WRITE(IPR,900) DELQ
 900  FORMAT(1H0,10X,92HDIFFERENCE( DQI) BETWEEN SIM. AND OBS. DISCHARGE
     1 AT LAST OBSERVED ORDINATE OF PREVIOUS RUN =,F10.1,5H CMS.)
      WRITE(IPR,910) NBLND
 910  FORMAT(1H0,10X,50HNUMBER OF PERIODS( NBI ) BLENDED IN PREVIOUS RUN
     1 =,I2)
      WRITE(IPR,940) ABSQ
 940  FORMAT(1H0,10X,52HLAST OBSERVED DISCHARGE VALUE(QBI) OF PREVIOUS R
     1UN =,F7.1,5H CMS.)
 10   WRITE(IPR,920)
 920  FORMAT(1H0,10X,69HSIMULATED DISCHARGE FROM PREVIOUS RUN FOR LAST D
     1AY FROM 2400 TO 2400:/11X,33H(VALUES BEYOND LHR ARE NOT USED.))
      WRITE(IPR,925) (CADJ(I),I=1,MM)
 925  FORMAT(1H0,15X,10F10.1)
      GO TO 20
C
 15   WRITE(IPR,930)
 930  FORMAT(1H0,10X,72HDEFAULT CARRYOVER VALUES ARE USED. ALL VALUES AR
     1E INITIALLY EQUAL TO 0.0)
C.......................................................................
 20   CONTINUE
      RETURN
      END
