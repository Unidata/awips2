C MEMBER PRP40
C  (from old member MCPRP40)
C
      SUBROUTINE PRP40(PO)
C***************************************
C
C     THE FUNCTION OF THIS SUBROUTINE IS TO PRINT THE CONTENTS OF THE PO
C     ARRAY FOR THE WATER BALANCE OPERATION.
C
C     THIS SUBROUTINE WAS WRITTEN BY:
C     ROBERT M. HARPER  HRL  MAY 1991
C***************************************
C
      DIMENSION PO(1),SNOW(2),SNWNAM(2),RR(2),RRNAM(2),SNAME(2)
C
      COMMON/IONUM/IN,IPR,IPU
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/calb_waterbal/RCS/prp40.f,v $
     . $',                                                             '
     .$Id: prp40.f,v 1.1 1996/02/14 15:17:21 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SNAME/'PRP4','0   '/
C***************************************
C
C     TRACE LEVEL=1, NO DEBUG OUTPUT
      IF(ITRACE .GE. 1) WRITE(IODBUG,900) SNAME
  900 FORMAT(/10X,'** ',2A4,' ENTERED')
C***************************************
C
C     CONTROL VARIABLES
      NOSUB=PO(14)
      AREA=PO(13)
C***************************************
C
      WRITE(IPR,500) (PO(J),J=2,6)
      WRITE(IPR,510) NOSUB
      WRITE(IPR,520) AREA
      IF((PO(15) .LT. 1.0) .AND. (PO(17) .LT. 1.0)) GO TO 10
      WRITE(IPR,530)
      IF(PO(15) .LT. 1.0) GO TO 20
      WRITE(IPR,540)
   20 IF(PO(17) .LT. 1.0) GO TO 10
      WRITE(IPR,550)
   10 WRITE(IPR,560)
      WRITE(IPR,570)
      WRITE(IPR,580) PO(7),PO(8),PO(9)
      WRITE(IPR,580) PO(10),PO(11),PO(12)
      WRITE(IPR,590)
      WRITE(IPR,600)
      WRITE(IPR,610)
      DO 30 N=1,NOSUB
        I1=23+(N-1)*17
        NAM=I1+4
        WT=PO(I1+5)
        SNOW(1)=PO(I1+6)
        SNOW(2)=PO(I1+7)
        SNWNAM(1)=PO(I1+8)
        SNWNAM(2)=PO(I1+9)
        RR(1)=PO(I1+11)
        RR(2)=PO(I1+12)
        RRNAM(1)=PO(I1+13)
        RRNAM(2)=PO(I1+14)
        WRITE(IPR,620) (PO(J),J=I1,NAM),WT,SNOW,SNWNAM,RR,RRNAM
   30 CONTINUE
C***************************************
C
  500 FORMAT(/10X,'WATER BALANCE OPERATION FOR ',5A4)
  510 FORMAT(/15X,'WATER BALANCE DISPLAYS WILL BE GENERATED FOR',I5,' AR
     &EA(S)')
  520 FORMAT(/15X,'TOTAL AREA ABOVE FLOW POINT = ',F8.1,' SQ.KM.')
  530 FORMAT(/10X,'DISPLAY OPTIONS SELECTED:')
  540 FORMAT(/15X,'YEARLY WATER BALANCE SUMMARY DISPLAY')
  550 FORMAT(15X,'MULTI-YEAR AVERAGE ZONE CONTENTS DISPLAY')
  560 FORMAT(/10X,'GENERAL DISCHARGE TIME SERIES INFORMATION:')
  570 FORMAT(/15X,'T.S. IDENTIFIER',9X,'T.S. DATA TYPE')
  580 FORMAT(18X,2A4,18X,A4)
  590 FORMAT(/10X,'GENERAL AREA/SUBAREA INFORMATION:')
  600 FORMAT(/53X,'SNOW OPERATION',13X,'RAINFALL-RUNOFF OPERATION')
  610 FORMAT(15X,'NAME',20X,'WEIGHT',6X,'TYPE',11X,'NAME',11X,'TYPE',11
     &X,'NAME')
  620 FORMAT(15X,5A4,4X,F5.3,7X,2A4,7X,2A4,7X,2A4,7X,2A4)
C***************************************
C
C     TRACE LEVEL=1
      IF(ITRACE .GE. 1) WRITE(IODBUG,910) SNAME
  910 FORMAT(/1X,'** ',2A4,' EXITED')
C
      RETURN
      END
