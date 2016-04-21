C MEMBER FPSNW5
C  (from old member MCEX5)
C
C @PROCESS LVL(77)
C
      SUBROUTINE FPSNW5(NSNOW,LSNW,PO,P,MP,C,MC)
C.......................................
C     THIS SUBROUTINE PRINTS INFORMATION ABOUT SNOW-17 OPERATIONS
C        FOR THE SAC-PLOT OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY...
C       ERIC ANDERSON-HRL AUG. 1980
C.......................................
C     VARIABLES IN THE ARGUMENT LIST ARE
C       NAME           DESCRIPTION
C       NSNOW         NUMBER OF SNOW-17 OPERATIONS
C       LSNW          LOCATION OF SNOW-17 OPERATIONS IN THE P ARRAY.
C       PO            PARAMETER ARRAY FOR THE SAC-PLOT OPERATION
C       P             THE ENTIRE P ARRAY
C       MP            DIMENSON OF THE P ARRAY
C       C             THE ENTIRE C ARRAY
C       MC            DIMENSION OF THE C ARRAY
C.......................................
      DIMENSION P(MP),C(MC)
      DIMENSION PO(*),LSNW(*)
C
C     COMMON BLOCKS
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/calb_sacplot/RCS/fpsnw5.f,v $
     . $',                                                             '
     .$Id: fpsnw5.f,v 1.2 1996/07/11 19:24:53 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     TRACE LEVEL=1
      IF (ITRACE.GE.1) WRITE(IODBUG,10)
10    FORMAT(1H0,17H** FPSNW5 ENTERED)
C.......................................
C     PRINT PARAMETER VALUES.
      WRITE(IPR,20)
20    FORMAT(1H0,8HOP. NAME,2X,5HPXADJ,3X,4HELEV,4X,3HSCF,2X,5HMFMAX,
     12X,5HMFMIN,3X,4HUADJ,5X,2HSI,4X,3HNMF,3X,4HTIPM,2X,5HMBASE,1X,
     26HPXTEMP,2X,5HPLWHC,2X,5HDAYGM,3X,4HLAT.)
      DO 40 N=1,NSNOW
      LPM=LSNW(N)
      IV=P(LPM+24)-1.0
      LV=IV+13
      LPM5 = LPM-5
      LPM4 = LPM-4
      LPMIV = LPM+IV
      LPMLV = LPM+LV
C
C  FOLLOWING WRITE CONVERTED FOR CONFORMITY WITH VS FORTRAN LVL 66.
C
C         WRITE(IPR,902) P(LPM-5),P(LPM-4),(P(LPM+I),I=IV,LV)
C
      WRITE(IPR,30) P(LPM5),P(LPM4),(P(I),I=LPMIV,LPMLV)
30    FORMAT(1H ,2A4,F7.2,F7.0,3F7.2,F7.3,F7.0,2F7.2,2F7.1,2F7.2,F7.1)
40    CONTINUE
C.......................................
C     PRINT CARRYOVER AND SUMMATIONS
      LP=PO(14)
      DO 50 N=1,NSNOW
      I=PO(LP+N*3)
      IF (I.EQ.0) GO TO 50
      GO TO 70
50    CONTINUE
C
C     PRINT TITLE
      WRITE(IPR,60)
60    FORMAT(1H0,21X,9HVARIABLES,/1X,8HOP. NAME,4X,3HSWE,2X,5HNEGHS,
     13X,4HLIQW,4X,3HATI,1X,6HMAX WE)
      GO TO 90
70    WRITE(IPR,80)
80    FORMAT(1H0,21X,9HVARIABLES,21X,24HWATER BALANCE COMPONENTS,20X,
     18HRAIN-ON-,2X,8HNON-RAIN,6X,4HRAIN,/1X,8HOP. NAME,4X,3HSWE,2X,
     25HNEGHS,3X,4HLIQW,4X,3HATI,1X,6HMAX WE,3X,7HPRECIP.,1X,
     39HRAIN+MELT,2X,8HDELTA WE,2X,8HRESIDUAL,2X,8HSNOWFALL,1X,
     49HBARE GRND,4X,4HMELT,8X,4HMELT)
C
C     PRINT VALUES.
90    DO 130 N=1,NSNOW
      LPM=LSNW(N)
      LOC=P(LPM-1)
      IV=LOC+1
      LV=LOC+4
      ITPX=P(LPM+9)
      NEXLAG=5/ITPX+2
      TEX=0.0
      DO 100 I=1,NEXLAG
100   TEX=TEX+C(LOC+9+I)
      SWE=C(LOC)+C(LOC+2)+C(LOC+8)+TEX
      LPM5 = LPM-5
      LPM4 = LPM-4
      I=PO(LP+N*3)
      IF(I.GT.0) GO TO 120
C
C     SUMS NOT STORED.
C
C  FOLLOWING WRITE CONVERTED FOR CONFORMITY WITH VS FORTRAN LVL 66
C
C         WRITE(IPR,905)P(LPM-5),P(LPM-4),SWE,(C(I),I=IV,LV)
C
      WRITE(IPR,110)P(LPM5),P(LPM4),SWE,(C(I),I=IV,LV)
110   FORMAT(1H ,2A4,F7.0,3F7.1,F7.0,3F10.1,F10.3,4F10.1)
      GO TO 130
C
C     SUMS STORED
120   LS=LSNW(N)+I-2
      DWE=P(LS+1)-P(LS+3)-P(LS+7)
      LS1 = LS+1
      LS2 = LS+2
      LS3 = LS+3
      LS4 = LS+4
      LS5 = LS+5
      LS6 = LS+6
      LS7 = LS+7
C
C  FOLLOWING WRITE CONVERTED FOR CONFORMITY WITH VS FORTRAN LVL 66
C
C         WRITE(IPR,905) P(LPM-5),P(LPM-4),SWE,(C(I),I=IV,LV),P(LS+1),
C
      WRITE(IPR,110) P(LPM5),P(LPM4),SWE,(C(I),I=IV,LV),P(LS1),
     1P(LS3),DWE,P(LS7),P(LS2),P(LS6),P(LS4),P(LS5)
130   CONTINUE
C.......................................
C     PRINT SEPARATOR.
      WRITE(IPR,140)
140   FORMAT(1H0,50H**************************************************)
C.......................................
      RETURN
      END
