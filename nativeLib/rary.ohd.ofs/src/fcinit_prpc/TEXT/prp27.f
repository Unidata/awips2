C MODULE PRP27
C-----------------------------------------------------------------------
C
C  THIS IS THE PRINT PARAMETER ROUTINE FOR THE OPERATION LIST-FTW.
C
C  WRITTEN BY - DAVE REED - WGRFC - 4/1987
C
      SUBROUTINE PRP27 (PO)
C
      CHARACTER*8 SNAME
C
      DIMENSION PO(*),SNAME(2)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fengmt'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prp27.f,v $
     . $',                                                             '
     .$Id: prp27.f,v 1.2 2002/02/11 13:08:38 michaelo Exp $
     . $' /
C    ===================================================================
C
      DATA RYES/4HYES /
      DATA RNO/4HNO  /
      DATA RNONE/4HNONE/
      DATA RBLK/4H    /
C
C
      SNAME='PRP27'
      LTRACE=1
      IOPNUM=27
      CALL FPRBUG (SNAME,LTRACE,IOPNUM,IBUG)
C
      IF (IBUG.EQ.0) GO TO 60
      WRITE (IODBUG,10)(PO(J),J=1,30)
10    FORMAT (1H0,11HP(  1-  6) ,6F5.0,8H P(7-11),5(1X,A4) /
     1 10X,11HP( 12- 30) ,20F5.0)
      NTITL=PO(3)
      IPT=30
      DO 30 I=1,NTITL
      IP1=IPT+1
      IP2=IP1+17
      WRITE (IODBUG,20) IP1,IP2,(PO(J),J=IP1,IP2)
20    FORMAT (1H ,2HP(,I3,1H-,I3,2H) ,18A4)
      IPT=IPT+18
30    CONTINUE
      IPT=30+NTITL*18
      NTS=PO(4)
      DO 50 I=1,NTS
      IP1=IPT+1
      IP2=IP1+17
      WRITE (IODBUG,40) IP1,IP2,(PO(J),J=IP1,IP2)
40    FORMAT (1H ,2HP(,I3,1H-,I3,2H) ,2A4,1X,A4,1X,3F6.0,
     1 1X,A4,7F6.0,2(1X,A4),2F10.3)
      IPT=IPT+18
50    CONTINUE
60    CONTINUE
C
C    BEGIN HEADING AND VERSION NUMBER OF NEEDED
C
      WRITE (IPR,70)
70    FORMAT (1H0,10X,30HWGRFC TABULAR OUTPUT OPERATION)
      I1=INT(PO(1))
      IF (I1.GT.1.5) WRITE (IPR,80) I1
80    FORMAT (15X,7HVERSION,I3)
C
C    GENERAL PARAMETERS
C
      WRITE (IPR,90)
90    FORMAT (15X,18HGENERAL PARAMETERS /
     1       15X,46H# TITLE  # TIME  NEW  YDA   TDA   TMA   RATING,
     * 4X,6HNUMBER,
     2 /,     15X,45H  LINES  SERIES  PAGE FLAG  FLAG  FLAG  CURVE,
     * 5X,7HCOLUMNS)
C
      R=RNO
      IF (PO(5).GT.0.5)R=RYES
      RC1=PO(10)
      RC2=PO(11)
      IF (RC1.EQ.RBLK.AND.RC2.EQ.RBLK)RC1=RNONE
C
      I3=PO(3)
      I4=PO(4)
      I8=PO(8)
      I9=PO(9)
      WRITE (IPR,100) I3,I4,R,PO(7),PO(8),PO(9),
     1 RC1,RC2,PO(6)
100   FORMAT (19X,I3,I8,2X,A4,1X,A4,2X,A4,2X,A4,2X,2A4,F6.0)
C
C  PRINT TITLE LINES
      NTITL=PO(3)
      DO 130 I=1,NTITL
         I1=31+(I-1)*18
         I2=I1+17
         IF (I.EQ.1) WRITE (IPR,110) I,(PO(J),J=I1,I2)
110   FORMAT (1H0,15X,10HTITLE LINE,I3,2H: ,18A4)
         IF (I.NE.1) WRITE (IPR,120) I,(PO(J),J=I1,I2)
120   FORMAT (26X,I3,2H: ,18A4)
130      CONTINUE
C
C  WRITE OUTPUT FORMAT OF TIME SERIES AND TIME SERIES
C
      WRITE (IPR,140)
140   FORMAT ('0',10X,'OUTPUT FORMAT AND TIME SERIES' /
     1 14X,'TIME SERIES    INT PRT ENGL METR   INTO LEFT',
     2     ' RIGHT  MSNG RFC METRIC TO ENGLISH CON' ,
     *     '   PRINT OPTION' /
     3 14X,'TSID     TYPE  HRS INT UNIT UNIT FUTURE DEC',
     4     '   DEC  VALS   ID ENG=MET*SLOPE + INT'  /
     * 80X,'SLOPE',8X,'INT ' /
     5     14X,'-------- ---- ---- --- ---- ---- ------ ----' ,
     6     ' ----- ---- ---- --------------------- ',3X,20('-'))
C
      NTS=PO(4)
      ILINE=1
      IBLK=PO(12)
C
      DO 250 I=1,NTS
         IPT=(NTITL+I)*18+12
         IPOPT=PO(IPT+11)
C     IF THIS IS FIRST TIME SERIES, SEE IF THERE SHOULD BE A
C     BLANK LINE AFTER HEADING
         IF (I.NE.1.OR.IBLK.EQ.0) GOTO 180
         DO 170 J=1,IBLK
            IF (PO(J+12).GT.0.5) GO TO 170
            IF (ILINE.EQ.1) WRITE (IPR,150) ILINE
150   FORMAT (5X,4HLINE,I2,8H : BLANK)
            IF (ILINE.GT.1) WRITE (IPR,160) ILINE
160   FORMAT (09X,I2,8H : BLANK)
            ILINE=ILINE+1
170         CONTINUE
C     DETERMINE FORMAT AND UNITS
180      CRIT=PO(IPT+13)
         RUNTS=PO(IPT+16)
         IF (PO(IPT+14).GT.0.5)RUNTS=PO(IPT+15)
         IF (METRIC.LT.0) GO TO 200
            IF (METRIC.EQ.0) GO TO 190
               IF (PO(IPT+14).GT.0.5) GO TO 200
                  CRIT=(PO(IPT+13)-PO(IPT+18))/PO(IPT+17)
                 RUNTS=PO(IPT+15)
                 GOTO 200
190           IF (PO(IPT+14).LT.0.5) GO TO 200
            CRIT=PO(IPT+17)*PO(IPT+13)+PO(IPT+18)
            RUNTS=PO(IPT+16)
C      START OF CHANGES TO FIX MAINTENANCE REQUEST 1496
200      RMSGF=RYES
         IF (PO(IPT+6).GE.0.05) RMSGF=RNO
C      END OF CHANGES TO FIX MAINTENANCE REQUEST 1496
         I8=PO(IPT+8)
         I10=PO(IPT+10)
         I9=PO(IPT+9)
         I1=IPT+1
         I2=IPT+3
         I4=PO(IPT+4)
         I5=PO(IPT+5)
         IF (IPOPT.EQ.0) WRITE (IPR,210) ILINE,(PO(J),J=I1,I2),
     1      I4,I5,PO(IPT+16),PO(IPT+15),I8,I10,I9,RMSGF,PO(IPT+7),
     2      PO(IPT+17),PO(IPT+18)
210   FORMAT (9X,I2,3H : ,2A4,1X,A4,I5,I4,1X,A4,1X,A4,I7,I5,I6,1X,A4,
     1 1X,A4,2F11.3,15H   ALWAYS PRINT)
         IF (IPOPT.EQ.1) WRITE (IPR,220) ILINE,(PO(IPT+J),J=1,3),
     1      I4,I5,PO(IPT+16),PO(IPT+15),I8,I10,I9,RMSGF,PO(IPT+7),
     2      PO(IPT+17),PO(IPT+18),CRIT,RUNTS
220   FORMAT (9X,I2,3H : ,2A4,1X,A4,I5,I4,1X,A4,1X,A4,I7,I5,I6,1X,A4,
     1 1X,A4,2F11.3,10H   LIMITS>,F14.3,1X,A4)
         IF (IPOPT.EQ.2) WRITE (IPR,230) ILINE,(PO(IPT+J),J=1,3),
     1      I4,I5,PO(IPT+16),PO(IPT+15),I8,I10,I9,RMSGF,PO(IPT+7),
     2      PO(IPT+17),PO(IPT+18),CRIT,RUNTS
230   FORMAT (9X,I2,3H : ,2A4,1X,A4,I5,I4,1X,A4,1X,A4,I7,I5,I6,1X,A4,
     1 1X,A4,2F11.3,12H   CRITERIA>,F12.3,1X,A4)
      ILINE=ILINE+1
C     CHECK FOR BLANK LINES
         IF (IBLK.EQ.0) GOTO 250
            DO 240 J=1,IBLK
               ILN=INT(PO(12+J))
               IF (ILN.NE.I) GO TO 240
               WRITE (IPR,160) ILINE
               ILINE=ILINE+1
240         CONTINUE
250      CONTINUE
C
      IF (ITRACE.GE.1) WRITE (IODBUG,*) 'EXIT PRP27'
C
      RETURN
C
      END
