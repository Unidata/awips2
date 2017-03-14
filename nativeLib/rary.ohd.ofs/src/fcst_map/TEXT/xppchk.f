C MODULE XPPCHK
C-----------------------------------------------------------------------
C
      SUBROUTINE XPPCHK (PP24,PT24,PPVR,PTVR,MDR6,CHAR,PARM,
     1 LWORK,IERR)
C.......................................
C     THIS ROUTINE PERFORMS PRELIMINARY CHECKS ON THE
C       PRECIPITATION DATA.  THESE ARE DONE IN TWO PASSES.
C       PASS ONE:--
C          1.  APPLY CORRECTION FACTORS TO ALL NON ZERO AMOUNTS.
C          2.  SET  MISSING DATA TO ZERO FOR STATIONS WHERE THIS
C              ACTION IS SPECIFIED.
C          3.  CONVERT  ALL LESS THAN 24-HOUR STATIONS
C              TO 6-HOURLY.
C          4.  SET 24-HOUR TOTAL TO SUM OF LESS THAN 24 HOUR
C              AMOUNTS WHEN THE 24-HOUR TOTAL IS MISSNG AND ALL
C              6-HOUR VALUES ARE GIVEN.
C
C       PASS TWO:-- ONLY IF NEEDED.
C          1.  IF MORE THAN ONE STATION IS AT THE SAME LOCATION,
C              ESTIMATE EACH ONE FROM THE OTHERS WHEN POSSIBLE
C          2.  SET MISSING REPORTS TO ZERO BASED ON MDR WHEN
C              POSSIBLE.
C.......................................
C     WRITTEN BY -- ERIC ANDERSON, HRL -- FEBRUARY 1983
C.......................................
C
      PARAMETER (LARRAY=500)
      DIMENSION ARRAY(LARRAY)
      CHARACTER STAID*8,DTYPE*4
      INTEGER*2 PP24(1),PT24(1),PPVR(1),PTVR(1),MDR6(1),MDRSIX(4)
      INTEGER*2 MSNG24,MSNG6,MSGMDR,MSNGSR,MDR24,TOT,T6,CHAR(1)
      DIMENSION PARM(1)
C
C     COMMON BLOCKS
      INCLUDE 'common/ionum'
      COMMON/PUDBUG/IOPDBG,IPTRCE,NDBUG,PDBUG(20),IPALL
      COMMON/XSIZE/NMAP,NDUPL,NSLT24,NSLOT6,LDATA6,LMDR,LPPSR,
     1  MSNG24,MSNG6,MSGMDR,MSNGSR,NRECTP,MXTEMP,SMALL
      COMMON/XTIME/KZDA,KDA,KHR,LSTMO,KMO,KID,KYR,KIH,TZCODE,ISW,
     1  IUTMP,NSSR,IDAY
      COMMON/XMDR/MDRSTA,ICTMDR,MDRST6,ICMDR,NCMDR,IRMDR,NRMDR,NMDR,
     1  MDR,MDRNUV
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_map/RCS/xppchk.f,v $
     . $',                                                             '
     .$Id: xppchk.f,v 1.2 1999/04/23 21:13:46 page Exp $
     . $' /
C    ===================================================================
C
C     DATA STATEMENTS.
      DATA XPCK,APP24,APPVR/4HXPCK,4HPP24,4HPPVR/
C
C      
C.......................................
C     CHECK TRACE LEVEL
      IF(IPTRCE.GE.1) WRITE(IOPDBG,900)
  900 FORMAT(1H0,17H** XPPCHK ENTERED)
C.......................................
C     CHECK IF DEBUG IS ON.
      IBUG=0
      IF (IPBUG(XPCK).EQ.1) IBUG=1
C.......................................
C     INITIAL VALUES
      N6=KHR/6
      PSMALL=SMALL*N6/4.0
      NOMSG=1
      IERR=0
      IONEPT=0
      LP24=NSLT24*5
      LP6=NSLOT6*4
C.......................................
C.......................................
C     BEGIN LOOP THROUGH ALL STATIONS--PASS ONE
      DO 100 N=1,NSLT24
      I=(N-1)*5+1
      IF(PT24(I).EQ.0) GO TO 100
      J=PT24(I+3)
      IZ=0
      IF(J.GT.0) GO TO 101
      IZ=1
      J=-J
  101 IW=J/100
      IS=J-IW*100
      IF(IS.EQ.0) GO TO 102
      IF(ISW.EQ.1) GO TO 102
      ICF=IS*5
      GO TO 105
  102 ICF=IW*5
  105 J=PT24(I+2)
      IF(J.GT.0) THEN
         IF (J.GT.LP6) THEN
            STAID=' '
            DTYPE='PCPN'
            IPTR=PT24(I)
            CALL RPPREC (STAID,DTYPE,IPTR,LARRAY,ARRAY,NFILL,IPTRNX,
     *         IERR)
            WRITE (IPR,107) J,I,LP6,STAID
  107 FORMAT ('0**ERROR** IN XPPCHK - PPVR POINTER (',I5,') ',
     *   'FOUND IN PP24 SLOT ',I5,' ',
     *   'EXCEEDS THE MAXIMUM VALUE (',I5,') ',
     *   'FOR STATION ',A,'.')  
            CALL ERROR      
            GO TO 100
            ENDIF
         GO TO 150
         ENDIF
C.......................................
C     STATION ONLY HAS 24 HOUR DATA
      IF(PP24(N).EQ.MSNG24) GO TO 110
C
C     NON-MISSING VALUE - APPLY CORRECTION
      IF(ICF.EQ.100) GO TO 100
      PP24(N)=(PP24(N)*ICF+50)/100
      GO TO 100
C
C     MISSING VALUE - CHECK IF CAN SET TO ZERO
  110 IF(IZ.EQ.0) GO TO 120
      PP24(N)=0
      PT24(I+3)=PT24(I+3)-10000
      GO TO 100
  120 IF (PT24(I).LT.0) IONEPT=1
      NOMSG=0
      GO TO 100
C.......................................
C     STATION HAS LESS THAN 24 HOUR DATA.
  150 IDT=PTVR(J+2)
      ISET=0
      IF (IDT.EQ.0) THEN
         STAID=' '
         DTYPE='PCPN'
         IPTR=PT24(I)
         CALL RPPREC (STAID,DTYPE,IPTR,LARRAY,ARRAY,NFILL,IPTRNX,IERR)
         WRITE (IPR,151) STAID,J
  151 FORMAT ('0**ERROR** IN XPPCHK - DATA TIME INTERVAL IS ZERO ',
     *   'FOR STATION ',A,' ',
     *   'AT SLOT ',I5,' OF PPVR POINTER ARRAY.')        
         GO TO 100
         ENDIF
      NV=6/IDT
      L=PTVR(J+3)-1
C
C     CHECK FOR MISSING--SET MISSING TO ZERO IF SPECIFIED.
C     CONVERT TO 6 HOUR IF ONE OR THREE HOUR
C     (TIME INTERVAL IN POINTER IS NOT CHANGED)
      MSG=0
      TOT=0
      DO 160 K=1,N6
      T6=0
      L6=(K-1)*NV+L
      MSG6=0
      DO 165 M=1,NV
      IF(PPVR(L6+M).EQ.MSNG6) GO TO 166
  169 T6=T6+PPVR(L6+M)
      GO TO 165
  166 IF(IZ.EQ.0) GO TO 167
C
C     SET MISSING TO ZERO
      PPVR(L6+M)=0
      ISET=1
      GO TO 169
  167 MSG6=1
      GO TO 161
  165 CONTINUE
      IF(ICF.EQ.100) GO TO 162
      T6=(T6*ICF+50)/100
  162 TOT=TOT+T6
      PPVR(L+K)=T6
      GO TO 160
  161 MSG=1
      PPVR(L+K)=MSNG6
  160 CONTINUE
      IF(MSG.EQ.1) GO TO 180
      IF (ISET.EQ.0) GO TO 170
      SUM=TOT*0.01
      IF (SUM.GE.PSMALL) PT24(I+3)=PT24(I+3)-10000
C
C     NO MISSING SIX HOUR VALUES.
  170 IF(PP24(N).EQ.MSNG24) GO TO 171
      IF(ICF.EQ.100) GO TO 100
      PP24(N)=(PP24(N)*ICF+50)/100
      GO TO 100
  171 PP24(N)=TOT
      IF ((TOT.EQ.0).AND.(ISET.EQ.1)) PT24(I+3)=PT24(I+3)-10000
      GO TO 100
C
C     SOME MISSING SIX HOUR VALUES REMAIN.
 180  IF(PP24(N).EQ.MSNG24) GO TO 190
      IF(ICF.EQ.100) GO TO 190
      PP24(N)=(PP24(N)*ICF+50)/100
C
C     SOME MISSING DATA REMAINS--SET FIRST PTVR VALVE NEGATIVE.
 190  IF(PT24(I).LT.0) IONEPT=1
      PTVR(J)=-PTVR(J)
      NOMSG=0
 100  CONTINUE
C     END OF PASS ONE
C.......................................
C.......................................
C     CHECK IF PASS TWO NEEDED.
      IF(NOMSG.EQ.1) GO TO 99
      IF((MDRSTA.NE.0).AND.(MDR.NE.0)) GO TO 199
      IF(IONEPT.EQ.0) GO TO 99
  199 IF (IBUG.EQ.0) GO TO 201
      CALL PDMPDY(APP24,KZDA,MSNG24,PT24,LP24,PP24,NSLT24)
      CALL PDMPDY(APPVR,KZDA,MSNG6,PTVR,LP6,PPVR,LDATA6)
C.......................................
C.......................................
C     BEGIN PASS TWO THROUGH ALL STATIONS.
 201  DO 200 N=1,NSLT24
      I=(N-1)*5+1
      IREC=PT24(I)
      IF(IREC.EQ.0) GO TO 200
      J=PT24(I+2)
C.......................................
C     CHECK IF STATION HAS MISSING DATA.
      IF(PP24(N).EQ.MSNG24) GO TO 210
      IF(J.LE.0) GO TO 200
      IF(PTVR(J).GT.0) GO TO 200
C.......................................
C     MISSING DATA EXISTS.
 210  IF(J.GT.0) L=PTVR(J+3)-1
C.......................................
C     CHECK IF ANOTHER GAGE AT THE SAME LOCATION.
      IF(IREC.GT.0) GO TO 220
      CALL XONEPT(N,J,L,PP24,PT24,PPVR,PTVR,MSNG24,MSNG6,N6,
     1I,CHAR,IREC,PARM,LWORK,MSG,IERR)
      IF(IERR.EQ.1) GO TO 99
      IF(MSG.EQ.0) GO TO 200
C.......................................
C     MISSING DATA STILL EXISTS--SEE IF CAN BE SET TO ZERO
C       BASED ON MDR.
 220  IF((MDRSTA.EQ.0).OR.(MDR.EQ.0)) GO TO 200
      MDRBOX=PT24(I+4)
      IF(MDRBOX.EQ.0) GO TO 200
      CALL XPPMDR(MDRBOX,MDR6,KHR,MDR24,MDRSIX,MSNG,ISTAT)
      IF(ISTAT.EQ.1) GO TO 200
      IF(J.GT.0) GO TO 225
C     STATION WITH 24-HOUR DATA ONLY.
      IF((MDR24.EQ.MSNG24).OR.(MDR24.GT.0)) GO TO 200
      PP24(N)=0
      PT24(I+4)=-MDRBOX
      GO TO 200
C
C     STATION WITH LESS THAN 24 HOUR DATA.
 225  TOT=0
      MSG=0
      DO 230 K=1,N6
      IF(PPVR(L+K).NE.MSNG6) GO TO 226
      IF((MDRSIX(K).EQ.MSNG6).OR.(MDRSIX(K).GT.0)) GO TO 227
      PPVR(L+K)=0
 226  TOT=TOT+PPVR(L+K)
      GO TO 230
 227  MSG=1
 230  CONTINUE
      IF(MSG.EQ.1) GO TO 200
      PTVR(J)=-PTVR(J)
      SUM=TOT*0.01
      IF(SUM.GE.PSMALL) PT24(I+4)=-MDRBOX
      IF(PP24(N).NE.MSNG24) GO TO 200
      PP24(N)=TOT
      IF(TOT.EQ.0) PT24(I+4)=-PT24(I+4)
 200  CONTINUE
C     END OF PASS TWO
C.......................................
C.......................................
   99 IF(IPTRCE.GE.1) WRITE(IOPDBG,901)
  901 FORMAT(1H0,14H** EXIT XPPCHK)
      RETURN
      END
