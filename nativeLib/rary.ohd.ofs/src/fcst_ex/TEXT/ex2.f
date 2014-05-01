C$PRAGMA C (SORTINGMODS,GETTHISOP)
C MODULE EX2
C-----------------------------------------------------------------------
C
      SUBROUTINE EX2 (PO,CO,RO,Q,QT,C,opname,iuhgd)
C
C  THIS IS THE EXECUTION ROUTINE FOR THE UNIT HYDROGRAPH OPERATION.
C
C  WRITTEN BY LARRY BRAZIL - AUGUST 1979
C
c     UHG enhancement - added start date, end date for UHG mod
c     
      CHARACTER*8 RTNNAM,opname
      DIMENSION PO(1),CO(1),RO(1),Q(1),QT(1),C(1)
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fctime'
      INCLUDE 'common/fcary'
      INCLUDE 'common/moduhg'
      INCLUDE 'common/fprog'
      integer  PList(1000), index, TmpLen
      real TmpVal(20,100)
      integer TmpDates(2,20), TmpModLen(20)
      
      
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ex/RCS/ex2.f,v $
     . $',                                                             '
     .$Id: ex2.f,v 1.9 2005/09/28 19:40:30 xfan Exp $
     . $' /
C    ===================================================================
C
C
      RTNNAM='EX2'
C
      IF (ITRACE.GE.1) WRITE (IODBUG,*) 'ENTER ',RTNNAM
C
      LTRACE=1
      NUMOPT=2
      index = 0
      CALL FPRBUG (RTNNAM,LTRACE,NUMOPT,IBUG)
      IBUG=IFBUG('UHG ')

C  VALUES OF CONTROL VARIABLES
      IDTR=PO(16)
      IDTQ=PO(20)
      NV=PO(10)
      NRO=PO(21)
      NTIME=IDTR/IDTQ
      NPO=PO(9)
      IORD=PO(22)-1
      
C  SAVE CARRYOVER COUNTER
      IC=1
C
C  DEBUG OUTPUT - PRINT PO() AND CO()
      
      IF (IBUG.EQ.1) THEN
         WRITE (IODBUG,910) NPO,NRO
  910 FORMAT (' CONTENTS OF PO AND CO ARRAYS',5X,
     1 'NUMBER OF VALUES PO=',I3,2X,'CO=',I2)
         WRITE (IODBUG,911) (PO(I),I=1,NPO)
  911 FORMAT (1H0,15F10.3)
         IF (NRO.LE.0)WRITE(IODBUG,912)
  912 FORMAT ('0 NO CARRYOVER')
         IF (NRO.GT.0)WRITE (IODBUG,911) (CO(I),I=1,NRO)
         ENDIF
        
C
C  COMPUTE QT BEGINNING AND ENDING LOCATIONS
      IQT=(IDA-IDADAT)*24/IDTQ+IHR/IDTQ-IDTR/IDTQ+1
      LQT=(LDA-IDADAT)*24/IDTQ+LHR/IDTQ
       
C  INITIALIZE WORKING SPACE
      DO 110 I=IQT,LQT
         QT(I)=0.0
  110    CONTINUE
       
CC av sort UHG mods by start and end dates 

      if(mainum .ne. 1) go to 119 
ckwz added for r23-29..................
c......................................
      if(iuhgd .eq. 1) then
         call getThisOp(uhgopn, opname, UHGVAL, ndtuhg,jdtuhg,NVLUHG,
     1               TmpVAL, TmpLen,TmpDates,TmpModLen)
         call SortingMods(TmpDates,TmpLen,(IDARUN-1)*24+IHRRUN,
     1                 (LDARUN-1)*24+LHRRUN,IDTQ,PList)
      end if
      
c................................
ckwz
c      goto 119
C      
C  COMPUTE CARRYOVER AND ADD TO QT()
c
c  cfan HSD bug r24-61
c  Problem: When making a UHGCDATE mod there has been some odd behavior
c       noticed.  One is QINE raise has occured after the mod has ended
c       (15 days), another is observed timeseries has been altered.
c 
c  Solution: in QT(J), J should be between IQT and LQT. 
c
c
      IF (NRO.LE.0)GO TO 120
      DO M=1,NRO
          KRO=NV-((NRO+1-M)*IDTR/IDTQ)

          if(iuhgd .eq. 1) then
            if (PList(1).EQ.0) then
              DO 114 I=1,KRO
                L=NV+PO(22)-I
                J=IQT+KRO-I
                IF (J.GT.LQT) GO TO 114                
                IF (J.LT.IQT) GO TO 114         !cfan HSD bug r24-61
                QT(J)=CO(M)*PO(L)+QT(J)
 114          CONTINUE
             else 
ckwz added else for the ungcnhg enhancement. 6/6/03
cav 4/21/04 added for UHGCDATE calculation
                
                 do 113 I=1,TmpModLen(PList(1))
                   J=IQT+KRO-I
                   IF (J.GT.LQT) GO TO 113
                   IF (J.LT.IQT) GO TO 113      !cfan HSD bug r24-61
                   QT(J)=CO(M)*TmpVAL(PList(1),TmpModLen(PList(1))-I+1)+
     1               QT(J)

  113            continue
              end if 
          else
              DO 714 I=1,KRO
                L=NV+PO(22)-I
                J=IQT+KRO-I
                IF (J.GT.LQT) GO TO 714
                IF (J.LT.IQT) GO TO 714         !cfan HSD bug r24-61
                QT(J)=CO(M)*PO(L)+QT(J)
 714          continue
          end if
      END DO
      
C
      go to 120
c  COMPUTE CARRYOVER AND ADD TO QT()
  119 IF (NRO.LE.0)GO TO 120
         DO 116 M=1,NRO
            KRO=NV-((NRO+1-M)*IDTR/IDTQ)
            DO 115 I=1,KRO
               L=NV+PO(22)-I
               J=IQT+KRO-I
               IF (J.GT.LQT) GO TO 115
               QT(J)=CO(M)*PO(L)+QT(J)
  115          CONTINUE
  116       CONTINUE

C  INITIALIZE DAY AND HOUR
  120 KDA=IDA
      KHR=IHR
CC av sort UHG mods by start and end dates 
CCkwz moved to before COMPUTE CARRYOVER AND ADD TO QT()
Ckwz      if(mainum .ne. 1) go to 213 
Ckwz         call SortingMods(JDTUHG,NDTUHG,(IDARUN-1)*24+IHRRUN+IDTQ,
Ckwz     1 (LDARUN-1)*24+LHRRUN,IDTQ,PList,uhgopn,opname)
      
      index=1

  200 CONTINUE
      if(mainum .ne. 1) go to 213
cav ...........................
      if(iuhgd .eq. 1) then
         index=index+1
      end if
cav ...........................
C
C  BEGIN COMPUTATIONAL TIME INTERVAL LOOP
      K=(KDA-IDADAT)*24/IDTR+KHR/IDTR
      K1=(K-1)*IDTR/IDTQ
      K2=K*IDTR/IDTQ
C
C  CONVERT RUNOFF TO DISCHARGE AND STORE IN QT()
C PList(index) returns array of values 0,1,2..up to number of uhg mods
C value = 0 use base uhg
c       = 1 use first uhg mod
c       = 2 use second uhg mod and so on  
c 
cav 12-20-02
cav ........................... 
      if(iuhgd .eq. 1) then     
        if (PList(index).EQ.0) then
           DO I=1,NV
             J=K1+I
             IF (J.GT.LQT) GO TO 211
             QT(J)=RO(K)*PO(IORD+I)+QT(J)
           END DO
cckwz added the else part for the uhgch enhancement. 12-10-02
        else
cav ..handle uhgcdate ...............
           DO I=1,TmpModLen(PList(index))
              J=K1+I
              IF (J.GT.LQT) GO TO 211
              QT(J)=RO(K)*TmpVAL(PList(index),I)+QT(J)
           END DO
        end if
      else
          DO I=1,NV
             J=K1+I
             IF (J.GT.LQT) GO TO 211
             QT(J)=RO(K)*PO(IORD+I)+QT(J)

          END DO
      end if      
      go to 211
C  BEGIN COMPUTATIONAL TIME INTERVAL LOOP
  213 K=(KDA-IDADAT)*24/IDTR+KHR/IDTR
      K1=(K-1)*IDTR/IDTQ
      K2=K*IDTR/IDTQ
      
      DO 215 I=1,NV
         J=K1+I
         IF (J.GT.LQT) GO TO 211
         QT(J)=RO(K)*PO(IORD+I)+QT(J)         
  215 CONTINUE
      
C
C  CHECK IF CARRYOVER IS TO BE SAVED
211   IF (NRO.LE.0) GO TO 250
      IF (IFILLC.EQ.0) GO TO 250
      IF (NCSTOR.EQ.0) GO TO 250
      IF (IC.GT.NCSTOR) GO TO 250
      IF (KDA.EQ.ICDAY(IC).AND.KHR.EQ.ICHOUR(IC)) GO TO 222
      GO TO 250
C
C  SAVE CARRYOVER
  222 CALL FSAV2(K,PO(1),CO(1),C(1),RO(1))
      CALL FCWTCO(KDA,KHR,C,NRO)
      IF (IBUG.EQ.1) THEN
         WRITE(IODBUG,940) ICDAY(IC),ICHOUR(IC)
  940 FORMAT (' CARRYOVER VALUES FOR DAY',I5,' HOUR',I3)
         WRITE(IODBUG,920) (C(I),I=1,NRO)
         ENDIF
      IC=IC+1
C
C  TRANSFER QT() VALUES TO Q()
250   BASE=PO(24)
      IF (BASE.LE.0.01) BASE=0.0
      DO 260 I=1,NTIME
         J=K1+I
         Q(J)=Q(J)+QT(J)+BASE
  260    CONTINUE
C
C  CHECK FOR LAST DAY AND HOUR
      IF (KDA.EQ.LDA.AND.KHR.EQ.LHR) GO TO 280
C
C  INCREMENT TIME STEP
      KHR=KHR+IDTR
      IF (KHR.LE.24) GO TO 200
      KHR=IDTR
      KDA=KDA+1
      GO TO 200
C
  280 IF (NRO.LE.0) GO TO 290
      IF (IFILLC.EQ.0) GO TO 290
      CALL FSAV2(K,PO(1),CO(1),C(1),RO(1))
      DO 285 I=1,NRO
         CO(I)=C(I)
  285    CONTINUE
C
C  DEBUG OUTPUT - PRINT RO() AND Q() AND CO()
290   IF (IBUG.EQ.1) THEN
         ITIR=(IDA-IDADAT)*24/IDTR+IHR/IDTR
         LTIR=(LDA-IDADAT)*24/IDTR+LHR/IDTR
         WRITE(IODBUG,930)
  930 FORMAT (' RUNOFF VALUES:')
         WRITE (IODBUG,920) (RO(I),I=ITIR,LTIR)
  920 FORMAT (1H0,15F8.3)
         WRITE(IODBUG,931)
  931 FORMAT (' DISCHARGE VALUES:')
         WRITE (IODBUG,920) (Q(I),I=IQT,LQT)
         IF (NRO.GT.0) THEN
           WRITE(IODBUG,960)
  960 FORMAT (' CARRYOVER VALUES:')
           WRITE(IODBUG,920) (CO(I),I=1,NRO)
           ENDIF
        ENDIF
C

      IF (ITRACE.GE.1) WRITE (IODBUG,*) 'EXIT ',RTNNAM
C
      RETURN
C
      END
