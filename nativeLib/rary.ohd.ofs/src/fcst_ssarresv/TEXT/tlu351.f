C MEMBER TLU351
C DESC TABLE-LOOKUP FOR C2 TABLE. REWRITTEN 11/20/89 TO RETURN 
C
C@PROCESS LVL(77)
C
      SUBROUTINE TLU351 (E1,E2,NSCI,T,Q,KE) 
C
CC  KSH CHANGE START
CC  NOTE: THIS CODE IS FROM SSARR ROUTINE TLU3 (E1,E2,NSCI,T,Q,KE) 
CC      THE NAME OF ROUTINE STLU3L HAS BEEN CHANGED TO LU3L51
CC  KSH CHANGE END
C
C     TABLE-LOOKUP FOR C2 TABLE. REWRITTEN 11/20/89 TO RETURN 
C     Q=0 AND KE=NSCI IF ARGUMENTS ARE OUTSIDE DEFINED SURFACE. 
C 
C     GIVEN TWO INDEPENDENT VARIABLES OF AT-SITE ELEVATION, AND CONTROL 
C  ELEVATION OR FLOW, INTERPOLATE THE CORRESPONDING FLOW. 
C     E1-AT SITE ELEVATION 
C     E2-CONTROL VALUE 
C     T(1)-TABLE CONTAINING Q VS.E1,E2 FOR EACH POINT. 
C  THE TABLE IS TERMINATED BY A Q .lt. -99999. 
C     NSCI-SCAN INDEX,INDEX TO TABLE WHERE PREVIOUS REFERENCE TO THIS 
C  TABLE FOUND ARGUMENT,INPUT EQUAL ZERO ON FIRST REFERENCE TO 
C  FUNCTION. MUST NOT BE CHANGED BY CALLING PROGRAM (EXCEPT TO RESET 
C  TO ZERO) 
C     KE-RETURNED=0 IF ALL WELL. KE=n IF ARGUMENT IS OFF DEFINED 
C  SURFACE, WHERE n IS POINT LAST SEARCHED. 
C     NWP - NUMBER OF WORDS PER POINT 
C
CC  KSH CHANGE START
CC      INCLUDE 'common/job'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/sarr51'
CC  KSH CHANGE END
C
      DIMENSION T(*) 
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ssarresv/RCS/tlu351.f,v $
     . $',                                                             '
     .$Id: tlu351.f,v 1.1 1996/03/21 13:43:13 page Exp $
     . $' /
C    ===================================================================
C
      NWP=3 
      KE=0 
C       SWITCH ISWD SHOWS DIRECTION OF SEARCH. 
      ISWD = 0 
C       MAKE SURE THAN INDEX (N) REFERENCES A VALID POINT 
      N = IABS(NSCI/NWP) 
      N = N*NWP + 1 
C       FIND BRACKETING POINTS ON INITIAL LINE, TO SEE IF WE MUST 
C       GO FORWARD OR BACKWARD. 
      CALL LU3L51(ISWD,E1,E2,T,N,E1N,QN,KE) 
C
CC  KSH CHANGE START
CC      IF(IRTRC.NE.0) 
CC     1 write(6,
CC  KSH CHANGE END
C
      IF(IBUG.GE.2)
     1 write(IODBUG,
     &  '('' e1,e2,n,e1n,qn,iswd'',2f11.3,i5,f11.3,f10.1,i5)') 
     2             e1,e2,n,e1n,qn,iswd 
      IF(KE.NE.0) GO TO 100 
      IF(E1.EQ.E1N) GO TO 80 
      IF(E1.LT.E1N) THEN 
C       ARGUMENT LIES BELOW LINE, SO BACKUP 
             ISWD=-1 
10           E1U=E1N 
             QU=QN 
20           IF(N.LE.1) GO TO 100 
             N=N-NWP 
             IF(T(N).EQ.QN) GO TO 20 
             CALL LU3L51(ISWD,E1,E2,T,N,E1N,QN,KE) 
C
CC  KSH CHANGE START
CC             IF(irtrc.ne.0) 
CC     1 write(6,
CC  KSH CHANGE END
C
             IF(IBUG.GE.2)
     1 write(IODBUG,
     &  '('' iswd,e1,e2,n,e1n,qn'',i2,2f11.3,i5,f11.3,f10.1)') 
     2             iswd,e1,e2,n,e1n,qn 
C
CC  KSH CHANGE START
CC             IF(KE.NE.0) GO TO 100 
CC  KSH CHANGE END
C
             IF(E1.EQ.E1N) GO TO 80 
             IF(E1.LT.E1N) GO TO 10 
C       FOUND LINE BELOW ARGUMENT. INTERPOLATE 
             IF(E1U.EQ.E1N) GO TO 90 
             Q=QN + (QU-QN) * (E1-E1N)/(E1U-E1N) 
             NSCI=N 
             RETURN 
      ELSE 
C      ARGUMENT LIES ABOVE LINE, SO GO FORWARD IN TABLE. 
             ISWD=1 
30           E1L=E1N 
             QL=QN 
             NL=N 
40           IF(T(N+NWP).LT. -99999.) GO TO 100 
             N=N+NWP 
             IF(T(N).EQ.QN) GO TO 40 
             CALL LU3L51(ISWD,E1,E2,T,N,E1N,QN,KE) 
CC
CC  KSH CHANGE START
CC             if(irtrc.ne.0) 
CC     1 write(6,
             if(IBUG.GE.2)
     1 write(IODBUG,
     &  '('' iswd,e1,e2,n,e1n,qn'',i2,2f11.3,i5,f11.3,f10.1)') 
     2             iswd,e1,e2,n,e1n,qn 
CC             IF(KE.NE.0) GO TO 100 
CC  KSH CHANGE END
C
             IF(E1.EQ.E1N) GO TO 80 
             IF(E1.GT.E1N) GO TO 30 
C       FOUND LINE ABOVE ARGUMENT. INTERPOLATE 
             IF(E1L.EQ.E1N) GO TO 90 
             Q=QL + (QN-QL) * (E1-E1L)/(E1N-E1L) 
             NSCI=NL 
             RETURN 
      ENDIF 
C       ARGUMENT LIES ON LINE CONTAINING POINT T(N) 
80    NSCI=N 
      Q=QN 
      RETURN 
CC
CC  KSH CHANGE START
90    CONTINUE
CC90    WRITE(6,'('' STLU3 ERROR - FLAT SPOT IN'', 
CC     1      '' C2 TABLE.  ARG1, ARG2, NSCI ='',2F11.3,I5/ 
CC     2      '' 1ST 2 POINTS IN TABLE:'',6F11.3)') E1,E2,N, 
CC     3      (T(I),I=1,6) 
CC       ARGUMENT IS OFF EDGE OF SURFACE 
CC100   NSCI=1 
CC      KE=1 + N/NWP 
CC      Q=0. 
C      COULD NOT FIND BRACKETTING POINTS
C      USE LOWER MOST OR UPPER MOST VALUES AS SOLUTION
 100  KE=1
      NSCI=N
      Q=T(N)
CC  KSH CHANGE END
CC
      RETURN
      END
