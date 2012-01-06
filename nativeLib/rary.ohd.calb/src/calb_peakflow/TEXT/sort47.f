C MODULE SORT47
C
C  THIS ROUTINE SORTS THE OBSERVED AND SIMULATED PEAK FLOW DATA BY
C  ORDER OF MAGNITUDE AND FINDS THE MAXIMUM AND MINIMUM VALUES IN THE
C  DATA SET FOR THE PURPOSE OF GRAPHING SIM VS OBS PEAK FLOWS.
cbf
cbf routine modified by Bryce Finnerty December 1997, to use new
cbf po array structure and remove the graph options.
cbf
C
C  ROUTINE SORT47 WAS FIRTS WRITTEN BY:
C    BRYCE FINNERTY, HYDROLOGIC RESEARCH LAB, DECEMBER 1994.
C
      SUBROUTINE SORT47(PO,CO,WORK,SWORK,ISTAT)
C
      DIMENSION PO(*),CO(*),WORK(*),SWORK(*),SNAME(2)
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/calb_peakflow/RCS/sort47.f,v $
     . $',                                                             '
     .$Id: sort47.f,v 1.3 1998/04/07 13:34:41 page Exp $
     . $' /
C    ===================================================================
C
C
      DATA SNAME /4HSORT,4H47  /
C
C
      NP=PO(14)
C
C  TRACE LEVEL FOR ROUTINE=1, DEBUG SWITCH=IBUG
      CALL FPRBUG(SNAME,1,47,IBUG)
C  SORT OBSERVED PEAKS AND THE RESULTING SIMULATED PEAK BY MAGNITUDE
C
c      IBUG=0
c      IF (IBUG.GT.0) THEN
c        WRITE(IODBUG,550)
c        WRITE(IODBUG,551) (PO(I),I=1,24)
c      END IF
c 550  FORMAT('** DEBUG OUTPUT - PO ARRAY IN THE SORT47 ROUTINE ',
c     &' BEFORE THE USORT2 ROUTINE IS CALLED  **')
c 551  FORMAT(10X,I5,2X,5A4,2X,2A4,2X,A4,I5,2A4,7I5,9F7.2)
C
      JP=NP
      CALL USORT2(20,JP,1,1,WORK,SWORK,0,ISTAT)
C
c     IBUG=1
      IENDAT=NP*20
      IF (IBUG.GT.0) THEN
        WRITE(IODBUG,260)
        WRITE(IODBUG,270) (SWORK(K),K=1,IENDAT)
  260   FORMAT(10X,'** OBSERVED swork() PEAK DATA SORTED BY MAGNITUDE ',
     &  'after usort2**')
  270   FORMAT(10X,5F10.1)
      END IF
C
C
c      IBUG=0
c      IF (IBUG.GT.0) THEN
c        WRITE(IODBUG,510)
c        WRITE(IODBUG,512) (PO(I),I=1,24)
c      END IF
c 510  FORMAT('** DEBUG OUTPUT - PO ARRAY IN THE SORT47 ROUTINE AFTER',
c     &' USORT2 IS CALLED **')
c 512  FORMAT(10X,I5,2X,5A4,2X,2A4,2X,A4,I5,2A4,7I5,9F7.2)
C
C  WRITE SWORK ARRAY IN DESCENDING ORDER TO THE WORK ARRAY AND FIND
C  THE MAXIMUM AND MINIMUM VALUES OF THE COMBINED SIMULATED AND
C  OBSERVED DATA SETS.
C
      XMAX=-9999.
      XMIN=9999999.
      NPPP=NP
      DO 300 i=1,NP
        DO 301 J=1,20
          indwrk=(i-1)*20+J
          indswk=(NPPP-1)*20+J
	  if (ibug .ge. 1) then
            WRITE(IODBUG,244)indwrk,indswk
 244        format('indwrk=',i6,' indswk=',i6)
	  end if
          WORK(indwrk)=0.01
          WORK(indwrk)=SWORK(indswk)
c          IF ((J.EQ.1).OR.(J.EQ.11)) THEN
c            XPKFLO=WORK((I-1)*20+J)
c            IF ((XPKFLO.GT.0).AND.(XPKFLO.LT.XMIN)) XMIN=XPKFLO
c            IF ((XPKFLO.GT.0).AND.(XPKFLO.GT.XMAX)) XMAX=XPKFLO
c          END IF
  301   CONTINUE
        npppdu=nppp-1
        NPPP=npppdu
	if (ibug .ge. 1) then
          write(iodbug,243)i,nppp
  243     format('i=',i5,'  nppp=',i5)
	end if
  300 CONTINUE
C
      CO(4)=XMAX
      CO(5)=XMIN
C
      IENDAT=NP*20
      IF (IBUG.GT.0) THEN
        WRITE(IODBUG,240)
        WRITE(IODBUG,250) (WORK(K),K=1,IENDAT)
  240   FORMAT(10X,'** work() PEAK DATA SORTED BY MAGNITUDE after',
     &  ' ascending order**')
  250   FORMAT(10X,5F10.1)
      END IF
c
      IF (IBUG.GT.0) THEN
        WRITE(IODBUG,280)
        WRITE(IODBUG,290) (SWORK(K),K=1,IENDAT)
  280   FORMAT(10X,'** swork() PEAK DATA SORTED BY MAGNITUDE after',
     &  ' ascending order**')
  290   FORMAT(10X,5F10.1)
      END IF
C
C
c      IBUG=0
c      IF (IBUG.GT.0) THEN
c        WRITE(IODBUG,514)
c        WRITE(IODBUG,516) (PO(I),I=1,24)
c      END IF
c 514  FORMAT('** DEBUG OUTPUT - PO ARRAY IN THE SORT47 ROUTINE ',
c     &' JUST BEFORE EXITING THE ROUTINE  **')
c 516  FORMAT(10X,I5,2X,5A4,2X,2A4,2X,A4,I5,2A4,7I5,9F7.2)
C
C     ITRACE=1
      IF (ITRACE.GE.1) WRITE(IODBUG,910)
  910 FORMAT(10X,'** SORT47 EXITED **')
C
      RETURN
      END
