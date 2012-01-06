C MEMBER SBPDBG
C  (from old member SBDBUTIL)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/05/95.10:36:48 BY $WC20SV
C
C  ROUTINES NEEDED FOR PPINIT DEBUG OTHER THAN THOSE USED TO READ AND
C  PROCESS THE OPTIONS ON THE @DEBUG COMMAND.
C
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
      SUBROUTINE SBPDBG
C
C  ROUTINE TO PRINT DEBUG OPTIONS IN EFFECT
C
      INCLUDE 'uio'
      INCLUDE 'ucmdbx'
      INCLUDE 'udsi'
      INCLUDE 'udebug'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/sysbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/sbpdbg.f,v $
     . $',                                                             '
     .$Id: sbpdbg.f,v 1.1 1995/09/17 19:20:25 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISLEFT(5).GT.0) CALL SUPAGE
C
      WRITE (LP,50)
      CALL SULINE (LP,2)
C
      IF (ISLEFT(5).GT.0) CALL SUPAGE
C
C  PRINT DEBUG OPTIONS FOR NON-PPINIT
      WRITE (LP,60)
      CALL SULINE (LP,4)
      WRITE (LP,70) 'COMNSRCE',ICMTRC,ICMDBG,ICMPRU
      CALL SULINE (LP,1)
      WRITE (LP,70) 'UTIL',IUTLTR,IUTLDB,IOGDB
      CALL SULINE (LP,1)
      WRITE (LP,70) 'DERW',IDETR,IDEDB,IOGDB
      CALL SULINE (LP,1)
      WRITE (LP,70) 'HCLRW',IHCLTR,IHCLDB,IOGDB
      CALL SULINE (LP,1)
      WRITE (LP,70) 'PPPRW',IPDTR,IPDDB,IOGDB
      CALL SULINE (LP,1)
      WRITE (LP,70) 'PRDRW',IPRTR,IPRDB,IOGDB
      CALL SULINE (LP,1)
      WRITE (LP,70) 'PPPRW',IPPTR,IPPDB,IOGDB
      CALL SULINE (LP,1)
      WRITE (LP,70) 'FCRW-SYS',ITRACE,IALL,IODBUG
      CALL SULINE (LP,1)
      WRITE (LP,70) 'FCRW',ITRACE,IDBALL,IODBUG
      CALL SULINE (LP,1)
      WRITE (LP,80) 'NOBUG',NOBUG
      CALL SULINE (LP,1)
C
C
C  PRINT DEBUG OPTIONS FOR PPINIT
      WRITE (LP,90) IOSDBG
      CALL SULINE (LP,2)
      WRITE (LP,100) ISTRCE
      CALL SULINE (LP,2)
      WRITE (LP,110) ISDBGL
      CALL SULINE (LP,2)
      IF (ISDBUG.GT.0) WRITE (LP,120) ISDBUG
      IF (ISDBUG.GT.0) CALL SULINE (LP,2)
      IF (NSDBUG.GT.0) GO TO 10
         WRITE (LP,140) 'NONE'
         CALL SULINE (LP,2)
         GO TO 40
10    WRITE (LP,130) NSDBUG
      CALL SULINE (LP,2)
      IF (NSDBUG.EQ.0) GO TO 40
         IF (ISLEFT(5).GT.0) CALL SUPAGE
         WRITE (LP,150)
         CALL SULINE (LP,4)
         NUM=0
         DO 20 I=1,NSDBUG
            IF (SDBUG(I).EQ.' ') GO TO 20
               NUM=NUM+1
               WRITE (LP,160) NUM,SDBUG(I),ISLTRC(I),ISLDBG(I),
     *           SDRTN2(I)
               CALL SULINE (LP,1)
20          CONTINUE
         DO 30 I=1,NSDBUG
            IF (SDBUG2(I).EQ.' ') GO TO 30
               NUM=NUM+1
               WRITE (LP,170) NUM,SDBUG2(I),ISLTRC(I),ISLDBG(I),
     *           SDRTN2(I)
               CALL SULINE (LP,1)
30          CONTINUE
C
40    RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
50    FORMAT ('0- DEBUG OPTIONS IN EFFECT - ')
60    FORMAT ('0',12X,8(' '),5X,'TRACE LEVEL',5X,'DEBUG LEVEL',5X,
     *      'UNIT #' /
     *   26X,11('-'),5X,11('-'),5X,6('-'))
70    FORMAT (' ',13X,A,T31,I2,14X,I2 : 12X,I2.2)
80    FORMAT (' ',13X,A,T31,'NA',14X,I2)
90    FORMAT ('0',T5,'PPINIT  : PRINT UNIT=',I2)
100   FORMAT ('0',T15,'TRACE LEVEL=',I2)
110   FORMAT ('0',T15,'DEBUG LEVEL=',I2)
120   FORMAT ('0',T15,'ISDBUG=',I2)
130   FORMAT ('0',T15,I2,' CODES SPECIFIED')
140   FORMAT ('0',T15,'CODES : ',20(A4,2X))
150   FORMAT ('0',T15,'CODE    ',5X,'TRACE LEVEL',5X,
     *   'DEBUG LEVEL',5X,'AT ROUTINE' /
     *   ' ',T15,8('-'),5X,11('-'),5X,11('-'),5X,10('-'))
160   FORMAT (' ',T12,I2,1X,A4,10X,I5,11X,I5,10X,A8)
170   FORMAT (' ',T12,I2,1X,A8,6X,I5,11X,I5,10X,A8)
C
      END
