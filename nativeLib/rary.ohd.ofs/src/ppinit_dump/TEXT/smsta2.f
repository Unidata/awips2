C MEMBER SMSTA2
C  (from old member SMSTA)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 06/23/95.10:32:58 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE SMSTA2 (MOPTN,OPTN,LOPTN,NSTAN,NPCPN,NTEMP,NPE,NRRS,
     *   NUMINC,IMXSTA,STAINC,NCOMPL,INCMPL,IPSTA)
C
C DESC ROUTINE TO PRINT STATISTICS FOR STATIONS PROCESSED BY
C DESC DUMP STATION COMMAND.
C
      REAL XNOT/4HNOT /
      REAL XONLY/4HONLY/
      REAL*8 OPTN(MOPTN)
      REAL*8 XFMT1(5),XFMT2(5)
      REAL*8 XINC(4)/8HINCLUDIN,8HG INCOMP,8HLETE STA,8HTIONS). /
C
      DIMENSION STAINC(2,1)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_dump/RCS/smsta2.f,v $
     . $',                                                             '
     .$Id: smsta2.f,v 1.1 1995/09/17 19:13:13 dws Exp $
     . $' /
C    ===================================================================
C
C
C
C  SET TRACE LEVEL
      LTRACE=ISTRC('DUMP')
C
      IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,90)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('DUMP')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,100) LOPTN,NSTAN,NPCPN,NTEMP,NPE,
     *      NRRS,NUMINC,NCOMPL,INCMPL,IPSTA
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (IPSTA.EQ.0) GO TO 80
C
C  INITIALIZE FORMATS TO BLANK
      MAXFMT=40
      CALL UREPET (' ',XFMT1,MAXFMT)
      CALL UREPET (' ',XFMT2,MAXFMT)
      CALL SUBSTR ('(',1,1,XFMT1,1)
      CALL SUBSTR ('(',1,1,XFMT2,1)
C
C  SET PRINT FORMAT
      IF (INCMPL.EQ.-1) GO TO 10
         IF (INCMPL.EQ.0) GO TO 20
            CALL SUBSTR (XINC,1,32,XFMT1,2)
            CALL SUBSTR (XINC,1,32,XFMT2,2)
            GO TO 30
10       CALL SUBSTR (XONLY,1,4,XFMT1,2)
         CALL SUBSTR (XINC,11,22,XFMT1,7)
         CALL SUBSTR (XFMT1,1,MAXFMT,XFMT2,1)
         GO TO 30
20    CALL SUBSTR (XINC,1,32,XFMT1,2)
      CALL SUBSTR (XNOT,1,4,XFMT2,2)
      CALL SUBSTR (XINC,1,32,XFMT2,6)
C
30    IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,110) XFMT1
         CALL SULINE (IOSDBG,1)
         ENDIF
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,120) XFMT2
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      WRITE (LP,130)
      CALL SULINE (LP,1)
C
      IF (NUMINC.EQ.0) GO TO 50
C
C  PRINT INCOMPLETE STATIONS
      IF (ISLEFT(2+NUMINC).GT.0) CALL SUPAGE
      WRITE (LP,140)
      CALL SULINE (LP,2)
      DO 40 I=1,NUMINC
         WRITE (LP,150) I,STAINC(1,I),STAINC(2,I)
         CALL SULINE (LP,1)
40       CONTINUE
      IF (IMXSTA.GT.0) THEN
         WRITE (LP,160) IMXSTA
         CALL SULINE (LP,2)
         ENDIF
C
50    IF (INCMPL.EQ.2) GO TO 70
C
C  PRINT NUMBER OF STATIONS WITH EACH PARAMETER TYPE
      IF (LOPTN.GE.3.AND.LOPTN.LE.7) GO TO 60
         IF (NSTAN.GT.0) THEN
            WRITE (LP,180) NSTAN,OPTN(3),XFMT1
            CALL SULINE (LP,2)
            ENDIF
         IF (NPCPN.GT.0) THEN
            WRITE (LP,190) NPCPN,OPTN(4),XFMT2
            CALL SULINE (LP,2)
            ENDIF
         IF (NTEMP.GT.0) THEN
            WRITE (LP,190) NTEMP,OPTN(5),XFMT2
            CALL SULINE (LP,2)
            ENDIF
         IF (NPE.GT.0) THEN
            WRITE (LP,190) NPE,OPTN(6),XFMT2
            CALL SULINE (LP,2)
            ENDIF
         IF (NRRS.GT.0) THEN
            WRITE (LP,190) NRRS,OPTN(7),XFMT2
            CALL SULINE (LP,2)
            ENDIF
         IF (NSTAN.EQ.0) THEN
            WRITE (LP,170) OPTN(3)
            CALL SULINE (LP,2)
            ENDIF
         IF (NPCPN.EQ.0) THEN
            WRITE (LP,170) OPTN(4)
            CALL SULINE (LP,2)
            ENDIF
         IF (NTEMP.EQ.0) THEN
            WRITE (LP,170) OPTN(5)
            CALL SULINE (LP,2)
            ENDIF
         IF (NPE.EQ.0) THEN
            WRITE (LP,170) OPTN(6)
            CALL SULINE (LP,2)
            ENDIF
         IF (NRRS.EQ.0) THEN
            WRITE (LP,170) OPTN(7)
            CALL SULINE (LP,2)
            ENDIF
         GO TO 70
C
60    IF (LOPTN.EQ.3.AND.NSTAN.GT.0) THEN
         WRITE (LP,180) NSTAN,OPTN(3),XFMT1
         CALL SULINE (LP,2)
         ENDIF
      IF (LOPTN.EQ.4.AND.NPCPN.GT.0) THEN
         WRITE (LP,190) NPCPN,OPTN(4),XFMT2
         CALL SULINE (LP,2)
         ENDIF
      IF (LOPTN.EQ.5.AND.NTEMP.GT.0) THEN
         WRITE (LP,190) NTEMP,OPTN(5),XFMT2
         CALL SULINE (LP,2)
         ENDIF
      IF (LOPTN.EQ.6.AND.NPE.GT.0) THEN
         WRITE (LP,190) NPE,OPTN(6),XFMT2
         CALL SULINE (LP,2)
         ENDIF
      IF (LOPTN.EQ.7.AND.NRRS.GT.0) THEN
         WRITE (LP,190) NRRS,OPTN(7),XFMT2
         CALL SULINE (LP,2)
         ENDIF
      IF (LOPTN.EQ.3.AND.NSTAN.EQ.0) THEN
         WRITE (LP,170) OPTN(3)
         CALL SULINE (LP,2)
         ENDIF
      IF (LOPTN.EQ.4.AND.NPCPN.EQ.0) THEN
         WRITE (LP,170) OPTN(4)
         CALL SULINE (LP,2)
         ENDIF
      IF (LOPTN.EQ.5.AND.NTEMP.EQ.0) THEN
         WRITE (LP,170) OPTN(5)
         CALL SULINE (LP,2)
         ENDIF
      IF (LOPTN.EQ.6.AND.NPE.EQ.0) THEN
         WRITE (LP,170) OPTN(6)
         CALL SULINE (LP,2)
         ENDIF
      IF (LOPTN.EQ.7.AND.NRRS.EQ.0) THEN
         WRITE (LP,170) OPTN(7)
         CALL SULINE (LP,2)
         ENDIF
C
70    NSTAN=0
      NTEMP=0
      NPE=0
      NRRS=0
      NPCPN=0
      NCOMPL=0
      NUMINC=0
      IPSTA=0
C
80    IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,200)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
90    FORMAT (' *** ENTER SMSTA2')
100   FORMAT (' LOPTN=',I2,3X,'NSTAN=',I4,3X,'NPCPN=',I4,3X,
     *   'NTEMP=',I4,3X,'NPE=',I4,3X,'NRRS=',I4,3X,
     *   'NUMINC=',I3,3X,'NCOMPL=',I4,3X,'INCMPL=',I2,3X,
     *   'IPSTA=',I2)
110   FORMAT (' XFMT1=',5A8)
120   FORMAT (' XFMT2=',5A8)
130   FORMAT (' ')
140   FORMAT ('0*** NOTE - THE FOLLOWING STATIONS HAVE A STATUS OF ',
     *   'INCOMPLETE.')
150   FORMAT (T13,I4,2X,2A4)
160   FORMAT ('0*** NOTE - ',I4,' STATION IDENTIFIERS FOR INCOMPLETE ',
     *   'STATIONS COULD NOT BE STORED.')
170   FORMAT ('0*** NOTE -   NO STATIONS WITH ',A4,' PARAMETERS ',
     *   'WERE PROCESSED.')
180   FORMAT ('0*** NOTE - ',I4,' STATIONS WITH ',A4,' PARAMETERS ',
     *   'DEFINED WERE PROCESSED ',5A8)
190   FORMAT ('0*** NOTE - ',I4,' STATIONS WITH ',A4,' PARAMETERS ',
     *   'DEFINED WERE PROCESSED ',5A8)
200   FORMAT (' *** EXIT SMSTA2')
C
      END
