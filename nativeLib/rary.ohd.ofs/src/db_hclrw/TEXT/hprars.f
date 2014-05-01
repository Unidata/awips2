C MODULE HPRARS
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT ARGUMENT NAME AND VALUES FOR OPTION RECORDS.
C
      SUBROUTINE HPRARS (KBUF,IREC,ITYPE,IBUF,LIBUF)
C
C  ARGUMENT LIST:
C
C       NAME    TYPE  I/O   DIM   DESCRIPTION
C       ------  ----  ---   ---   -----------
C       KBUF      I    I     ?    INPUT BUFFER CONTAINING ARGUMENTS
C       IREC      I    I     1    RECORD NUMBER OF TECHNIQUE
C       ITYPE     I    I     1    TYPE OF TECHNIQUE
C
      CHARACTER*8 ARGNAM,LOGLVL
      CHARACTER*72 LBUF
      DIMENSION IBUF(LIBUF)
      DIMENSION KBUF(48)
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'udatas'
      INCLUDE 'hclcommon/hunits'
      INCLUDE 'hclcommon/hword3'
      INCLUDE 'hclcommon/hdflts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hprars.f,v $
     . $',                                                             '
     .$Id: hprars.f,v 1.3 2002/02/11 20:36:31 dws Exp $
     . $' /
C    ===================================================================
C
      DATA IQUES/4H?   /
      DATA IPLUS/4H+   /
      DATA IMINUS/4H-   /
C
C
      IF (IHCLTR.GT.0) WRITE (IOGDB,*) 'ENTER HPRARS'
C
      LLBUF=LEN(LBUF)/4
C
      CALL ULINE (LP,1)
      WRITE (LP,'(24X,''ARGUMENTS:'')')
C
C  GET THE TECHNIQUE RECORD
      IUNIT=KDEFNL
      IF (ITYPE.LT.0) IUNIT=KDEFNG
      CALL HGTRDN (IUNIT,IREC,IBUF,LIBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 220
      NUMARG=IBUF(10)
C
C  MATCH UP ARGUMENT POINTERS TO GET ARG NAME
      NUMARS=KBUF(1)
      J=2
      DO 210 I=1,NUMARS
         IARPT=KBUF(J)
         M=0
         DO 20 K=1,NUMARG
            IF (IBUF(14+M).EQ.IARPT) GO TO 40
            M=M+4
20          CONTINUE
C        NO MATCH FOUND
            CALL ULINE (LP,2)
            WRITE (LP,30) IARPT,IBUF
30        FORMAT ('0**ERROR** IN HPRARS - SYSTEM ERROR LOOKING FOR ',
     *           I4,' IN TECHNIQUE RECORD.' / 1X,I3,3A4,(1X,20I5))
            GO TO 240
C      FOUND THE POINTER - MOVE THE NAME AND USE TYPE TO SET FORMAT
40        CALL UMEMOV (IBUF(11+M),ARGNAM,2)
          NARWDS=KBUF(J+1)
          IARTP=IBUF(13+M)
          IF (IARTP.LT.0) IARTP=3
          GO TO (50,70,90,110,130),IARTP
C     INTEGER VALUE
50       INT=KBUF(J+2)
         CALL ULINE (LP,1)
         WRITE (LP,60) ARGNAM,INT
60       FORMAT (26X,A,' = ',I7)
         GO TO 200
C     REAL VALUE
70       CALL USWITC (KBUF(J+2),REAL)
         CALL ULINE (LP,1)
         WRITE (LP,80) ARGNAM,REAL
80       FORMAT (26X,A,' = ',F9.3)
         GO TO 200
C     CHARACTER STRING
90       CALL HGTSTR (LLBUF,KBUF(J+2),LBUF,LENGTH,ISTAT)
         IF (ISTAT.NE.0) GO TO 220
            CALL ULINE (LP,1)
            WRITE (LP,100) ARGNAM,LBUF(1:LENGTH)
100         FORMAT (26X,A,' = ',A)
            GO TO 200
C     LOGICAL VALUE
110      CALL UMEMOV (LTRUE,LOGLVL,2)
         IF (KBUF(J+2).EQ.0) CALL UMEMOV (LFALSE,LOGLVL,2)
         CALL ULINE (LP,1)
         WRITE (LP,120) ARGNAM,LOGLVL
120       FORMAT (26X,A,' = ',A)
          GO TO 200
C     DATE VALUE
130      CALL HSETHR (KBUF(J+2))
         IMO=KBUF(J+3)
         IDAY=KBUF(J+4)
         IYR=KBUF(J+5)
         IHR=KBUF(J+6)
         ICODE=KBUF(J+7)
         IMIN=KBUF(J+8)
         ITIME=IHR*100+IMIN
C     CHECK FOR ASTERISK
         IF (IMO.EQ.IASTR) THEN
            IF (IDAY.EQ.0) THEN
               CALL ULINE (LP,1)
               WRITE (LP,140) ARGNAM,IASTR,ITIME,ICODE
140            FORMAT (26X,A,' = ',A1,'/',I4.4,A4)
               GO TO 200
               ENDIF
            IF (IDAY.LT.0) THEN
               IDAY=-IDAY
               CALL ULINE (LP,1)
               WRITE (LP,150) ARGNAM,IDAY,ITIME,ICODE
150            FORMAT (26X,A,' = ','*-',I2.2,'/',I4.4,A4)
               GO TO 200
               ENDIF
            CALL ULINE (LP,1)
            WRITE (LP,160) ARGNAM,IDAY,ITIME,ICODE
160         FORMAT (26X,A,' = ','*+',I2.2,'/',I4.4,A4)
            GO TO 200
            ENDIF
         ITIME=IHR*100+IMIN
C     CHECK FOR ASTERISK
         IF (IMO.EQ.IPOUND) THEN
            IPSIGN=IQUES
            IF (IDAY.EQ.0) IPSIGN=IMINUS
            IF (IDAY.EQ.1) IPSIGN=IPLUS
            IF (IHR.EQ.0) THEN
               INTVL=INTDFL
               ELSE
                  INTVL=IHR
               ENDIF
            CALL ULINE (LP,1)
            WRITE (LP,170) ARGNAM,IPOUND,IPSIGN,INTVL
170         FORMAT (26X,A,' = ',A1,A1,I2.2)
            GO TO 200
            ENDIF
         IF (IMO.EQ.IPRCNT) THEN
            IPSIGN=IQUES
            IF (IDAY.EQ.0) IPSIGN=IMINUS
            IF (IDAY.EQ.1) IPSIGN=IPLUS
            CALL ULINE (LP,1)
            WRITE (LP,180) ARGNAM,IPRCNT,IPSIGN,IHR,ICODE
180         FORMAT (26X,A,' = ',A1,A1,I2.2,A4)
            GO TO 200
            ENDIF
         CALL ULINE (LP,1)
         WRITE (LP,190) ARGNAM,IMO,IDAY,IYR,ITIME,ICODE,' <<=='
190      FORMAT (26X,A,' = ',I2.2,'/',I2.2,'/',I4.4,' - ',I4.4,A4,A)
200      J=J+NARWDS+2
210      CONTINUE
C
      GO TO 240
C
220   CALL ULINE (LP,3)
      WRITE (LP,230) KBUF
230   FORMAT ('0**ERROR** IN HPRARS - SYSTEM ERROR. KBUF=' / (1X,20I5))
C
240   IF (IHCLTR.GT.0) WRITE (IOGDB,*) 'EXIT HPRARS'
C
      RETURN
C
      END
