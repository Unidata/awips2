C MODULE FFF2A
C-----------------------------------------------------------------------
C
C  ROUTINE TO CONVERT REAL VALUES TO CHARACTERS.
C
      SUBROUTINE FFF2A (IBUF,IPOS,IWIDTH,IDEC,NUM,XLOC)
C
      CHARACTER*1 IBUF(1)
      CHARACTER*3 CHAR1,CHAR2
      CHARACTER*10 FMT
      CHARACTER*100 TEMP
C
      REAL XLOC(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/fff2a.f,v $
     . $',                                                             '
     .$Id: fff2a.f,v 1.2 1999/04/22 14:09:10 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (NUM.LT.1 ) GO TO 50
      IF (IWIDTH.LT.1.OR.IWIDTH.GT.LEN(TEMP)) GO TO 50
C
      IF (IDEC.LT.0.OR.IDEC.GT.IWIDTH) IDEC=0
C
C  FILL FMT WITH PROPER VALUE BASED ON IWIDTH AND IDEC
      WRITE (CHAR1,'(I3)') IWIDTH
      WRITE (CHAR2,'(I3)') IDEC
      IF (IDEC.GT.0) FMT='(F'//CHAR1//'.'//CHAR2//')'
      IF (IDEC.EQ.0) FMT='(I'//CHAR1//')'
C
C  LOOP FOR EACH NUMBER
      IE=IPOS-1
      DO 40 I=1,NUM
         IS=IE+1
         IE=IE+IWIDTH
C     CHECK IF VALID REAL NUMBER         
         WRITE (TEMP,'(F99.0)',ERR=20,IOSTAT=IERR) XLOC(I)
         IF (IERR.NE.0.OR.TEMP.EQ.'NaN') GO TO 20
         IF (IDEC.GT.0) THEN
            WRITE (TEMP(1:IWIDTH),FMT,ERR=20) XLOC(I)
            ENDIF
         IF (IDEC.EQ.0) THEN
            WRITE (TEMP(1:IWIDTH),FMT,ERR=20) IFIX(XLOC(I))
            ENDIF
         J=1
         DO 10 K=IS,IE
            IBUF(K)=TEMP(J:J)
            J=J+1
10          CONTINUE
         IF (IBUF(IS).NE.'$'.AND.IBUF(IS).NE.'=') GO TO 40
C     ERROR CONVERTING VALUE
20       DO 30 J=IS,IE
            IBUF(J)='*'
30          CONTINUE
40       CONTINUE
C
50    RETURN
C
      END
