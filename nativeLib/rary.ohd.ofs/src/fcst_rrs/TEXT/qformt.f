C MODULE QFORMT
C-----------------------------------------------------------------------
C
C  ROUTINE QFORMT CREATES THE FORMAT USED TO PRINT A STRING CENTERED IN
C  THE LINE.
C
      SUBROUTINE QFORMT (STRNG,CARCTL,FORMT)
C
      CHARACTER*(*) STRNG,CARCTL,FORMT
      CHARACTER*3 CHAR3
C
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_rrs/RCS/qformt.f,v $
     . $',                                                             '
     .$Id: qformt.f,v 1.2 2000/12/19 16:00:26 jgofus Exp $
     . $' /
C    ===================================================================
C
C
C
C  GET NUMBER OF SPACES TO INDENT STRING
      INDENT=(132-LENSTR(STRNG))/2
      IBEG=1
      NCHAR=LEN(CHAR3)
      IPRERR=1
      CALL UFI2A (INDENT,CHAR3,IBEG,NCHAR,IPRERR,IPR,IERR)
      CALL ULEFTC (CHAR3,LENSTR(CHAR3),LCHAR)
C      
C  CHANGE SPECIAL CHARACTERS IN STRING
       DO 10 I=1,LENSTR(STRNG)
          IF (STRNG(I:I).EQ.'~') STRNG(I:I)=' '
10        CONTINUE
C
C  CREATE FORMAT TO PRINT STRING
      FORMT=' '
      CALL UCNCAT (FORMT,'(''',IERR)
      CALL UCNCAT (FORMT,CARCTL,IERR)
      CALL UCNCAT (FORMT,'''',IERR)
      CALL UCNCAT (FORMT,',',IERR)
      CALL UCNCAT (FORMT,CHAR3,IERR)
      CALL UCNCAT (FORMT,'X',IERR)
      CALL UCNCAT (FORMT,',',IERR)
      CALL UCNCAT (FORMT,'''',IERR)
      CALL UCNCAT (FORMT,STRNG,IERR)
      CALL UCNCAT (FORMT,''')',IERR)
C
C  CHANGE SPECIAL CHARACTERS IN FORMAT
       DO 20 I=1,LENSTR(FORMT)
          IF (FORMT(I:I).EQ.'~') FORMT(I:I)=' '
20        CONTINUE
C
      RETURN
C
      END
