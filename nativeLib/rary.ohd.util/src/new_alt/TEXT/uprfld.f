C MODULE UPRFLD
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT VALUES RETURNED BY ROUTINE TO GET NEXT FIELD USING
C  FREE FORMAT.
C
      SUBROUTINE UPRFLD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *   LCHAR,CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,ISTAT)
C
      DIMENSION CHAR(1)
C
      INCLUDE 'uiox'
      INCLUDE 'ucmdbx'
      INCLUDE 'ufreex'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/new_alt/RCS/uprfld.f,v $
     . $',                                                             '
     .$Id: uprfld.f,v 1.2 1998/07/02 19:45:49 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) '*** ENTER UPRFLD'
         ENDIF
C
      CALL ULINE (LP,1)
      WRITE (LP,10) NFLD,ISTRT,LENGTH,ITYPE,INTEGR,REAL
      NCHAR=LCHAR
      IF (NCHAR.LT.0) NCHAR=-NCHAR/4
      CALL ULINE (LP,1)
      WRITE (LP,20) LCHAR,(CHAR(I),I=1,NCHAR)
      CALL ULINE (LP,1)
      WRITE (LP,30) NREP,LLPAR,LRPAR,LASK,LATSGN,LAMPS,
     *   LEQUAL,ISTAT,NRDCRD
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) '*** EXIT UPRFLD'
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
10    FORMAT (' NFLD=',I2,3X,'ISTRT=',I2,3X,'LENGTH=',I2,3X,
     *   'ITYPE=',I2,3X,'INTEGR=',I11,3X,'REAL=',G15.5)
20    FORMAT (' LCHAR=',I3,3X,'CHAR=',20A4)
30    FORMAT (' NREP=',I3,3X,' LLPAR=',I2,3X,'LRPAR=',I2,3X,
     *   'LASK=',I2,3X,'LATSGN=',I2,3X,'LAMPS=',I2,3X,
     *   'LEQUAL=',I2,3X,'ISTAT=',I2,3X,'NRDCRD=',I3)
C
      END
