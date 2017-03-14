C MODULE PWDDC
C-----------------------------------------------------------------------
C
      SUBROUTINE PWDDC (DBEGD,DENDD,ICOND)
C
C   IN: DBEGD ..... BEGINNING DEFAULT DATE AS 8-CHAR WORD - CHAR
C   IN: DENDD ..... ENDING DEFAULT DATE AS AS 8-CHAR WORD - CHAR
C  I/O: ICOND ..... PGM COND: SET TO 1 FOR ERR - INT
C
      INCLUDE 'uio'
      INCLUDE 'ufreei'
C
      CHARACTER*4 BLANK/' '/
      CHARACTER*8 DBEGD,DENDD
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil_dmp24/RCS/pwddc.f,v $
     . $',                                                             '
     .$Id: pwddc.f,v 1.2 2000/07/21 19:54:31 page Exp $
     . $' /
C    ===================================================================
C
C
      CALL PVSUBB ('PWDDC   ',ICOND)
C
C  GET FIELDS
      CALL UFREE (1,72)
C
C  CHECK IF NO DATE EXISTS AFTER FIRST WORD
      IF (NFIELD.GT.1) GO TO 20
C
      N1 = 1
      IF (NFIELD.EQ.1) N1 = IFSTOP(1)+2
      N2 = N1+8
      N3 = N1+12
      N4 = N1+20
      ISTAT = 0
      IF (N1+24.GT.72) ISTAT = 1
      IF (ISTAT.EQ.0) CALL UNPAKS (DBEGD,IBUF(N1),2,73-N1,ISTAT)
      IF (ISTAT.EQ.0) CALL UNPAKS (BLANK,IBUF(N2),1,73-N2,ISTAT)
      IF (ISTAT.EQ.0) CALL UNPAKS (DENDD,IBUF(N3),2,73-N3,ISTAT)
      IF (ISTAT.EQ.0) CALL UNPAKS (BLANK,IBUF(N4),1,73-N4,ISTAT)
      IF (ISTAT.NE.0) THEN
         WRITE (LP,10)
10    FORMAT ('0**ERROR** NO ROOM IN INPUT ARRSY FOR DEFAULT DATES.')
         ICOND=1
	 ELSE	    
            WRITE (LP,15) 'BEGINNING',DBEGD(1:LENSTR(DBEGD))
15    FORMAT ('0**NOTE** ',A,' DATE SET TO ',A,'.')
            IF (DENDD.NE.' ') THEN
	       WRITE (LP,15) 'ENDING',DENDD(1:LENSTR(DENDD))
	       ENDIF
	 ENDIF
C
      IF (ICOND.EQ.0) CALL UFREE (1,72)
C
20    CALL PVSUBE ('PWDDC   ',ICOND)
C
      RETURN
C
      END
