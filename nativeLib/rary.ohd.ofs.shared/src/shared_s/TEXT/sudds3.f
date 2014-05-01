C MODULE SUDDS3
C-----------------------------------------------------------------------
C
      SUBROUTINE SUDDS3 (DBNAME,NALLOC,MALLOC,NOTFND,IPOS,NUMWRN,
     *   LDEBUG,ISTAT)
C
      CHARACTER*(*) DBNAME
C
      DIMENSION NOTFND(*)
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/suddsx'
      INCLUDE 'scommon/supagx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/sudds3.f,v $
     . $',                                                             '
     .$Id: sudds3.f,v 1.2 2001/06/13 13:31:45 dws Exp $
     . $' /
C    ===================================================================
C
C
C  GET LENGTH OF DATA BASE NAME
      CALL ULENTH (DBNAME,LEN(DBNAME),LENGTH)
C
C  CHECK IF ALL DATA FILES ALLOCATED
      IF (NALLOC.EQ.MALLOC) THEN
C     ALL FILES ALLOCATED
         IF (IABS(IDBALC(IPOS)).NE.2) IDBALC(IPOS)=1
         IF (IDBALC(IPOS).EQ.-2) IDBALC(IPOS)=2
         ELSE
C        ALL FILES NOT ALLOCATED
            IDBALC(IPOS)=0
            ISTAT=1
            IF (NPSPAG.EQ.0) CALL SUPAGE
            IF (NALLOC.EQ.0) THEN
C           NO FILES ALLOCATED
               WRITE (LP,10) DBNAME(1:LENGTH)
               CALL SUWRNS (LP,2,NUMWRN)
               ELSE
C              SOME FILES ALLOCATED
                  WRITE (LP,20) DBNAME(1:LENGTH)
                  CALL SUWRNS (LP,2,NUMWRN)
                  IDIM1=1
                  NVAL=10
                  ISPTR=0
C              SORT UNIT NUMBERS
                  CALL SUSORT (IDIM1,NVAL,NOTFND,NOTFND,ISPTR,IERR)
                  NUM1=1
                  DO 15 I=NVAL,1,-1
                     IF (NOTFND(I).EQ.0) THEN
                        NUM1=I+1
                        GO TO 17
                        ENDIF
15                   CONTINUE
C              PRINT UNIT NUMBERS
17                WRITE (LP,30) (NOTFND(N),N=NUM1,NVAL)
                  CALL SULINE (LP,1)
               ENDIF
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
10    FORMAT ('0*** WARNING - ',A,' NOT ALLOCATED.')
20    FORMAT ('0*** WARNING - ONE OR MORE ',A,
     *   ' FILES NOT ALLOCATED.')
30    FORMAT (T16,'THE FOLLOWING UNITS ARE NOT ALLOCATED: ',
     *   10(I2,1X))
C
      END
