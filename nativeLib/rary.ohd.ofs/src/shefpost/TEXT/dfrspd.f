C MODULE DFRSPD
C-----------------------------------------------------------------------
C
C  THIS ROUTINE READS THE SHEFPPDB FILE INTO THE ARRAY ISHPDB
C
      SUBROUTINE DFRSPD (MSHPDB,ISHPDB,NSHPDB,ISTAT)

      DIMENSION ISHPDB(9,MSHPDB)
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpost/RCS/dfrspd.f,v $
     . $',                                                             '
     .$Id: dfrspd.f,v 1.3 2002/02/11 21:30:22 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (IDETR.GT.0) WRITE (LP,*) 'ENTER DFRSPD'
C
C  READ FILE INTO ARRAY
      CALL DFGSPD ('INITZ',MSHPDB,ISHPDB,NSHPDB,ISTAT)
      IF (ISTAT .NE. 0) THEN
         WRITE (LP,10)
10    FORMAT ('0**ERROR** IN DFRSPD - ERROR ENCOUNTERED IN DFSGPD.')
         ENDIF
      IF (IDEDB .NE. 0) THEN
         DO 20 I=1,NSHPDB
            WRITE (LP,'(3X,A4,2X,2A4,2X,A4,3I6,2X,2A4)')
     $            (ISHPDB(J,I),J=1,9)
20          CONTINUE
         ENDIF
C
      IF (IDETR.GT.0) WRITE (LP,*) 'EXIT DFRSPD'
C
      RETURN
C
      END
