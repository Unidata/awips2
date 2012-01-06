C MODULE OPREAD
C-----------------------------------------------------------------------
C
      SUBROUTINE OPREAD (D,MD,IPASS1,ITUNIT,ITREC)
C
C  THIS ROUTINE WRITES TIME SERIES DATA TO A TEMPORARY FILE ON THE 
C  FIRST PASS AND READS THE TIME SERIES DATA FROM THE TEMPORARY
C  FILE ON SUBSEQUENT PASSES.
C
      DIMENSION D(MD)
      CHARACTER*3 CTUNIT
      CHARACTER*20 FILENAME
      CHARACTER*128 PATHNAME
C
      SAVE PATHNAME
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fctime'
      COMMON /FCTSRDX/ LDNXT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/opt3_shared/RCS/opread.f,v $
     . $',                                                             '
     .$Id: opread.f,v 1.5 2001/06/13 10:42:04 mgm Exp $
     . $' /
C    ===================================================================
C
C
      IERR=0
C
C  SET NAME FOR TEMPORARY FILE
      IBEG=1
      NCHAR=LEN(CTUNIT)
      IPRERR=1
      CALL UFI2A (ITUNIT,CTUNIT,IBEG,NCHAR,IPRERR,IPR,IERR)
      FILENAME='opt3_temp'
      CALL ULEFTC (CTUNIT,LEN(CTUNIT),LCTUNIT)
      CALL UCNCAT (FILENAME,CTUNIT,IERR)
C
C  CHECK IF TO WRITE TO TEMPORARY FILE 
      IF (IPASS1.EQ.1) THEN    
         ITREC=ITREC+1
         IF (ITREC.EQ.1) THEN
C        OPEN FILE         
            CALL UPHOMF (FILENAME,PATHNAME)
            IF (PATHNAME.NE.' ') THEN
               CALL UPOPEN (ITUNIT,PATHNAME,LDNXT,'U',IERR)
               IF (IERR.NE.0) THEN
                  WRITE (IPR,10) IERR,'OPENING',ITUNIT,
     *               PATHNAME(1:LENSTR(PATHNAME))
                  CALL ERROR
                  CALL STOP
                  ENDIF
               ELSE
                  WRITE (IPR,5) 
                  CALL ERROR
                  CALL STOP
               ENDIF
            ENDIF
         WRITE (ITUNIT,REC=ITREC,IOSTAT=IERR) (D(I),I=1,LDNXT)
         IF (IERR.NE.0) THEN
            WRITE (IPR,10) IERR,'WRITING TO',ITUNIT,
     *         PATHNAME(1:LENSTR(PATHNAME))
            CALL ERROR
            CALL STOP
            ENDIF            
         ENDIF
C 
C  CHECK IF TO READ FROM TEMPORARY FILE  
      IF (IPASS1.EQ.0) THEN         
         IF (IDA.EQ.IDARUN) ITREC=0
         ITREC=ITREC+1
         READ (ITUNIT,REC=ITREC,IOSTAT=IERR) (D(I),I=1,LDNXT)
         IF (IERR.NE.0) THEN
            WRITE (IPR,10) IERR,'READING FROM',ITUNIT,
     *         PATHNAME(1:LENSTR(PATHNAME))
            CALL STOP
            ENDIF            
         ENDIF
C  
C  CHECK IF TO DELETE TEMPORARY FILE       
      IF (IPASS1.LT.0) THEN
         CALL UPDELE (ITUNIT,PATHNAME,IERR)
         IF (IERR.NE.0) THEN
            WRITE (IPR,10) IERR,'DELETING',ITUNIT,
     *         PATHNAME(1:LENSTR(PATHNAME))
            ENDIF            
         ENDIF
C
      RETURN
C
5     FORMAT ('0**ERROR** IN OPREAD - PATH NAME RETURNED FROM ',
     *   'ROUTINE UPHOMF IS BLANK.')
10    FORMAT ('0**ERROR** IN OPREAD - STATUS CODE ',I3,
     *    ' ENCOUNTERED ',A,' THE FOLLOWING FILE ON UNIT ',I2,':' /
     +    ' ',A)
C      
      END
