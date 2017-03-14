C$PRAGMA C (OFSCLEAN)
C  =====================================================================
C  pgm: OFSCLEAN() .. removes TEMP files associated with this process.
C
C  =====================================================================
      SUBROUTINE OFSCLN()
      
      INTEGER       STAT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/ofscln.f,v $
     . $',                                                             '
     .$Id: ofscln.f,v 1.1 2004/05/03 21:37:28 hank Exp $
     . $' /
C    ===================================================================
C

      write (*,*) 'Cleaning up ofs temporary files...'
      CALL OFSCLEAN(STAT)
      
      RETURN
      END
