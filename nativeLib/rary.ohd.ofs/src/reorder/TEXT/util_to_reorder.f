C MODULE UTIL_TO_REORDER
C-----------------------------------------------------------------------
C
      SUBROUTINE UTIL_TO_REORDER
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/util_to_reorder.f,v $
     . $',                                                             '
     .$Id: util_to_reorder.f,v 1.2 1998/07/08 16:14:39 page Exp $
     . $' /
C    ===================================================================
C
      RETURN
      END
C
C  CONVERT FROM UTILITY TO REORDER ERROR AND WARNING COUNTING
C
      SUBROUTINE UEROR (NUNIT,NLINES,NUMERR)
      CALL SUERRS (NUNIT,NLINES,NUMERR)
      END
      
      SUBROUTINE UWARN (NUNIT,NLINES,NUMWRN)
      CALL SUWRNS (NUNIT,NLINES,NUMWRN)
      END
C      
C  CONVERT FROM UTIL TO REORDER LINE COUNTING
C     
      SUBROUTINE ULINE (IUNIT,NLINES)
      CALL SULINE (IUNIT,NLINES)
      END

      SUBROUTINE ULINEL (IUNIT,NLINES,IRETRN)
      IRETRN=ISLEFT(NLINES)
      END

