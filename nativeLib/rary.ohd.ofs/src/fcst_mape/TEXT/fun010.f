C MODULE FUN010
C-----------------------------------------------------------------------
C
      SUBROUTINE FUN010
C
C  MAIN ROUTINE FOR THE MAPE FUNCTION
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_mape/RCS/fun010.f,v $
     . $',                                                             '
     .$Id: fun010.f,v 1.2 1999/07/07 11:55:58 page Exp $
     . $' /
C    ===================================================================
C
C
      CALL UMEMOV ('FUN010  ',OPNAME,2)
C
      CALL VDEBUG
      CALL VROOT
C
      CALL STOPFN ('MAPE    ')
C
      RETURN
C
      END
