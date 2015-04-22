        SUBROUTINE DB_GETPARM  ( parm, iret )
C************************************************************************
C* DB_GETPARM                                                           *
C*                                                                      *
C* This subroutine gets DBPARM from common.                             *
C*                                                                      *
C* DB_GETPARM  ( PARM, IRET )                                           *
C*                                                                      *
C* Input parameters:                                                    *
C*      PARM          CHAR*          DBparm                             *
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C**                                                                     *
C* Log:                                                                 *
C* m.gamazaychikov/CWS	07/09                                           *
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
        INCLUDE         'dbcmn.cmn'
C*
        CHARACTER*(*)   parm
C*
C------------------------------------------------------------------------
C*
        CALL ST_LSTR ( dbparms, ldbparms, ier )
        IF ( ldbparms .eq. 0) THEN 
           iret = -1
           parm = ' '
        ELSE
           parm = dbparms ( :ldbparms)
           iret = 0   
        END IF
        CALL ST_NULL (parm, parm, lstr, ier)
        RETURN
        END
