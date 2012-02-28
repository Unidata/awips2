        SUBROUTINE DB_GETENSMBRS  ( aname, iret )
C************************************************************************
C* DB_GETENSMBRS                                                        *
C*                                                                      *
C* This subroutine gets ensnames from  common.                          *
C*                                                                      *
C* DB_GETENSMBRS  ( ANAME, IRET )                                       *
C*                                                                      *
C* Input parameters:                                                    *
C*      ANAME           CHAR*           Ens members names               *
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C**                                                                     *
C* Log:                                                                 *
C* m.gamazaychikov/CWS  07/09                                           *
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
        INCLUDE         'dbcmn.cmn'
C*
        CHARACTER*(*)   aname
C*
C------------------------------------------------------------------------
C*
        IF ( isensnames ) THEN
           CALL ST_LSTR ( ensnames, lensnames, ier )
           aname = ensnames ( :lensnames)
           iret = 0
         ELSE
           iret = -1
           aname =  ' '
        END IF
        CALL ST_NULL (aname, aname, lstr, ier)
        RETURN
        END
