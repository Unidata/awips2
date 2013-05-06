        SUBROUTINE DB_GETNAVTIME  ( atime, iret )
C************************************************************************
C* DB_GETNAVTIME                                                        *
C*                                                                      *
C* This subroutine gets navtime from  common.                           *
C*                                                                      *
C* DB_GETNAVTIME  ( ATIME, IRET )                                       *
C*                                                                      *
C* Input parameters:                                                    *
C*      ATIME           CHAR*           Nav time                        *
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
        CHARACTER*(*)   atime
C*
C------------------------------------------------------------------------
C*
        IF ( isnavtime ) THEN 
           CALL ST_LSTR ( navtime, lnavtime, ier )
           atime = navtime ( :lnavtime)
           iret = 0   
c           isnavtime = .false.
         ELSE
           iret = -1
           atime = ' '
        END IF
        CALL ST_NULL (atime, atime, lstr, ier)
        RETURN
        END
