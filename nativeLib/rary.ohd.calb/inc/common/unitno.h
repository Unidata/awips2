/*-----------------------------------------------------------------------------
** unitno.h - flags used to handle unit numbers for NWSRFS
**-----------------------------------------------------------------------------
** notes:       (1)     These flags are used by routines that set or check
**                      file status, such as:
**
**                              isda
**                              isres
**                              isseq
**                              isused (calls "isda" and "isseq")
**
**                              setda
**                              setres
**                              setseq
**
**-----------------------------------------------------------------------------
** history:
**
** 1.0 (9-17-93)        Steven A. Malers, RTi           Created file.
**-----------------------------------------------------------------------------
*/

#define NWSRFS_FILE_UNUSED              0x0     /* file is not used */
#define NWSRFS_FILE_DAIO                0x1     /* file is direct access */
#define NWSRFS_FILE_SEQUENTIAL          0x2     /* file is sequential access */
#define NWSRFS_FILE_RESERVED            0x4     /* file is reserved for use */
