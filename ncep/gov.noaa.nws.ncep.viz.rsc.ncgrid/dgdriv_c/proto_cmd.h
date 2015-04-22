/************************************************************************
 * proto_cmd.h                                                          *
 *                                                                      *
 * This include file contains public function prototypes for the        *
 * c files in the CMD library.                                          *
 *                                                                      *
 * Log:                                                                 *
 **                                                                     *
 * S.Danz/AWC           03/06   Created                                 *
 * S.Danz/AWC           07/06   Added cmd_osclear                       *
 ***********************************************************************/

#ifndef _PROTO_CMD
#define _PROTO_CMD
typedef int Handle;
typedef void* CMDObject;
typedef void* CMDObjectSet;

#endif

void cmd_obgetbb(
    CMDObject,
    float*,
    float*,
    float*,
    float*,
    int*
);

void cmd_obgetcntr(
    CMDObject,
    float*,
    float*,
    int*
);

void cmd_obgetcntrd(
    CMDObject,
    float*,
    float*,
    int*
);


void cmd_obgetid(
    CMDObject,
    Handle*,
    int*
);

void cmd_obgetpoints(
    CMDObject,
    const float**,
    const float**,
    int*,
    int*
);

void cmd_obispoly(
    CMDObject,
    int*,
    int*);

void cmd_obisvisible(
    CMDObject,
    int *,
    int *);

void cmd_osaddob(
    CMDObjectSet,
    Handle,
    float*,
    float*,
    int,
    int*);

void cmd_osclear(
    CMDObjectSet,
    int*);

void cmd_osdel(
    CMDObjectSet,
    int*);

void cmd_osdelob(
    CMDObjectSet,
    Handle,
    int*);

void cmd_osgetob(
    CMDObjectSet,
    Handle,
    CMDObject*,
    int*);

void cmd_ositerinit(
    CMDObjectSet,
    int*);

void cmd_ositernext(
    CMDObjectSet,
    CMDObject*,
    int*);

void cmd_osnew(
    CMDObjectSet*,
    int*);

void cmd_ossetarea(
    CMDObjectSet,
    float,
    float,
    float,
    float,
    int*);
