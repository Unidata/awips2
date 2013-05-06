/************************************************************************
 * proto_cap.h                                                          *
 *                                                                      *
 * This include file contains public function prototypes for the        *
 * functions in the CAP library.                                        *
 *                                                                      *
 * Log:                                                                 *
 **                                                                     *
 * S.Danz/AWC           03/06   Created                                 *
 * S.Danz/AWC           07/06   Added cap_psclear                       *
 ***********************************************************************/
#ifndef _PROTO_CAP
#define _PROTO_CAP
#include "proto_cmd.h"

/* Opaque public typedefs for objects managed by the CAP placement system */
typedef void* PlacementSet;
typedef void* Placement;

/* Placement mode type information */
typedef enum {
    IMMEDIATE,
    ONESHOT,
    DELAYED,
    MODE_UNDEF
} PlacementMode;

void cap_plgetpcntr(Placement place, int *check_center, int *iret);

/* Public functions for the CAP object placement module */
void cap_psplace(
        PlacementSet    placements,
        CMDObjectSet    objects,
        int             *iret
    );

/* Public functions for CAP placement set data management */

void cap_psnew(
        PlacementSet    *placements,
        int             *iret
);

void cap_psdel(
        PlacementSet    placements,
        int             *iret
);

void cap_psclear(
        PlacementSet    placements,
        int             *iret
);

void cap_psmarkint(
        PlacementSet    placements,
        CMDObjectSet    objects,
        float           *bbox, 
        int             *found, 
        float           inf_bbox[4], 
        int             *iret
);

void cap_pssetarea(
        PlacementSet    placements,
        float           plot_area_min_x,
        float           plot_area_max_x,
        float           plot_area_min_y,
        float           plot_area_max_y,
        int             *iret
    );

void cap_pssetdist(
        PlacementSet    placements, 
        float           dist, 
        int             *iret
    );

void cap_pssetincr(
        PlacementSet    placements, 
        float           increment, 
        int             *iret
    );

void cap_psgetarea(
        PlacementSet    placements,
        float           *plot_area_min_x,
        float           *plot_area_max_x,
        float           *plot_area_min_y,
        float           *plot_area_max_y,
        int             *iret
    );

void cap_psaddpl(
        PlacementSet    placements,
        Handle          id,
        Handle          ref,
        int             allow_center,
        int             both_sides,
        int             max_tries,
        float           dist_incr,
        float           dist_offset,
        PlacementMode   pmode,
        int             point_center,
        int             *iret
    );

void cap_psgetpl(
        PlacementSet    placements, 
        Handle          id,
        Placement       *placement,
        int             *iret
    );

void cap_psgetplarea(
        PlacementSet    placements,
        CMDObjectSet    objects,
        Handle          id,
        int             to_delete,
        float           bbox[4], 
        int             *iret
);

void cap_psdelpl(
        PlacementSet    placements, 
        Handle          id,
        int             *iret
    );

void cap_psclrplaced(
        PlacementSet    placements,
        int             *iret
    );

void cap_psiterinit(
        PlacementSet    placements,
        int             *iret
    );

void cap_psiternext(
        PlacementSet    placements,
        Placement       *placement,
        int             *iret
    );

/* Public functions for CAP placement information data management */

void cap_plclrplace(
        Placement       placement, 
        int             *iret
    );

void cap_plsetref(
        Placement       placement, 
        int             ref,
        int             *iret
    );

void cap_plsetcenter(
        Placement       placement, 
        int             allow_center,
        int             *iret
    );

void cap_plsetpcntr(
        Placement       place, 
        int             check_center, 
        int             *iret
    );

void cap_plsetsides(
        Placement       placement, 
        int             both_sides,
        int             *iret
    );

void cap_plsettries(
        Placement       placement, 
        int             max_tries,
        int             *iret
    );

void cap_plsetdistincr(
        Placement       placement, 
        float           dist_incr,
        int             *iret
    );

void cap_plsetdistoffset(
        Placement       placement, 
        float           dist_offset,
        int             *iret
    );

void cap_plsetmode(
        Placement       placement, 
        PlacementMode   pmode,
        int             *iret
    );

void cap_plgetref(
        Placement       placement, 
        Handle          *ref,
        int             *iret
    );

void cap_plgetplaced(
        Placement       placement, 
        int             *placed,
        int             *iret
    );

void cap_plgetoffset(
        Placement       placement, 
        float           *offset_x,
        float           *offset_y,
        int             *iret
    );

void cap_plgetline(
        Placement       placement, 
        float           *x,
        float           *y,
        int             *iret
    );

void cap_plgetid(
        Placement       placement, 
        Handle          *id,
        int             *iret
    );

void cap_plgetcenter(
        Placement       placement, 
        int             *center,
        int             *iret
    );

void cap_plgetincntr(
        Placement       placement, 
        int             *in_center,
        int             *iret
    );

void cap_plgetdistincr(
        Placement       placement, 
        float           *dist_incr,
        int             *iret
    );

void cap_plgetdistoffset(
        Placement       placement, 
        float           *dist_offset,
        int             *iret
    );

void cap_plgetmode(
        Placement       placement, 
        PlacementMode   *mode,
        int             *iret
    );

void cap_plgetsides(
        Placement       placement, 
        int             *sides,
        int             *iret
    );

void cap_plgettries(
        Placement       placement, 
        int             *tries,
        int             *iret
    );
#endif
