/*
    File: DamFeatures.h
    Author  : CDBGEN
    Created : Wed Aug 06 12:34:47 EDT 2008 using database dc_ob7empty
    Description: This header file is associated with its .pgc file 
            and defines functions and the table's record structure.
*/
#ifndef DamFeatures_h
#define DamFeatures_h


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <memory.h>
#include "DbmsAccess.h"
#include "DbmsUtils.h"
#include "List.h"
#include "GeneralUtil.h"
#include "dbmserrs.h"
#include "datetime.h"
#include "time_convert.h"



typedef struct _DamFeatures
{
    Node		node;
    char		nidid[11];
    char		other_dam_name[66];
    char		dam_former_name[51];
    char		stateid[21];
    char		section_t_r[31];
    char		owner_name[51];
    char		owner_type[15];
    char		dam_designer[66];
    char		private_on_federal[7];
    char		dam_type[7];
    char		core[7];
    char		foundation[7];
    char		purposes[9];
    char		year_completed[21];
    char		year_modified[21];
    char		emerg_action_plan[4];
    char		inspection_date[21];
    char		inspection_freq[21];
    char		st_reg_dam[7];
    char		st_reg_agency[31];
    char		spillway_type[7];
    double		spillway_width;
    char		outlet_gates[7];
    double		volume_dam;
    double		number_locks;
    double		length_locks;
    double		width_locks;
    char		fed_funding[21];
    char		fed_design[21];
    char		fed_construction[21];
    char		fed_regulatory[21];
    char		fed_inspection[21];
    char		fed_operation[21];
    char		fed_other[21];
    char		fed_owner[21];
    char		source_agency[61];
    double		drainage_area;
    char		topo_map[23];
    long		return_flow_region;
    double		dam_length;
    double		dam_height;
    double		structural_height;
    double		hydraulic_height;
    double		nid_height;
    double		max_discharge;
    double		normal_storage;
    double		nid_storage;
    double		surface_area;
    double		elev;
    char		prebreak_avail[2];
    char		comments[31];
    dtime_t		updated;
    List		list;
} DamFeatures;
/*
    Function Prototypes
*/
    DamFeatures* GetDamFeatures(const char * where);
    DamFeatures* SelectDamFeatures(const char * where);
    int SelectDamFeaturesCount(const char * where);
    int PutDamFeatures(const DamFeatures * structPtr);
    int InsertDamFeatures(const DamFeatures * structPtr);
    int UpdateDamFeatures(const DamFeatures* structPtr, const char *where);
    int DeleteDamFeatures(const char *where);
    int UpdateDamFeaturesByRecord (const DamFeatures * newStructPtr, const DamFeatures * oldStructPtr);
    int InsertOrUpdateDamFeatures(const DamFeatures * structPtr);
    int InsertIfUniqueDamFeatures(const DamFeatures * structPtr, bool *isUnique);
    bool DamFeaturesExists(const DamFeatures * structPtr);
    int DeleteDamFeaturesByRecord(const DamFeatures * structPtr);
    void GetDamFeaturesPrimaryKeyWhereString (const DamFeatures * structPtr, char returnWhereString[] );
    void FreeDamFeatures(DamFeatures * structPtr);
    DbStatus * GetDamFeaturesDbStatus();
    void SetDamFeaturesErrorLogging(int value);
#endif
