#ifndef PURGEDATA_SHOW_H
#define PURGEDATA_SHOW_H

#include <Xm/Xm.h>

#include "DbmsDefs.h"
#include "PurgeProduct.h"
#include "PurgeDynData.h"

/* prototypes */

void purgedata_show(Widget 	w);
void add_purgedata_cbs();

void load_purgepe_list();
void load_purgeprod_list();

void load_purgepe_textCB();
void load_purgeprod_textCB();

void purgepe_updateCB();

void purgeprod_addCB();
void purgeprod_updateCB();
void purgeprod_deleteCB();

void read_pe_info(PurgeDynData   *peinfo);
void read_prod_info(PurgeProduct *prodinfo);

void purgedata_selectpe(PurgeDynData *pePtr);
void purgedata_selectprod(PurgeProduct *prodPtr);
void purgedata_show ( Widget w ) ;

void free_purgedata();

void ok_purgedataCB();

#endif 
