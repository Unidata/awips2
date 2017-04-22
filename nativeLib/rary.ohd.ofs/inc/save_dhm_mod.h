#ifndef SAVE_DHM_MOD_H
#define SAVE_DHM_MOD_H


void save_dhm_mod_in_b2_format(Mods_everythingStruct *data, char *modcontent);
void save_dhm_mod_in_b3_format(Mods_everythingStruct *data, char *modcontent);
void load_new_mods_edit(Mods_everythingStruct *data);
char *getModContent();

#endif
