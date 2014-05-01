/* SHEF PE code and data base table name information */
    
typedef struct _pe_map_struct {
    char   pe[3];
    char   table[25];
    }  pe_map_struct;

pe_map_struct   *pe_map;

int             num_pe_maps;
