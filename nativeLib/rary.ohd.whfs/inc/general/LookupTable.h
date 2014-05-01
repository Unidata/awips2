#ifndef LOOKUPTABLE_H
#define LOOKUPTABLE_H

#define DEFAULT_LOOKUP_TABLE_NUM_ELEMENTS 10
#define DEFAULT_LOOKUP_TABLE_EXPANSION 10

typedef char * LookupTableKey;
typedef void * LookupTableEntry;
typedef void ( * LookupCleanup ) ( LookupTableEntry );

typedef struct LookupTableRecord
{
   LookupTableKey key;
   LookupTableEntry entry;
}  LookupTableRecord;

typedef struct LookupTable
{
   int total_elements;
   int num_elements_filled;
   int num_elements_to_increment_by;
   LookupCleanup cleanup;
   
   /* This is an array of LookupTableRecord structures. */
   LookupTableRecord * table;
}  LookupTable;

#define LookupSuccess 1
#define LookupFailure 0
#define LookupMessageLength 256 

/* Lookup table function prototypes. */
int AddTableEntry ( LookupTable * pTable,
                    const LookupTableKey key,
                    const LookupTableEntry entry );

int DeleteLookupTable ( LookupTable * pTable );

void DeleteTableEntry ( LookupTable * pTable,
                        const LookupTableKey key );


const char * GetLookupMessage ( );

void * GetTableEntry ( const LookupTable * pTable,
                       const LookupTableKey key );

LookupTableEntry * GetTableEntries ( const LookupTable * pTable, 
                                     int * pStatus );

int GetTableEntryCount ( const LookupTable * pTable );

int InitLookupTable ( int num_elements,
                      int expansion_size,
                      LookupTable * pTable,
                      LookupCleanup cleanup );

LookupTableKey * GetTableKeys ( const LookupTable * pTable, int * pStatus );

#endif /* #ifndef LOOKUPTABLE_H */
