#ifndef POINTCONTROL_DEFS_H
#define POINTCONTROL_DEFS_H

#include "DbmsDefs.h"
#include "pointcontrol_options.h" /* so that it knows about QueryMode */


/* The length of the PE + 2 spaces + the maximum length of the PE 
   description + space for the terminating null character. */
typedef char PhysTypeSrcText [ SHEF_PE_LEN + 2 + SHEF_PE_NAME_LEN + 1 ] ;
typedef char Char100Array [ 100 ] ;
typedef char ElementTypeText[30];

#define LIST_LOCATION_OF_PROCESSED_ENTRY 16

/* Structure used for managing the scrolled lists in the GUI;
   including the PhysicalElement, TypeSource, and DataSource lists */
typedef struct
{
   int	nitems;

   int	nriver;
   int	nrain;
   int	nsnow;
   int	ntemp;

   int	element_count;
   int	adhoc_type_source_count;

   /*
    * orgbuf holds the Physical Element immendiately followed by Type/Source
    * The evil thing is that PhysTypeSrcText is bastardized to do something else, which
    * takes up more space.
    */
    
   ElementTypeText *elementTypeTextArray;
   int element_type_count; 
    
   PhysTypeSrcText *orgbuf; 

   PhysTypeSrcText *riverbuf;
   PhysTypeSrcText *rainbuf;
   PhysTypeSrcText *snowbuf;
   PhysTypeSrcText *tempbuf;

   PhysTypeSrcText *otherbuf;

   PhysTypeSrcText *element_buffer;	 /* Current unique active pe list */
   PhysTypeSrcText *adhoc_typeSourceBuffer;	 /* Current unique active ts list */

   Char100Array	*adhoc_datasrcbuf;
   
   Char100Array *timestep_typeSourceBuffer;
   int timestep_type_source_count;

} pc_pets_struct;





/* Function prototypes. */
int save_PeTsIngestfilter ( ) ;

void count_PhysElemTypes ( ) ;
int count_OtherTypes ( int pos ) ;
int check_ShefProcObs ( ) ;
int check_ShefPostLatest ( ) ;
void addNameFromShefPe ( char * buf ) ;
void addNameFromShefTs ( char * buf ) ;

void load_physicalelement_list ( PhysTypeSrcText * textbuf ,
                                 int nitems ) ;
                                 
void pc_engine_LoadElementTypes(QueryMode mode);
void pc_engine_LoadAdHocElementTypes();

void load_adhoc_typesource_list ( int pos ) ;
pc_pets_struct * get_pc_petsdata ( ) ;

#endif /* #ifndef POINTCONTROL_DEFS_H */
