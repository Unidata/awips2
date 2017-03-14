/*********************************************************************
   read_names.c
   
   read_names()
   
   ********************************************************************/

#include <string.h>                  /* string library functions */
#include <stdio.h>                   /* standard io library functions */
#include <stdlib.h>

#include "read_names.h"      /* function prototypes */


/*********************************************************************
   read_names()
   
   PURPOSE
   To handle the reading of the template names in the template file
   and the loading of the names into the structure.
   
   
   ********************************************************************/

void read_names(char 			*selected_office,
		templatenames_struct	*templatenames)

{
   char msg_text[80];
   int section_index;
   
   
   /* read the names of the header section templates */
   
   sprintf(msg_text, "Reading template names from %s.%s", 
	   HEADER_TEMPLATEFILE, selected_office);
   log_msg("", msg_text); 
   section_index = HEADER;
   read_generic_templates(HEADER_TEMPLATEFILE, selected_office, 
			  section_index, &templatenames->header);
   
   
   /* read the names of the basis section templates */
   
   sprintf(msg_text, "Reading template names from %s.%s", 
	   BASIS_TEMPLATEFILE,selected_office );
   log_msg("", msg_text);    
   section_index = BASIS;
   read_generic_templates(BASIS_TEMPLATEFILE, selected_office, 
			  section_index, &templatenames->basis);
   
   
   /* read the names of the headline section templates */
   
   sprintf(msg_text, "Reading template names from %s.%s",
	   HEADLINE_TEMPLATEFILE, selected_office);
   log_msg("", msg_text);    
   section_index = HEADLINE;
   read_generic_templates(HEADLINE_TEMPLATEFILE, selected_office, 
			  section_index, &templatenames->headline);
   
   
   /* read the names of the summary section templates */

   sprintf(msg_text, "Reading template names from %s.%s", 
	   SUMMARY_TEMPLATEFILE, selected_office);
   log_msg("", msg_text);    
   section_index = SUMMARY;
   read_generic_templates(SUMMARY_TEMPLATEFILE, selected_office,
			  section_index, &templatenames->summary);
   
     
   /* read the names of the tabular section templates */
   
   sprintf(msg_text, "Reading template names from %s.%s",
	   TABULAR_TEMPLATEFILE, selected_office);
   log_msg("", msg_text);    
   section_index = TABULAR;
   read_generic_templates(TABULAR_TEMPLATEFILE, selected_office,
			  section_index, &templatenames->tabular);
   
   
   /* read the names of the data roundup subsection templates */
   
   sprintf(msg_text, "Reading template names from %s.%s", 
	   ROUNDUP_TEMPLATEFILE, selected_office);
   log_msg("", msg_text);    
   section_index = DATA_ROUNDUP;
   read_generic_templates(ROUNDUP_TEMPLATEFILE, selected_office, 
			  section_index, &templatenames->roundup);

   
   /* read the names of the impact subsection templates */
   
   sprintf(msg_text, "Reading template names from %s.%s", 
	   IMPACT_TEMPLATEFILE, selected_office);
   log_msg("", msg_text);    
   section_index = IMPACT_STATEMENT;
   read_generic_templates(IMPACT_TEMPLATEFILE, selected_office,
			  section_index, &templatenames->impact);

   
   /* read the names of the crest comparison subsection templates */
   
   sprintf(msg_text, "Reading template names from %s.%s", 
	   COMPARISON_TEMPLATEFILE, selected_office);
   log_msg("", msg_text);    
   section_index = HISTORICAL_COMPARISON;
   read_generic_templates(COMPARISON_TEMPLATEFILE, selected_office,
			  section_index, &templatenames->comparison);

   
   /* read the names of the call-to-action section templates */
   
   sprintf(msg_text, "Reading template names from %s.%s", 
	   CTA_TEMPLATEFILE, selected_office);
   log_msg("", msg_text);    
   section_index = CALL_TO_ACTION;
   read_generic_templates(CTA_TEMPLATEFILE, selected_office,
			  section_index, &templatenames->cta);
   
   
   return;
}

