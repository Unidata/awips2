/********************************************************************
   log_data.c

   log_fp_grp()
   log_templatenames()
   log_tempnames()
   log_pccnames()
   log_template_info()
   log_template_buffers()
   
   ********************************************************************/

#include <stdio.h>                  /* standard io library */
#include <stdlib.h>                 /* standard libarary */
#include <string.h>                 /* string library */
#include <time.h>                   /* time library */

#include "log_data.h"               /* protypes for functions */
 
extern char outputdir[];

/********************************************************************
  log_fp_grp()
  
  PURPOSE 
  Lof information regarding the forecast points and groups.
   
   
   ********************************************************************/

void log_fp_grp(const int 		numfps,
		const fp_struct 	*fp,
		const int		numgrps,
		const grp_struct	*grp)
{
   FILE *file_ptr;
   int i, j;
   char filename[MAXLEN_FILENAME];
   
   /* open the diagnostic output file */
   
   sprintf(filename, "%s/%s", outputdir, FP_GRP_LOGFILE); 
   file_ptr = fopen(filename, "w");
   if(file_ptr == NULL)
   {
      log_msg(FILE_OPENWARN, filename);
      return;
   }
   
   /* dump the contents of the forecast point info */
   
   fprintf(file_ptr, "*** CONTENTS OF FORECAST POINT INFO STRUCTURE ***\n");
   
   for (i = 0; i < numfps; i++)
   {
      fprintf(file_ptr, "\n");
      fprintf(file_ptr, "ID = %s\n", fp[i].id);
      fprintf(file_ptr, "  Name      = %s\n", fp[i].name);
      fprintf(file_ptr, "  County    = %s\n", fp[i].county);
      fprintf(file_ptr, "  State     = %s\n", fp[i].state);
      fprintf(file_ptr, "  Stream    = %s\n", fp[i].stream);
      fprintf(file_ptr, "  Proximity = %s\n", fp[i].proximity);
      fprintf(file_ptr, "  Reach     = %s\n", fp[i].reach);
      fprintf(file_ptr, "  GrpId     = %s\n", fp[i].grpid);
      fprintf(file_ptr, "  PE        = %s\n", fp[i].pe);
      fprintf(file_ptr, "  Fs,bfs,fq = %6.2f %6.2f %6.2f\n",
	      fp[i].fs, fp[i].bf, fp[i].fq);
      fprintf(file_ptr,
	      "  Cat stages          =  %6.2f %6.2f %6.2f %6.2f %6.2f\n",
	      fp[i].cat[0], fp[i].cat[1], fp[i].cat[2], 
	      fp[i].cat[3], fp[i].cat[4]);
      fprintf(file_ptr, "  Prev avail, id, time    = %d %s %s",
	      fp[i].prev_avail, fp[i].prev_prod_categ,
	      asctime(gmtime(&fp[i].prev_prodtime)));
      fprintf(file_ptr, "  Obs  cat, time          = %f %s",
	      fp[i].prev_curobs_val, asctime(gmtime(&fp[i].prev_curobs_time)));
      fprintf(file_ptr, "  Fcst cat, time          = %f %s",
	      fp[i].prev_maxfcst_val, asctime(gmtime(&fp[i].prev_maxfcst_time)));
      fprintf(file_ptr, "  Fcst create time        = %s",
	      asctime(gmtime(&fp[i].prev_maxfcst_ctime)));
      
   } 
   
   fprintf(file_ptr, "\n*** CONTENTS OF FORECAST GROUP INFO STRUCTURE ***\n");
   
   for (i = 0; i < numgrps; i++)
   {
      fprintf(file_ptr, "\n");
      fprintf(file_ptr, "ID = %s\n", grp[i].id);
      fprintf(file_ptr, "  Name      = %s\n", grp[i].name);
      fprintf(file_ptr, "  Num fps   = %d\n", grp[i].numfps);
      for (j = 0; j < grp[i].numfps; j++)
      {
	     fprintf(file_ptr, " %s (%d)\n",
		 fp[grp[i].fpindex[j]].id, grp[i].fpindex[j]);
      }
   }
   

   
   /* close the file */
   
   fclose(file_ptr);
   
   return;
}

   
      
/*********************************************************************
   log_templatenames()
   
   PURPOSE
   This function writes the tempatenames info structure to a file.
      
   NOTES
   
   ********************************************************************/
void log_templatenames(const templatenames_struct *templatenames)
{
   FILE *file_ptr;
   char filename[MAXLEN_FILENAME];
   
   /* open the diagnostic output file */

   sprintf(filename, "%s/%s", outputdir, TEMPNAME_LOGFILE); 
   file_ptr = fopen(filename, "w");
   if(file_ptr == NULL)
   {
      log_msg(FILE_OPENWARN, filename);
      return;
   }

   fprintf(file_ptr, "*** CONTENTS OF TEMPLATE NAMES STRUCTURE ***\n");
   
   fprintf(file_ptr, "\nFor the Header section:\n");
   log_tempnames(&templatenames->header, file_ptr);
   
   fprintf(file_ptr, "\nFor the  Summary section:\n");
   log_tempnames(&templatenames->summary, file_ptr);
   
   fprintf(file_ptr, "\nFor the  Headline section:\n");
   log_tempnames(&templatenames->headline, file_ptr);
   
   fprintf(file_ptr, "\nFor the Basis section:\n");
   log_tempnames(&templatenames->basis, file_ptr);
   
   fprintf(file_ptr, "\nFor the Tabular section:\n");
   log_tempnames(&templatenames->tabular, file_ptr);
   
   fprintf(file_ptr, "\nFor the Roundup section:\n");
   log_tempnames(&templatenames->roundup, file_ptr);
   
   fprintf(file_ptr, "\nFor the Impact section:\n");
   log_tempnames(&templatenames->impact, file_ptr);
      
   fprintf(file_ptr, "\nFor the Comparison section:\n");
   log_tempnames(&templatenames->comparison, file_ptr);
   
   fprintf(file_ptr, "\nFor the Call-to-action section:\n");
   log_tempnames(&templatenames->cta, file_ptr);
   
   /* close the file */
   
   fclose(file_ptr);

   return;
}


/*********************************************************************
   log_tempnames()
   
   PURPOSE
   This function writes the templatenames info for a given 
   product section /subsection to a file.
      
   NOTES
   
   ********************************************************************/
void log_tempnames(const temp_name_struct 	*tempnames,
		         FILE			*file_ptr)
{
   int i;
   
   fprintf(file_ptr, " number = %d\n", tempnames->number);
   
   for (i = 0; i < tempnames->number; i++)
      fprintf(file_ptr, " %s\n", tempnames->name[i]);
   
   return;
}


/*********************************************************************
   log_pccnames()
   
   PURPOSE
   This function writes the pcc name and info to a file.
      
   NOTES
   
   ********************************************************************/
void log_pccnames(pccnames_struct 	*pccnames)
{
   pccnames_struct *pccname = NULL;
   FILE *file_ptr;
   int i;
   char filename[MAXLEN_FILENAME];
   
   /* open the diagnostic output file */

   sprintf(filename, "%s/%s", outputdir, PCCNAME_LOGFILE);
   file_ptr = fopen(filename, "w");
   if(file_ptr == NULL)
   {
      log_msg(FILE_OPENWARN, filename);
      return;
   }

   fprintf(file_ptr, "*** CONTENTS OF PCC NAMES STRUCTURE ***\n");

   if (pccnames != NULL)
	pccname = (pccnames_struct *)ListFirst(&pccnames->list);
   for (i = 0; pccname; i++)
   {
      fprintf(file_ptr, "\n%s\n%s\n", pccname->filename, pccname->descr); 
      pccname = (pccnames_struct *)ListNext(&pccname->node);
   }
   
   /* close the file */
   
   fclose(file_ptr);
   
   return;
}


/*********************************************************************
   log_template_info()
   
   PURPOSE
   This function writes the template info structure to a file.
      
   NOTES
   
   ********************************************************************/
void log_template_info(const template_info_struct *template_info)
{
   
   template_item_struct *template_items;
   
   int cnt, i;
   FILE *file_ptr;
   char filename[MAXLEN_FILENAME];
   
   /* open the diagnostic output file */
   
   sprintf(filename, "%s/%s", outputdir, TEMPLATE_INFO_LOGFILE); 
   file_ptr = fopen(filename, "w");
   if(file_ptr == NULL)
   {
      log_msg(FILE_OPENWARN, filename);
      return;
   }

   fprintf(file_ptr, "*** CONTENTS OF TEMPLATE INFO STRUCTURE ***\n");
   fprintf(file_ptr, "Template name = %s\n", template_info->name);
   fprintf(file_ptr, "Number of conditions,phrases = %i %i\n\n", 
	   template_info->num_conditions, template_info->num_phrases);
      
   for (i = 0; i < template_info->num_conditions; ++i)
   {
      fprintf(file_ptr, "For phrase # = %i\n", i); 
      fprintf(file_ptr, "stacksize = %i\n", template_info->stacksize[i]); 
      
      template_items = template_info->bottom_condition_stack[i];

      for(cnt = 0; cnt < template_info->stacksize[i]; cnt++)
      {
	 if (template_items[cnt].type == RPF_INT)
	    fprintf(file_ptr, " %2i : Integer (type,val) = %2i %i\n",
		   cnt, template_items[cnt].type, template_items[cnt].value.i);
	 
	 else if (template_items[cnt].type == RPF_FLT)
	    fprintf(file_ptr, " %2i : Float   (type,val) = %2i %f\n",
		   cnt, template_items[cnt].type, template_items[cnt].value.f);
	 
	 else if (template_items[cnt].type == RPF_STR)
	    fprintf(file_ptr, " %2i : String  (type,val) = %2i %s\n",
		   cnt, template_items[cnt].type, template_items[cnt].value.s);
	 
	 else if (isvar(template_items[cnt].type)  == TRUE)
	    fprintf(file_ptr, " %2i : Variable(type,indx)= %2i %2i\n",
		   cnt, template_items[cnt].type, 
		   template_items[cnt].varinfo->varindex);
	 
	 else
	    fprintf(file_ptr, " %2i : Other   (type)= %2i \n",
		   cnt, template_items[cnt].type);
      }     
   }
   
   for (i = 0; i < template_info->num_phrases; ++i)
   {
      fprintf(file_ptr, "Phrase # = %i\n", i); 
      fprintf(file_ptr, "%i %s\n", template_info->phraselen[i],
	      template_info->phrase[i]);
   }

   /* close the file */
   
   fclose(file_ptr);
   
   return;
}


/*********************************************************************
   log_template_buffers()
   
   PURPOSE
   This function writes the tabular buffer info to a file.
      
   NOTES
   
   ********************************************************************/
void log_template_buffers(const format_struct	*format,
		          const variable_struct	*variable,
			  const spectime_struct *spectime)   
{
   FILE *file_ptr;
   int i;
   char filename[MAXLEN_FILENAME];
   
   /* open the diagnostic output file */

   sprintf(filename, "%s/%s", outputdir, TEMPLATE_BUFFERS_LOGFILE); 
   file_ptr = fopen(filename, "w");
   if(file_ptr == NULL)
   {
      log_msg(FILE_OPENWARN, filename);
      return;
   }

   fprintf(file_ptr, "* TEMPLATE BUFFER RECORDS INFORMATION *\n\n");
   
   /* log the format buffer */
   
   fprintf(file_ptr, "FORMAT BUFFER:\n");
   fprintf(file_ptr, " Num of items = %d\n", format->num_of_formats);
   for (i = 0; i < format->num_of_formats; i++)
   {
      /* date/time formats are stored differently than the others */
      
      if (format->type[i] == VAR_TIM || format->type[i] == VAR_DAT)
         fprintf(file_ptr, " #%d : type, formatnum = %d %d\n",
	         i, format->type[i], format->size[i].i);
      else
      {
         fprintf(file_ptr, " #%d : type, type_size = %d %d \n",
	         i, format->type[i], format->type_size[i]);
         if (format->type_size[i] == RPF_INT)
	    fprintf(file_ptr, "      size            = %d\n",
		    format->size[i].i);
         else if (format->type_size[i] == RPF_FLT)
	    fprintf(file_ptr, "      size            = %f\n",
		    format->size[i].f);
         else if (format->type_size[i] == RPF_STR)
	    fprintf(file_ptr, "      size            = %s\n",
		    format->size[i].s);
      }
   }
   
   /* log the variable buffer */
   
   fprintf(file_ptr, "\nVARIABLE BUFFER: \n");
   fprintf(file_ptr, " Num of vars = %d \n", variable->num_of_variables);
   for (i = 0; i < variable->num_of_variables; i++)
   {
      fprintf(file_ptr, " #%d : varindex = %d \n",
	      i, variable->varinfo[i].varindex);
   }
   
   /* log the spectime buffer */
   
   fprintf(file_ptr, "\nSPECTIME BUFFER: \n");
   fprintf(file_ptr, " Num of spectimes = %d \n", spectime->num_of_spectimes);
   fprintf(file_ptr, " Reference time = %s\n", 
	   asctime(gmtime(&spectime->basetime)));
   for (i = 0; i < spectime->num_of_spectimes; i++)
   {
      fprintf(file_ptr, " #%d : relday, relhr, window = %d %d %d \n", i,
	      spectime->relday[i], spectime->relhour[i],
	      spectime->window[i]);
   }
   
   /* close the file */
   
   fclose(file_ptr);

   return;
}
