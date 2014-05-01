/* Description:
 * ------------
 * Routine to obtain a token value from the .Apps_defaults file using the get_apps_defaults()
 * function. This routine buffers the token value into a dynamically allocated character array and
 * this value is memory resident throughout the run of the program. The free function at the end
 * must be called before the program exits. This function calculates the token value the first time
 * and when called subsequently it returns a pointer to the array that already has the values which
 * were obtained the first time this function was called. This function also modifies the token 
 * values not to contain leading or trailing spaces and converts any upper case characters to
 * lower case values.
 * There is a limit on the maximum token size which is set in gageqc_defs.h
*/

#include <ctype.h>
#include <string.h>
#include "gageqc_types.h"
#include "gageqc_defs.h"
#include "GeneralUtil.h"
#include "mpe_log_utils.h"

static char buf_to_hold_rfc_areas_token[MAX_TOKEN_SIZE];
static char buf_to_hold_rfc_areas_token1[MAX_TOKEN_SIZE];
static char** names_of_rfc_areas;
static char* temp_buffer;
static char* site_id_local;
static int num_tokens = 1;
static int tokenizer_check;
static int rfc_areas_token_length;
static int call_once;

char** get_rfc_areas(int* num)
{
	int i;
	int tok_pos_count = 0;
	int tok_realloc_count = 0;
	char* p_str = NULL;
	int is_areas_null = 0;

	if(call_once != 0)
	{
                * num = num_tokens;
		return names_of_rfc_areas;
	}
	rfc_areas_token_length = 0;
	rfc_areas_token_length = strlen("mpe_site_id");
	site_id_local = (char*) malloc(sizeof(char)*100);
	strcpy(site_id_local, "");
	get_apps_defaults("mpe_site_id",&rfc_areas_token_length,
	                   site_id_local,&rfc_areas_token_length);
	strip_lblanks(site_id_local);
	strip_tblanks(site_id_local);
	for(i=0;i<strlen(site_id_local);i++)
	{
		if(isupper(site_id_local[i]))
		{
			site_id_local[i] = tolower(site_id_local[i]);
		}
	}
	if(!strcmp(site_id_local, ""))
	{
		logMessage("Site ID Token Not Set...Exiting...");
	logMessage("Site ID Token Not Set...Exiting...\n");
		exit(0);
	}
	rfc_areas_token_length = 0;
	memset(buf_to_hold_rfc_areas_token, '\0', MAX_TOKEN_SIZE);
	memset(buf_to_hold_rfc_areas_token1, '\0', MAX_TOKEN_SIZE);
	rfc_areas_token_length = strlen("mpe_area_names");
	get_apps_defaults("mpe_area_names",&rfc_areas_token_length,
	                   buf_to_hold_rfc_areas_token,&rfc_areas_token_length);
	if(buf_to_hold_rfc_areas_token[0] != '\0')
	{
		strcpy(buf_to_hold_rfc_areas_token1,buf_to_hold_rfc_areas_token);
	}
	else
	{
		is_areas_null = 1;
	}
	if(!is_areas_null)
	{
	while(1)
	{
		temp_buffer = NULL;
		while(tok_pos_count < MAX_TOKEN_SIZE)
		{
			if(buf_to_hold_rfc_areas_token[tok_pos_count] == ',')
			{
				temp_buffer = realloc(temp_buffer,(tok_pos_count-tok_realloc_count)+1);
				tok_pos_count++;
				tok_realloc_count = tok_pos_count;
				break;
			}
			tok_pos_count++;
		}
		if(buf_to_hold_rfc_areas_token1 != NULL)
		{
			if(tokenizer_check == 0)
			{
				temp_buffer = (char*)strtok(buf_to_hold_rfc_areas_token1,",");
				tokenizer_check = 1;
			}
			else
			{
				temp_buffer = (char*)strtok(NULL,",");
			}
		}
		if(temp_buffer != NULL)
		{
			strip_lblanks(temp_buffer);
			strip_tblanks(temp_buffer);
			for(i=0;i<strlen(temp_buffer);i++)
			{
				if(isupper(temp_buffer[i]))
				{
					temp_buffer[i] = tolower(temp_buffer[i]);
				}
			}
			names_of_rfc_areas = (char**) realloc(names_of_rfc_areas,(num_tokens+1)*sizeof(char*));
			p_str = (char*) malloc(strlen(temp_buffer)+1);
			strcpy(p_str,temp_buffer);
			names_of_rfc_areas[num_tokens] = p_str;
			num_tokens++;
		}
		else
		{
			break;
		}
	}
	}//end if
	if(is_areas_null == 1)
	{
		names_of_rfc_areas = (char**) malloc(sizeof(char*));
	}
	names_of_rfc_areas[0] = site_id_local;
	*num = num_tokens;
	call_once = 1;
	return names_of_rfc_areas;
}

void free_rfc_areas_token_memory()
{
	int i = 0;
	for(i=0;i<num_tokens;i++)
	{
		if(names_of_rfc_areas[i] != NULL)
		{
			free(names_of_rfc_areas[i]);
			names_of_rfc_areas[i] = NULL;
		}
	}
	if(names_of_rfc_areas != NULL)
	{
		free(names_of_rfc_areas);
		names_of_rfc_areas = NULL;
	}
	if(temp_buffer != NULL)
	{
		free(temp_buffer);
		temp_buffer = NULL;
	}
	if(site_id_local != NULL)
	{
		free(site_id_local);
		site_id_local = NULL;
	}

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
