#include <glob.h>
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include "globfiles.h"

#ifdef USEMAIN
int main(int argc, char *argv[])
{
	int    ret;
	char **fnames=NULL;

	fnames = getfiles(argv[1], &ret);

	if (ret == 0) {
	  if (!*fnames) { 
	    printf("No match.\n"); 
	  }
	  else {
	    while(*fnames) {
	      printf("%s\n", *fnames);
	      fnames++;
	    }
	  }
	}
	else {
	  fprintf(stderr,"An error occurred processing the file list.\n");
	  exit(1);
	}

	return(0);
}
#endif

char **getfiles(char *pattern, int *status)
{
	int      i, ret;
	char   **filenames=NULL;
	glob_t   pglob;

	*status = 1;

	if (!*pattern || !pattern)
	  return filenames;

	ret = glob(pattern, 0, NULL, &pglob);
	if (ret != 0) {
	  switch (ret) {
	    case GLOB_NOSPACE:
	      fprintf(stderr,"glob: memory allocation screwed up.\n");
	      return filenames;
	    break;
	    case GLOB_ABORTED:
	      fprintf(stderr,"glob: an error occurred. phooey..\n");
	      return filenames;
	    break;
	    case GLOB_NOMATCH:
	      ret = 0;
	    break;
	  }
	}

	*status = ret;

	/* Here you'd create your array of filenames */
	filenames = calloc(pglob.gl_pathc+1, sizeof(char *));
	if (!filenames)
	  return filenames;

	for (i=0; i<pglob.gl_pathc; i++) {
	  filenames[i] = malloc(PATH_MAX);
	  if (!filenames[i]) {
	    fprintf(stderr,"getfiles: Could not malloc for all filenames.\n");
	    fprintf(stderr,"getfiles: returning only %d of %d files found.\n",
	      i+1, pglob.gl_pathc);
	    break;
	  }
	  else {
	    strcpy(filenames[i], pglob.gl_pathv[i]);
	  }
	}
	filenames[i] = NULL;

	globfree(&pglob);

	return filenames;
}
