#****************************************************************************
#* NCSA HDF                                                                 *
#* National Comptational Science Alliance                                   *
#* University of Illinois at Urbana-Champaign                               *
#* 605 E. Springfield, Champaign IL 61820                                   *
#*                                                                          *
#* For conditions of distribution and use, see the accompanying             *
#* COPYING file.                                                            *
#*                                                                          *
#****************************************************************************/

all:
	(cd intro; $(MAKE);)
	(cd groups; $(MAKE);)
	(cd datasets; $(MAKE);)
	(cd datatypes; $(MAKE);)

check:
	chmod u+x runExample.sh
	$(RM) *.h5;
	$(RM) h5ex_d_extern.data;
	$(RM) *.out;
	(cd intro; $(MAKE) check;)
	$(MAKE) run-intro;
	(cd groups; $(MAKE) check;)
	$(MAKE) run-groups;
	(cd datasets; $(MAKE) check;)
	$(MAKE) run-datasets;
	(cd datatypes; $(MAKE) check;)
	$(MAKE) run-datatypes;
	$(RM) *.out;

clean:
	(cd intro; $(MAKE) clean;)
	(cd groups; $(MAKE) clean;)
	(cd datasets; $(MAKE) clean;)
	(cd datatypes; $(MAKE) clean;)
	$(RM) *.h5;
	$(RM) h5ex_d_extern.data;
	$(RM) *.out;

run-intro:
	@if ./runExample.sh intro.H5_CreateDataset; then \
		echo "  PASSED      intro.H5_CreateDataset"; else  \
		echo "**FAILED**    intro.H5_CreateDataset"; fi;
	@if ./runExample.sh intro.H5_CreateAttribute; then \
		echo "  PASSED      intro.H5_CreateAttribute"; else  \
		echo "**FAILED**    intro.H5_CreateAttribute"; fi;
	@if ./runExample.sh intro.H5_CreateFile; then \
		echo "  PASSED      intro.H5_CreateFile"; else  \
		echo "**FAILED**    intro.H5_CreateFile"; fi;
	@if ./runExample.sh intro.H5_CreateGroup; then \
		echo "  PASSED      intro.H5_CreateGroup"; else  \
		echo "**FAILED**    intro.H5_CreateGroup"; fi;
	@if ./runExample.sh intro.H5_CreateGroupAbsoluteRelative; then \
		echo "  PASSED      intro.H5_CreateGroupAbsoluteRelative"; else  \
		echo "**FAILED**    intro.H5_CreateGroupAbsoluteRelative"; fi;
	@if ./runExample.sh intro.H5_CreateGroupDataset; then \
		echo "  PASSED      intro.H5_CreateGroupDataset"; else  \
		echo "**FAILED**    intro.H5_CreateGroupDataset"; fi;
	@if ./runExample.sh intro.H5_ReadWrite; then \
		echo "  PASSED      intro.H5_ReadWrite"; else  \
		echo "**FAILED**    intro.H5_ReadWrite"; fi;

run-groups:
	@if ./runExample.sh groups.H5Ex_G_Create; then \
		echo "  PASSED      groups.H5Ex_G_Create"; else  \
		echo "**FAILED**    groups.H5Ex_G_Create"; fi;
	cp groups/h5ex_g_iterate.h5 h5ex_g_iterate.h5; 
	@if ./runExample.sh groups.H5Ex_G_Iterate; then \
		echo "  PASSED      groups.H5Ex_G_Iterate"; else  \
		echo "**FAILED**    groups.H5Ex_G_Iterate"; fi;

run-datasets:
	@if ./runExample.sh datasets.H5Ex_D_Alloc; then \
		echo "  PASSED      datasets.H5Ex_D_Alloc"; else  \
		echo "**FAILED**    datasets.H5Ex_D_Alloc"; fi;
	@if ./runExample.sh datasets.H5Ex_D_Checksum; then \
		echo "  PASSED      datasets.H5Ex_D_Checksum"; else  \
		echo "**FAILED**    datasets.H5Ex_D_Checksum"; fi;
	@if ./runExample.sh datasets.H5Ex_D_Chunk; then \
		echo "  PASSED      datasets.H5Ex_D_Chunk"; else  \
		echo "**FAILED**    datasets.H5Ex_D_Chunk"; fi;
	@if ./runExample.sh datasets.H5Ex_D_Compact; then \
		echo "  PASSED      datasets.H5Ex_D_Compact"; else  \
		echo "**FAILED**    datasets.H5Ex_D_Compact"; fi;
	@if ./runExample.sh datasets.H5Ex_D_External; then \
		echo "  PASSED      datasets.H5Ex_D_External"; else  \
		echo "**FAILED**    datasets.H5Ex_D_External"; fi;
	@if ./runExample.sh datasets.H5Ex_D_FillValue; then \
		echo "  PASSED      datasets.H5Ex_D_FillValue"; else  \
		echo "**FAILED**    datasets.H5Ex_D_FillValue"; fi;
	@if ./runExample.sh datasets.H5Ex_D_Gzip; then \
		echo "  PASSED      datasets.H5Ex_D_Gzip"; else  \
		echo "**FAILED**    datasets.H5Ex_D_Gzip"; fi;
	@if ./runExample.sh datasets.H5Ex_D_Hyperslab; then \
		echo "  PASSED      datasets.H5Ex_D_Hyperslab"; else  \
		echo "**FAILED**    datasets.H5Ex_D_Hyperslab"; fi;
	@if ./runExample.sh datasets.H5Ex_D_ReadWrite; then \
		echo "  PASSED      datasets.H5Ex_D_ReadWrite"; else  \
		echo "**FAILED**    datasets.H5Ex_D_ReadWrite"; fi;
	@if ./runExample.sh datasets.H5Ex_D_Shuffle; then \
		echo "  PASSED      datasets.H5Ex_D_Shuffle"; else  \
		echo "**FAILED**    datasets.H5Ex_D_Shuffle"; fi;
	@if ./runExample.sh datasets.H5Ex_D_Szip; then \
		echo "  PASSED      datasets.H5Ex_D_Szip"; else  \
		echo "**FAILED**    datasets.H5Ex_D_Szip"; fi;
	@if ./runExample.sh datasets.H5Ex_D_UnlimitedAdd; then \
		echo "  PASSED      datasets.H5Ex_D_UnlimitedAdd"; else  \
		echo "**FAILED**    datasets.H5Ex_D_UnlimitedAdd"; fi;
	@if ./runExample.sh datasets.H5Ex_D_UnlimitedGzip; then \
		echo "  PASSED      datasets.H5Ex_D_UnlimitedGzip"; else  \
		echo "**FAILED**    datasets.H5Ex_D_UnlimitedGzip"; fi;
	@if ./runExample.sh datasets.H5Ex_D_UnlimitedMod; then \
		echo "  PASSED      datasets.H5Ex_D_UnlimitedMod"; else  \
		echo "**FAILED**    datasets.H5Ex_D_UnlimitedMod"; fi;

run-datatypes:
	@if ./runExample.sh datatypes.H5Ex_T_Array; then \
		echo "  PASSED      datatypes.H5Ex_T_Array"; else  \
		echo "**FAILED**    datatypes.H5Ex_T_Array"; fi;
	@if ./runExample.sh datatypes.H5Ex_T_ArrayAttribute; then \
		echo "  PASSED      datatypes.H5Ex_T_ArrayAttribute"; else  \
		echo "**FAILED**    datatypes.H5Ex_T_ArrayAttribute"; fi;
	@if ./runExample.sh datatypes.H5Ex_T_Bit; then \
		echo "  PASSED      datatypes.H5Ex_T_Bit"; else  \
		echo "**FAILED**    datatypes.H5Ex_T_Bit"; fi;
	@if ./runExample.sh datatypes.H5Ex_T_BitAttribute; then \
		echo "  PASSED      datatypes.H5Ex_T_BitAttribute"; else  \
		echo "**FAILED**    datatypes.H5Ex_T_BitAttribute"; fi;
	@if ./runExample.sh datatypes.H5Ex_T_Commit; then \
		echo "  PASSED      datatypes.H5Ex_T_Commit"; else  \
		echo "**FAILED**    datatypes.H5Ex_T_Commit"; fi;
	@if ./runExample.sh datatypes.H5Ex_T_Compound; then \
		echo "  PASSED      datatypes.H5Ex_T_Compound"; else  \
		echo "**FAILED**    datatypes.H5Ex_T_Compound"; fi;
	@if ./runExample.sh datatypes.H5Ex_T_CompoundAttribute; then \
		echo "  PASSED      datatypes.H5Ex_T_CompoundAttribute"; else  \
		echo "**FAILED**    datatypes.H5Ex_T_CompoundAttribute"; fi;
	@if ./runExample.sh datatypes.H5Ex_T_Float; then \
		echo "  PASSED      datatypes.H5Ex_T_Float"; else  \
		echo "**FAILED**    datatypes.H5Ex_T_Float"; fi;
	@if ./runExample.sh datatypes.H5Ex_T_FloatAttribute; then \
		echo "  PASSED      datatypes.H5Ex_T_FloatAttribute"; else  \
		echo "**FAILED**    datatypes.H5Ex_T_FloatAttribute"; fi;
	@if ./runExample.sh datatypes.H5Ex_T_Integer; then \
		echo "  PASSED      datatypes.H5Ex_T_Integer"; else  \
		echo "**FAILED**    datatypes.H5Ex_T_Integer"; fi;
	@if ./runExample.sh datatypes.H5Ex_T_IntegerAttribute; then \
		echo "  PASSED      datatypes.H5Ex_T_IntegerAttribute"; else  \
		echo "**FAILED**    datatypes.H5Ex_T_IntegerAttribute"; fi;
	@if ./runExample.sh datatypes.H5Ex_T_ObjectReference; then \
		echo "  PASSED      datatypes.H5Ex_T_ObjectReference"; else  \
		echo "**FAILED**    datatypes.H5Ex_T_ObjectReference"; fi;
	@if ./runExample.sh datatypes.H5Ex_T_ObjectReferenceAttribute; then \
		echo "  PASSED      datatypes.H5Ex_T_ObjectReferenceAttribute"; else  \
		echo "**FAILED**    datatypes.H5Ex_T_ObjectReferenceAttribute"; fi;
	@if ./runExample.sh datatypes.H5Ex_T_Opaque; then \
		echo "  PASSED      datatypes.H5Ex_T_Opaque"; else  \
		echo "**FAILED**    datatypes.H5Ex_T_Opaque"; fi;
	@if ./runExample.sh datatypes.H5Ex_T_OpaqueAttribute; then \
		echo "  PASSED      datatypes.H5Ex_T_OpaqueAttribute"; else  \
		echo "**FAILED**    datatypes.H5Ex_T_OpaqueAttribute"; fi;
	@if ./runExample.sh datatypes.H5Ex_T_String; then \
		echo "  PASSED      datatypes.H5Ex_T_String"; else  \
		echo "**FAILED**    datatypes.H5Ex_T_String"; fi;
	@if ./runExample.sh datatypes.H5Ex_T_StringAttribute; then \
		echo "  PASSED      datatypes.H5Ex_T_StringAttribute"; else  \
		echo "**FAILED**    datatypes.H5Ex_T_StringAttribute"; fi;
