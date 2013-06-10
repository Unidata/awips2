/*****************************************************************************************
 * COPYRIGHT (c), 2009, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/

/*
 * Thin wrapper around the NCEP decoder.  This implementation pulls the raw values from the file
 * to be processed by the python
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/7/09       1994        bphillip    Initial Creation
 * Mar 25, 2013 1821        bsteffen    Make grib2 decoding more multithreaded
 *
 * </pre>
 *
 * @author bphillip
 * @version 1
 */

#include <Python.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "numpy/arrayobject.h"
#include "grib2.h"

static PyObject *Grib2FileError;

int getRecord(FILE * fptr, gribfield ** gfld, int recordNumber,
		g2int fieldNumber, g2int unpack) {

	unsigned char *cgrib;
	g2int listsec0[3], listsec1[13];
	g2int iseek = 0;
	g2int lskip;
	g2int lgrib = 1;
	g2int numfields;
	g2int numlocal;
	g2int ierr, expand = 1;
	int ret = 1;
	size_t lengrib;

	// Seek to the correct position in the file
	int i = 0;
	for (i = 0; i <= recordNumber; i++) {
		seekgb(fptr, iseek, 32000, &lskip, &lgrib);
		iseek = lskip + lgrib;
	}

	// No more data
	if (lgrib == 0) {
		return -1;
	}

	// Pull out the data
	cgrib = (unsigned char *) malloc(lgrib);
	if (cgrib == NULL) {
		printf("getRecord: failed to malloc cgrib\n");
		return -1;
	}
	ret = fseek(fptr, lskip, SEEK_SET);
	lengrib = fread(cgrib, sizeof(unsigned char), lgrib, fptr);
	iseek = lskip + lgrib;
	ierr = g2_info(cgrib, listsec0, listsec1, &numfields, &numlocal);

	if (ierr != 0) {
		free(cgrib);
		return -1;
	}

	ierr = g2_getfld(cgrib, fieldNumber, unpack, expand, gfld);

	// Detected a grib1
	if (ierr != 0) {
		free(cgrib);
		return -2;
	}

	free(cgrib);
	return numfields;
}

/////////////////////////////////////////////////////////////////////////
//	Extracts the data values from the grib file
//
//	INPUT ARGUMENTS:
//		fptr		- The pointer to the file being decoded
//		recordNumber	- The number of the record being decoded
//		fieldNumber	- The number of the field being decoded
//
//	OUTPUT ARGUMENTS:
//		idSection	- An array to hold the ID section
//		localUseSection	- An array to hold the Local Use Section
//		gdsTemplate	- An array to hold the GDS Template values
//		pdsTemplate	- An array to hold the PDS Template values
//		data		- An array to hold the data values
//		bitMap		- An array to hold the bitmap values
//
//	RETURN VALUE: The number of fields associated with this record
//
/////////////////////////////////////////////////////////////////////////
static PyObject * grib2_getData(PyObject *self, PyObject* args)
/*FILE * fptr, int recordNumber, int fieldNumber, int idSection[],
 int localUseSection[], int gdsTemplate[],int pdsTemplate[],float data[],
 int bitMap[], int list_opt[], float coord_list[]) */{

	PyObject * fileInfo;
	FILE * fptr;
	int recordNumber;
	g2int fieldNumber;
	Py_ssize_t sizeSection = 0;
	int sectionCounter = 0;

	PyArg_ParseTuple(args, "Oii", &fileInfo, &recordNumber, &fieldNumber);

	fptr = PyFile_AsFile(fileInfo);

	gribfield * gfld;
	long numfields;
	npy_intp dimSize[1];
	PyObject *response = PyDict_New();
	Py_BEGIN_ALLOW_THREADS
	numfields = getRecord(fptr, &gfld, recordNumber, fieldNumber, 1);
	Py_END_ALLOW_THREADS

	PyObject * numberOfFields = PyInt_FromLong(numfields);
	PyDict_SetItemString(response, "numFields", numberOfFields);
	//Py_DECREF(numberOfFields);

	// Copy the ID Section
	PyObject * idSection;
	sizeSection = gfld->idsectlen;
	idSection = PyList_New(sizeSection);
	for (sectionCounter = 0; sectionCounter < gfld->idsectlen; sectionCounter++) {
		PyList_SetItem(idSection, sectionCounter, Py_BuildValue("i",
				gfld->idsect[sectionCounter]));
	}
	PyDict_SetItemString(response, "idSection", idSection);
	Py_DECREF(idSection);
	// Copy the Local Section if exists
	if (gfld->locallen > 0) {
		PyObject * localSection;
		sizeSection = gfld->locallen;
		localSection = PyList_New(sizeSection);
		for(sectionCounter = 0; sectionCounter < gfld->locallen; sectionCounter++) {
			PyList_SetItem(localSection, sectionCounter, Py_BuildValue("i",gfld->local[sectionCounter]));
		}
		PyDict_SetItemString(response, "localSection", localSection);
		Py_DECREF(localSection);
	}

	// Copy the number of points per row for quasi-regular grids
	if (gfld->num_opt > 0) {
		PyObject * listOps;
		sizeSection = gfld->num_opt;
		listOps = PyList_New(sizeSection);
		for(sectionCounter = 0; sectionCounter < gfld->num_opt; sectionCounter++) {
			PyList_SetItem(listOps, sectionCounter, Py_BuildValue("i",gfld->list_opt[sectionCounter]));
		}
		PyDict_SetItemString(response, "listOps", listOps);
		Py_DECREF(listOps);
	}

	// Copy the vertical discretisation values for hybrid coordinate vertical levels
	if (gfld->num_coord > 0) {
		PyObject * coordList;
		sizeSection = gfld->num_coord;
		coordList = PyList_New(sizeSection);
		for(sectionCounter = 0; sectionCounter < gfld->num_coord; sectionCounter++) {
			PyList_SetItem(coordList, sectionCounter, Py_BuildValue("f",gfld->coord_list[sectionCounter]));
		}
		PyDict_SetItemString(response, "coordList", coordList);
		Py_DECREF(coordList);
	}

	// Copy the GDS Template
	PyObject * gdsTemplate;
	sizeSection = gfld->igdtlen;
	gdsTemplate = PyList_New(sizeSection);
	for(sectionCounter = 0; sectionCounter < gfld->igdtlen; sectionCounter++) {
		PyList_SetItem(gdsTemplate, sectionCounter, Py_BuildValue("i",gfld->igdtmpl[sectionCounter]));
	}
	PyDict_SetItemString(response, "gdsTemplate", gdsTemplate);
	Py_DECREF(gdsTemplate);

	// Copy the PDS Template
	PyObject * pdsTemplate;
	sizeSection = gfld->ipdtlen;
	pdsTemplate = PyList_New(sizeSection);
	for(sectionCounter = 0; sectionCounter < gfld->ipdtlen; sectionCounter++) {
		PyList_SetItem(pdsTemplate, sectionCounter, Py_BuildValue("i",gfld->ipdtmpl[sectionCounter]));
	}
	PyDict_SetItemString(response, "pdsTemplate", pdsTemplate);
	Py_DECREF(pdsTemplate);

	// Copy the data
	PyObject * npyData;
	dimSize[0] = gfld->ngrdpts;
	npyData = PyArray_SimpleNew(1, dimSize, NPY_FLOAT);
	memcpy(((PyArrayObject *) npyData)->data, gfld->fld, gfld->ngrdpts
			* sizeof(float));
	PyDict_SetItemString(response, "data", npyData);
	Py_DECREF(npyData);

	// Copy the bitmap if exists
	if (gfld->ibmap == 0 || gfld->ibmap == 254) {
		PyObject * npyBitmap;
		dimSize[0] = gfld->ngrdpts;
		npyBitmap = PyArray_SimpleNew(1, dimSize, NPY_INT);
		memcpy(((PyArrayObject *) npyBitmap)->data, gfld->bmap, gfld->ngrdpts
				* sizeof(int));
		PyDict_SetItemString(response, "bitmap", npyBitmap);
		Py_DECREF(npyBitmap);
	}
	
	// Copy the Data Representation Section
	PyObject * drsTemplate;
	sizeSection = gfld->idrtlen;
	drsTemplate = PyList_New(sizeSection);
	for(sectionCounter = 0; sectionCounter < gfld->idrtlen; sectionCounter++) {
		PyList_SetItem(drsTemplate, sectionCounter, Py_BuildValue("i",gfld->idrtmpl[sectionCounter]));
	}
	PyDict_SetItemString(response, "drsTemplate", drsTemplate);
	Py_DECREF(drsTemplate);
	
	g2_free(gfld);
	return response;
}

/////////////////////////////////////////////////////////////////////////
//	Extracts the metadata values from the grib file.
//	The metadata is an array containing the values in the gribfield
//	structure
//
//	INPUT ARGUMENTS:
//		fptr		- The pointer to the file being decoded
//		recordNumber	- The number of the record being decoded
//		fieldNumber	- The number of the field being decoded
//
//	OUTPUT ARGUMENTS:
//		metadata	- An array holding the gribfield values
//
//	RETURN VALUE: The number of fields associated with this record
//
/////////////////////////////////////////////////////////////////////////
static PyObject * grib2_getMetadata(PyObject *self, PyObject* args)
/* FILE * fptr, int recordNumber,int fieldNumber, int metadata[]) */{

	PyObject * fileInfo;
	FILE * fptr;
	int recordNumber;
	int fieldNumber;
	int debug;
	Py_ssize_t sizeSection = 0;
	int sectionCounter = 0;

	PyArg_ParseTuple(args, "Oiii", &fileInfo, &recordNumber, &fieldNumber,
			&debug);
	fptr = PyFile_AsFile(fileInfo);

	gribfield * gfld;
	long numfields;
	//int metadata[21];
	numfields = getRecord(fptr, &gfld, recordNumber, fieldNumber, 0);
	PyObject *response = PyDict_New();

	PyObject * numberOfFields = PyInt_FromLong(numfields);
	PyDict_SetItemString(response, "numFields", numberOfFields);
	//Py_DECREF(numberOfFields);

	if (numfields == -1) {
		return response;
	} else if (numfields == -2) {
		g2_free(gfld);
		return response;
	}


	int metadata[21];

	// Length of array containing ID section values
	metadata[0] = gfld->idsectlen;

	// Length of array containing local section values
	metadata[1] = gfld->locallen;

	// Field number within GRIB message
	metadata[2] = gfld->ifldnum;

	// Source of grid definition
	metadata[3] = gfld->griddef;

	// Number of grid points in the defined grid
	metadata[4] = gfld->ngrdpts;

	// Number of octets needed for each additional grid points definition
	metadata[5] = gfld->numoct_opt;

	// Interpretation of list for optional points definition (Table 3.11)
	metadata[6] = gfld->interp_opt;

	// Grid Definition Template Number
	metadata[7] = gfld->igdtnum;

	// Length of array containing GDS Template values
	metadata[8] = gfld->igdtlen;

	// The number of entries in array ideflist(Used if numoct_opt != 0)
	metadata[9] = gfld->num_opt;

	// Product Definition Template Number
	metadata[10] = gfld->ipdtnum;

	// Length of array containing PDS Template values
	metadata[11] = gfld->ipdtlen;

	// Number of values in array gfld->coord_list[]
	metadata[12] = gfld->num_coord;

	// Number of data points unpacked and returned
	metadata[13] = gfld->ndpts;

	// Data Representation Template Number
	metadata[14] = gfld->idrtnum;

	// Length of array containing DRT template values
	metadata[15] = gfld->idrtlen;

	// Logical value indicating whether the bitmap and data values were unpacked
	metadata[16] = gfld->unpacked;

	// Logical value indicating whether the data field was expanded to the grid in
	// the case where a bit-map is present
	metadata[17] = gfld->expanded;

	// Bitmap indicator
	metadata[18] = gfld->ibmap;

	// Parameter Discipline
	metadata[19] = gfld->discipline;

	metadata[20] = sizeof(gfld->list_opt);

	PyObject * metadataList;
	sizeSection = 21;
	metadataList = PyList_New(sizeSection);
	for (sectionCounter = 0; sectionCounter < 21; sectionCounter++) {
		PyList_SetItem(metadataList, sectionCounter, Py_BuildValue("i",
				metadata[sectionCounter]));
	}
	PyDict_SetItemString(response, "metadata", metadataList);
	Py_DECREF(metadataList);
	if(debug) {
		int j = 0;
		printf("Metadata Values: ");
		for(j = 0; j < 21; j++) {
			printf(" %d",metadata[j]);
		}
		printf(".\n");
	}

	g2_free(gfld);
	return response;
}

static PyObject * grib2_checkVersion(PyObject *self, PyObject* args) {
	char * inputFile;
	long gribversion = 2;
	if (!PyArg_ParseTuple(args, "s", &inputFile)) {
		return NULL;
	}
	FILE * gribFile = fopen(inputFile, "rb");
	if (!gribFile) {
		PyErr_SetString(Grib2FileError,
				"Could not open the GRIB file specified.");
		return NULL;
	}
	//Search for grib header
	char header[100];
	fread(header, 1, 100, gribFile);
	int gCounter = 0;
	int foundHeader = 0;
	for (gCounter = 0; gCounter < 100; gCounter++) {
		if (header[gCounter] == 'G' && header[gCounter + 1] == 'R'
				&& header[gCounter + 2] == 'I' && header[gCounter + 3] == 'B') {
			foundHeader = 1;
			break;
		}
	}

	//char * start = strstr(header, "GRIB");
	if (!foundHeader) {
		PyErr_SetString(Grib2FileError, "Invalid Grib file detected.");
		fclose(gribFile);
		return NULL;
	}
	fclose(gribFile);
	if (header[gCounter + 7] == 0x01) { // GRIB 1 data
		gribversion = 1;
	} else if (header[gCounter + 7] != 0x02) {
		PyErr_SetString(Grib2FileError, "Unrecognized GRIB version.");
		return NULL;
	}

	return PyInt_FromLong(gribversion);
}

static PyObject * grib2_grib1Togrib2(PyObject *self, PyObject* args) {
	char * inputFile;
	char * outputFile;
	PyArg_ParseTuple(args, "ss", &inputFile, &outputFile);
	return PyInt_FromLong(1);
}

static PyMethodDef grib2_methods[] = { { "getMetadata", grib2_getMetadata,
		METH_VARARGS, "Returns the metadata for the grib file." }, { "getData",
		grib2_getData, METH_VARARGS,
		"Returns the data values for the grib file." }, { "checkVersion",
		grib2_checkVersion, METH_VARARGS,
		"Returns a file handle to a grib record." }, { "oneTotwo",
		grib2_grib1Togrib2, METH_VARARGS, "Converts grib1 files to grib2." }, {
		NULL, NULL, 0, NULL } /* sentinel */
};

void initgrib2(void) {
	PyObject *m;
	import_array();
	PyImport_AddModule("grib2");
	m = Py_InitModule("grib2", grib2_methods);
	Grib2FileError = PyErr_NewException("grib2.error", NULL, NULL);
	Py_INCREF(Grib2FileError);
	PyModule_AddObject(m, "error", Grib2FileError);
}
