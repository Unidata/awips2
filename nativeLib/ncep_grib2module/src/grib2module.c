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
 * Thin wrapper around the NCEP decoder. Provides a single decode method that
 * can be used to extract all of the data from a grib message. The return value
 * is a list of dict with one list entry for each field in the grib
 * file(usually 1). Each dict contains keys for each field in the gribfield
 * struct. Detailed documentation for this structure can be found in the file
 * dependencies/src/g2clib-1.1.8/grib2c.doc. All g2int fields are converted to
 * python integer types and all data pointers are converted to numpy arrays of
 * the appropriate type.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Oct 07, 2013  2402     bsteffen    Rewritten to work on file extents and
 *                                    more closely mirror C api
 *
 * </pre>
 *
 * @author bsteffen
 * @version 2
 */

#include <Python.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "numpy/arrayobject.h"
#include "grib2.h"

static PyObject *Grib2FileError;

/////////////////////////////////////////////////////////////////////////
//    Helper method to add a g2int to a PyDict object.
//
//    INPUT ARGUMENTS:
//        dp   - PyDict object to be added too.
//        key  - Key to put item into the dict.
//        item - the g2int item to place in the dict.
//
/////////////////////////////////////////////////////////////////////////
static void PyDict_SetIntItemString(PyObject *dp, const char *key, g2int item) {
	PyObject* pyitem = PyInt_FromLong(item);
	PyDict_SetItemString(dp, key, pyitem);
	Py_DECREF(pyitem);
}

/////////////////////////////////////////////////////////////////////////
//    Translates a gribfield into a PyDict.
//
//    INPUT ARGUMENTS:
//        gfld - A single decoded gribfield.
//
//    RETURN VALUE: A PyDict containing all fields from gfld, the caller is
//                  responsible for decrementing the ref count of the returned
//                  object.
//
/////////////////////////////////////////////////////////////////////////
static PyObject* translateField(gribfield *gfld) {
	PyObject* result;
	PyObject* section;
	npy_intp sectionSize[1];

	result = PyDict_New();

	/* version */
	PyDict_SetIntItemString(result, "version", gfld->version);
	/* discipline */
	PyDict_SetIntItemString(result, "discipline", gfld->discipline);

	/* idsect */
	sectionSize[0] = gfld->idsectlen;
	section = PyArray_SimpleNew(1, sectionSize, NPY_INT);
	memcpy(((PyArrayObject *) section)->data, gfld->idsect,
			gfld->idsectlen * sizeof(g2int));
	PyDict_SetItemString(result, "idsect", section);
	Py_DECREF(section);
	/* idsectlen */
	PyDict_SetIntItemString(result, "idsectlen", gfld->idsectlen);

	/* local */
	sectionSize[0] = gfld->locallen;
	section = PyArray_SimpleNew(1, sectionSize, NPY_UBYTE);
	memcpy(((PyArrayObject *) section)->data, gfld->local,
			gfld->locallen * sizeof(unsigned char));
	PyDict_SetItemString(result, "local", section);
	Py_DECREF(section);
	/* locallen */
	PyDict_SetIntItemString(result, "locallen", gfld->locallen);

	/* ifldnum */
	PyDict_SetIntItemString(result, "ifldnum", gfld->ifldnum);
	/* griddef */
	PyDict_SetIntItemString(result, "griddef", gfld->griddef);
	/* ngrdpts */
	PyDict_SetIntItemString(result, "ngrdpts", gfld->ngrdpts);

	/* numoct_opt */
	PyDict_SetIntItemString(result, "numoct_opt", gfld->numoct_opt);
	/* interp_opt */
	PyDict_SetIntItemString(result, "interp_opt", gfld->interp_opt);
	/* num_opt */
	PyDict_SetIntItemString(result, "num_opt", gfld->num_opt);
	/* interp_opt */
	PyDict_SetIntItemString(result, "interp_opt", gfld->interp_opt);
	/* list_opt */
	sectionSize[0] = gfld->num_opt;
	section = PyArray_SimpleNew(1, sectionSize, NPY_INT);
	memcpy(((PyArrayObject *) section)->data, gfld->list_opt,
			gfld->num_opt * sizeof(g2int));
	PyDict_SetItemString(result, "list_opt", section);
	Py_DECREF(section);

	/* igdtnum */
	PyDict_SetIntItemString(result, "igdtnum", gfld->igdtnum);
	/* igdtlen */
	PyDict_SetIntItemString(result, "igdtlen", gfld->igdtlen);
	/* igdtmpl */
	sectionSize[0] = gfld->igdtlen;
	section = PyArray_SimpleNew(1, sectionSize, NPY_INT);
	memcpy(((PyArrayObject *) section)->data, gfld->igdtmpl,
			gfld->igdtlen * sizeof(g2int));
	PyDict_SetItemString(result, "igdtmpl", section);
	Py_DECREF(section);

	/* ipdtnum */
	PyDict_SetIntItemString(result, "ipdtnum", gfld->ipdtnum);
	/* ipdtlen */
	PyDict_SetIntItemString(result, "ipdtlen", gfld->ipdtlen);
	/* ipdtmpl */
	sectionSize[0] = gfld->ipdtlen;
	section = PyArray_SimpleNew(1, sectionSize, NPY_INT);
	memcpy(((PyArrayObject *) section)->data, gfld->ipdtmpl,
			gfld->ipdtlen * sizeof(g2int));
	PyDict_SetItemString(result, "ipdtmpl", section);
	Py_DECREF(section);

	/* num_coord */
	PyDict_SetIntItemString(result, "num_coord", gfld->num_coord);
	/* coord_list */
	sectionSize[0] = gfld->num_coord;
	section = PyArray_SimpleNew(1, sectionSize, NPY_FLOAT);
	memcpy(((PyArrayObject *) section)->data, gfld->coord_list,
			gfld->num_coord * sizeof(g2float));
	PyDict_SetItemString(result, "coord_list", section);
	Py_DECREF(section);

	/* ndpts */
	PyDict_SetIntItemString(result, "ndpts", gfld->ndpts);
	/* idrtnum */
	PyDict_SetIntItemString(result, "idrtnum", gfld->idrtnum);
	/* idrtlen */
	PyDict_SetIntItemString(result, "idrtlen", gfld->idrtlen);
	/* idrtmpl */
	sectionSize[0] = gfld->idrtlen;
	section = PyArray_SimpleNew(1, sectionSize, NPY_INT);
	memcpy(((PyArrayObject *) section)->data, gfld->idrtmpl,
			gfld->idrtlen * sizeof(g2int));
	PyDict_SetItemString(result, "idrtmpl", section);
	Py_DECREF(section);

	/* unpacked */
	PyDict_SetIntItemString(result, "unpacked", gfld->unpacked);
	/* expanded */
	PyDict_SetIntItemString(result, "expanded", gfld->expanded);

	/* ibmap */
	PyDict_SetIntItemString(result, "ibmap", gfld->ibmap);
	/* bmap */
	if (gfld->ibmap == 0 || gfld->ibmap == 254) {
		sectionSize[0] = gfld->ngrdpts;
		section = PyArray_SimpleNew(1, sectionSize, NPY_INT);
		memcpy(((PyArrayObject *) section)->data, gfld->bmap,
				gfld->ngrdpts * sizeof(g2int));
		PyDict_SetItemString(result, "bmap", section);
		Py_DECREF(section);
	}

	/* fld */
	sectionSize[0] = gfld->ngrdpts;
	section = PyArray_SimpleNew(1, sectionSize, NPY_FLOAT);
	memcpy(((PyArrayObject *) section)->data, gfld->fld,
			gfld->ngrdpts * sizeof(g2float));
	PyDict_SetItemString(result, "fld", section);
	Py_DECREF(section);

	return result;
}

/////////////////////////////////////////////////////////////////////////
//    Extracts the data from a grib record already in memory.
//
//    INPUT ARGUMENTS:
//        rawData - An array of raw bytes from a grib file.
//
//    RETURN VALUE: A PyList of PyDicts, one PyDict for each field in the grib
//                  message. The caller is responsible for decrementing the ref
//                  count of the returned object.
//
/////////////////////////////////////////////////////////////////////////
static PyObject* decodeData(unsigned char* rawData) {
	PyObject* result = NULL;
	g2int listsec0[3], listsec1[13];
	g2int numfields;
	g2int numlocal;
	g2int ierr;
	g2int fieldIndex;
	gribfield *gfld;

	ierr = g2_info(rawData, listsec0, listsec1, &numfields, &numlocal);
	if (ierr != 0) {
		PyErr_SetString(Grib2FileError, "Failed to get grib info.\n");
		return NULL;
	}
	result = PyList_New(numfields);
	for (fieldIndex = 0; fieldIndex < numfields; fieldIndex++) {
		Py_BEGIN_ALLOW_THREADS
			ierr = g2_getfld(rawData, fieldIndex + 1, 1, 1, &gfld);
			Py_END_ALLOW_THREADS
		if (ierr != 0) {
			Py_DECREF(result);
			PyErr_SetString(Grib2FileError, "Failed to get grib field.\n");
			return NULL;
		}
		PyList_SET_ITEM(result, fieldIndex, translateField(gfld));
		g2_free(gfld);
	}
	return result;
}

/////////////////////////////////////////////////////////////////////////
//    Extracts the data from the specified portion of a grib file.
//
//    INPUT ARGUMENTS:
//        self          - The grib2 module object
//        fileInfo      - The PyFile for an open grib file.
//        startPosition - The start position of the file portion to decode.
//        messageLength - The length of the file portion to decode.
//
//    RETURN VALUE: A PyList of PyDicts, one PyDict for each field in the grib
//                  message. The caller is responsible for decrementing the ref
//                  count of the returned object.
//
/////////////////////////////////////////////////////////////////////////
static PyObject* grib2_decode(PyObject* self, PyObject* args) {
	PyObject* result = NULL;
	PyObject* fileInfo;
	int startPosition;
	int messageLength;
	FILE * fptr;
	int ierr;
	unsigned char* rawData;

	PyArg_ParseTuple(args, "Oii", &fileInfo, &startPosition, &messageLength);

	fptr = PyFile_AsFile(fileInfo);
	ierr = fseek(fptr, startPosition, SEEK_SET);
	if (ierr != 0) {
		PyErr_SetString(Grib2FileError, "Failed to seek to start position.\n");
		return NULL;
	}

	rawData = (unsigned char *) malloc(messageLength);
	if (rawData == NULL) {
		PyErr_SetString(Grib2FileError, "Failed to malloc rawData.\n");
		return NULL;
	}
	ierr = fread(rawData, sizeof(unsigned char), messageLength, fptr);
	if (ierr != messageLength) {
		free(rawData);
		PyErr_SetString(Grib2FileError, "Failed to read full grib message.\n");
		return NULL;
	}

	result = decodeData(rawData);
	free(rawData);
	return result;
}

static PyMethodDef grib2_methods[] = { { "decode", grib2_decode, METH_VARARGS,
		"Returns grib data for a range within a file." },
		{ NULL, NULL, 0, NULL } /* sentinel */
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
