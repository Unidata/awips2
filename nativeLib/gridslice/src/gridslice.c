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
 * Python module that utilizes the AWIPSI sliceConvert functions to offer
 * slicing capability to numpy arrays.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 11/17/09     3580        brockwoo    Initial Creation
 *
 * </pre>
 *
 * @author brockwoo
 * @version 1
 */

#include <Python.h>
#include <stdio.h>
#include <string.h>
#include "numpy/arrayobject.h"
#include "sliceConvert.h"

static PyObject *GridSliceError;

static int dimensions(PyObject * array) {
	int returnValue = 0;
	int aDim = PyArray_NDIM(array);
	if (aDim == 3) {
		returnValue |= 4; // 2d arrays
	} else if (aDim == 2) {
		npy_intp * aDimList = PyArray_DIMS(array);
		if (aDimList[0] == 1) {
			returnValue |= 1; //linear array
		}
	} else {
		returnValue |= 8; // Unsupported
	}
	return returnValue;
}

static PyObject * defineNumpySlice(PyObject *self, PyObject* args)
/* float ** vc3d, float ** param3d, int mnx,
 int nx, int ny, int nz, float param, int sense, float * vc2d) */{
	PyObject * vc;
	PyObject * param;
	//int mx = 0;
	int nx = 0;
	int ny = 0;
	int nz = 0;
	float targetLevel;
	int sense;
	if (!PyArg_ParseTuple(args, "OOfi", &vc, &param, &targetLevel, &sense)) {
		return NULL;
	}

	int vu = dimensions(vc);
	int pu = dimensions(param);
	int uniformity = (vu | pu);

	if ((uniformity >> 3) == 1) {
		PyErr_SetString(GridSliceError,
				"One of the numpy arrays passed cannot be used for slicing.");
		return NULL;
	}

	npy_intp * vdimList = PyArray_DIMS(vc);
	npy_intp * pdimList = PyArray_DIMS(param);
	float * vc2d = 0;
	if (uniformity == 4) { // two cubes
		if (vdimList[0] != pdimList[0] || vdimList[1] != pdimList[1]
				|| vdimList[2] != pdimList[2]) {
			PyErr_SetString(GridSliceError,
					"Dimensions are different between cubes.  Calculation cannot be done.");
			return NULL;
		}
		nz = vdimList[0];
		ny = vdimList[1];
		nx = vdimList[2];
		float ** vc3d = (float**) malloc(nz * sizeof(float*));
		float ** param3d = (float**) malloc(nz * sizeof(float*));
		vc2d = (float*) malloc(nx * ny * sizeof(float));
		int levelCount = 0;
		for (levelCount = 0; levelCount < nz; levelCount++) {
			vc3d[levelCount] = (float *) PyArray_GETPTR1(vc, levelCount);
			param3d[levelCount] = (float *) PyArray_GETPTR1(param, levelCount);
		}

		defineSlice(vc3d, param3d, nx, nx, ny, nz, targetLevel, sense, vc2d);
		free(vc3d);
		free(param3d);
	} else if (uniformity < 4 || uniformity == 5) { // one cube and one constant or two constants
		int vnz = (vu == 4) ? vdimList[0] : vdimList[1];
		int pnz = (pu == 4) ? pdimList[0] : pdimList[1];
		int vny = (vu == 4) ? vdimList[1] : 0;
		int pny = (pu == 4) ? pdimList[1] : 0;
		int vnx = (vu == 4) ? vdimList[2] : 0;
		int pnx = (pu == 4) ? pdimList[2] : 0;
		if (vnz != pnz) {
			PyErr_SetString(GridSliceError,
					"Dimensions are different between the arrays.  Calculation cannot be done.");
			return NULL;
		}
		nz = vnz;
		ny = vny >= pny ? vny : pny;
		nx = vnx >= pnx ? vnx : pnx;
		float ** vc3d = (float**) malloc(nz * sizeof(float*));
		float ** param3d = (float**) malloc(nz * sizeof(float*));
		vc2d = (float*) malloc(nx * ny * sizeof(float));
		int * vc3dDim = (int*) malloc(nz * sizeof(int));
		int * param3dDim = (int*) malloc(nz * sizeof(int));

		int levelCount = 0;
		for (levelCount = 0; levelCount < nz; levelCount++) {
			if (vu == 4) {
				vc3d[levelCount] = (float *) PyArray_GETPTR1(vc, levelCount);
				vc3dDim[levelCount] = 2;
			} else {
				vc3d[levelCount] = (float *) PyArray_GETPTR2(vc, 0, levelCount);
				vc3dDim[levelCount] = 0;
			}
			if (pu == 4) {
				param3d[levelCount]
						= (float *) PyArray_GETPTR1(param, levelCount);
				param3dDim[levelCount] = 2;
			} else {
				param3d[levelCount]
						= (float *) PyArray_GETPTR2(param, 0, levelCount);
				param3dDim[levelCount] = 0;
			}

		}
		defineSliceD(vc3d, vc3dDim, param3d, param3dDim, nx, nx, ny, nz,
				targetLevel, sense, vc2d);
		free(vc3d);
		free(param3d);
		free(vc3dDim);
		free(param3dDim);
	}

	if (vc2d) {
		PyObject * pyVc2d;
		npy_intp dimSize[2];
		dimSize[0] = ny;
		dimSize[1] = nx;
		pyVc2d = PyArray_SimpleNew(2, dimSize, NPY_FLOAT);
		memcpy(((PyArrayObject *) pyVc2d)->data, vc2d, nx * ny * sizeof(float));
		free(vc2d);
		return pyVc2d;
	} else {
		PyErr_SetString(
				GridSliceError,
				"The result grid returned was empty.  Please check your initial data and try again.");
		return NULL;
	}
}

static PyObject * createNumpySlice(PyObject *self, PyObject* args)
/*float ** vc3d, float * vc2d,
 float ** slice3d, int mnx, int nx, int ny, int nz, int sense,
 float * slice)*/{

	PyObject * vc;
	PyObject * s3d;
	//int mx = 0;
	int nx = 0;
	int ny = 0;
	int nz = 0;
	PyObject * targetLevel;
	int sense;
	int hyb = DOINTERP;
	if (!PyArg_ParseTuple(args, "OOOi|i", &vc, &s3d, &targetLevel, &sense, &hyb)) {
		return NULL;
	}

	int vu = dimensions(vc);
	int su = dimensions(s3d);
	int uniformity = (vu | su);

	npy_intp * vdimList = PyArray_DIMS(vc);
	npy_intp * sdimList = PyArray_DIMS(s3d);
	float * slice = 0;
	if (uniformity == 4) {
		if (vdimList[0] != sdimList[0] || vdimList[1] != sdimList[1]
				|| vdimList[2] != sdimList[2]) {
			PyErr_SetString(GridSliceError,
					"Dimensions are different between cubes.  Calculation cannot be done.");
			return NULL;
		}
		nz = vdimList[0];
		ny = vdimList[1];
		nx = vdimList[2];
		float ** vc3d = (float**) malloc(nz * sizeof(float*));
		float ** slice3d = (float**) malloc(nz * sizeof(float*));
		slice = (float*) malloc(nx * ny * sizeof(float));
		int levelCount = 0;
		float * vc2d = (float *) PyArray_GETPTR1(targetLevel, 0);
		for (levelCount = 0; levelCount < nz; levelCount++) {
			vc3d[levelCount] = (float *) PyArray_GETPTR1(vc, levelCount);
			slice3d[levelCount] = (float *) PyArray_GETPTR1(s3d, levelCount);
		}
		if (hyb == DOINTERP) {
			createSlice(vc3d, vc2d, slice3d, nx, nx, ny, nz, sense, slice);
		} else {
			sampleSlice(vc3d, vc2d, slice3d, nx, nx, ny, nz, sense, hyb, slice);
		}

		free(vc3d);
		free(slice3d);
	} else if (vu < 4 && su == 4) { // one cube and one constant or two constants
		int vnz = (vu == 4) ? vdimList[0] : vdimList[1];
		if (vnz != sdimList[0]) {
			PyErr_SetString(GridSliceError,
					"Dimensions are different between the arrays.  Calculation cannot be done.");
			return NULL;
		}
		nz = sdimList[0];
		ny = sdimList[1];
		nx = sdimList[2];
		float ** vc3d = (float**) malloc(nz * sizeof(float*));
		float ** slice3d = (float**) malloc(nz * sizeof(float*));
		int * vc3dDim = (int*) malloc(nz * sizeof(int));
		slice = (float*) malloc(nx * ny * sizeof(float));
		int levelCount = 0;
		for (levelCount = 0; levelCount < nz; levelCount++) {
			if (vu == 4) {
				vc3d[levelCount] = (float *) PyArray_GETPTR1(vc, levelCount);
				vc3dDim[levelCount] = 2;
			} else {
				vc3d[levelCount] = (float *) PyArray_GETPTR2(vc, 0, levelCount);
				vc3dDim[levelCount] = 0;
			}
			slice3d[levelCount] = (float *) PyArray_GETPTR1(s3d, levelCount);
		}
		float * vc2d = (float *) PyArray_GETPTR1(targetLevel, 0);
		if (hyb == DOINTERP) {
			createSliceD(vc3d, vc3dDim, vc2d, 2, slice3d, nx, nx, ny, nz, sense, slice);
		} else {
			sampleSliceD(vc3d, vc3dDim, vc2d, 2, slice3d, nx, nx, ny, nz, sense, hyb, slice);
		}
		free(vc3d);
		free(slice3d);
		free(vc3dDim);
	}

	if (slice) {
		PyObject * pyVc2d;
		npy_intp dimSize[2];
		dimSize[0] = ny;
		dimSize[1] = nx;
		pyVc2d = PyArray_SimpleNew(2, dimSize, NPY_FLOAT);
		memcpy(((PyArrayObject *) pyVc2d)->data, slice, nx * ny * sizeof(float));
		free(slice);
		return pyVc2d;
	} else {
		PyErr_SetString(
				GridSliceError,
				"The result grid returned was empty.  Please check your initial data and try again.");
		return NULL;
	}

	return NULL;
}

static PyObject * createNumpySliceD(PyObject *self, PyObject* args)
/*float ** vc3d, int * dim3, float * vc2d,
 int dim2, float ** slice3d, int mnx, int nx, int ny, int nz, int sense,
 float * slice)*/{
	return NULL;
}

static PyObject * sampleNumpySlice(PyObject *self, PyObject* args)
/*float ** vc3d, float * vc2d,
 float ** slice3d, int mnx, int nx, int ny, int nz, int sense, int hyb,
 float * slice)*/{
	return NULL;
}

static PyObject * sampleNumpySliceD(PyObject *self, PyObject* args)
/*float ** vc3d, int * dim3, float * vc2d,
 int dim2, float ** slice3d, int mnx, int nx, int ny, int nz, int sense,
 int hyb, float * slice)*/{
	return NULL;
}

static PyObject * defineNumpySlices(PyObject *self, PyObject* args)
/*float * vc3d, int senseA, float * param3d,
 int senseB, int nx, int ny, int nz, float * paramC, int nc, float * vcC)*/{
	return NULL;
}

static PyObject * createNumpySlices(PyObject *self, PyObject* args)
/*float * vc3d, float * param3d, int sense,
 int nx, int ny, int nz, float * vcC, int nc, float * paramC)*/{
	return NULL;
}

static PyMethodDef gridslice_methods[] = { { "defineNumpySlice",
		defineNumpySlice, METH_VARARGS, "Description to be decided." }, {
		"createNumpySlice", createNumpySlice, METH_VARARGS,
		"Description to be decided." }, { "createNumpySliceD",
		createNumpySliceD, METH_VARARGS, "Description to be decided." }, {
		"sampleNumpySlice", sampleNumpySlice, METH_VARARGS,
		"Description to be decided." }, { "sampleNumpySliceD",
		sampleNumpySliceD, METH_VARARGS, "Description to be decided." }, {
		"defineNumpySlices", defineNumpySlices, METH_VARARGS,
		"Description to be decided." }, { "createNumpySlices",
		createNumpySlices, METH_VARARGS, "Description to be decided." }, {
		NULL, NULL, 0, NULL } /* sentinel */
};

void initgridslice(void) {
	PyObject *m;
	import_array();
	PyImport_AddModule("gridslice");
	m = Py_InitModule("gridslice", gridslice_methods);
	GridSliceError = PyErr_NewException("gridslice.error", NULL, NULL);
	Py_INCREF(GridSliceError);
	PyModule_AddObject(m, "error", GridSliceError);
}
