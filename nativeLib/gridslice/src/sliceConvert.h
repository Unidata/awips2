/*
 * sliceConvert.h
 *
 *  Created on: Nov 17, 2009
 *      Author: brockwoo
 */

#ifndef SLICECONVERT_H_
#define SLICECONVERT_H_

#define DOINTERP -999999

void defineSlice(float ** vc3d, float ** param3d, int mnx, int nx, int ny,
		int nz, float param, int sense, float * vc2d);
void defineSliceD(float ** vc3d, int * dimvc, float ** param3d, int * dimpar,
		int mnx, int nx, int ny, int nz, float param, int sense, float * vc2d);
void createSlice(float ** vc3d, float * vc2d, float ** slice3d, int mnx,
		int nx, int ny, int nz, int sense, float * slice);
void createSliceD(float ** vc3d, int * dim3, float * vc2d, int dim2,
		float ** slice3d, int mnx, int nx, int ny, int nz, int sense,
		float * slice);
void sampleSlice(float ** vc3d, float * vc2d, float ** slice3d, int mnx,
		int nx, int ny, int nz, int sense, int hyb, float * slice);
void sampleSliceD(float ** vc3d, int * dim3, float * vc2d, int dim2,
		float ** slice3d, int mnx, int nx, int ny, int nz, int sense, int hyb,
		float * slice);
void defineSlices(float * vc3d, int senseA, float * param3d, int senseB,
		int nx, int ny, int nz, float * paramC, int nc, float * vcC);
void createSlices(float * vc3d, float * param3d, int sense, int nx, int ny,
		int nz, float * vcC, int nc, float * paramC);

#endif /* SLICECONVERT_H_ */
