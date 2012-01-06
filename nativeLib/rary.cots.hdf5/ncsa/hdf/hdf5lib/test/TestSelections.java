/****************************************************************************
 * NCSA HDF                                                                 *
 * National Comptational Science Alliance                                   *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * java-hdf5/COPYING file.                                                  *
 *                                                                          *
 ****************************************************************************/

package ncsa.hdf.hdf5lib.test;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import ncsa.hdf.hdf5lib.*;
import ncsa.hdf.hdf5lib.exceptions.*;

public class TestSelections implements TestModule
{
	private String FILE_ROOT = null;

	String dbgInfo;
	boolean testResult = false;

	public TestSelections()
	{
	}

	public boolean setUp( String workingDir, boolean cleanFilesAtStart) {
		FILE_ROOT = workingDir;
		return true;
	}

	public boolean cleanUp( boolean saveFiles )
        {
                try {
                H5.H5close();
                } catch (HDF5Exception ex) {
                        dbgInfo += "\nH5close(): FAILED "+ex;
			return false;
                }
		return true;
        }


	public String getTestName() {
		String desc = "Test Integer selections";
		return desc;
	}

	public String getTestDescription() {
		String desc = "Test Integer selections";
		return desc;
	}

	public void runTest() {
		testResult = false;
		boolean res;
		int ntests = 4;
		int passed = 0;
		dbgInfo = "\n\n========== Test selection 2D Integer  ==========";
		if(testReadSelection2D()) {
			passed++;
		}
		dbgInfo += "\n\n========== Test write selection 2D Integer  ==========";
		if(testWriteSelection2D()) {
			passed++;
		}
		dbgInfo += "\n\n========== Test selection of elements 2D Integer  ==========";
		if(testReadPoints2D()) {
			passed++;
		}
		dbgInfo += "\n\n========== Test write selection of elements 2D Integer  ==========";
		if(testWritePoints2D()) {
			passed++;
		}
		dbgInfo += "\n\n========== Integer selection tests complete: "+passed+" of "+ntests+" passed  ==========";
		testResult = (passed == ntests);
	}

	public boolean testPassed()
	{
		return testResult;
	}

	public String getVerboseResult()
	{
		return dbgInfo;
	}


	/**
	 * Tests
	 */
	private boolean testReadSelection2D()
	{
		String fileName = FILE_ROOT+"ReadSelection2D.h5";
		String dataset1Name = "IntegerArray";
		int nx = 100;
		int ny = 200;
		int rank = 2;
		int status;
		int i, j;
		int filespace;
		int memspace;
		int file, dataset1;
		int tfile, tdataset1;
		int datatype1, dataspace1;   /* handles */
		long[] dimsf = {100, 200};	 /* dataset dimensions */
		long[] offset = new long[2];	 /* selection corner */
		long[] size = new long[2];	 /* selection range */
		int[][] data1 = new int[nx][ny];			/* data to write */
		int[][] outData1 = new int[nx][ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++) {
				data1[j][i] = (int)(j*ny+i);
			}
		}


		// Create a new file using H5F_ACC_TRUNC access,
		// default file creation properties, and default file
		// access properties.
		file = -1;
		try {
		file = H5.H5Fcreate(fileName,
			HDF5Constants.H5F_ACC_TRUNC,
			HDF5Constants.H5P_DEFAULT,
			HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5create() Failed, exception: "+ex;
			return false;
		}

		// Describe the size of the array and create the data space
		// for fixed size dataset.
		dataspace1 = -1;
		try {
			dataspace1 = H5.H5Screate_simple(rank, dimsf, null);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Screate_simple: dataspace1 failed. "+ex; 
			return false;
		}

		// Define datatype for the data in the file.
		datatype1 = -1;
		try {
			datatype1 = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT));
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tcopy: datatype1 failed. "+ex; 
			return false; 
		}

		// Create a new dataset within the file using defined dataspace and
		// datatype and default dataset creation properties.
		try {
			dataset1 = H5.H5Dcreate(file, dataset1Name, 
				datatype1, dataspace1,
				HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dcreate() dataset1 Failed, exception: "+ex;
			return false;
		}

		// Write the data to the dataset using default transfer properties.
		try {
			status = H5.H5Dwrite(dataset1,
			H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT),
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5P_DEFAULT,
			data1);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dwrite(): failed dataset1 "+ex;
			return false;
		}

		try {
			H5.H5Sclose(dataspace1);
		} catch (Exception ex) {
			dbgInfo += "\nH5Sclose: dataspace 1 failed. "+ex;
			return false;
		}
		try {
			H5.H5Tclose(datatype1);
		} catch (Exception ex) {
			dbgInfo += "\nH5Tclose: datatype1 failed. "+ex;
			return false;
		}
		try {
			H5.H5Dclose(dataset1);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: dataset1 failed. "+ex;
			return false;
		}

		try {
			H5.H5Fclose(file);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Fclose(): FAILED "+ex;
			return false;
		}

 
 		tfile = -1;
 		try {
 		tfile = H5.H5Fopen(fileName,
                                 HDF5Constants.H5F_ACC_RDONLY,
                                 HDF5Constants.H5P_DEFAULT);
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Fopen() second open Failed, exception: "+ex;
 			return false;
 		}
 
 		try {
 			tdataset1 = H5.H5Dopen(tfile, dataset1Name); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Dopen() dataset1 Failed, exception: "+ex;
 			return false;
 		}

		offset[0] = 50;
		offset[1] = 75;
		size[0] = 10;
		size[1] = 20;
		filespace = -1;
		try {
			filespace = H5.H5Dget_space(tdataset1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dget_space, filespace: failed. "+ex; 
ex.printStackTrace();
			return false;
		}

		try {
			H5.H5Sselect_hyperslab( filespace, HDF5Constants.H5S_SELECT_SET, offset, null, size, null);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dselect_hyperslab, filespace: failed. "+ex; 
ex.printStackTrace();
			return false;
		}

		memspace = -1;
		try {
			memspace = H5.H5Screate_simple(rank, dimsf, null);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Screate_simple: memspace failed. "+ex; 
			return false;
		}
		try {
			H5.H5Sselect_hyperslab( memspace, HDF5Constants.H5S_SELECT_SET, offset, null, size, null);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dselect_hyperslab, memspace: failed. "+ex; 
			return false;
		}

		dbgInfo += "\nRead subset of Integer data as Integer";
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT),
 				memspace,
 				filespace,
 				HDF5Constants.H5P_DEFAULT,
 				outData1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed. "+ex; 
ex.printStackTrace();
			return false;
		}

 		for (j = 50; j < 60; j++)
 		{
 			for (i = 75; i < 95; i++)  {
 				if (outData1[j][i] !=( data1[j][i])) {
 
 					dbgInfo += "\n* bad data at: ["+j+"]["+i+"]  original: "+data1[j][i]+" read back as "+outData1[j][i];
 					//  // System.out.println("* bad data at: ["+j+"]["+i+"]  original: "+data1[j*ny+i]+" read back as "+outData1[j*ny+i]);System.out.flush();
 				}
 			}
 		}
 
 		try {
 			H5.H5Sclose(memspace);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Sclose: memspace after read 1 failed. "+ex;
			return false;
		}


		offset[0] = 0;
		offset[1] = 0;
		memspace = -1;
		try {
			memspace = H5.H5Screate_simple(rank, dimsf, null);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Screate_simple: memspace failed. "+ex; 
			return false;
		}
		try {
			H5.H5Sselect_hyperslab( memspace, HDF5Constants.H5S_SELECT_SET, offset, null, size, null);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dselect_hyperslab, memspace: failed. "+ex; 
			return false;
		}

		dbgInfo += "\nRead subset of Integer data as Integer";
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT),
 				memspace,
 				filespace,
 				HDF5Constants.H5P_DEFAULT,
 				outData1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

 		for (j = 0; j < 10; j++)
 		{
 			for (i = 0; i < 20; i++)  {
 				if (outData1[j][i] !=( data1[j+50][i+75])) {
 
 					dbgInfo += "\n* bad data at: ["+(j+50)+"]["+(i+75)+"]  original: "+data1[(j+50)][(i+75)]+" read back as "+outData1[(j+50)][(i+75)];
 					//  // System.out.println("* bad data at: ["+i+"]["+j+"]  original: "+data1[j*ny+i]+" read back as "+outData1[j*ny+i]);System.out.flush();
 				}
 			}
 		}
 
 		try {
 			H5.H5Sclose(memspace);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Sclose: memspace after read 1 failed. "+ex;
			return false;
		}
 		try {
 			H5.H5Sclose(filespace);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Sclose: filespace after read 1 failed. "+ex;
			return false;
		}
 		try {
 			H5.H5Dclose(tdataset1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: dataset1 after read 1 failed. "+ex;
			return false;
		}

 		try {
 			H5.H5Fclose(tfile);
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\nH5Fclose(): FAILED "+ex;
 			return false;
 		}
 		dbgInfo += "\nReadSelection2D(): OK ";
 		return true;
 	}

	private boolean testWriteSelection2D()
	{
		String fileName = FILE_ROOT+"WriteSelection2D.h5";
		String dataset1Name = "IntegerArray";
		int nx = 100;
		int ny = 200;
		int rank = 2;
		int status;
		int i, j;
		int filespace;
		int memspace;
		int file, dataset1;
		int tfile, tdataset1;
		int datatype1, dataspace1;   /* handles */
		long[] dimsf = {100, 200};	 /* dataset dimensions */
		long[] offset = new long[2];	 /* selection corner */
		long[] size = new long[2];	 /* selection range */
		int[][] data1 = new int[nx][ny];			/* data to write */
		int[][] data2 = new int[nx][ny];			/* data to write */
		int[][] outData1 = new int[nx][ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++) {
				data1[j][i] = (int)(j*ny+i);
				data2[j][i] = (int)(-(j*ny+i));
			}
		}


		// Create a new file using H5F_ACC_TRUNC access,
		// default file creation properties, and default file
		// access properties.
		file = -1;
		try {
		file = H5.H5Fcreate(fileName,
			HDF5Constants.H5F_ACC_TRUNC,
			HDF5Constants.H5P_DEFAULT,
			HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5create() Failed, exception: "+ex;
			return false;
		}

		// Describe the size of the array and create the data space
		// for fixed size dataset.
		dataspace1 = -1;
		try {
			dataspace1 = H5.H5Screate_simple(rank, dimsf, null);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Screate_simple: dataspace1 failed. "+ex; 
			return false;
		}

		// Define datatype for the data in the file.
		datatype1 = -1;
		try {
			datatype1 = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT));
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tcopy: datatype1 failed. "+ex; 
			return false; 
		}

		// Create a new dataset within the file using defined dataspace and
		// datatype and default dataset creation properties.
		try {
			dataset1 = H5.H5Dcreate(file, dataset1Name, 
				datatype1, dataspace1,
				HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dcreate() dataset1 Failed, exception: "+ex;
			return false;
		}

		// Write the data to the dataset using default transfer properties.
		try {
			status = H5.H5Dwrite(dataset1,
			H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT),
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5P_DEFAULT,
			data1);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dwrite(): failed dataset1 "+ex;
			return false;
		}

		try {
			H5.H5Sclose(dataspace1);
		} catch (Exception ex) {
			dbgInfo += "\nH5Sclose: dataspace 1 failed. "+ex;
			return false;
		}
		try {
			H5.H5Tclose(datatype1);
		} catch (Exception ex) {
			dbgInfo += "\nH5Tclose: datatype1 failed. "+ex;
			return false;
		}
		try {
			H5.H5Dclose(dataset1);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: dataset1 failed. "+ex;
			return false;
		}

		try {
			H5.H5Fclose(file);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Fclose(): FAILED "+ex;
			return false;
		}

 
 		tfile = -1;
 		try {
 		tfile = H5.H5Fopen(fileName,
                                 HDF5Constants.H5F_ACC_RDWR,
                                 HDF5Constants.H5P_DEFAULT);
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Fopen() second open Failed, exception: "+ex;
 			return false;
 		}
 
 		try {
 			tdataset1 = H5.H5Dopen(tfile, dataset1Name); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Dopen() dataset1 Failed, exception: "+ex;
 			return false;
 		}

		offset[0] = 50;
		offset[1] = 75;
		size[0] = 10;
		size[1] = 20;
		filespace = -1;
		try {
			filespace = H5.H5Dget_space(tdataset1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dget_space, filespace: failed. "+ex; 
ex.printStackTrace();
			return false;
		}

		try {
			H5.H5Sselect_hyperslab( filespace, HDF5Constants.H5S_SELECT_SET, offset, null, size, null);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dselect_hyperslab, filespace: failed. "+ex; 
ex.printStackTrace();
			return false;
		}

		memspace = -1;
		try {
			memspace = H5.H5Screate_simple(rank, dimsf, null);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Screate_simple: memspace failed. "+ex; 
			return false;
		}
		try {
			H5.H5Sselect_hyperslab( memspace, HDF5Constants.H5S_SELECT_SET, offset, null, size, null);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dselect_hyperslab, memspace: failed. "+ex; 
			return false;
		}

		dbgInfo += "\nWrite subset of Integer data as Integer";
 		try {
 			status = H5.H5Dwrite(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT),
 				memspace,
 				filespace,
 				HDF5Constants.H5P_DEFAULT,
 				data2);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dwrite: failed. "+ex; 
			return false;
		}
 		try {
 			H5.H5Sclose(memspace);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Sclose: memspace after write failed. "+ex;
			return false;
		}
 		try {
 			H5.H5Sclose(filespace);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Sclose: filespace after write failed. "+ex;
			return false;
		}
 		try {
 			H5.H5Dclose(tdataset1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: dataset1 after write failed. "+ex;
			return false;
		}

 		try {
 			H5.H5Fclose(tfile);
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\nH5Fclose(): FAILED "+ex;
 			return false;
 		}

 		tfile = -1;
 		try {
 		tfile = H5.H5Fopen(fileName,
                                 HDF5Constants.H5F_ACC_RDWR,
                                 HDF5Constants.H5P_DEFAULT);
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Fopen() second open Failed, exception: "+ex;
 			return false;
 		}
 
 		try {
 			tdataset1 = H5.H5Dopen(tfile, dataset1Name); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Dopen() dataset1 Failed, exception: "+ex;
 			return false;
 		}
 
		dbgInfo += "\nre-read all Integer data ";
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}


 		for (j = 0; j < 49; j++) {
 			for (i = 0; i < ny; i++)  {
 				if (outData1[j][i] !=( data1[j][i])) {
 
 					dbgInfo += "\n* bad data at: ["+j+"]["+i+"]  original: "+data1[j][i]+" read back as "+outData1[j][i];
 					//  // System.out.println("* bad data at: ["+i+"]["+j+"]  original: "+data1[j*ny+i]+" read back as "+outData1[j*ny+i]);System.out.flush();
return false;
 				}
			}
		}

 		for (j = 50; j < 60; j++)
 		{
 			for (i = 0; i < 75; i++)  {
 				if (outData1[j][i] !=( data1[j][i])) {
 
 					dbgInfo += "\n* bad data at: ["+j+"]["+i+"]  original: "+data1[j][i]+" read back as "+outData1[j][i];
 					//  // System.out.println("* bad data at: ["+i+"]["+j+"]  original: "+data1[j*ny+i]+" read back as "+outData1[j*ny+i]);System.out.flush();
return false;
 				}
 			}
 			for (i = 75; i < 95; i++)  {
 				if (outData1[j][i] !=( data2[j][i])) {
 
 					dbgInfo += "\n* bad data at: ["+j+"]["+i+"]  original: "+data2[j][i]+" read back as "+outData1[j][i];
 					//  // System.out.println("* bad data at: ["+i+"]["+j+"]  original: "+data1[j*ny+i]+" read back as "+outData1[j*ny+i]);System.out.flush();
return false;
 				}
 			}
 			for (i = 95; i < ny; i++)  {
 				if (outData1[j][i] !=( data1[j][i])) {
 
 					dbgInfo += "\n* bad data at: ["+j+"]["+i+"]  original: "+data1[j][i]+" read back as "+outData1[j][i];
 					//  // System.out.println("* bad data at: ["+i+"]["+j+"]  original: "+data1[j*ny+i]+" read back as "+outData1[j*ny+i]);System.out.flush();
return false;
 				}
 			}
 		}
 		for (j = 60; j < nx; j++) {
 			for (i = 0; i < ny; i++)  {
 				if (outData1[j][i] !=( data1[j][i])) {
 
 					dbgInfo += "\n* bad data at: ["+j+"]["+i+"]  original: "+data1[j][i]+" read back as "+outData1[j][i];
 					//  // System.out.println("* bad data at: ["+i+"]["+j+"]  original: "+data1[j*ny+i]+" read back as "+outData1[j*ny+i]);System.out.flush();
return false;
 				}
			}
		}
 
 		try {
 			H5.H5Dclose(tdataset1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: dataset1 after read 1 failed. "+ex;
			return false;
		}
 		try {
 			H5.H5Fclose(tfile);
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\nH5Fclose(): FAILED "+ex;
 			return false;
 		}
 		dbgInfo += "\nIntegerSelection1D(): OK ";
 		return true;
 	}
 
	private boolean testReadPoints2D()
	{
		String fileName = FILE_ROOT+"ReadPoints2D.h5";
		String dataset1Name = "IntegerArray";
		int nx = 100;
		int ny = 200;
		int rank = 2;
		int status;
		int i, j;
		int filespace;
		int memspace;
		int file, dataset1;
		int tfile, tdataset1;
		int datatype1, dataspace1;   /* handles */
		long[] dimsf = {100, 200};	 /* dataset dimensions */
		long[] offset = new long[2];	 /* selection corner */
		long[] size = new long[2];	 /* selection range */

		int npoints = 10;
		long[][] coords = new long [npoints][rank];
		long[][] coords2 = new long [npoints][rank];
		int[][] data1 = new int[nx][ny];			/* data to write */
		int[][] data2 = new int[nx][ny];			/* data to write */
		int[][] outData1 = new int[nx][ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++) {
				data1[j][i] = (int)(j*ny+i);
				data2[j][i] = (int)(-(j*ny+i));
			}
		}


		// Create a new file using H5F_ACC_TRUNC access,
		// default file creation properties, and default file
		// access properties.
		file = -1;
		try {
		file = H5.H5Fcreate(fileName,
			HDF5Constants.H5F_ACC_TRUNC,
			HDF5Constants.H5P_DEFAULT,
			HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5create() Failed, exception: "+ex;
			return false;
		}

		// Describe the size of the array and create the data space
		// for fixed size dataset.
		dataspace1 = -1;
		try {
			dataspace1 = H5.H5Screate_simple(rank, dimsf, null);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Screate_simple: dataspace1 failed. "+ex; 
			return false;
		}

		// Define datatype for the data in the file.
		datatype1 = -1;
		try {
			datatype1 = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT));
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tcopy: datatype1 failed. "+ex; 
			return false; 
		}

		// Create a new dataset within the file using defined dataspace and
		// datatype and default dataset creation properties.
		try {
			dataset1 = H5.H5Dcreate(file, dataset1Name, 
				datatype1, dataspace1,
				HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dcreate() dataset1 Failed, exception: "+ex;
			return false;
		}

		// Write the data to the dataset using default transfer properties.
		try {
			status = H5.H5Dwrite(dataset1,
			H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT),
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5P_DEFAULT,
			data1);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dwrite(): failed dataset1 "+ex;
			return false;
		}

		try {
			H5.H5Sclose(dataspace1);
		} catch (Exception ex) {
			dbgInfo += "\nH5Sclose: dataspace 1 failed. "+ex;
			return false;
		}
		try {
			H5.H5Tclose(datatype1);
		} catch (Exception ex) {
			dbgInfo += "\nH5Tclose: datatype1 failed. "+ex;
			return false;
		}
		try {
			H5.H5Dclose(dataset1);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: dataset1 failed. "+ex;
			return false;
		}

		try {
			H5.H5Fclose(file);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Fclose(): FAILED "+ex;
			return false;
		}

 
 		tfile = -1;
 		try {
 		tfile = H5.H5Fopen(fileName,
                                 HDF5Constants.H5F_ACC_RDONLY,
                                 HDF5Constants.H5P_DEFAULT);
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Fopen() second open Failed, exception: "+ex;
 			return false;
 		}
 
 		try {
 			tdataset1 = H5.H5Dopen(tfile, dataset1Name); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Dopen() dataset1 Failed, exception: "+ex;
 			return false;
 		}

		for (i = 0; i < npoints; i++) {
			coords[i][0] = (long)(10+i); /* diagonal */
			coords[i][1] = (long)(10+i);
			coords2[i][0] = (long)i; /* one row */
			coords2[i][1] = (long)0;
		}
		filespace = -1;
		try {
			filespace = H5.H5Dget_space(tdataset1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dget_space, filespace: failed. "+ex; 
ex.printStackTrace();
			return false;
		}

		try {
			H5.H5Sselect_none( filespace );
			H5.H5Sselect_elements( filespace, HDF5Constants.H5S_SELECT_SET, npoints, coords);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dselect_elements, filespace: failed. "+ex; 
ex.printStackTrace();
			return false;
		}

		memspace = -1;
		try {
			memspace = H5.H5Screate_simple(rank, dimsf, null);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Screate_simple: memspace failed. "+ex; 
			return false;
		}
		try {
			H5.H5Sselect_none( memspace );
			H5.H5Sselect_elements( memspace, HDF5Constants.H5S_SELECT_SET, npoints, coords);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dselect_elements, memspace: failed. "+ex; 
			return false;
		}

		dbgInfo += "\nRead 2D subset of Integer data as 2D Integer";
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT),
 				memspace,
 				filespace,
 				HDF5Constants.H5P_DEFAULT,
 				outData1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed. "+ex; 
ex.printStackTrace();
			return false;
		}

		int errs = 0;
		for (i = 0; i < npoints; i++) {
			int jj = (int)coords[i][0];
			int ii = (int)coords[i][1];
			if (outData1[jj][ii] !=( data1[jj][ii])) {
				errs++;
				dbgInfo += "\n* bad data at: ["+jj+"]["+ii+"]  original: "+data1[jj][ii]+" read back as "+outData1[jj][ii];
			}
		}

 		try {
 			H5.H5Sclose(memspace);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Sclose: memspace after read 1 failed. "+ex;
			return false;
		}


		memspace = -1;
		try {
			memspace = H5.H5Screate_simple(rank, dimsf, null);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Screate_simple: memspace failed. "+ex; 
			return false;
		}
		try {
			H5.H5Sselect_none( memspace );
			H5.H5Sselect_elements(memspace, HDF5Constants.H5S_SELECT_SET, npoints, coords2);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dselect_elements, memspace 2: failed. "+ex; 
			return false;
		}

		dbgInfo += "\nRead 2D subset of Integer data as 1D Integer";
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT),
 				memspace,
 				filespace,
 				HDF5Constants.H5P_DEFAULT,
 				outData1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

		for (i = 0; i < npoints; i++) {
			int jjj = (int)coords[i][0];
			int iii = (int)coords[i][1];
			int jj = (int)coords2[i][0];
			int ii = (int)coords2[i][1];
			if (outData1[jj][ii] !=( data1[jjj][iii])) {
				errs++;
				dbgInfo += "\n** bad data at: ["+jj+"]["+ii+"]  original: "+data1[jjj][iii]+" read back as "+outData1[jj][ii];
			}
		}
 		try {
 			H5.H5Sclose(memspace);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Sclose: memspace after read 1 failed. "+ex;
			return false;
		}
 		try {
 			H5.H5Sclose(filespace);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Sclose: filespace after read 1 failed. "+ex;
			return false;
		}
 		try {
 			H5.H5Dclose(tdataset1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: dataset1 after read 1 failed. "+ex;
			return false;
		}

 		try {
 			H5.H5Fclose(tfile);
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\nH5Fclose(): FAILED "+ex;
 			return false;
 		}
 		dbgInfo += "\nReadPoints2D(): OK ";
 		return true;
 	}

	private boolean testWritePoints2D()
	{
		String fileName = FILE_ROOT+"WritePoints2D.h5";
		String dataset1Name = "IntegerArray";
		int nx = 100;
		int ny = 200;
		int rank = 2;
		int status;
		int i, j;
		int filespace;
		int memspace;
		int file, dataset1;
		int tfile, tdataset1;
		int datatype1, dataspace1;   /* handles */
		long[] dimsf = {100, 200};	 /* dataset dimensions */
		long[] offset = new long[2];	 /* selection corner */
		long[] size = new long[2];	 /* selection range */
		int[][] data1 = new int[nx][ny];			/* data to write */
		int[][] data2 = new int[nx][ny];			/* data to write */
		int[][] outData1 = new int[nx][ny];
		int npoints = 10;
		long[][] coords = new long [npoints][rank];
		long[][] coords2 = new long [npoints][rank];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++) {
				data1[j][i] = (int)(j*ny+i);
				data2[j][i] = (int)(-(j*ny+i));
			}
		}


		// Create a new file using H5F_ACC_TRUNC access,
		// default file creation properties, and default file
		// access properties.
		file = -1;
		try {
		file = H5.H5Fcreate(fileName,
			HDF5Constants.H5F_ACC_TRUNC,
			HDF5Constants.H5P_DEFAULT,
			HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5create() Failed, exception: "+ex;
			return false;
		}

		// Describe the size of the array and create the data space
		// for fixed size dataset.
		dataspace1 = -1;
		try {
			dataspace1 = H5.H5Screate_simple(rank, dimsf, null);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Screate_simple: dataspace1 failed. "+ex; 
			return false;
		}

		// Define datatype for the data in the file.
		datatype1 = -1;
		try {
			datatype1 = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT));
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tcopy: datatype1 failed. "+ex; 
			return false; 
		}

		// Create a new dataset within the file using defined dataspace and
		// datatype and default dataset creation properties.
		try {
			dataset1 = H5.H5Dcreate(file, dataset1Name, 
				datatype1, dataspace1,
				HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dcreate() dataset1 Failed, exception: "+ex;
			return false;
		}

		// Write the data to the dataset using default transfer properties.
		try {
			status = H5.H5Dwrite(dataset1,
			H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT),
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5P_DEFAULT,
			data1);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dwrite(): failed dataset1 "+ex;
			return false;
		}

		try {
			H5.H5Sclose(dataspace1);
		} catch (Exception ex) {
			dbgInfo += "\nH5Sclose: dataspace 1 failed. "+ex;
			return false;
		}
		try {
			H5.H5Tclose(datatype1);
		} catch (Exception ex) {
			dbgInfo += "\nH5Tclose: datatype1 failed. "+ex;
			return false;
		}
		try {
			H5.H5Dclose(dataset1);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: dataset1 failed. "+ex;
			return false;
		}

		try {
			H5.H5Fclose(file);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Fclose(): FAILED "+ex;
			return false;
		}

 
 		tfile = -1;
 		try {
 		tfile = H5.H5Fopen(fileName,
                                 HDF5Constants.H5F_ACC_RDWR,
                                 HDF5Constants.H5P_DEFAULT);
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Fopen() second open Failed, exception: "+ex;
 			return false;
 		}
 
 		try {
 			tdataset1 = H5.H5Dopen(tfile, dataset1Name); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Dopen() dataset1 Failed, exception: "+ex;
 			return false;
 		}

		for (i = 0; i < npoints; i++) {
			coords[i][0] = (long)(10+i); /* diagonal */
			coords[i][1] = (long)(10+i);
			coords2[i][0] = (long)i; /* one row */
			coords2[i][1] = (long)0;
		}
		filespace = -1;
		try {
			filespace = H5.H5Dget_space(tdataset1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dget_space, filespace: failed. "+ex; 
ex.printStackTrace();
			return false;
		}

		try {
			H5.H5Sselect_elements( filespace, HDF5Constants.H5S_SELECT_SET, npoints, coords);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dselect_elements, filespace: failed. "+ex; 
			return false;
		}

		memspace = -1;
		try {
			memspace = H5.H5Screate_simple(rank, dimsf, null);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Screate_simple: memspace failed. "+ex; 
			return false;
		}
		try {
			H5.H5Sselect_elements( memspace, HDF5Constants.H5S_SELECT_SET, npoints, coords);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dselect_elements, memspace: failed. "+ex; 
			return false;
		}

		dbgInfo += "\nWrite subset of Integer data as Integer";
 		try {
 			status = H5.H5Dwrite(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT),
 				memspace,
 				filespace,
 				HDF5Constants.H5P_DEFAULT,
 				data2);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dwrite: failed. "+ex; 
			return false;
		}
 		try {
 			H5.H5Sclose(memspace);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Sclose: memspace after write failed. "+ex;
			return false;
		}
 		try {
 			H5.H5Sclose(filespace);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Sclose: filespace after write failed. "+ex;
			return false;
		}
 		try {
 			H5.H5Dclose(tdataset1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: dataset1 after write failed. "+ex;
			return false;
		}

 		try {
 			H5.H5Fclose(tfile);
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\nH5Fclose(): FAILED "+ex;
 			return false;
 		}

 		tfile = -1;
 		try {
 		tfile = H5.H5Fopen(fileName,
                                 HDF5Constants.H5F_ACC_RDWR,
                                 HDF5Constants.H5P_DEFAULT);
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Fopen() second open Failed, exception: "+ex;
 			return false;
 		}
 
 		try {
 			tdataset1 = H5.H5Dopen(tfile, dataset1Name); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Dopen() dataset1 Failed, exception: "+ex;
 			return false;
 		}
 
		dbgInfo += "\nre-read all Integer data ";
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}


		int errs = 0;
		for (j = 0; j < 10; j++) {
			for (i = 0; i < ny; i++) {
			if (outData1[j][i] !=( data1[j][i])) {
				errs++;
				dbgInfo += "\n* bad data at: ["+j+"]["+i+"]  original: "+data1[j][i]+" read back as "+outData1[j][i];
				return false;
			}
			}
		}	
		for (i = 0; i < npoints; i++) {
			int jj = (int)coords[i][0];
			int ii = (int)coords[i][1];
			if (outData1[jj][ii] !=( data2[jj][ii])) {
				errs++;
				dbgInfo += "\n* bad data at: ["+jj+"]["+ii+"]  original: "+data1[jj][ii]+" read back as "+outData1[jj][ii];
				return false;
			}
		}
		for (j = 10; j < 20; j++) {
			for (i = 0; i < ny; i++) {
			if (i == j) continue;
			if (outData1[j][i] !=( data1[j][i])) {
				errs++;
				dbgInfo += "\n* bad data at: ["+j+"]["+i+"]  original: "+data1[j][i]+" read back as "+outData1[j][i];
				return false;
			}
			}
		}	
		for (j = 20; j < nx; j++) {
			for (i = 0; i < ny; i++) {
			if (outData1[j][i] !=( data1[j][i])) {
				errs++;
				dbgInfo += "\n* bad data at: ["+j+"]["+i+"]  original: "+data1[j][i]+" read back as "+outData1[j][i];
				return false;
			}
			}
		}	
 
 		try {
 			H5.H5Dclose(tdataset1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: dataset1 after read 1 failed. "+ex;
			return false;
		}
 		try {
 			H5.H5Fclose(tfile);
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\nH5Fclose(): FAILED "+ex;
 			return false;
 		}
 		dbgInfo += "\nWritePoints2D(): OK ";
 		return true;
 	}


}
