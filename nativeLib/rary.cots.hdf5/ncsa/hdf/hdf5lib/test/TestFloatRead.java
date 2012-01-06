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

public class TestFloatRead implements TestModule
{
	private String FILE_ROOT = null;


	String dbgInfo;
	boolean testResult = false;

	public TestFloatRead()
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
		String desc = "Test Float/float write & read";
		return desc;
	}

	public String getTestDescription() {
		String desc = "Test float and Float data types, write and read either way.";
		return desc;
	}

	public void runTest() {
		testResult = false;
		boolean res;
		int ntests = 2;
		int passed = 0;
		dbgInfo = "\n\n========== Test Write & read 2D Float/float flattened  ==========";
		if (testFloatFloatFlat() ) {
			passed++;
		}
		dbgInfo += "\n\n========== Test Write & read 2D Float/float Array  ==========";
		if (testFloatFloatArray() ) {
			passed++;
		}
		dbgInfo += "\n\n========== Float/float tests complete: "+passed+" of "+ntests+" passed  ==========";
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
	 * Test 
	 */
	private boolean testFloatFloatFlat()
	{
		String fileName = FILE_ROOT+"FloatFloatFlat.h5";
		String dataset1Name = "floatArray";
		String dataset2Name = "FloatArray";
		int nx = 100;
		int ny = 200;
		int rank = 2;
		int status;
		int errs = 0;
		int i, j;
		int file, dataset1, dataset2;
		int tfile, tdataset1, tdataset2;
		int datatype2, dataspace2;   /* handles */
		int datatype1, dataspace1;   /* handles */
		long[] dimsf = {100, 200};	 /* dataset dimensions */
		Float[] data1 = new Float[nx*ny];			/* data to write */
		Float[] outData1 = new Float[nx*ny];
		Float[] outData3 = new Float[nx*ny];
		float[] data2 = new float[nx*ny];			/* data to write */
		float[] outData2 = new float[nx*ny];
		float[] outData4 = new float[nx*ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++) {
				data1[j*ny+i] = new Float((float)(j+(float)(float)i/100.0));
				data2[j*ny+i] = (float)(j+(float)(float)i/100.0);
			}
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
 
		dbgInfo += "\nRead Float data as Float";
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_FLOAT),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed: "+ex; 
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (!outData1[j*ny+i].equals(data1[j*ny+i])) {
					errs++;
					if (errs > 10 ) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j*ny+i]+" read back as "+outData1[j*ny+i];
 				}
 				}
 			}
 		}
 
 		try {
 			H5.H5Dclose(tdataset1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: dataset1 after read 1 failed: "+ex;
			return false;
		}

 		try {
 			tdataset1 = H5.H5Dopen(tfile, dataset1Name); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Dopen() dataset1 after read 1 Failed, exception: "+ex;
 			return false;
 		}
 
		dbgInfo += "\nRead Float data as float";
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_FLOAT),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData2);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed: "+ex; 
			return false;
		}
 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (outData2[j*ny+i] != data1[j*ny+i].floatValue()) {
				errs++;
				if (errs > 20 ) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j*ny+i]+" read back as "+outData2[j*ny+i];
 				}
 				}
 			}
 		}
 
 		try {
 			H5.H5Dclose(tdataset1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: tdataset1 read 2 failed: "+ex;
			return false;
		}

 		try {
 			tdataset2 = H5.H5Dopen(tfile, dataset2Name); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Dopen() Failed, dataset2 read 1 exception: "+ex;
 			return false;
 		}
 
		dbgInfo += "\nRead float data as float";
 		try {
 			status = H5.H5Dread(tdataset2,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_FLOAT),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData4);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed: "+ex; 
			return false;
		}
 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (outData4[j*ny+i] != data2[j*ny+i]) {
				errs++;
				if (errs > 30 ) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data2[j*nx+i]+" read back as "+outData2[j*nx+i];
 				}
 				}
 			}
 		}
 
 		try {
 			H5.H5Dclose(tdataset2);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: dataset 2 read 1 failed: "+ex;
			return false;
		}

 		try {
 			tdataset2 = H5.H5Dopen(tfile, dataset2Name); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\nH5Dopen() dataset2 read 2 Failed, exception: "+ex;
 			return false;
 		}
 
		dbgInfo += "\nRead float data as Float";
 		try {
 			status = H5.H5Dread(tdataset2,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_FLOAT),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData3);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: dataset 2 read 2 failed: "+ex; 
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (outData3[j*nx+i].floatValue() != data2[j*nx+i]) {
				errs++;
				if (errs > 40 ) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data2[j*nx+i]+" read back as "+outData3[j*nx+i];
 				}
 				}
 			}
 		}
 
 		try {
 			H5.H5Dclose(tdataset2);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: dataset 2 read 2 failed: "+ex;
			return false;
		}

 		try {
 			H5.H5Fclose(tfile);
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\nH5Fclose(): FAILED "+ex;
 			return false;
 		}
		if (errs > 0) {
			dbgInfo += "\nFloatFloatFlat(): at least "+errs+" errors";
			return false;
		} else {
			dbgInfo += "\nFloatFloatFlat(): OK ";
			return true;
		}
 	}

	private boolean testFloatFloatArray()
	{
		String fileName = FILE_ROOT+"FloatFloatArray.h5";
		String dataset1Name = "floatArray";
		String dataset2Name = "FloatArray";
		int nx = 100;
		int ny = 200;
		int rank = 2;
		int status;
		int errs = 0;
		int i, j;
		int file, dataset1, dataset2;
		int tfile, tdataset1, tdataset2;
		int datatype2, dataspace2;   /* handles */
		int datatype1, dataspace1;   /* handles */
		long[] dimsf = {100, 200};	 /* dataset dimensions */
		Float[][] data1 = new Float[nx][ny];			/* data to write */
		Float[][] outData1 = new Float[nx][ny];
		Float[][] outData3 = new Float[nx][ny];
		float[][] data2 = new float[nx][ny];			/* data to write */
		float[][] outData2 = new float[nx][ny];
		float[][] outData4 = new float[nx][ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++) {
				data1[j][i] = new Float((float)(j+(float)(float)i/100.0));
				data2[j][i] = (float)(j+(float)(float)i/100.0);
			}
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
 
		dbgInfo += "\nRead Float data as Float";
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_FLOAT),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed: "+ex; 
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (!outData1[j][i].equals(data1[j][i])) {
				errs++;
				if (errs > 10 ) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j][i]+" read back as "+outData1[j][i];
 				}
 				}
 			}
 		}
 
 		try {
 			H5.H5Dclose(tdataset1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: dataset1 after read 1 failed: "+ex;
			return false;
		}

 		try {
 			tdataset1 = H5.H5Dopen(tfile, dataset1Name); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Dopen() dataset1 after read 1 Failed, exception: "+ex;
 			return false;
 		}
 
		dbgInfo += "\nRead Float data as float";
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_FLOAT),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData2);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed: "+ex; 
			return false;
		}
 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (outData2[j][i] != data1[j][i].floatValue()) {
				errs++;
				if (errs > 20 ) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j][i]+" read back as "+outData2[j][i];
 				}
 				}
 			}
 		}
 
 		try {
 			H5.H5Dclose(tdataset1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: tdataset1 read 2 failed: "+ex;
			return false;
		}

 		try {
 			tdataset2 = H5.H5Dopen(tfile, dataset2Name); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Dopen() Failed, dataset2 read 1 exception: "+ex;
 			return false;
 		}
 
		dbgInfo += "\nRead float data as float";
 		try {
 			status = H5.H5Dread(tdataset2,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_FLOAT),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData4);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed: "+ex; 
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (outData4[j][i] != data2[j][i]) {
				errs++;
				if (errs > 30 ) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data2[j][i]+" read back as "+outData2[j][i];
 				}
 				}
 			}
 		}
 
 		try {
 			H5.H5Dclose(tdataset2);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: dataset 2 read 1 failed: "+ex;
			return false;
		}


 		try {
 			tdataset2 = H5.H5Dopen(tfile, dataset2Name); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\nH5Dopen() dataset2 read 2 Failed, exception: "+ex;
 			return false;
 		}
 
		dbgInfo += "\nRead float data as Float";
 		try {
 			status = H5.H5Dread(tdataset2,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_FLOAT),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData3);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: dataset 2 read 2 failed: "+ex; 
			return false;
		}
 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (outData3[j][i].floatValue() != data2[j][i]) {
				errs++;
				if (errs > 40 ) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data2[j][i]+" read back as "+outData3[j][i];
 				}
 				}
 			}
 		}
 
 		try {
 			H5.H5Dclose(tdataset2);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: dataset 2 read 2 failed: "+ex;
			return false;
		}

 		try {
 			H5.H5Fclose(tfile);
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\nH5Fclose(): FAILED "+ex;
 			return false;
 		}
		if (errs > 0) {
			dbgInfo += "\nFloatFloatArray(): at least "+errs+" errors";
			return false;
		} else {
			dbgInfo += "\nFloatFloatArray(): OK ";
			return true;
		}
 	}

}
