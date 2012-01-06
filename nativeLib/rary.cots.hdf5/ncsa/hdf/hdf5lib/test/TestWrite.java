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

public class TestWrite  implements TestModule
{
	private String FILE_ROOT = null;


	String dbgInfo;
	boolean testResult = false;

	public TestWrite()
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
		String desc = "Test basic data types, write & read";
		return desc;
	}

	public String getTestDescription() {
		String desc = "Test basic data types, write and read.";
		return desc;
	}

	public void runTest() {
		testResult = false;
		boolean res;
		int ntests = 10;
		int passed = 0;
		dbgInfo = "\n\n========== Test Write 2D Int flattened  ==========";
		if (testWriteInt2DFlat()) {
			passed++;
		}
		dbgInfo += "\n\n========== Test Write 2D FLoat flattened  ==========";
		if (testWriteFloat2DFlat()) {
			passed++;
		}
		dbgInfo += "\n\n========== Test Write 2D Double flattened  ==========";
		if (testWriteDouble2DFlat()) {
			passed++;
		}
		dbgInfo += "\n\n========== Test Write 2D Short flattened  ==========";
		if (testWriteShort2DFlat()) {
			passed++;
		}
		dbgInfo += "\n\n========== Test Write 2D Long flattened  ==========";
		if (testWriteLong2DFlat()) {
			passed++;
		}

		dbgInfo += "\n\n========== Test Write 2D Int Array  ==========";
		if (testWriteInt2DArray()) {
			passed++;
		}

		dbgInfo += "\n\n========== Test Write 2D Short Array  ==========";
		if (testWriteShort2DArray()) {
			passed++;
		}
		dbgInfo += "\n\n========== Test Write 2D Float Array  ==========";
		if (testWriteFloat2DArray()) {
			passed++;
		}

		dbgInfo += "\n\n========== Test Write 2D Double Array  ==========";
		if (testWriteDouble2DArray()) {
			passed++;
		}

		dbgInfo += "\n\n========== Test Write 2D Long Array  ==========";
		if (testWriteLong2DArray()) {
			passed++;
		}
		dbgInfo += "\n\n========== simple write tests complete: "+passed+" of "+ntests+" passed  ==========";
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

	private boolean testWriteInt2DFlat()
	{
		String fileName = FILE_ROOT+"int2dFlat.h5";
		String datasetName = "IntArray";
		int nx = 100;
		int ny = 200;
		int rank = 2;
		int status;
		int i, j;
		int file, dataset;		 /* file and dataset handles */
		int tfile, tdataset;		 /* file and dataset handles */
		int datatype, dataspace;   /* handles */
		int tdatatype, tdataspace;   /* handles */
		long[] dimsf = {100, 200};	 /* dataset dimensions */
		int[] data = new int[nx*ny];			/* data to write */
		int[] outData = new int[nx*ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++)
				data[j*ny+i] = j*100+i;
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
		dataspace = -1;
		try {
			dataspace = H5.H5Screate_simple(rank, dimsf, null);
		} catch (Exception ex) {
			dbgInfo += "\nH5Screate_simple: failed "+ex;
			return false;
		}

		// Define datatype for the data in the file.
		datatype = -1;
		try {
			datatype = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT));
		} catch (Exception ex) {
			dbgInfo += "\nH5Tcopy: failed "+ex; 
			return false; 
		}

		// Create a new dataset within the file using defined dataspace and
		// datatype and default dataset creation properties.
		try {
			dataset = H5.H5Dcreate(file, datasetName, 
				datatype, dataspace,
				  HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dcreate() Failed, exception: "+ex;
			return false;
		}

		// Write the data to the dataset using default transfer properties.
		try {
			status = H5.H5Dwrite(dataset,
			H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT),
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5P_DEFAULT,
			data);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dwrite(): failed "+ex;
			return false;
		}

		try {
			H5.H5Sclose(dataspace);
		} catch (Exception ex) {dbgInfo += "\nH5Sclose: failed.";return false;}
		try {
			H5.H5Tclose(datatype);
		} catch (Exception ex) {
			dbgInfo += "\nH5Tclose: failed: "+ex;
			return false;
		}
		try {
			H5.H5Dclose(dataset);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: failed: "+ex;
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
			dbgInfo += "\n\nH5create() Failed, exception: "+ex;
			return false;
		}

		try {
			tdataset = H5.H5Dopen(tfile, datasetName); 
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dcreate() Failed, exception: "+ex;
			return false;
		}

		try {
			status = H5.H5Dread(tdataset,
				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT),
				HDF5Constants.H5S_ALL,
				HDF5Constants.H5S_ALL,
				HDF5Constants.H5P_DEFAULT,
				outData);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed: "+ex;
			return false;
		}
		for (j = 0; j < nx; j++)
		{
			for (i = 0; i < ny; i++)  {
				if (outData[j*ny+i] != data[j*ny+i]) {
					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data[j*ny+i]+" read back as "+outData[j*ny+i];
				}
			}
		}

		try {
			H5.H5Dclose(tdataset);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: failed: "+ex;
			return false;
		}
		try {
			H5.H5Fclose(tfile);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Fclose(): FAILED "+ex;
			return false;
		}
		dbgInfo += "\nWrite2DIntFlat(): OK ";
		return true;
	}

	private boolean testWriteFloat2DFlat()
	{
		String fileName = FILE_ROOT+"float2dFlat.h5";
		String datasetName = "FloatArray";
		int nx = 100;
		int ny = 200;
		int rank = 2;
		int status;
		int i, j;
		int file, dataset;		 /* file and dataset handles */
		int tfile, tdataset;		 /* file and dataset handles */
		int datatype, dataspace;   /* handles */
		int tdatatype, tdataspace;   /* handles */
		long[] dimsf = {100, 200};	 /* dataset dimensions */
		float[] data = new float[nx*ny];			/* data to write */
		float[] outData = new float[nx*ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++)
				data[j*ny+i] = (float)(j*100+i);
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
		dataspace = -1;
		try {
			dataspace = H5.H5Screate_simple(rank, dimsf, null);
		} catch (Exception ex) {
			dbgInfo += "\nH5Screate_simple: failed "+ex;
			return false;
		}

		// Define datatype for the data in the file.
		datatype = -1;
		try {
			datatype = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_NATIVE_FLOAT));
		} catch (Exception ex) {
			dbgInfo += "\nH5Tcopy: failed"+ex;
			return false;
		}


		// Create a new dataset within the file using defined dataspace and
		// datatype and default dataset creation properties.
		try {
			dataset = H5.H5Dcreate(file, datasetName, 
				datatype, dataspace,
				  HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dcreate() Failed, exception: "+ex;
			return false;
		}

		// Write the data to the dataset using default transfer properties.
		try {
			status = H5.H5Dwrite(dataset,
			H5.J2C(HDF5CDataTypes.JH5T_NATIVE_FLOAT),
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5P_DEFAULT,
			data);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dwrite(): failed "+ex;
			return false;
		}

		try {
			H5.H5Sclose(dataspace);
		} catch (Exception ex) {
			dbgInfo += "\nH5Sclose: failed: "+ex;
			return false;
		}
		try {
			H5.H5Tclose(datatype);
		} catch (Exception ex) {
			dbgInfo += "\nH5Tclose: failed: "+ex;
			return false;
		}
		try {
			H5.H5Dclose(dataset);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: failed: "+ex;
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
			dbgInfo += "\n\nH5create() Failed, exception: "+ex;
			return false;
		}


		try {
			tdataset = H5.H5Dopen(tfile, datasetName); 
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dcreate() Failed, exception: "+ex;
			return false;
		}

		try {
			status = H5.H5Dread(tdataset,
				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_FLOAT),
				HDF5Constants.H5S_ALL,
				HDF5Constants.H5S_ALL,
				HDF5Constants.H5P_DEFAULT,
				outData);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed: "+ex;
			return false;
		}
		for (j = 0; j < nx; j++)
		{
			for (i = 0; i < ny; i++)  {
				if (outData[j*ny+i] != data[j*ny+i]) {
					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data[j*ny+i]+" read back as "+outData[j*ny+i];
				}
			}
		}

		try {
			H5.H5Dclose(tdataset);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: failed: "+ex;
			return false;
		}
		try {
			H5.H5Fclose(tfile);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Fclose(): FAILED "+ex;
			return false;
		}
		dbgInfo += "\nWrite2DFloatFlat(): OK ";
		return true;
	}

	private boolean testWriteShort2DFlat()
	{
		String fileName = FILE_ROOT+"short2dFlat.h5";
		String datasetName = "ShortArray";
		int nx = 100;
		int ny = 200;
		int rank = 2;
		int status;
		int i, j;
		int file, dataset;		 /* file and dataset handles */
		int tfile, tdataset;		 /* file and dataset handles */
		int datatype, dataspace;   /* handles */
		int tdatatype, tdataspace;   /* handles */
		long[] dimsf = {100, 200};	 /* dataset dimensions */
		short[] data = new short[nx*ny];			/* data to write */
		short[] outData = new short[nx*ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++)
				data[j*ny+i] = (short)(j*100+i);
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
		dataspace = -1;
		try {
			dataspace = H5.H5Screate_simple(rank, dimsf, null);
		} catch (Exception ex) {
			dbgInfo += "\nH5Screate_simple: failed "+ex;
			return false;
		}

		// Define datatype for the data in the file.
		datatype = -1;
		try {
			datatype = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_NATIVE_SHORT));
		} catch (Exception ex) {
			dbgInfo += "\nH5Tcopy: failed"+ex;
			return false;
		}


		// Create a new dataset within the file using defined dataspace and
		// datatype and default dataset creation properties.
		try {
			dataset = H5.H5Dcreate(file, datasetName, 
				datatype, dataspace,
				  HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dcreate() Failed, exception: "+ex;
			return false;
		}

		// Write the data to the dataset using default transfer properties.
		try {
			status = H5.H5Dwrite(dataset,
			H5.J2C(HDF5CDataTypes.JH5T_NATIVE_SHORT),
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5P_DEFAULT,
			data);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dwrite(): failed "+ex;
			return false;
		}

		try {
			H5.H5Sclose(dataspace);
		} catch (Exception ex) {
			dbgInfo += "\nH5Sclose: failed: "+ex;
			return false;
		}
		try {
			H5.H5Tclose(datatype);
		} catch (Exception ex) {
			dbgInfo += "\nH5Tclose: failed: "+ex;
			return false;
		}
		try {
			H5.H5Dclose(dataset);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: failed: "+ex;
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
			dbgInfo += "\n\nH5create() Failed, exception: "+ex;
			return false;
		}

		try {
			tdataset = H5.H5Dopen(tfile, datasetName); 
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dcreate() Failed, exception: "+ex;
			return false;
		}

		try {
			status = H5.H5Dread(tdataset,
				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_SHORT),
				HDF5Constants.H5S_ALL,
				HDF5Constants.H5S_ALL,
				HDF5Constants.H5P_DEFAULT,
				outData);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed: "+ex;
			return false;
		}
		for (j = 0; j < nx; j++)
		{
			for (i = 0; i < ny; i++)  {
				if (outData[j*ny+i] != data[j*ny+i]) {
					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data[j*ny+i]+" read back as "+outData[j*ny+i];
				}
			}
		}

		try {
			H5.H5Dclose(tdataset);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: failed: "+ex;
			return false;
		}
		try {
			H5.H5Fclose(tfile);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Fclose(): FAILED "+ex;
			return false;
		}
		dbgInfo += "\nWriteShort2DFlat(): OK ";
		return true;
	}

	private boolean testWriteLong2DFlat()
	{
		String fileName = FILE_ROOT+"long2dFlat.h5";
		String datasetName = "LongArray";
		int nx = 10;
		int ny = 20;
		int rank = 2;
		int status;
		int i, j;
		int file, dataset;		 /* file and dataset handles */
		int tfile, tdataset;		 /* file and dataset handles */
		int datatype, dataspace;   /* handles */
		int tdatatype, tdataspace;   /* handles */
		long[] dimsf = {10, 20};	 /* dataset dimensions */
		long[] data = new long[nx*ny];			/* data to write */
		long[] outData = new long[nx*ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++)
				data[j*ny+i] = (long)(j*100+i);
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
		dataspace = -1;
		try {
			dataspace = H5.H5Screate_simple(rank, dimsf, null);
		} catch (Exception ex) {
			dbgInfo += "\nH5Screate_simple: failed "+ex;
			return false;
		}

		// Define datatype for the data in the file.
		datatype = -1;
		try {
			datatype = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_STD_I64BE));
		} catch (Exception ex) {
			dbgInfo += "\nH5Tcopy: failed"+ex;
			return false;
		}


		// Create a new dataset within the file using defined dataspace and
		// datatype and default dataset creation properties.
		try {
			dataset = H5.H5Dcreate(file, datasetName, 
				datatype, dataspace,
				  HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dcreate() Failed, exception: "+ex;
			return false;
		}

		// Write the data to the dataset using default transfer properties.
		try {
			status = H5.H5Dwrite(dataset,
			H5.J2C(HDF5CDataTypes.JH5T_STD_I64BE),
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5P_DEFAULT,
			data);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dwrite(): failed "+ex;
			return false;
		}

		try {
			H5.H5Sclose(dataspace);
		} catch (Exception ex) {
			dbgInfo += "\nH5Sclose: failed: "+ex;
			return false;
		}
		try {
			H5.H5Tclose(datatype);
		} catch (Exception ex) {
			dbgInfo += "\nH5Tclose: failed: "+ex;
			return false;
		}
		try {
			H5.H5Dclose(dataset);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: failed: "+ex;
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
			dbgInfo += "\n\nH5create() Failed, exception: "+ex;
			return false;
		}

		// Describe the size of the array and create the data space
		// for fixed size dataset.

		try {
			tdataset = H5.H5Dopen(tfile, datasetName); 
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dcreate() Failed, exception: "+ex;
			return false;
		}

		try {
			status = H5.H5Dread(tdataset,
				H5.J2C(HDF5CDataTypes.JH5T_STD_I64BE),
				HDF5Constants.H5S_ALL,
				HDF5Constants.H5S_ALL,
				HDF5Constants.H5P_DEFAULT,
				outData);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed: "+ex;
			return false;
		}
		for (j = 0; j < nx; j++)
		{
			for (i = 0; i < ny; i++)  {
				if (outData[j*ny+i] != data[j*ny+i]) {
					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data[j*ny+i]+" read back as "+outData[j*ny+i];
				}
				System.out.flush();
			}
		}

		try {
			H5.H5Dclose(tdataset);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: failed: "+ex;
			return false;
		}
		try {
			H5.H5Fclose(tfile);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Fclose(): FAILED "+ex;
			return false;
		}
		dbgInfo += "\nWriteLong2DFlat(): OK ";
		return true;
	}

	private boolean testWriteDouble2DFlat()
	{
		String fileName = FILE_ROOT+"double2dFlat.h5";
		String datasetName = "DoubleArray";
		int nx = 100;
		int ny = 200;
		int rank = 2;
		int status;
		int i, j;
		int file, dataset;		 /* file and dataset handles */
		int tfile, tdataset;		 /* file and dataset handles */
		int datatype, dataspace;   /* handles */
		int tdatatype, tdataspace;   /* handles */
		long[] dimsf = {100, 200};	 /* dataset dimensions */
		double[] data = new double[nx*ny];			/* data to write */
		double[] outData = new double[nx*ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++)
				data[j*ny+i] = (double)(j*100+i);
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
		dataspace = -1;
		try {
			dataspace = H5.H5Screate_simple(rank, dimsf, null);
		} catch (Exception ex) {
			dbgInfo += "\nH5Screate_simple: failed "+ex;
			return false;
		}

		// Define datatype for the data in the file.
		datatype = -1;
		try {
			datatype = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_NATIVE_DOUBLE));
		} catch (Exception ex) {
			dbgInfo += "\nH5Tcopy: failed"+ex;
			return false;
		}


		// Create a new dataset within the file using defined dataspace and
		// datatype and default dataset creation properties.
		try {
			dataset = H5.H5Dcreate(file, datasetName, 
				datatype, dataspace,
				  HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dcreate() Failed, exception: "+ex;
			return false;
		}

		// Write the data to the dataset using default transfer properties.
		try {
			status = H5.H5Dwrite(dataset,
			H5.J2C(HDF5CDataTypes.JH5T_NATIVE_DOUBLE),
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5P_DEFAULT,
			data);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dwrite(): failed "+ex;
			return false;
		}

		try {
			H5.H5Sclose(dataspace);
		} catch (Exception ex) {
			dbgInfo += "\nH5Sclose: failed: "+ex;
			return false;
		}
		try {
			H5.H5Tclose(datatype);
		} catch (Exception ex) {
			dbgInfo += "\nH5Tclose: failed: "+ex;
			return false;
		}
		try {
			H5.H5Dclose(dataset);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: failed: "+ex;
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
			dbgInfo += "\n\nH5create() Failed, exception: "+ex;
			return false;
		}

		try {
			tdataset = H5.H5Dopen(tfile, datasetName); 
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dcreate() Failed, exception: "+ex;
			return false;
		}

		try {
			status = H5.H5Dread(tdataset,
				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_DOUBLE),
				HDF5Constants.H5S_ALL,
				HDF5Constants.H5S_ALL,
				HDF5Constants.H5P_DEFAULT,
				outData);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed: "+ex;
			return false;
		}
		for (j = 0; j < nx; j++)
		{
			for (i = 0; i < ny; i++)  {
				if (outData[j*ny+i] != data[j*ny+i]) {
					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data[j*ny+i]+" read back as "+outData[j*ny+i];
				}
			}
		}

		try {
			H5.H5Dclose(tdataset);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: failed: "+ex;
			return false;
		}
		try {
			H5.H5Fclose(tfile);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Fclose(): FAILED "+ex;
			return false;
		}
		dbgInfo += "\nWriteDouble2DFlat(): OK ";
		return true;
	}


	private boolean testWriteInt2DArray()
	{
		String fileName = FILE_ROOT+"int2dArray.h5";
		String datasetName = "IntArray";
		int nx = 100;
		int ny = 200;
		int rank = 2;
		int status;
		int i, j;
		int file, dataset;		 /* file and dataset handles */
		int tfile, tdataset;		 /* file and dataset handles */
		int datatype, dataspace;   /* handles */
		int tdatatype, tdataspace;   /* handles */
		long[] dimsf = {100, 200};	 /* dataset dimensions */
		int[][] data = new int[nx][ny];			/* data to write */
		int[][] outData = new int[nx][ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++)
				data[j][i] = (int)(j*100+i);
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
		dataspace = -1;
		try {
			dataspace = H5.H5Screate_simple(rank, dimsf, null);
		} catch (Exception ex) {
			dbgInfo += "\nH5Screate_simple: failed "+ex;
			return false;
		}

		// Define datatype for the data in the file.
		datatype = -1;
		try {
			datatype = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT));
		} catch (Exception ex) {
			dbgInfo += "\nH5Tcopy: failed"+ex;
			return false;
		}


		// Create a new dataset within the file using defined dataspace and
		// datatype and default dataset creation properties.
		try {
			dataset = H5.H5Dcreate(file, datasetName, 
				datatype, dataspace,
				  HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dcreate() Failed, exception: "+ex;
			return false;
		}

		// Write the data to the dataset using default transfer properties.
		try {
			status = H5.H5Dwrite(dataset,
			H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT),
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5P_DEFAULT,
			data);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dwrite(): failed "+ex;
			return false;
		}

		try {
			H5.H5Sclose(dataspace);
		} catch (Exception ex) {
			dbgInfo += "\nH5Sclose: failed: "+ex;
			return false;
		}
		try {
			H5.H5Tclose(datatype);
		} catch (Exception ex) {
			dbgInfo += "\nH5Tclose: failed: "+ex;
			return false;
		}
		try {
			H5.H5Dclose(dataset);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: failed: "+ex;
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
			dbgInfo += "\n\nH5create() Failed, exception: "+ex;
			return false;
		}

		try {
			tdataset = H5.H5Dopen(tfile, datasetName); 
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dcreate() Failed, exception: "+ex;
			return false;
		}

		try {
			status = H5.H5Dread(tdataset,
				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT),
				HDF5Constants.H5S_ALL,
				HDF5Constants.H5S_ALL,
				HDF5Constants.H5P_DEFAULT,
				outData);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed: "+ex;
			return false;
		}
		for (j = 0; j < nx; j++)
		{
			for (i = 0; i < ny; i++)  {
				if (outData[j][i] != data[j][i]) {
					dbgInfo += "\n* bad data at: ["+j+"]["+i+"]  original: "+data[j][i]+" read back as "+outData[j][i];
				}
			}
		}

		try {
			H5.H5Dclose(tdataset);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: failed: "+ex;
			return false;
		}
		try {
			H5.H5Fclose(tfile);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Fclose(): FAILED "+ex;
			return false;
		}
		dbgInfo += "\nWriteInt2DArray(): OK ";
		return true;
	}

	private boolean testWriteDouble2DArray()
	{
		String fileName = FILE_ROOT+"double2dArray.h5";
		String datasetName = "DoubleArray";
		int nx = 100;
		int ny = 200;
		int rank = 2;
		int status;
		int i, j;
		int file, dataset;		 /* file and dataset handles */
		int tfile, tdataset;		 /* file and dataset handles */
		int datatype, dataspace;   /* handles */
		int tdatatype, tdataspace;   /* handles */
		long[] dimsf = {100, 200};	 /* dataset dimensions */
		double[][] data = new double[nx][ny];			/* data to write */
		double[][] outData = new double[nx][ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++)
				data[j][i] = (double)(j*100+i);
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
		dataspace = -1;
		try {
			dataspace = H5.H5Screate_simple(rank, dimsf, null);
		} catch (Exception ex) {
			dbgInfo += "\nH5Screate_simple: failed "+ex;
			return false;
		}

		// Define datatype for the data in the file.
		datatype = -1;
		try {
			datatype = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_NATIVE_DOUBLE));
		} catch (Exception ex) {
			dbgInfo += "\nH5Tcopy: failed"+ex;
			return false;
		}


		// Create a new dataset within the file using defined dataspace and
		// datatype and default dataset creation properties.
		try {
			dataset = H5.H5Dcreate(file, datasetName, 
				datatype, dataspace,
				  HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dcreate() Failed, exception: "+ex;
			return false;
		}

		// Write the data to the dataset using default transfer properties.
		try {
			status = H5.H5Dwrite(dataset,
			H5.J2C(HDF5CDataTypes.JH5T_NATIVE_DOUBLE),
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5P_DEFAULT,
			data);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dwrite(): failed "+ex;
			return false;
		}

		try {
			H5.H5Sclose(dataspace);
		} catch (Exception ex) {
			dbgInfo += "\nH5Sclose: failed: "+ex;
			return false;
		}
		try {
			H5.H5Tclose(datatype);
		} catch (Exception ex) {
			dbgInfo += "\nH5Tclose: failed: "+ex;
			return false;
		}
		try {
			H5.H5Dclose(dataset);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: failed: "+ex;
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
			dbgInfo += "\n\nH5create() Failed, exception: "+ex;
			return false;
		}

		try {
			tdataset = H5.H5Dopen(tfile, datasetName); 
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dcreate() Failed, exception: "+ex;
			return false;
		}

		try {
			status = H5.H5Dread(tdataset,
				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_DOUBLE),
				HDF5Constants.H5S_ALL,
				HDF5Constants.H5S_ALL,
				HDF5Constants.H5P_DEFAULT,
				outData);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed: "+ex;
			return false;
		}
		for (j = 0; j < nx; j++)
		{
			for (i = 0; i < ny; i++)  {
				if (outData[j][i] != data[j][i]) {
					dbgInfo += "\n* bad data at: ["+j+"]["+i+"]  original: "+data[j][i]+" read back as "+outData[j][i];
				}
			}
		}

		try {
			H5.H5Dclose(tdataset);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: failed: "+ex;
			return false;
		}
		try {
			H5.H5Fclose(tfile);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Fclose(): FAILED "+ex;
			return false;
		}
		dbgInfo += "\nWriteDouble2DArray(): OK ";
		return true;
	}

	private boolean testWriteShort2DArray()
	{
		String fileName = FILE_ROOT+"short2dArray.h5";
		String datasetName = "ShortArray";
		int nx = 100;
		int ny = 200;
		int rank = 2;
		int status;
		int i, j;
		int file, dataset;		 /* file and dataset handles */
		int tfile, tdataset;		 /* file and dataset handles */
		int datatype, dataspace;   /* handles */
		int tdatatype, tdataspace;   /* handles */
		long[] dimsf = {100, 200};	 /* dataset dimensions */
		short[][] data = new short[nx][ny];			/* data to write */
		short[][] outData = new short[nx][ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++)
				data[j][i] = (short)(j*100+i);
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
		dataspace = -1;
		try {
			dataspace = H5.H5Screate_simple(rank, dimsf, null);
		} catch (Exception ex) {
			dbgInfo += "\nH5Screate_simple: failed "+ex;
			return false;
		}

		// Define datatype for the data in the file.
		datatype = -1;
		try {
			datatype = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_NATIVE_SHORT));
		} catch (Exception ex) {
			dbgInfo += "\nH5Tcopy: failed"+ex;
			return false;
		}


		// Create a new dataset within the file using defined dataspace and
		// datatype and default dataset creation properties.
		try {
			dataset = H5.H5Dcreate(file, datasetName, 
				datatype, dataspace,
				  HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dcreate() Failed, exception: "+ex;
			return false;
		}

		// Write the data to the dataset using default transfer properties.
		try {
			status = H5.H5Dwrite(dataset,
			H5.J2C(HDF5CDataTypes.JH5T_NATIVE_SHORT),
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5P_DEFAULT,
			data);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dwrite(): failed "+ex;
			return false;
		}

		try {
			H5.H5Sclose(dataspace);
		} catch (Exception ex) {
			dbgInfo += "\nH5Sclose: failed: "+ex;
			return false;
		}
		try {
			H5.H5Tclose(datatype);
		} catch (Exception ex) {
			dbgInfo += "\nH5Tclose: failed: "+ex;
			return false;
		}
		try {
			H5.H5Dclose(dataset);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: failed: "+ex;
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
			dbgInfo += "\n\nH5create() Failed, exception: "+ex;
			return false;
		}

		try {
			tdataset = H5.H5Dopen(tfile, datasetName); 
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dcreate() Failed, exception: "+ex;
			return false;
		}

		try {
			status = H5.H5Dread(tdataset,
				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_SHORT),
				HDF5Constants.H5S_ALL,
				HDF5Constants.H5S_ALL,
				HDF5Constants.H5P_DEFAULT,
				outData);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed: "+ex;
			return false;
		}
		for (j = 0; j < nx; j++)
		{
			for (i = 0; i < ny; i++)  {
				if (outData[j][i] != data[j][i]) {
					dbgInfo += "\n* bad data at: ["+j+"]["+i+"]  original: "+data[j][i]+" read back as "+outData[j][i];
				}
			}
		}

		try {
			H5.H5Dclose(tdataset);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: failed: "+ex;
			return false;
		}
		try {
			H5.H5Fclose(tfile);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Fclose(): FAILED "+ex;
			return false;
		}
		dbgInfo += "\nWriteDouble2DArray(): OK ";
		return true;
	}

	private boolean testWriteFloat2DArray()
	{
		String fileName = FILE_ROOT+"float2DdArray.h5";
		String datasetName = "FloatArray";
		int nx = 100;
		int ny = 200;
		int rank = 2;
		int status;
		int i, j;
		int file, dataset;		 /* file and dataset handles */
		int tfile, tdataset;		 /* file and dataset handles */
		int datatype, dataspace;   /* handles */
		int tdatatype, tdataspace;   /* handles */
		long[] dimsf = {100, 200};	 /* dataset dimensions */
		float[][] data = new float[nx][ny];			/* data to write */
		float[][] outData = new float[nx][ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++)
				data[j][i] = (float)(j*100+i);
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
		dataspace = -1;
		try {
			dataspace = H5.H5Screate_simple(rank, dimsf, null);
		} catch (Exception ex) {
			dbgInfo += "\nH5Screate_simple: failed "+ex;
			return false;
		}

		// Define datatype for the data in the file.
		datatype = -1;
		try {
			datatype = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_NATIVE_FLOAT));
		} catch (Exception ex) {
			dbgInfo += "\nH5Tcopy: failed"+ex;
			return false;
		}


		// Create a new dataset within the file using defined dataspace and
		// datatype and default dataset creation properties.
		try {
			dataset = H5.H5Dcreate(file, datasetName, 
				datatype, dataspace,
				  HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dcreate() Failed, exception: "+ex;
			return false;
		}

		// Write the data to the dataset using default transfer properties.
		try {
			status = H5.H5Dwrite(dataset,
			H5.J2C(HDF5CDataTypes.JH5T_NATIVE_FLOAT),
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5P_DEFAULT,
			data);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dwrite(): failed "+ex;
			return false;
		}

		try {
			H5.H5Sclose(dataspace);
		} catch (Exception ex) {
			dbgInfo += "\nH5Sclose: failed: "+ex;
			return false;
		}
		try {
			H5.H5Tclose(datatype);
		} catch (Exception ex) {
			dbgInfo += "\nH5Tclose: failed: "+ex;
			return false;
		}
		try {
			H5.H5Dclose(dataset);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: failed: "+ex;
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
			dbgInfo += "\n\nH5create() Failed, exception: "+ex;
			return false;
		}

		try {
			tdataset = H5.H5Dopen(tfile, datasetName); 
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dcreate() Failed, exception: "+ex;
			return false;
		}

		try {
			status = H5.H5Dread(tdataset,
				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_FLOAT),
				HDF5Constants.H5S_ALL,
				HDF5Constants.H5S_ALL,
				HDF5Constants.H5P_DEFAULT,
				outData);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed: "+ex;
			return false;
		}
		for (j = 0; j < nx; j++)
		{
			for (i = 0; i < ny; i++)  {
				if (outData[j][i] != data[j][i]) {
					dbgInfo += "\n* bad data at: ["+j+"]["+i+"]  original: "+data[j][i]+" read back as "+outData[j][i];
				}
			}
		}

		try {
			H5.H5Dclose(tdataset);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: failed: "+ex;
			return false;
		}
		try {
			H5.H5Fclose(tfile);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Fclose(): FAILED "+ex;
			return false;
		}
		dbgInfo += "\nWriteFloat2DArray(): OK ";
		return true;
	}

	private boolean testWriteLong2DArray()
	{
		String fileName = FILE_ROOT+"long2DdArray.h5";
		String datasetName = "LongArray";
		int nx = 100;
		int ny = 200;
		int rank = 2;
		int status;
		int i, j;
		int file, dataset;		 /* file and dataset handles */
		int tfile, tdataset;		 /* file and dataset handles */
		int datatype, dataspace;   /* handles */
		int tdatatype, tdataspace;   /* handles */
		long[] dimsf = {100, 200};	 /* dataset dimensions */
		long[][] data = new long[nx][ny];			/* data to write */
		long[][] outData = new long[nx][ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++)
				data[j][i] = (long)(j*100+i);
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
		dataspace = -1;
		try {
			dataspace = H5.H5Screate_simple(rank, dimsf, null);
		} catch (Exception ex) {
			dbgInfo += "\nH5Screate_simple: failed "+ex;
			return false;
		}

		// Define datatype for the data in the file.
		datatype = -1;
		try {
			datatype = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_STD_I64BE));
		} catch (Exception ex) {
			dbgInfo += "\nH5Tcopy: failed"+ex;
			return false;
		}


		// Create a new dataset within the file using defined dataspace and
		// datatype and default dataset creation properties.
		try {
			dataset = H5.H5Dcreate(file, datasetName, 
				datatype, dataspace,
				  HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dcreate() Failed, exception: "+ex;
			return false;
		}

		// Write the data to the dataset using default transfer properties.
		try {
			status = H5.H5Dwrite(dataset,
			H5.J2C(HDF5CDataTypes.JH5T_STD_I64BE),
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5P_DEFAULT,
			data);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dwrite(): failed "+ex;
			return false;
		}

		try {
			H5.H5Sclose(dataspace);
		} catch (Exception ex) {
			dbgInfo += "\nH5Sclose: failed: "+ex;
			return false;
		}
		try {
			H5.H5Tclose(datatype);
		} catch (Exception ex) {
			dbgInfo += "\nH5Tclose: failed: "+ex;
			return false;
		}
		try {
			H5.H5Dclose(dataset);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: failed: "+ex;
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
			dbgInfo += "\n\nH5create() Failed, exception: "+ex;
			return false;
		}

		try {
			tdataset = H5.H5Dopen(tfile, datasetName); 
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dcreate() Failed, exception: "+ex;
			return false;
		}

		try {
			status = H5.H5Dread(tdataset,
				H5.J2C(HDF5CDataTypes.JH5T_STD_I64BE),
				HDF5Constants.H5S_ALL,
				HDF5Constants.H5S_ALL,
				HDF5Constants.H5P_DEFAULT,
				outData);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed: "+ex;
			return false;
		}
		for (j = 0; j < nx; j++)
		{
			for (i = 0; i < ny; i++)  {
				if (outData[j][i] != data[j][i]) {
					dbgInfo += "\n* bad data at: ["+j+"]["+i+"]  original: "+data[j][i]+" read back as "+outData[j][i];
				}
			}
		}

		try {
			H5.H5Dclose(tdataset);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: failed: "+ex;
			return false;
		}
		try {
			H5.H5Fclose(tfile);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Fclose(): FAILED "+ex;
			return false;
		}
		dbgInfo += "\nWriteLong2DArray(): OK ";
		return true;
	}
}
