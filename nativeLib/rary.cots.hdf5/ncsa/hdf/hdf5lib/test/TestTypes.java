
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

public class TestTypes  implements TestModule
{
	private String FILE_ROOT = null;


	String dbgInfo;
	boolean testResult = false;

	public TestTypes()
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
		String desc = "Test simple tests of numeric types write & read";
		return desc;
	}

	public String getTestDescription() {
		String desc = "Test simple numeric data types, write and read .";
		return desc;
	}

	public void runTest() {
		testResult = false;
		boolean res;
		int ntests = 12;
		int passed = 0;
		dbgInfo = "\n\n========== Test Write 2D Integer flattened  ==========";
		if (testInteger2DFlat()) {
			passed++;
		}
		dbgInfo += "\n\n========== Test Write 2D Integer Array  ==========";
		if (testInteger2DArray()) {
			passed++;
		}
		dbgInfo += "\n\n========== Test Write 2D Short flattened  ==========";
		if (testShort2DFlat()) {
			passed++;
		}
		dbgInfo += "\n\n========== Test Write 2D Short Array  ==========";
		if (testShort2DArray()) {
			passed++;
		}
		dbgInfo += "\n\n========== Test Write 2D Byte flattened  ==========";
		if (testByte2DFlat()) {
			passed++;
		}
		dbgInfo += "\n\n========== Test Write 2D Byte Array  ==========";
		if (testByte2DArray()) {
			passed++;
		}


		dbgInfo += "\n\n========== Test Write 2D Float flattened  ==========";
		if (testFloat2DFlat()) {
			passed++;
		}
		dbgInfo += "\n\n========== Test Write 2D Float Array  ==========";
		if (testFloat2DArray()) {
			passed++;
		}

		dbgInfo += "\n\n========== Test Write 2D Double flattened  ==========";
		if (testDouble2DFlat()) {
			passed++;
		}
		dbgInfo += "\n\n========== Test Write 2D Double Array  ==========";
		if (testDouble2DArray()) {
			passed++;
		}
		dbgInfo += "\n\n========== Test Write 2D Long flattened  ==========";
		if (testLong2DFlat()) {
			passed++;
		}
		dbgInfo += "\n\n========== Test Write 2D Long Array  ==========";
		if (testLong2DArray()) {
			passed++;
		}
		dbgInfo += "\n\n========== simple number type tests complete: "+passed+" of "+ntests+" passed  ==========";
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
	private boolean testInteger2DFlat()
	{
		String fileName = FILE_ROOT+"Int2dFlat.h5";
		String datasetName = "IntegerArray";
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
		Integer[] data = new Integer[nx*ny];			/* data to write */
		Integer[] outData = new Integer[nx*ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++)
				data[j*ny+i] = new Integer(j*nx+i);
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
			datatype = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_STD_I32BE));
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
			H5.J2C(HDF5CDataTypes.JH5T_STD_I32BE),
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
			// System.out.println("\nH5Dread: failed: "+ex+ex);System.out.flush();
			// ex.printStackTrace();
			dbgInfo += "\nH5Dread: failed: "+ex; 
			return false;
		}
 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (!outData[j*ny+i].equals(data[j*ny+i])) {
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
 		dbgInfo += "\nInteger2DFlat(): OK ";
 		return true;
 	}

	private boolean testInteger2DArray()
	{
		String fileName = FILE_ROOT+"Int2dArray.h5";
		String datasetName = "IntegerArray";
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
		Integer[][] data = new Integer[nx][ny];			/* data to write */
		Integer[][] outData = new Integer[nx][ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++)
				data[j][i] = new Integer(j*nx+i);
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
			datatype = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_STD_I32BE));
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
			H5.J2C(HDF5CDataTypes.JH5T_STD_I32BE),
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
			// System.out.println("\nH5Dread: failed: "+ex+ex);System.out.flush();
			dbgInfo += "\nH5Dread: failed: "+ex; 
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (!outData[j][i].equals(data[j][i])) {
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
 		dbgInfo += "\nInteger2DArray(): OK ";
 		return true;
 	}
 
	private boolean testShort2DFlat()
	{
		String fileName = FILE_ROOT+"Short2dFlat.h5";
		String datasetName = "ShortArray";
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
		Short[] data = new Short[nx*ny];			/* data to write */
		Short[] outData = new Short[nx*ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++)
				data[j*ny+i] = new Short((short)((j*nx)+i));
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
			datatype = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_STD_I16BE));
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
			H5.J2C(HDF5CDataTypes.JH5T_STD_I16BE),
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
			// System.out.println("\nH5Dread: failed: "+ex);System.out.flush();
			dbgInfo += "\nH5Dread: failed: "+ex; 
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (!outData[j*ny+i].equals(data[j*ny+i])) {
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
 		dbgInfo += "\nShort2DFlat(): OK ";
 		return true;
 	}

	private boolean testShort2DArray()
	{
		String fileName = FILE_ROOT+"Short2dArray.h5";
		String datasetName = "ShortArray";
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
		Short[][] data = new Short[nx][ny];			/* data to write */
		Short[][] outData = new Short[nx][ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++)
				data[j][i] = new Short((short)((j*nx)+i));
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
			datatype = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_STD_I16BE));
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
			H5.J2C(HDF5CDataTypes.JH5T_STD_I16BE),
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
			// System.out.println("\nH5Dread: failed: "+ex+ex);System.out.flush();
			dbgInfo += "\nH5Dread: failed: "+ex; 
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (!outData[j][i].equals(data[j][i])) {
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
 		dbgInfo += "\nShort2DArray(): OK ";
 		return true;
 	}

	private boolean testByte2DFlat()
	{
		String fileName = FILE_ROOT+"Byte2dFlat.h5";
		String datasetName = "ByteArray";
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
		Byte[] data = new Byte[nx*ny];			/* data to write */
		Byte[] outData = new Byte[nx*ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++)
/*
				data[j*ny+i] = new Byte((byte)((j*nx)+i));
*/
				data[j*ny+i] = new Byte((byte)i);
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
			datatype = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_STD_I8BE));
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
			H5.J2C(HDF5CDataTypes.JH5T_STD_I8BE),
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
 				H5.J2C(HDF5CDataTypes.JH5T_STD_I8BE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData);
 		} catch (Exception ex) {
			// System.out.println("\nH5Dread: failed: "+ex);System.out.flush();
			dbgInfo += "\nH5Dread: failed: "+ex; 
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (!outData[j*ny+i].equals(data[j*ny+i])) {
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
 		dbgInfo += "\nByte2DFlat(): OK ";
 		return true;
 	}

	private boolean testByte2DArray()
	{
		String fileName = FILE_ROOT+"Byte2dArray.h5";
		String datasetName = "ByteArray";
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
		Byte[][] data = new Byte[nx][ny];			/* data to write */
		Byte[][] outData = new Byte[nx][ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++)
				data[j][i] = new Byte((byte)i);
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
			datatype = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_STD_I8BE));
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
			H5.J2C(HDF5CDataTypes.JH5T_STD_I8BE),
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
 				H5.J2C(HDF5CDataTypes.JH5T_STD_I8BE),
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
 				if (!outData[j][i].equals(data[j][i])) {
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
 		dbgInfo += "\nByte2DArray(): OK ";
 		return true;
 	}

	private boolean testFloat2DFlat()
	{
		String fileName = FILE_ROOT+"Float2dFlat.h5";
		String datasetName = "FloatArray";
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
		Float[] data = new Float[nx*ny];			/* data to write */
		Float[] outData = new Float[nx*ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++)
				data[j*ny+i] = new Float((float)((j*nx)+i));
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
			H5.J2C(HDF5CDataTypes.JH5T_NATIVE_FLOAT),
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5P_DEFAULT,
			data);
		} catch (Exception ex) {
			// System.out.println("\nH5Dwrite(): failed "+ex);
			// ex.printStackTrace();
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
			// System.out.println("\nH5Dread: failed: "+ex);System.out.flush();
			dbgInfo += "\nH5Dread: failed: "+ex;
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (!outData[j*ny+i].equals(data[j*ny+i])) {
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
 		dbgInfo += "\nFloat2DFlat(): OK ";
 		return true;
 	}

	private boolean testFloat2DArray()
	{
		String fileName = FILE_ROOT+"Float2dArray.h5";
		String datasetName = "FloatArray";
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
		Float[][] data = new Float[nx][ny];			/* data to write */
		Float[][] outData = new Float[nx][ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++)
				data[j][i] = new Float((float)((j*nx)+i));
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
			dbgInfo += "\nH5Tcopy: failed "+ex;
			return false; }

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
			// System.out.println("\nH5Dread: failed: "+ex+ex);System.out.flush();
			dbgInfo += "\nH5Dread: failed: "+ex;
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (!outData[j][i].equals(data[j][i])) {
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
 		dbgInfo += "\nFloat2DArray(): OK ";
 		return true;
 	}

	private boolean testDouble2DFlat()
	{
		String fileName = FILE_ROOT+"Double2dFlat.h5";
		String datasetName = "DoubleArray";
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
		Double[] data = new Double[nx*ny];			/* data to write */
		Double[] outData = new Double[nx*ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++)
				data[j*ny+i] = new Double((double)((j*nx)+i));
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
			dbgInfo += "\nH5Tcopy: failed "+ex;
			return false; }

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
			// System.out.println("\nH5Dwrite(): failed "+ex);
			// ex.printStackTrace();
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
			// System.out.println("\nH5Dread: failed: "+ex);System.out.flush();
			dbgInfo += "\nH5Dread: failed: "+ex;
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (!outData[j*ny+i].equals(data[j*ny+i])) {
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
 		dbgInfo += "\nDouble2DFlat(): OK ";
 		return true;
 	}

	private boolean testDouble2DArray()
	{
		String fileName = FILE_ROOT+"Double2dArray.h5";
		String datasetName = "DoubleArray";
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
		Double[][] data = new Double[nx][ny];			/* data to write */
		Double[][] outData = new Double[nx][ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++)
				data[j][i] = new Double((float)((j*nx)+i));
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
			dbgInfo += "\nH5Tcopy: failed "+ex;
			return false; }

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
			// System.out.println("\nH5Dread: failed: "+ex+ex);System.out.flush();
			dbgInfo += "\nH5Dread: failed: "+ex;
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (!outData[j][i].equals(data[j][i])) {
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
 		dbgInfo += "\nDouble2DArray(): OK ";
 		return true;
 	}

	private boolean testLong2DFlat()
	{
		String fileName = FILE_ROOT+"Long2dFlat.h5";
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
		Long[] data = new Long[nx*ny];			/* data to write */
		Long[] outData = new Long[nx*ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++)
				data[j*ny+i] = new Long((long)((j*nx)+i));
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
			datatype = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_STD_I16BE));
		} catch (Exception ex) {
			dbgInfo += "\nH5Tcopy: failed "+ex;
			return false; }

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
			// System.out.println("\nH5Dread: failed: "+ex);System.out.flush();
			dbgInfo += "\nH5Dread: failed: "+ex;
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (!outData[j*ny+i].equals(data[j*ny+i])) {
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
 		dbgInfo += "\nLong2DFlat(): OK ";
 		return true;
 	}

	private boolean testLong2DArray()
	{
		String fileName = FILE_ROOT+"Long2dArray.h5";
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
		Long[][] data = new Long[nx][ny];			/* data to write */
		Long[][] outData = new Long[nx][ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++)
				data[j][i] = new Long((long)((j*nx)+i));
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
			dbgInfo += "\nH5Tcopy: failed "+ex;
			return false; }

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
			// System.out.println("\nH5Dread: failed: "+ex+ex);System.out.flush();
			dbgInfo += "\nH5Dread: failed: "+ex;
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (!outData[j][i].equals(data[j][i])) {
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
 		dbgInfo += "\nLong2DArray(): OK ";
 		return true;
 	}
}
