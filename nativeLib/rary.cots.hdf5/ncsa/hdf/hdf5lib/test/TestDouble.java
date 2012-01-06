/***************************************************************************
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

public class TestDouble implements TestModule
{
	private String FILE_ROOT = null;


	String dbgInfo;
	boolean testResult = false;

	public TestDouble()
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
		String desc = "Test Double/double/float write & read";
		return desc;
	}

	public String getTestDescription() {
		String desc = "Test Double data types, write and read either way.";
		return desc;
	}


	public void runTest() {
		testResult = false;
		boolean res;
		int ntests = 4;
		int passed = 0;
		dbgInfo = "\n\n========== Test Write & read Double/double flattened  ==========";
		if (testDoubleDoubleFlat() ) {
			passed++;
		}

		dbgInfo += "\n\n========== Test Write & read Double/double Array  ==========";
		if (testDoubleDoubleArray() ) {
			passed++;
		}

		dbgInfo += "\n\n========== Test Write & read 2D Double/float Flat  ==========";
		if (testDoubleFloatFlat() ) {
			passed++;
		}

		dbgInfo += "\n\n========== Test Write & read 2D Double/float Array  ==========";
		if (testDoubleFloatArray() ) {
			passed++;
		}

		dbgInfo += "\n\n========== Double tests complete: "+passed+" of "+ntests+" passed  ==========";
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
	private boolean testDoubleDoubleFlat()
	{
		String fileName = FILE_ROOT+"DoubleDoubleFlat.h5";
		String dataset1Name = "doubleArray";
		String dataset2Name = "DoubleArray";
		int nx = 100;
		int ny = 200;
		int rank = 2;
		int status;
		int i, j;
		int file, dataset1, dataset2;
		int tfile, tdataset1, tdataset2;
		int datatype2, dataspace2;   /* handles */
		int datatype1, dataspace1;   /* handles */
		long[] dimsf = {100, 200};	 /* dataset dimensions */
		Double[] data1 = new Double[nx*ny];			/* data to write */
		Double[] outData1 = new Double[nx*ny];
		Double[] outData3 = new Double[nx*ny];
		double[] data2 = new double[nx*ny];			/* data to write */
		double[] outData2 = new double[nx*ny];
		double[] outData4 = new double[nx*ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++) {
				data1[j*ny+i] = new Double((double)(j+((double)i/100.0)));
				data2[j*ny+i] = (double)(j+((double)i/100.0));
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
			dbgInfo += "\nH5Screate_simple: dataspace1 failed: "+ex; 
			return false;
		}

		// Define datatype for the data in the file.
		datatype1 = -1;
		try {
			datatype1 = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_NATIVE_DOUBLE));
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tcopy: datatype1 failed: "+ex; 
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
			H5.J2C(HDF5CDataTypes.JH5T_NATIVE_DOUBLE),
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
			dbgInfo += "\nH5Sclose: dataspace 1 failed: "+ex;
			return false;
		}
		try {
			H5.H5Tclose(datatype1);
		} catch (Exception ex) {
			dbgInfo += "\nH5Tclose: datatype1 failed: "+ex;
			return false;
		}
		try {
			H5.H5Dclose(dataset1);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: dataset1 failed: "+ex;
			return false;
		}


		// Describe the size of the array and create the data space
		// for fixed size dataset.
		dataspace2 = -1;
		try {
			dataspace2 = H5.H5Screate_simple(rank, dimsf, null);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Screate_simple: dataspace2 failed: "+ex; 
			return false;
		}

		// Define datatype for the data in the file.
		datatype2 = -1;
		try {
			datatype2 = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_NATIVE_DOUBLE));
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tcopy: datatype2 failed: "+ex; 
			return false; 
		}

		// Create a new dataset within the file using defined dataspace and
		// datatype and default dataset creation properties.
		try {
			dataset2 = H5.H5Dcreate(file, dataset2Name, 
				datatype2, dataspace2,
				HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dcreate() dataset2 Failed, exception: "+ex;
			return false;
		}

		// Write the data to the dataset using default transfer properties.
		try {
			status = H5.H5Dwrite(dataset2,
			H5.J2C(HDF5CDataTypes.JH5T_NATIVE_DOUBLE),
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5P_DEFAULT,
			data2);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dwrite(): dataset2 failed: "+ex;
			return false;
		}

		try {
			H5.H5Sclose(dataspace2);
		} catch (Exception ex) {
			dbgInfo += "\nH5Sclose: dataspace2 failed: "+ex;
			return false;
		}
		try {
			H5.H5Tclose(datatype2);
		} catch (Exception ex) {
			dbgInfo += "\nH5Tclose: datatype2 failed: "+ex;
			return false;
		}
		try {
			H5.H5Dclose(dataset2);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: dataset2 failed: "+ex;
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
 
		dbgInfo += "\nRead Double data as Double";
		// System.out.println("Read Double data as Double");
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_DOUBLE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData1);
 		} catch (Exception ex) {
			// System.out.println("\nH5Dread: dataset1, outData1 failed: "+ex);System.out.flush();
	//		// ex.printStackTrace();
			dbgInfo += "\nH5Dread: failed: "+ex; 
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (!outData1[j*ny+i].equals(data1[j*ny+i])) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j*ny+i]+" read back as "+outData1[j*ny+i];
 					// System.out.println("* bad data at: ["+i+"]["+j+"]  original: "+data1[j*ny+i]+" read back as "+outData1[j*ny+i]);System.out.flush();
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
 
		dbgInfo += "\nRead Double data as double";
		// System.out.println("Read Double data as double");
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_DOUBLE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData2);
 		} catch (Exception ex) {
			// System.out.println("\nH5Dread: dataset1 as int failed: "+ex);System.out.flush();
	//		// ex.printStackTrace();
			dbgInfo += "\nH5Dread: failed: "+ex; return false;
		}
 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (outData2[j*ny+i] != data1[j*ny+i].doubleValue()) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j*ny+i]+" read back as "+outData2[j*ny+i];
 					// System.out.println("* bad data at: ["+i+"]["+j+"]  original: "+data1[j*ny+i]+" read back as "+outData2[j*ny+i]);System.out.flush();
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
 
		dbgInfo += "\nRead double data as double";
		// System.out.println("Read double data as double");
 		try {
 			status = H5.H5Dread(tdataset2,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_DOUBLE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData4);
 		} catch (Exception ex) {
			// System.out.println("\nH5Dread: dataset 2 read 1 failed: "+ex);System.out.flush();
	//		// ex.printStackTrace();
			dbgInfo += "\nH5Dread: failed: "+ex; 
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (outData4[j*ny+i] != data2[j*ny+i]) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data2[j*nx+i]+" read back as "+outData2[j*nx+i];
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
 
		dbgInfo += "\nRead double data as Double";
		// System.out.println("Read double data as Double");
 		try {
 			status = H5.H5Dread(tdataset2,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_DOUBLE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData3);
 		} catch (Exception ex) {
			// System.out.println("\nH5Dread: dataset2 read 2 failed: "+ex);System.out.flush();
	//		// ex.printStackTrace();
			dbgInfo += "\nH5Dread: dataset 2 read 2 failed: "+ex; return false;
		}
 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (outData3[j*nx+i].doubleValue() != data2[j*nx+i]) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data2[j*nx+i]+" read back as "+outData3[j*nx+i];
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
 		dbgInfo += "\nDoubleDoubleFlat(): OK ";
 		return true;
 	}

	private boolean testDoubleDoubleArray()
	{
		String fileName = FILE_ROOT+"DoubleDoubleArray.h5";
		String dataset1Name = "doubleArray";
		String dataset2Name = "DoubleArray";
		int nx = 100;
		int ny = 200;
		int rank = 2;
		int status;
		int i, j;
		int file, dataset1, dataset2;
		int tfile, tdataset1, tdataset2;
		int datatype2, dataspace2;   /* handles */
		int datatype1, dataspace1;   /* handles */
		long[] dimsf = {100, 200};	 /* dataset dimensions */
		Double[][] data1 = new Double[nx][ny];			/* data to write */
		Double[][] outData1 = new Double[nx][ny];
		Double[][] outData3 = new Double[nx][ny];
		double[][] data2 = new double[nx][ny];			/* data to write */
		double[][] outData2 = new double[nx][ny];
		double[][] outData4 = new double[nx][ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++) {
				data1[j][i] = new Double((double)(j+((double)i/100.0)));
				data2[j][i] = (double)(j+((double)i/100.0));
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
			dbgInfo += "\nH5Screate_simple: dataspace1 failed: "+ex; 
			return false;
		}

		// Define datatype for the data in the file.
		datatype1 = -1;
		try {
			datatype1 = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_NATIVE_DOUBLE));
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tcopy: datatype1 failed: "+ex; 
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
			H5.J2C(HDF5CDataTypes.JH5T_NATIVE_DOUBLE),
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
			dbgInfo += "\nH5Sclose: dataspace 1 failed: "+ex;
			return false;
		}
		try {
			H5.H5Tclose(datatype1);
		} catch (Exception ex) {
			dbgInfo += "\nH5Tclose: datatype1 failed: "+ex;
			return false;
		}
		try {
			H5.H5Dclose(dataset1);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: dataset1 failed: "+ex;
			return false;
		}


		// Describe the size of the array and create the data space
		// for fixed size dataset.
		dataspace2 = -1;
		try {
			dataspace2 = H5.H5Screate_simple(rank, dimsf, null);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Screate_simple: dataspace2 failed: "+ex; 
			return false;
		}

		// Define datatype for the data in the file.
		datatype2 = -1;
		try {
			datatype2 = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_NATIVE_DOUBLE));
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tcopy: datatype2 failed: "+ex; 
			return false; 
		}

		// Create a new dataset within the file using defined dataspace and
		// datatype and default dataset creation properties.
		try {
			dataset2 = H5.H5Dcreate(file, dataset2Name, 
				datatype2, dataspace2,
				HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dcreate() dataset2 Failed, exception: "+ex;
			return false;
		}

		// Write the data to the dataset using default transfer properties.
		try {
			status = H5.H5Dwrite(dataset2,
			H5.J2C(HDF5CDataTypes.JH5T_NATIVE_DOUBLE),
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5P_DEFAULT,
			data2);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dwrite(): dataset2 failed: "+ex;
			return false;
		}

		try {
			H5.H5Sclose(dataspace2);
		} catch (Exception ex) {
			dbgInfo += "\nH5Sclose: dataspace2 failed: "+ex;
			return false;
		}
		try {
			H5.H5Tclose(datatype2);
		} catch (Exception ex) {
			dbgInfo += "\nH5Tclose: datatype2 failed: "+ex;
			return false;
		}
		try {
			H5.H5Dclose(dataset2);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: dataset2 failed: "+ex;
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
 
		dbgInfo += "\nRead Double data as Double";
		// System.out.println("Read Double data as Double");
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_DOUBLE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData1);
 		} catch (Exception ex) {
			// System.out.println("\nH5Dread: dataset1, outData1 failed: "+ex);System.out.flush();
	//		// ex.printStackTrace();
			dbgInfo += "\nH5Dread: failed: "+ex; 
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (!outData1[j][i].equals(data1[j][i])) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j][i]+" read back as "+outData1[j][i];
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
 
		dbgInfo += "\nRead Double data as double";
		// System.out.println("Read Double data as double");
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_DOUBLE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData2);
 		} catch (Exception ex) {
			// System.out.println("\nH5Dread: dataset1 as int failed: "+ex);System.out.flush();
	//		// ex.printStackTrace();
			dbgInfo += "\nH5Dread: failed: "+ex; 
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (outData2[j][i] != data1[j][i].doubleValue()) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j][i]+" read back as "+outData2[j][i];
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
 
		dbgInfo += "\nRead double data as double";
		// System.out.println("Read double data as double");
 		try {
 			status = H5.H5Dread(tdataset2,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_DOUBLE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData4);
 		} catch (Exception ex) {
			// System.out.println("\nH5Dread: dataset 2 read 1 failed: "+ex);System.out.flush();
	//		// ex.printStackTrace();
			dbgInfo += "\nH5Dread: failed: "+ex; 
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (outData4[j][i] != data2[j][i]) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data2[j][i]+" read back as "+outData2[j][i];
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
 
		dbgInfo += "\nRead double data as Double";
		// System.out.println("Read double data as Double");
 		try {
 			status = H5.H5Dread(tdataset2,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_DOUBLE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData3);
 		} catch (Exception ex) {
			// System.out.println("\nH5Dread: dataset2 read 2 failed: "+ex);System.out.flush();
	//		// ex.printStackTrace();
			dbgInfo += "\nH5Dread: dataset 2 read 2 failed: "+ex; 
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (outData3[j][i].doubleValue() != data2[j][i]) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data2[j][i]+" read back as "+outData3[j][i];
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
 		dbgInfo += "\nDoubleDoubleArray(): OK ";
 		return true;
 	}


	private boolean testDoubleFloatFlat()
	{
		String fileName = FILE_ROOT+"doubleFloatFlat.h5";
		String dataset1Name = "floatArray";
		String dataset2Name = "doubleArray";
		String dataset3Name = "DoubleArray";
		int nx = 100;
		int ny = 200;
		int rank = 2;
		int status;
		int i, j;
		int file, dataset1, dataset2;
		int tfile, tdataset1, tdataset2;
		int datatype2, dataspace2;   /* handles */
		int datatype1, dataspace1;   /* handles */
		long[] dimsf = {100, 200};	 /* dataset dimensions */
		float[] data1 = new float[nx*ny];			/* data to write */
		Double[] outData1 = new Double[nx*ny];
		double[] outData2 = new double[nx*ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++) {
				data1[j*ny+i] = (float)(j+(float)((float)i/100.0));
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
			dbgInfo += "\nH5Screate_simple: dataspace1 failed: "+ex; 
		return false;}

		// Define datatype for the data in the file.
		datatype1 = -1;
		try {
			datatype1 = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_NATIVE_FLOAT));
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tcopy: datatype1 failed: "+ex; 
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
			H5.J2C(HDF5CDataTypes.JH5T_NATIVE_FLOAT),
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
			dbgInfo += "\nH5Sclose: dataspace 1 failed: "+ex;
			return false;
		}
		try {
			H5.H5Tclose(datatype1);
		} catch (Exception ex) {
			dbgInfo += "\nH5Tclose: datatype1 failed: "+ex;
			return false;
		}
		try {
			H5.H5Dclose(dataset1);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: dataset1 failed: "+ex;
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
 
		dbgInfo += "\nRead float data as Double";
		// System.out.println("Read float data as Double");
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_DOUBLE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData1);
 		} catch (Exception ex) {
			// System.out.println("\nH5Dread: dataset1, outData1 failed: "+ex);System.out.flush();
			// ex.printStackTrace();
			dbgInfo += "\nH5Dread: failed: "+ex; 
			return false;
		}


 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (outData1[j*ny+i].floatValue() !=( data1[j*ny+i])) {
 
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j*ny+i]+" read back as "+outData1[j*ny+i];
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
 
		dbgInfo += "\nRead float data as double";
		// System.out.println("Read float data as double");
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_DOUBLE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData2);
 		} catch (Exception ex) {
			// System.out.println("\nH5Dread: dataset1 as int failed: "+ex);System.out.flush();
			// ex.printStackTrace();
			dbgInfo += "\nH5Dread: failed: "+ex; 
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if ((new Double(outData2[j*ny+i])).floatValue() != data1[j*ny+i]) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j*ny+i]+" read back as "+outData2[j*ny+i];
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
 			H5.H5Fclose(tfile);
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\nH5Fclose(): FAILED "+ex;
 			return false;
 		}
 		dbgInfo += "\nDoubleFloatFlat(): OK ";
 		return true;
 	}

	private boolean testDoubleFloatArray()
	{
		String fileName = FILE_ROOT+"DoubleFloatArray.h5";
		String dataset1Name = "floatArray";
		String dataset2Name = "doubleArray";
		String dataset3Name = "DoubleArray";
		int nx = 100;
		int ny = 200;
		int rank = 2;
		int status;
		int i, j;
		int file, dataset1, dataset2;
		int tfile, tdataset1, tdataset2;
		int datatype2, dataspace2;   /* handles */
		int datatype1, dataspace1;   /* handles */
		long[] dimsf = {100, 200};	 /* dataset dimensions */
		float[][] data1 = new float[nx][ny];			/* data to write */
		Double[][] outData1 = new Double[nx][ny];
		double[][] outData2 = new double[nx][ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++) {
				data1[j][i] = (float)(j+(float)((float)i/100.0));
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
			dbgInfo += "\nH5Screate_simple: dataspace1 failed: "+ex; 
			return false;
		}

		// Define datatype for the data in the file.
		datatype1 = -1;
		try {
			datatype1 = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_NATIVE_FLOAT));
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tcopy: datatype1 failed: "+ex; 
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
			H5.J2C(HDF5CDataTypes.JH5T_NATIVE_FLOAT),
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
			dbgInfo += "\nH5Sclose: dataspace 1 failed: "+ex;
			return false;
		}
		try {
			H5.H5Tclose(datatype1);
		} catch (Exception ex) {
			dbgInfo += "\nH5Tclose: datatype1 failed: "+ex;
			return false;
		}
		try {
			H5.H5Dclose(dataset1);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: dataset1 failed: "+ex;
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
 
		dbgInfo += "\nRead float data as Double";
		// System.out.println("Read float data as Double");
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_DOUBLE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData1);
 		} catch (Exception ex) {
			// System.out.println("\nH5Dread: dataset1, outData1 failed: "+ex);System.out.flush();
			// ex.printStackTrace();
			dbgInfo += "\nH5Dread: failed: "+ex; 
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (outData1[j][i].floatValue() !=( data1[j][i])) {
 
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j][i]+" read back as "+outData1[j][i];
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
 
		dbgInfo += "\nRead float data as double";
		// System.out.println("Read float data as double");
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_DOUBLE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData2);
 		} catch (Exception ex) {
			// System.out.println("\nH5Dread: dataset1 as int failed: "+ex);System.out.flush();
			// ex.printStackTrace();
			dbgInfo += "\nH5Dread: failed: "+ex; 
			return false;
		}
 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if ((new Double(outData2[j][i])).floatValue() != data1[j][i]) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j][i]+" read back as "+outData2[j][i];
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
 			H5.H5Fclose(tfile);
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\nH5Fclose(): FAILED "+ex;
 			return false;
 		}
 		dbgInfo += "\nDoubleFloatArray(): OK ";
 		return true;
 	}


}
