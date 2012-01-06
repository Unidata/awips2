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

public class TestByte implements TestModule
{
	private String FILE_ROOT = null;


	String dbgInfo;
	boolean testResult = false;

	public TestByte()
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
		String desc = "Test Byte/byte write & read";
		return desc;
	}

	public String getTestDescription() {
		String desc = "Test byte and Byte data types, write and read either way.";
		return desc;
	}

	public void runTest() {
		testResult = false;
		boolean res;
		int ntests = 2;
		int passed = 0;
		dbgInfo = "\n\n========== Test Write & read 2D Byte/byte flattened  ==========";
		if ( testByteByteFlat() ) {
			passed++;
		}

		dbgInfo += "\n\n========== Test Write & read 2D Byte/byte Array  ==========";
		if (testByteByteArray()) {
			passed++;
		}

		dbgInfo += "\n\n========== Byte/byte tests complete: "+passed+" of "+ntests+" passed  ==========";
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
	 * Test cases
	 */

	private boolean testByteByteFlat()
	{
		String fileName = FILE_ROOT+"ByteByteFlat.h5";
		String dataset1Name = "byteArray";
		String dataset2Name = "ByteArray";
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
		Byte[] data1 = new Byte[nx*ny];			/* data to write */
		Byte[] outData1 = new Byte[nx*ny];
		Byte[] outData3 = new Byte[nx*ny];
		byte[] data2 = new byte[nx*ny];			/* data to write */
		byte[] outData2 = new byte[nx*ny];
		byte[] outData4 = new byte[nx*ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++) {
				data1[j*ny+i] = new Byte((byte)(i));
				data2[j*ny+i] = (byte)(i);
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
			datatype1 = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_STD_I8BE));
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tcopy: datatype1 failed: "+ex; 
			return false; 
		}

/*
		try {
			H5.H5Tset_order(datatype1, 0);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tset_order: datatype1 failed: "+ex; 
			return false; 
		}
*/

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
			H5.J2C(HDF5CDataTypes.JH5T_STD_I8BE),
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
			dbgInfo += "H5Sclose: dataspace 1 failed: "+ex;
			return false;
		}
		try {
			H5.H5Tclose(datatype1);
		} catch (Exception ex) {
			dbgInfo += "H5Tclose: datatype1 failed: "+ex;
			return false;
		}
		try {
			H5.H5Dclose(dataset1);
		} catch (Exception ex) {
			dbgInfo += "H5Dclose: dataset1 failed. "+ex;
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
			datatype2 = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_STD_I8BE));
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tcopy: datatype2 failed  "+ex;
			return false; 
		}

/*
		try {
			H5.H5Tset_order(datatype2, 0);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tset_order: dattype2 failed: "+ex; 
			return false; 
		}
*/

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
			H5.J2C(HDF5CDataTypes.JH5T_STD_I8BE),
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5P_DEFAULT,
			data2);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dwrite(): dataset2 failed "+ex;
			return false;
		}

		try {
			H5.H5Sclose(dataspace2);
		} catch (Exception ex) {
			dbgInfo += "H5Sclose: dataspace2 failed. "+ex;
			return false;}
		try {
			H5.H5Tclose(datatype2);
		} catch (Exception ex) {
			dbgInfo += "H5Tclose: datatype2 failed. "+ex;
			return false;}
		try {
			H5.H5Dclose(dataset2);
		} catch (Exception ex) {
			dbgInfo += "H5Dclose: dataset2 failed. "+ex;
			return false;}

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
 
		dbgInfo += "\nRead Byte data as Byte";
		//System.out.println("Read Byte data as Byte");
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_STD_I8BE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData1);
 		} catch (Exception ex) {
		//	System.out.println("H5Dread: dataset1, outData1 failed."+ex);System.out.flush();
		//	ex.printStackTrace();
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (!outData1[j*ny+i].equals(data1[j*ny+i])) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j*ny+i]+" read back as "+outData1[j*ny+i];
 					//System.out.println("* bad data at: ["+i+"]["+j+"]  original: "+data1[j*ny+i]+" read back as "+outData1[j*ny+i]);System.out.flush();
 				}
 			}
 		}
 
 		try {
 			H5.H5Dclose(tdataset1);
 		} catch (Exception ex) {
			dbgInfo += "H5Dclose: dataset1 after read 1 failed. "+ex;
			return false;
		}

 		try {
 			tdataset1 = H5.H5Dopen(tfile, dataset1Name); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Dopen() dataset1 after read 1 Failed, exception: "+ex;
 			return false;
 		}
 
		dbgInfo += "\nRead Byte data as byte";
		//System.out.println("Read Byte data as byte");
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_STD_I8BE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData2);
 		} catch (Exception ex) {
		//	System.out.println("H5Dread: dataset1 as int failed."+ex);System.out.flush();
		//	ex.printStackTrace();
			dbgInfo += "\nH5Dread: failed. " +ex ; 
			return false;
		}
 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (outData2[j*ny+i] != data1[j*ny+i].byteValue()) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j*ny+i]+" read back as "+outData2[j*ny+i];
 				//	System.out.println("* bad data at: ["+i+"]["+j+"]  original: "+data1[j*ny+i]+" read back as "+outData2[j*ny+i]);System.out.flush();
 				}
 			}
 		}
 
 		try {
 			H5.H5Dclose(tdataset1);
 		} catch (Exception ex) {
			dbgInfo += "H5Dclose: tdataset1 read 2 failed. "+ex;
			return false;
		}

 		try {
 			tdataset2 = H5.H5Dopen(tfile, dataset2Name); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Dopen() Failed, dataset2 read 1 exception: "+ex;
 			return false;
 		}
 
		dbgInfo += "\nRead byte data as byte";
		//System.out.println("Read byte data as byte");
 		try {
 			status = H5.H5Dread(tdataset2,
 				H5.J2C(HDF5CDataTypes.JH5T_STD_I8BE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData4);
 		} catch (Exception ex) {
		//System.out.println("H5Dread: dataset 2 read 1 failed."+ex);System.out.flush();
		//	ex.printStackTrace();
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (outData4[j*ny+i] != data2[j*ny+i]) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data2[j*nx+i]+" read back as "+outData2[j*nx+i];
 				//	System.out.println("* bad data at: ["+i+"]["+j+"]  original: "+data2[j*nx+i]+" read back as "+outData4[j*nx+i]);System.out.flush();
 				}
 			}
 		}
 
 		try {
 			H5.H5Dclose(tdataset2);
 		} catch (Exception ex) {
			dbgInfo += "H5Dclose: dataset 2 read 1 failed. "+ex;
			return false;
		}


 		try {
 			tdataset2 = H5.H5Dopen(tfile, dataset2Name); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\nH5Dopen() dataset2 read 2 Failed, exception: "+ex;
 			return false;
 		}
 
		dbgInfo += "\nRead byte data as Byte";
		//System.out.println("Read byte data as Byte");
 		try {
 			status = H5.H5Dread(tdataset2,
 				H5.J2C(HDF5CDataTypes.JH5T_STD_I8BE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData3);
 		} catch (Exception ex) {
		//	System.out.println("H5Dread: dataset2 read 2 failed."+ex);System.out.flush();
		//	ex.printStackTrace();
			dbgInfo += "\nH5Dread: dataset 2 read 2 failed. " + ex; 
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (outData3[j*nx+i].byteValue() != data2[j*nx+i]) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data2[j*nx+i]+" read back as "+outData3[j*nx+i];
 				//	System.out.println("* bad data at: ["+i+"]["+j+"]  original: "+data2[j*nx+i]+" read back as "+outData3[j*nx+i]);System.out.flush();
 				}
 			}
 		}
 
 		try {
 			H5.H5Dclose(tdataset2);
 		} catch (Exception ex) {
			dbgInfo += "H5Dclose: dataset 2 read 2 failed. "+ex;
			return false;
		}

 		try {
 			H5.H5Fclose(tfile);
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\nH5Fclose(): FAILED "+ex;
 			return false;
 		}
 		dbgInfo += "\nByteByteFlat(): OK ";
 		return true;
 	}


	private boolean testByteByteArray()
	{
		String fileName = FILE_ROOT+"ByteByteArray.h5";
		String dataset1Name = "byteArray";
		String dataset2Name = "ByteArray";
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
		Byte[][] data1 = new Byte[nx][ny];			/* data to write */
		Byte[][] outData1 = new Byte[nx][ny];
		Byte[][] outData3 = new Byte[nx][ny];
		byte[][] data2 = new byte[nx][ny];			/* data to write */
		byte[][] outData2 = new byte[nx][ny];
		byte[][] outData4 = new byte[nx][ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++) {
				data1[j][i] = new Byte((byte)(i));
				data2[j][i] = (byte)(i);
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
			dbgInfo += "\nH5Screate_simple: dataspace1 failed "+ex; 
			return false;
		}

		// Define datatype for the data in the file.
		datatype1 = -1;
		try {
			datatype1 = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_STD_I8BE));
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tcopy: datatype1 failed "+ex; 
			return false; 
		}

/*
		try {
			H5.H5Tset_order(datatype1, 0);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tset_order: datatype1 failed "+ex; 
			return false; 
		}
*/

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
			H5.J2C(HDF5CDataTypes.JH5T_STD_I8BE),
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
			dbgInfo += "H5Sclose: dataspace 1 failed. "+ex;
			return false;
		}
		try {
			H5.H5Tclose(datatype1);
		} catch (Exception ex) {
			dbgInfo += "H5Tclose: datatype1 failed. "+ex;
			return false;
		}
		try {
			H5.H5Dclose(dataset1);
		} catch (Exception ex) {
			dbgInfo += "H5Dclose: dataset1 failed. "+ex;
			return false;
		}


		// Describe the size of the array and create the data space
		// for fixed size dataset.
		dataspace2 = -1;
		try {
			dataspace2 = H5.H5Screate_simple(rank, dimsf, null);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Screate_simple: dataspace2 failed "+ex; 
			return false;
		}

		// Define datatype for the data in the file.
		datatype2 = -1;
		try {
			datatype2 = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_STD_I8BE));
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tcopy: datatype2 failed "+ex; 
			return false; 
		}

/*
		try {
			H5.H5Tset_order(datatype2, 0);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tset_order: dattype2 failed "+ex; 
			return false; 
		}
*/

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
			H5.J2C(HDF5CDataTypes.JH5T_STD_I8BE),
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5P_DEFAULT,
			data2);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dwrite(): dataset2 failed "+ex;
			return false;
		}

		try {
			H5.H5Sclose(dataspace2);
		} catch (Exception ex) {
			dbgInfo += "H5Sclose: dataspace2 failed. "+ex;
			return false;
		}
		try {
			H5.H5Tclose(datatype2);
		} catch (Exception ex) {
			dbgInfo += "H5Tclose: datatype2 failed. "+ex;
			return false;
		}
		try {
			H5.H5Dclose(dataset2);
		} catch (Exception ex) {
			dbgInfo += "H5Dclose: dataset2 failed. "+ex;
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
 
		dbgInfo += "\nRead Byte data as Byte";
		//System.out.println("Read Byte data as Byte");
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_STD_I8BE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData1);
 		} catch (Exception ex) {
		//	System.out.println("H5Dread: dataset1, outData1 failed."+ex);System.out.flush();
		//	ex.printStackTrace();
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (!outData1[j][i].equals(data1[j][i])) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j][i]+" read back as "+outData1[j][i];
 		//			System.out.println("* bad data at: ["+i+"]["+j+"]  original: "+data1[j][i]+" read back as "+outData1[j][i]);System.out.flush();
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
 			tdataset1 = H5.H5Dopen(tfile, dataset1Name); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Dopen() dataset1 after read 1 Failed, exception: "+ex;
 			return false;
 		}
 
		dbgInfo += "\nRead Byte data as byte";
		//System.out.println("Read Byte data as byte");
 		try {
 			status = H5.H5Dread(tdataset1,
 				H5.J2C(HDF5CDataTypes.JH5T_STD_I8BE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData2);
 		} catch (Exception ex) {
		//	System.out.println("H5Dread: dataset1 as int failed."+ex);System.out.flush();
		//	ex.printStackTrace();
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (outData2[j][i] != data1[j][i].byteValue()) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data1[j][i]+" read back as "+outData2[j][i];
 		//			System.out.println("* bad data at: ["+i+"]["+j+"]  original: "+data1[j][i]+" read back as "+outData2[j][i]);System.out.flush();
 				}
 			}
 		}
 
 		try {
 			H5.H5Dclose(tdataset1);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: tdataset1 read 2 failed. "+ex;
			return false;
		}

 		try {
 			tdataset2 = H5.H5Dopen(tfile, dataset2Name); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Dopen() Failed, dataset2 read 1 exception: "+ex;
 			return false;
 		}
 
		dbgInfo += "\nRead byte data as byte";
	//	System.out.println("Read byte data as byte");
 		try {
 			status = H5.H5Dread(tdataset2,
 				H5.J2C(HDF5CDataTypes.JH5T_STD_I8BE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData4);
 		} catch (Exception ex) {
	//		System.out.println("H5Dread: dataset 2 read 1 failed."+ex);System.out.flush();
	//		ex.printStackTrace();
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}
 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (outData4[j][i] != data2[j][i]) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data2[j][i]+" read back as "+outData2[j][i];
 				//	System.out.println("* bad data at: ["+i+"]["+j+"]  original: "+data2[j][i]+" read back as "+outData4[j][i]);System.out.flush();
 				}
 			}
 		}
 
 		try {
 			H5.H5Dclose(tdataset2);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: dataset 2 read 1 failed. "+ex;
			return false;
		}


 		try {
 			tdataset2 = H5.H5Dopen(tfile, dataset2Name); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\nH5Dopen() dataset2 read 2 Failed, exception: "+ex;
 			return false;
 		}
 
		dbgInfo += "\nRead byte data as Byte";
		//System.out.println("Read byte data as Byte");
 		try {
 			status = H5.H5Dread(tdataset2,
 				H5.J2C(HDF5CDataTypes.JH5T_STD_I8BE),
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData3);
 		} catch (Exception ex) {
		//	System.out.println("H5Dread: dataset2 read 2 failed."+ex);System.out.flush();
		//	ex.printStackTrace();
			dbgInfo += "\nH5Dread: dataset 2 read 2 failed. "+ex; 
			return false;
		}
 		for (j = 0; j < nx; j++)
 		{
 			for (i = 0; i < ny; i++)  {
 				if (outData3[j][i].byteValue() != data2[j][i]) {
 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data2[j][i]+" read back as "+outData3[j][i];
 				//	System.out.println("* bad data at: ["+i+"]["+j+"]  original: "+data2[j][i]+" read back as "+outData3[j][i]);System.out.flush();
 				}
 			}
 		}
 
 		try {
 			H5.H5Dclose(tdataset2);
 		} catch (Exception ex) {
			dbgInfo += "H5Dclose: dataset 2 read 2 failed. "+ex;
			return false;
		}

 		try {
 			H5.H5Fclose(tfile);
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\nH5Fclose(): FAILED "+ex;
 			return false;
 		}
 		dbgInfo += "\nByteByteArray(): OK ";
 		return true;
 	}

}
