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

public class TestCompound implements TestModule
{
	private String FILE_ROOT = null;


	String dbgInfo;
	boolean testResult = false;

	public TestCompound()
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
		String desc = "Test simple 1D compound DT write & read";
		return desc;
	}

	public String getTestDescription() {
		String desc = "Test simple 1D Compound data types, write and read either way.";
		return desc;
	}

	public void runTest() {
		testResult = false;
		boolean res;
		int ntests = 6;
		int passed = 0;
		dbgInfo = "\n\n========== Test Write & read 1D compound DT  ==========";
		if ( testIntFloatCompound() ) {
			passed++;
		}
		dbgInfo += "\n\n========== Test Write & read 1D compound DT by components  ==========";
		if ( testIntFloatComponentsCompound() ) {
			passed++;
		}
		dbgInfo += "\n\n========== Test Write & read 1D compound DT  complex ==========";
		if ( testComplexCompound() ) {
			passed++;
		}
		dbgInfo += "\n\n========== Test Write & read 1D compound DT complex by components  ==========";
		if ( testComplexComponentsCompound() ) {
			passed++;
		}
		dbgInfo += "\n\n========== Test Write & read 1D compound DT  nested ==========";
		if ( testNestedCompound() ) {
			passed++;
		}
		dbgInfo += "\n\n========== Test Write & read 1D compound DT  nested by field==========";
		if ( testNestedCompoundComplex() ) {
			passed++;
		}
		dbgInfo += "\n\n========== simple Compoud DT tests complete: "+passed+" of "+ntests+" passed  ==========";
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

	private boolean testIntFloatCompound()
	{
		String fileName = FILE_ROOT+"IntFloatCompound.h5";
		String dataset1Name = "CompoundArray";
		int nx = 100;
		int ny = 1;
		int rankf = 1;
		int rankb = 1;
		int status;
		int i, j;
		int file, dataset1, dataset2;
		int tfile, tdataset1, tdataset2;
		int datatype1, dataspace1;   /* handles */
		int datatype2, dataspace2;   /* handles */
		long[] dimsc = {100};	 /* dataset dimensions */
		long[] dimsb = {800};	 /* dataset dimensions */
		byte[] data = new byte[100*8];			
		byte[] outData = new byte[100*8];

		int[] II = new int[1];
		float[] FF = new float[1];
		byte [] IRec = new byte[4];
		byte [] FRec = new byte[4];
		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
				II[0] = (byte)j;
				FF[0] =  (float)j+(float)((float)j/100.0);
				IRec = HDFNativeData.intToByte(0,1,II);
				FRec = HDFNativeData.floatToByte(0,1,FF);
				System.arraycopy(IRec,0,data,(j*8),4);
				System.arraycopy(FRec,0,data,(j*8)+4,4);
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
			dataspace1 = H5.H5Screate_simple(rankf, dimsc, null);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Screate_simple: dataspace1 failed: "+ex; 
			return false;
		}

		// Define datatype for the data in the file.
		datatype1 = -1;
		try {
			datatype1 = H5.H5Tcreate(HDF5Constants.H5T_COMPOUND,8);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tcreate: datatype1 failed: "+ex; 
			return false; 
		}

		try {
			H5.H5Tinsert(datatype1, "int", 0, H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT));
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tinsert: 'int' datatype1 failed: "+ex; 
			ex.printStackTrace();
			return false; 
		}
		try {
			H5.H5Tinsert(datatype1, "float", 4, H5.J2C(HDF5CDataTypes.JH5T_NATIVE_FLOAT));
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tinsert: 'float' datatype1 failed: "+ex; 
			ex.printStackTrace();
			return false; 
		}
		datatype2 = -1;
		try {
			datatype2 = H5.H5Tcopy(datatype1);
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
			datatype1,
			dataspace1,
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5P_DEFAULT,
			data);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dwrite(): failed dataset1 "+ex;
			ex.printStackTrace();
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
 			dataset2 = H5.H5Dopen(tfile, dataset1Name); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Dopen() dataset1 Failed, exception: "+ex;
 			return false;
 		}
 
		dbgInfo += "\nRead compound data as byte";
 		try {
 			status = H5.H5Dread(dataset2,
				datatype2,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData);
 		} catch (Exception ex) {
			ex.printStackTrace();
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
			if (outData[j] != data[j]) {
				dbgInfo += "\n* bad data at: ["+j+"]  original: "+data[j]+" read back as "+outData[j];
 			}
 		}
 
 		try {
 			H5.H5Tclose(datatype2);
 		} catch (Exception ex) {
			dbgInfo += "H5Tclose: type2 after read 1 failed. "+ex;
			return false;
		}
 		try {
 			H5.H5Dclose(dataset2);
 		} catch (Exception ex) {
			dbgInfo += "H5Dclose: dataset1 after read 1 failed. "+ex;
			return false;
		}
 		try {
 			H5.H5Fclose(tfile);
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\nH5Fclose(): FAILED "+ex;
 			return false;
 		}
 		dbgInfo += "\nIntFloatCompound(): OK ";
 		return true;
 	}

	private boolean testIntFloatComponentsCompound()
	{
		String fileName = FILE_ROOT+"IntFloatComponentsCompound.h5";
		String dataset1Name = "CompoundArray";
		int nx = 100;
		int ny = 1;
		int rankf = 1;
		int rankb = 1;
		int status;
		int i, j;
		int file, dataset1, dataset2;
		int tfile, tdataset1, tdataset2;
		int datatype1, dataspace1;   /* handles */
		int datatype2, dataspace2;   /* handles */
		long[] dimsc = {100};	 /* dataset dimensions */
		long[] dimsb = {800};	 /* dataset dimensions */
		byte[] data = new byte[100*8];			
		int[] outIData = new int[100];
		float[] outFData = new float[100];
		int[] inIData = new int[100];
		float[] inFData = new float[100];

		int[] II = new int[1];
		float[] FF = new float[1];
		byte [] IRec = new byte[4];
		byte [] FRec = new byte[4];
		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
				inIData[j] = II[0] = (byte)j;
				inFData[j] = FF[0] = (float)j+(float)((float)j/100.0);
				IRec = HDFNativeData.intToByte(0,1,II);
				FRec = HDFNativeData.floatToByte(0,1,FF);
				System.arraycopy(IRec,0,data,(j*8),4);
				System.arraycopy(FRec,0,data,(j*8)+4,4);
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
			dataspace1 = H5.H5Screate_simple(rankf, dimsc, null);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Screate_simple: dataspace1 failed: "+ex; 
			return false;
		}

		// Define datatype for the data in the file.
		datatype1 = -1;
		try {
			datatype1 = H5.H5Tcreate(HDF5Constants.H5T_COMPOUND,8);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tcreate: datatype1 failed: "+ex; 
			return false; 
		}

		try {
			H5.H5Tinsert(datatype1, "int", 0, H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT));
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tinsert: 'int' datatype1 failed: "+ex; 
			ex.printStackTrace();
			return false; 
		}
		try {
			H5.H5Tinsert(datatype1, "float", 4, H5.J2C(HDF5CDataTypes.JH5T_NATIVE_FLOAT));
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tinsert: 'float' datatype1 failed: "+ex; 
			ex.printStackTrace();
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
			datatype1,
			dataspace1,
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5P_DEFAULT,
			data);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dwrite(): failed dataset1 "+ex;
			ex.printStackTrace();
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
 			dataset2 = H5.H5Dopen(tfile, dataset1Name); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Dopen() dataset1 Failed, exception: "+ex;
 			return false;
 		}

		// Define datatype for the interger field of the compound DT.
		datatype2 = -1;
		try {
			datatype2 = H5.H5Tcreate(HDF5Constants.H5T_COMPOUND,4);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tcreate: datatype2 failed: "+ex; 
			return false; 
		}

		try {
			H5.H5Tinsert(datatype2, "int", 0, H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT));
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tinsert: 'int' datatype2 failed: "+ex; 
			ex.printStackTrace();
			return false; 
		}
 
		dbgInfo += "\nRead integer part of compound data as int";
 		try {
 			status = H5.H5Dread(dataset2,
				datatype2,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outIData);
 		} catch (Exception ex) {
			ex.printStackTrace();
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

		int errs = 0;
 		for (j = 0; j < nx; j++)
 		{
			if ((errs < 10) && (outIData[j] != inIData[j])) {
				errs++;
				dbgInfo += "\n* bad data at: ["+j+"]  original: "+inIData[j]+" read back as "+outIData[j];
 			}
 		}
 
 		try {
 			H5.H5Tclose(datatype2);
 		} catch (Exception ex) {
			dbgInfo += "H5Tclose: type2 after read 2 failed. "+ex;
			return false;
		}


		// Define datatype for the float field of the compound DT.
		datatype2 = -1;
		try {
			datatype2 = H5.H5Tcreate(HDF5Constants.H5T_COMPOUND,4);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tcreate: datatype2 failed: "+ex; 
			return false; 
		}

		try {
			H5.H5Tinsert(datatype2, "float", 0, H5.J2C(HDF5CDataTypes.JH5T_NATIVE_FLOAT));
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tinsert: 'float' datatype2 failed: "+ex; 
			ex.printStackTrace();
			return false; 
		}
 
		dbgInfo += "\nRead float part of compound data as int";
 		try {
 			status = H5.H5Dread(dataset2,
				datatype2,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outFData);
 		} catch (Exception ex) {
			ex.printStackTrace();
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

		//errs = 0;
 		for (j = 0; j < nx; j++)
 		{
			if ((errs < 20) && (outFData[j] != inFData[j])) {
				errs++;
				dbgInfo += "\n* bad data at: ["+j+"]  original: "+inFData[j]+" read back as "+outFData[j];
 			}
 		}
 
 		try {
 			H5.H5Tclose(datatype2);
 		} catch (Exception ex) {
			dbgInfo += "H5Tclose: type2 after read 3 failed. "+ex;
			return false;
		}

 		try {
 			H5.H5Dclose(dataset2);
 		} catch (Exception ex) {
			dbgInfo += "H5Dclose: dataset1 after read 1 failed. "+ex;
			return false;
		}

 		try {
 			H5.H5Fclose(tfile);
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\nH5Fclose(): FAILED "+ex;
 			return false;
 		}

		if (errs > 0) {
			dbgInfo += "\nIntFloatCompound(): "+errs+" errors";
			return false;
		}
 		dbgInfo += "\nIntFloatCompound(): OK ";
 		return true;
 	}

	private boolean testComplexCompound()
	{
		String fileName = FILE_ROOT+"ComplexCompound.h5";
		String dataset1Name = "CompoundArray";
		int nx = 100;
		int ny = 1;
		int rankf = 1;
		int rankb = 1;
		int status;
		int i, j;
		int file, dataset1, dataset2;
		int tfile, tdataset1, tdataset2;
		int datatype1, dataspace1;   /* handles */
		int datatype2, dataspace2;   /* handles */
		long[] dimsc = {100};	 /* dataset dimensions */
		long[] dimsb = {1600};	 /* dataset dimensions */
		byte[] data = new byte[100*16];			
		byte[] outData = new byte[100*16];

		double[] RR = new double[1];
		double[] II = new double[1];
		byte [] IRec = new byte[8];
		byte [] RRec = new byte[8];
		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
				II[0] =  (double)j+(double)((double)j/100.0);
				RR[0] =  (double)j+(double)((double)j/100.0);
				IRec = HDFNativeData.doubleToByte(0,1,II);
				RRec = HDFNativeData.doubleToByte(0,1,RR);
				System.arraycopy(RRec,0,data,(j*16),8);
				System.arraycopy(IRec,0,data,(j*16)+8,8);
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
			dataspace1 = H5.H5Screate_simple(rankf, dimsc, null);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Screate_simple: dataspace1 failed: "+ex; 
			return false;
		}

		// Define datatype for the data in the file.
		datatype1 = -1;
		try {
			datatype1 = H5.H5Tcreate(HDF5Constants.H5T_COMPOUND,16);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tcreate: datatype1 failed: "+ex; 
			return false; 
		}

		try {
			H5.H5Tinsert(datatype1, "real", 0, H5.J2C(HDF5CDataTypes.JH5T_NATIVE_DOUBLE));
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tinsert: 'real' part datatype1 failed: "+ex; 
			ex.printStackTrace();
			return false; 
		}
		try {
			H5.H5Tinsert(datatype1, "imaginary", 8, H5.J2C(HDF5CDataTypes.JH5T_NATIVE_DOUBLE));
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tinsert: 'imaginary' datatype1 failed: "+ex; 
			ex.printStackTrace();
			return false; 
		}
		datatype2 = -1;
		try {
			datatype2 = H5.H5Tcopy(datatype1);
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
			datatype1,
			dataspace1,
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5P_DEFAULT,
			data);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dwrite(): failed dataset1 "+ex;
			ex.printStackTrace();
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
 			dataset2 = H5.H5Dopen(tfile, dataset1Name); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Dopen() dataset1 Failed, exception: "+ex;
 			return false;
 		}
 
		dbgInfo += "\nRead compound data as byte";
 		try {
 			status = H5.H5Dread(dataset2,
				datatype2,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData);
 		} catch (Exception ex) {
			ex.printStackTrace();
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
			if (outData[j] != data[j]) {
				dbgInfo += "\n* bad data at: ["+j+"]  original: "+data[j]+" read back as "+outData[j];
 			}
 		}
 
 		try {
 			H5.H5Tclose(datatype2);
 		} catch (Exception ex) {
			dbgInfo += "H5Tclose: type2 after read 1 failed. "+ex;
			return false;
		}
 		try {
 			H5.H5Dclose(dataset2);
 		} catch (Exception ex) {
			dbgInfo += "H5Dclose: dataset1 after read 1 failed. "+ex;
			return false;
		}
 		try {
 			H5.H5Fclose(tfile);
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\nH5Fclose(): FAILED "+ex;
 			return false;
 		}
 		dbgInfo += "\nComplexCompound(): OK ";
 		return true;
 	}

	private boolean testComplexComponentsCompound()
	{
		String fileName = FILE_ROOT+"ComplexComponentsCompound.h5";
		String dataset1Name = "CompoundArray";
		int nx = 100;
		int ny = 1;
		int rankf = 1;
		int rankb = 1;
		int status;
		int i, j;
		int file, dataset1, dataset2;
		int tfile, tdataset1, tdataset2;
		int datatype1, dataspace1;   /* handles */
		int datatype2, dataspace2;   /* handles */
		long[] dimsc = {100};	 /* dataset dimensions */
		long[] dimsb = {1600};	 /* dataset dimensions */
		byte[] data = new byte[100*16];			
		double[] outIData = new double[100];
		double[] outRData = new double[100];
		double[] inIData = new double[100];
		double[] inRData = new double[100];

		double[] II = new double[1];
		double[] RR = new double[1];
		byte [] IRec = new byte[4];
		byte [] RRec = new byte[4];
		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
				inRData[j] = RR[0] = (double)j+(double)((double)j/100.0);
				inIData[j] = II[0] = (double)j+(double)((double)j/100.0);
				IRec = HDFNativeData.doubleToByte(0,1,II);
				RRec = HDFNativeData.doubleToByte(0,1,RR);
				System.arraycopy(RRec,0,data,(j*16),8);
				System.arraycopy(IRec,0,data,(j*16)+8,8);
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
			dataspace1 = H5.H5Screate_simple(rankf, dimsc, null);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Screate_simple: dataspace1 failed: "+ex; 
			return false;
		}

		// Define datatype for the data in the file.
		datatype1 = -1;
		try {
			datatype1 = H5.H5Tcreate(HDF5Constants.H5T_COMPOUND,16);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tcreate: datatype1 failed: "+ex; 
			return false; 
		}

		try {
			H5.H5Tinsert(datatype1, "real", 0, H5.J2C(HDF5CDataTypes.JH5T_NATIVE_DOUBLE));
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tinsert: 'real' datatype1 failed: "+ex; 
			ex.printStackTrace();
			return false; 
		}
		try {
			H5.H5Tinsert(datatype1, "imaginary", 8, H5.J2C(HDF5CDataTypes.JH5T_NATIVE_DOUBLE));
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tinsert: 'imaginary' datatype1 failed: "+ex; 
			ex.printStackTrace();
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
			datatype1,
			dataspace1,
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5P_DEFAULT,
			data);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dwrite(): failed dataset1 "+ex;
			ex.printStackTrace();
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
 			dataset2 = H5.H5Dopen(tfile, dataset1Name); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Dopen() dataset1 Failed, exception: "+ex;
 			return false;
 		}

		// Define datatype for the interger field of the compound DT.
		datatype2 = -1;
		try {
			datatype2 = H5.H5Tcreate(HDF5Constants.H5T_COMPOUND,8);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tcreate: datatype2 failed: "+ex; 
			return false; 
		}

		try {
			H5.H5Tinsert(datatype2, "real", 0, H5.J2C(HDF5CDataTypes.JH5T_NATIVE_DOUBLE));
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tinsert: 'real' datatype2 failed: "+ex; 
			ex.printStackTrace();
			return false; 
		}
 
		dbgInfo += "\nRead integer part of compound data as int";
 		try {
 			status = H5.H5Dread(dataset2,
				datatype2,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outRData);
 		} catch (Exception ex) {
			ex.printStackTrace();
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

		int errs = 0;
 		for (j = 0; j < nx; j++)
 		{
			if ((errs < 10) && (outRData[j] != inRData[j])) {
				errs++;
				dbgInfo += "\n* bad data at: ["+j+"]  original: "+inRData[j]+" read back as "+outRData[j];
 			}
 		}
 
 		try {
 			H5.H5Tclose(datatype2);
 		} catch (Exception ex) {
			dbgInfo += "H5Tclose: type2 after read 2 failed. "+ex;
			return false;
		}


		// Define datatype for the float field of the compound DT.
		datatype2 = -1;
		try {
			datatype2 = H5.H5Tcreate(HDF5Constants.H5T_COMPOUND,8);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tcreate: datatype2 failed: "+ex; 
			return false; 
		}

		try {
			H5.H5Tinsert(datatype2, "imaginary", 0, H5.J2C(HDF5CDataTypes.JH5T_NATIVE_DOUBLE));
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tinsert: 'imaginary' datatype2 failed: "+ex; 
			ex.printStackTrace();
			return false; 
		}
 
		dbgInfo += "\nRead imaginary part of compound data as double";
 		try {
 			status = H5.H5Dread(dataset2,
				datatype2,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outIData);
 		} catch (Exception ex) {
			ex.printStackTrace();
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

		//errs = 0;
 		for (j = 0; j < nx; j++)
 		{
			if ((errs < 20) && (outIData[j] != inIData[j])) {
				errs++;
				dbgInfo += "\n* bad data at: ["+j+"]  original: "+inIData[j]+" read back as "+outIData[j];
 			}
 		}
 
 		try {
 			H5.H5Tclose(datatype2);
 		} catch (Exception ex) {
			dbgInfo += "H5Tclose: type2 after read 3 failed. "+ex;
			return false;
		}

 		try {
 			H5.H5Dclose(dataset2);
 		} catch (Exception ex) {
			dbgInfo += "H5Dclose: dataset1 after read 1 failed. "+ex;
			return false;
		}

 		try {
 			H5.H5Fclose(tfile);
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\nH5Fclose(): FAILED "+ex;
 			return false;
 		}

		if (errs > 0) {
			dbgInfo += "\nComplexCompound(): "+errs+" errors";
			return false;
		}
 		dbgInfo += "\nComplexCompound(): OK ";
 		return true;
 	}

	private boolean testNestedCompound()
	{
		String fileName = FILE_ROOT+"NestedCompound.h5";
		String dataset1Name = "CompoundArray";
		int nx = 100;
		int ny = 1;
		int rankf = 1;
		int rankb = 1;
		int status;
		int i, j;
		int file, dataset1, dataset2;
		int tfile, tdataset1, tdataset2;
		int datatype1, dataspace1;   /* handles */
		int datatype2, dataspace2;   /* handles */

//
//     Records will be:

//     struct {
//            double realpart;
//            double imaginarypart;
//            struct {
//                   int intpart
//                   float floatpart
//            } embedded
//     }
		long[] dimsc = {100};	 /* dataset dimensions */
		long[] dimsb = {2400};	 /* dataset dimensions */
		byte[] data = new byte[100*24];			
		byte[] outData = new byte[100*24];

		int[] IR = new int[1];
		float[] FR = new float[1];
		double[] RR = new double[1];
		double[] II = new double[1];
		byte [] IRec = new byte[8];
		byte [] RRec = new byte[8];
		byte [] iRec = new byte[4];
		byte [] fRec = new byte[4];
		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
				II[0] =  (double)j+(double)((double)j/100.0);
				RR[0] =  (double)j+(double)((double)j/100.0);
				IR[0] =  (int)j+(int)((int)j/100.0);
				FR[0] =  (float)j+(float)((float)j/100.0);
				IRec = HDFNativeData.doubleToByte(0,1,II);
				RRec = HDFNativeData.doubleToByte(0,1,RR);
				iRec = HDFNativeData.intToByte(0,1,IR);
				fRec = HDFNativeData.floatToByte(0,1,FR);
				System.arraycopy(RRec,0,data,(j*24),8);
				System.arraycopy(IRec,0,data,(j*24)+8,8);
				System.arraycopy(iRec,0,data,(j*24)+12,4);
				System.arraycopy(fRec,0,data,(j*24)+16,4);
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
			dataspace1 = H5.H5Screate_simple(rankf, dimsc, null);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Screate_simple: dataspace1 failed: "+ex; 
			return false;
		}

		// Define datatype for the data in the file.
		datatype2 = -1;
		try {
			datatype2 = H5.H5Tcreate(HDF5Constants.H5T_COMPOUND,8);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tcreate: datatype1 failed: "+ex; 
			return false; 
		}

		try {
			H5.H5Tinsert(datatype2, "intpart", 0, H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT));
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tinsert: 'int' part datatype2 failed: "+ex; 
			ex.printStackTrace();
			return false; 
		}
		try {
			H5.H5Tinsert(datatype2, "floatpart", 4, H5.J2C(HDF5CDataTypes.JH5T_NATIVE_FLOAT));
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tinsert: 'floatpart' datatype2 failed: "+ex; 
			ex.printStackTrace();
			return false; 
		}

		// Define datatype for the data in the file.
		datatype1 = -1;
		try {
			datatype1 = H5.H5Tcreate(HDF5Constants.H5T_COMPOUND,24);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tcreate: datatype1 failed: "+ex; 
			return false; 
		}

		try {
			H5.H5Tinsert(datatype1, "real", 0, H5.J2C(HDF5CDataTypes.JH5T_NATIVE_DOUBLE));
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tinsert: 'real' part datatype1 failed: "+ex; 
			ex.printStackTrace();
			return false; 
		}
		try {
			H5.H5Tinsert(datatype1, "imaginary", 8, H5.J2C(HDF5CDataTypes.JH5T_NATIVE_DOUBLE));
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tinsert: 'imaginary' datatype1 failed: "+ex; 
			ex.printStackTrace();
			return false; 
		}
		try {
			H5.H5Tinsert(datatype1, "extension", 16, datatype2);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tinsert: 'extension' datatype2 into datatype1 failed: "+ex; 
			ex.printStackTrace();
			return false; 
		}
		datatype2 = -1;
		try {
			datatype2 = H5.H5Tcopy(datatype1);
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
			datatype1,
			dataspace1,
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5P_DEFAULT,
			data);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dwrite(): failed dataset1 "+ex;
			ex.printStackTrace();
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
 			dataset2 = H5.H5Dopen(tfile, dataset1Name); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Dopen() dataset1 Failed, exception: "+ex;
 			return false;
 		}
 
		dbgInfo += "\nRead compound data as byte";
 		try {
 			status = H5.H5Dread(dataset2,
				datatype2,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outData);
 		} catch (Exception ex) {
			ex.printStackTrace();
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

 		for (j = 0; j < 100*24; j++)
 		{
			if (outData[j] != data[j]) {
				dbgInfo += "\n* bad data at: ["+j+"]  original: "+data[j]+" read back as "+outData[j];
 			}
 		}
 
 		try {
 			H5.H5Tclose(datatype2);
 		} catch (Exception ex) {
			dbgInfo += "H5Tclose: type2 after read 1 failed. "+ex;
			return false;
		}
 		try {
 			H5.H5Dclose(dataset2);
 		} catch (Exception ex) {
			dbgInfo += "H5Dclose: dataset1 after read 1 failed. "+ex;
			return false;
		}
 		try {
 			H5.H5Fclose(tfile);
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\nH5Fclose(): FAILED "+ex;
 			return false;
 		}
 		dbgInfo += "\nNestedCompound(): OK ";
 		return true;
 	}

	private boolean testNestedCompoundComplex()
	{
		String fileName = FILE_ROOT+"NestedCompoundComplex.h5";
		String dataset1Name = "CompoundArray";
		int nx = 100;
		int ny = 1;
		int rankf = 1;
		int rankb = 1;
		int status;
		int i, j;
		int file, dataset1, dataset2;
		int tfile, tdataset1, tdataset2;
		int datatype1, dataspace1;   /* handles */
		int datatype2, dataspace2;   /* handles */

//
//     Records will be:

//     struct {
//            double realpart;
//            double imaginarypart;
//            struct {
//                   int intpart
//                   float floatpart
//            } embedded
//     }
		long[] dimsc = {100};	 /* dataset dimensions */
		long[] dimsb = {2400};	 /* dataset dimensions */
		byte[] data = new byte[100*24];			
		byte[] outData = new byte[100*24];
		double[] outReal = new double[100];
		double[] outImaginary = new double[100];
		int[] outextInt = new int[100];
		float[] outextFloat = new float[100];
		double[] inReal = new double[100];
		double[] inImaginary = new double[100];
		int[] inextInt = new int[100];
		float[] inextFloat = new float[100];

		int[] IR = new int[1];
		float[] FR = new float[1];
		double[] RR = new double[1];
		double[] II = new double[1];
		byte [] IRec = new byte[8];
		byte [] RRec = new byte[8];
		byte [] iRec = new byte[4];
		byte [] fRec = new byte[4];
		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
				II[0] =  (double)j+(double)((double)j/100.0);
				inImaginary[j] = II[0];
				RR[0] =  (double)j+(double)((double)j/100.0);
				inReal[j] = RR[0];
				IR[0] =  (int)j+(int)((int)j/100.0);
				inextInt[j] = IR[0];
				FR[0] =  (float)j+(float)((float)j/100.0);
				inextFloat[j] = FR[0];
				IRec = HDFNativeData.doubleToByte(0,1,II);
				RRec = HDFNativeData.doubleToByte(0,1,RR);
				iRec = HDFNativeData.intToByte(0,1,IR);
				fRec = HDFNativeData.floatToByte(0,1,FR);
				System.arraycopy(RRec,0,data,(j*24),8);
				System.arraycopy(IRec,0,data,(j*24)+8,8);
				System.arraycopy(iRec,0,data,(j*24)+12,4);
				System.arraycopy(fRec,0,data,(j*24)+16,4);
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
			dataspace1 = H5.H5Screate_simple(rankf, dimsc, null);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Screate_simple: dataspace1 failed: "+ex; 
			return false;
		}

		// Define datatype for the data in the file.
		datatype2 = -1;
		try {
			datatype2 = H5.H5Tcreate(HDF5Constants.H5T_COMPOUND,8);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tcreate: datatype1 failed: "+ex; 
			return false; 
		}

		try {
			H5.H5Tinsert(datatype2, "intpart", 0, H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT));
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tinsert: 'int' part datatype2 failed: "+ex; 
			ex.printStackTrace();
			return false; 
		}
		try {
			H5.H5Tinsert(datatype2, "floatpart", 4, H5.J2C(HDF5CDataTypes.JH5T_NATIVE_FLOAT));
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tinsert: 'floatpart' datatype2 failed: "+ex; 
			ex.printStackTrace();
			return false; 
		}

		// Define datatype for the data in the file.
		datatype1 = -1;
		try {
			datatype1 = H5.H5Tcreate(HDF5Constants.H5T_COMPOUND,24);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tcreate: datatype1 failed: "+ex; 
			return false; 
		}

		try {
			H5.H5Tinsert(datatype1, "real", 0, H5.J2C(HDF5CDataTypes.JH5T_NATIVE_DOUBLE));
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tinsert: 'real' part datatype1 failed: "+ex; 
			ex.printStackTrace();
			return false; 
		}
		try {
			H5.H5Tinsert(datatype1, "imaginary", 8, H5.J2C(HDF5CDataTypes.JH5T_NATIVE_DOUBLE));
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tinsert: 'imaginary' datatype1 failed: "+ex; 
			ex.printStackTrace();
			return false; 
		}
		try {
			H5.H5Tinsert(datatype1, "extension", 16, datatype2);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tinsert: 'extension' datatype2 into datatype1 failed: "+ex; 
			ex.printStackTrace();
			return false; 
		}
		datatype2 = -1;
		try {
			datatype2 = H5.H5Tcopy(datatype1);
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
			dbgInfo += "\n\nH5Dcreate() 0 dataset1 Failed, exception: "+ex;
ex.printStackTrace();
			return false;
		}

		// Write the data to the dataset using default transfer properties.
		try {
			status = H5.H5Dwrite(dataset1,
			datatype1,
			dataspace1,
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5P_DEFAULT,
			data);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dwrite(): failed dataset1 "+ex;
			ex.printStackTrace();
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
 			dataset2 = H5.H5Dopen(tfile, dataset1Name); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Dopen() dataset1 Failed, exception: "+ex;
 			return false;
 		}

		dataspace1 = -1;
		try {
			dataspace1 = H5.H5Dget_space(dataset2);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Dget_space: dataspace1 failed: "+ex; 
			return false;
		}

		datatype1 = -1;
		try {
			datatype1 = H5.H5Tcreate(HDF5Constants.H5T_COMPOUND,8);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tcreate: datatype1 failed: "+ex; 
			return false; 
		}

		try {
			H5.H5Tinsert(datatype1, "real", 0, H5.J2C(HDF5CDataTypes.JH5T_NATIVE_DOUBLE));
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tinsert: 'real' part datatype1 failed: "+ex; 
			ex.printStackTrace();
			return false; 
		}
		// datatype and default dataset creation properties.
		try {
			dataset1 = H5.H5Dopen(tfile, dataset1Name); 
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dopen() 1 dataset1 Failed, exception: "+ex;
ex.printStackTrace();
			return false;
		}
		dbgInfo += "\nRead 'real' part compound data as double";
 		try {
 			status = H5.H5Dread(dataset1,
				datatype1,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outReal);
 		} catch (Exception ex) {
			ex.printStackTrace();
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
			if (outReal[j] != inReal[j]) {
				dbgInfo += "\n* bad data at: ["+j+"]  original: "+inReal[j]+" read back as "+outReal[j];
 			}
 		}

		datatype1 = -1;
		try {
			datatype1 = H5.H5Tcreate(HDF5Constants.H5T_COMPOUND,8);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tcreate: datatype1 failed: "+ex; 
			return false; 
		}

		try {
			H5.H5Tinsert(datatype1, "imaginary", 0, H5.J2C(HDF5CDataTypes.JH5T_NATIVE_DOUBLE));
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tinsert: 'imaginary' part datatype1 failed: "+ex; 
			ex.printStackTrace();
			return false; 
		}
		// datatype and default dataset creation properties.
		try {
			dataset1 = H5.H5Dopen(tfile, dataset1Name ); 
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5open() 2 dataset1 Failed, exception: "+ex;
ex.printStackTrace();
			return false;
		}
		dbgInfo += "\nRead 'imaginary' part compound data as double";
 		try {
 			status = H5.H5Dread(dataset1,
				datatype1,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outImaginary);
 		} catch (Exception ex) {
			ex.printStackTrace();
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
			if (outImaginary[j] != inImaginary[j]) {
				dbgInfo += "\n* bad data at: ["+j+"]  original: "+inImaginary[j]+" read back as "+outImaginary[j];
 			}
 		}
 
		// Define datatype for the data in the file.
		datatype2 = -1;
		try {
			datatype2 = H5.H5Tcreate(HDF5Constants.H5T_COMPOUND,4);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tcreate: read datatype2 failed: "+ex; 
			return false; 
		}

		try {
			H5.H5Tinsert(datatype2, "intpart", 0, H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT));
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tinsert: 'int' part datatype2 failed: "+ex; 
			ex.printStackTrace();
			return false; 
		}

		// Define datatype for the data in the file.
		datatype1 = -1;
		try {
			datatype1 = H5.H5Tcreate(HDF5Constants.H5T_COMPOUND,4);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tcreate: datatype1 failed: "+ex; 
			return false; 
		}

		try {
			H5.H5Tinsert(datatype1, "extension", 0, datatype2);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tinsert: 'extension' part datatype1 failed: "+ex; 
			ex.printStackTrace();
			return false; 
		}
		// datatype and default dataset creation properties.
		try {
			dataset1 = H5.H5Dopen(tfile, dataset1Name);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dopen() 3 dataset1 Failed, exception: "+ex;
ex.printStackTrace();
			return false;
		}
		dbgInfo += "\nRead 'extension.int' data as int";
 		try {
 			status = H5.H5Dread(dataset1,
				datatype1,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outextInt);
 		} catch (Exception ex) {
			ex.printStackTrace();
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
			if (outextInt[j] != inextInt[j]) {
				dbgInfo += "\n* bad data at: ["+j+"]  original: "+inextInt[j]+" read back as "+outextInt[j];
 			}
 		}
 
		// Define datatype for the data in the file.
		datatype2 = -1;
		try {
			datatype2 = H5.H5Tcreate(HDF5Constants.H5T_COMPOUND,4);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tcreate: read datatype2 failed: "+ex; 
			return false; 
		}

		try {
			H5.H5Tinsert(datatype2, "floatpart", 0, H5.J2C(HDF5CDataTypes.JH5T_NATIVE_FLOAT));
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tinsert: 'float' part datatype2 failed: "+ex; 
			ex.printStackTrace();
			return false; 
		}

		// Define datatype for the data in the file.
		datatype1 = -1;
		try {
			datatype1 = H5.H5Tcreate(HDF5Constants.H5T_COMPOUND,4);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tcreate: datatype1 failed: "+ex; 
			return false; 
		}

		try {
			H5.H5Tinsert(datatype1, "extension", 0, datatype2);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Tinsert: 'extension' part datatype1 failed: "+ex; 
			ex.printStackTrace();
			return false; 
		}
		// datatype and default dataset creation properties.
		try {
			dataset1 = H5.H5Dopen(tfile, dataset1Name);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dopen() 4 dataset1 Failed, exception: "+ex;
ex.printStackTrace();
			return false;
		}
		dbgInfo += "\nRead 'extension.float' data as float";
 		try {
 			status = H5.H5Dread(dataset1,
				datatype1,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				outextFloat);
 		} catch (Exception ex) {
			ex.printStackTrace();
			dbgInfo += "\nH5Dread: failed. "+ex; 
			return false;
		}

 		for (j = 0; j < nx; j++)
 		{
			if (outextFloat[j] != inextFloat[j]) {
				dbgInfo += "\n* bad data at: ["+j+"]  original: "+inextFloat[j]+" read back as "+outextFloat[j];
 			}
 		}
 		try {
 			H5.H5Tclose(datatype1);
 		} catch (Exception ex) {
			dbgInfo += "H5Tclose: type2 after read 1 failed. "+ex;
			return false;
		}
 		try {
 			H5.H5Tclose(datatype2);
 		} catch (Exception ex) {
			dbgInfo += "H5Tclose: type2 after read 1 failed. "+ex;
			return false;
		}
 		try {
 			H5.H5Dclose(dataset2);
 		} catch (Exception ex) {
			dbgInfo += "H5Dclose: dataset1 after read 1 failed. "+ex;
			return false;
		}
 		try {
 			H5.H5Fclose(tfile);
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\nH5Fclose(): FAILED "+ex;
 			return false;
 		}
 		dbgInfo += "\nNestedCompoundComplex(): OK ";
 		return true;
 	}
}
