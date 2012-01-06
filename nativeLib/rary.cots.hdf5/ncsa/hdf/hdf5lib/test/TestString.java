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
import java.lang.reflect.*;
import ncsa.hdf.hdf5lib.*;
import ncsa.hdf.hdf5lib.exceptions.*;

public class TestString  implements TestModule
{
	private String FILE_ROOT = null;


	String dbgInfo;
	boolean testResult = false;

	public TestString()
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
		String desc = "Test String  write & read";
		return desc;
	}

	public String getTestDescription() {
		String desc = "Test Simple String data type, write and read either way.";
		return desc;
	}

	public void runTest() {
		testResult = false;
		boolean res;
		int ntests = 1;
		int passed = 0;
		dbgInfo = "\n\n========== Test Write String with Pads";
		if (testWriteString1DPadded()) {
			passed++;
		}
		dbgInfo += "\n\n========== String tests complete: "+passed+" of "+ntests+" passed  ==========";
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

	private boolean testWriteString1DZeroTerm()
	{
		String fileName = FILE_ROOT+"string1dZeroTerm.h5";
		String datasetName = "StringArray";
		int nx = 100;
		int maxStr = 100;
		int rank = 1;
		int status;
		int i, j;
		int file, dataset;		 /* file and dataset handles */
		int tfile, tdataset;		 /* file and dataset handles */
		int datatype, dataspace;   /* handles */
		int tdatatype, tdataspace;   /* handles */
		long[] dimsf = {nx};	 /* dataset dimensions */
		String[] data = new String[nx];	/* data to write */
		String[] outData = new String[nx];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			data[j] = new String("The String at [ "+j+"]");
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
			dbgInfo += "\nH5Screate_simple: failed: "+ex;
			return false;
		}

		// Define datatype for the data in the file.
		datatype = -1;
		try {
			datatype = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_C_S1));
			H5.H5Tset_size(datatype,maxStr);
			H5.H5Tset_strpad(datatype,HDF5Constants.H5T_STR_NULLPAD);
		} catch (Exception ex) {
			dbgInfo += "\nH5Tcopy: failed: "+ex;
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

		byte [][] bb = new byte[nx][maxStr];
		for (int ii = 0; ii < maxStr; ii++) {
			bb[ii] = data[ii].getBytes();
		}
		try {
			status = H5.H5Dwrite(dataset,
			datatype,
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5P_DEFAULT,
			/*data*/bb);
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
 			tdatatype = H5.H5Dget_type(tdataset); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Dcreate() Failed, exception: "+ex;
 			return false;
 		}
 
		byte [][] bab = new byte[nx][maxStr];
 		try {
 			status = H5.H5Dread(tdataset,
 				tdatatype,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				bab);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed."; return false;
		}
		for (int ii = 0; ii < maxStr; ii++) {
			outData[ii] = new String(bab[ii]);
		}
 		for (j = 0; j < nx; j++)
 		{
			if (!outData[j].equals(data[j])) {
				dbgInfo += "\n* bad data at: ["+j+"]  original: "+data[j]+" read back as "+outData[j];
 			}
 		}
 
 		try {
 			H5.H5Tclose(tdatatype);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: failed: "+ex;
			return false;
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
 		dbgInfo += "\nWrite1DStringZeroFill(): OK ";
		return true;
	}

	private boolean testWriteString1DPadded()
	{
		String fileName = FILE_ROOT+"string1DPadded.h5";
		String datasetName = "StringArray";
		int nx = 100;
		int maxStr = 100;
		int rank = 1;
		int status;
		int i, j;
		int file, dataset;		 /* file and dataset handles */
		int tfile, tdataset;		 /* file and dataset handles */
		int datatype, dataspace;   /* handles */
		int tdatatype, tdataspace;   /* handles */
		long[] dimsf = {nx};	 /* dataset dimensions */
		String[] data = new String[nx];	/* data to write */
		String[] outData = new String[nx];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			data[j] = new String("The String at [ "+j+"]");
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
			dbgInfo += "\nH5Screate_simple: failed"; return false;
		}

		// Define datatype for the data in the file.
		datatype = -1;
		try {
			datatype = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_C_S1));
			H5.H5Tset_size(datatype,maxStr);
			H5.H5Tset_strpad(datatype,HDF5Constants.H5T_STR_NULLPAD);
		} catch (Exception ex) {
			dbgInfo += "\nH5Tcopy: failed: "+ex;
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

		byte [][] bb = new byte[nx][maxStr];
		for (int ii = 0; ii < nx; ii++) {
			byte bx[] = data[ii].getBytes();
			int howMany = java.lang.reflect.Array.getLength(bx);
			if (howMany > maxStr) {
				howMany = maxStr;
			}
			System.arraycopy((Object)bx,0,(Object)(bb[ii]),0,howMany);
		}
		try {
			status = H5.H5Dwrite(dataset,
			datatype,
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5P_DEFAULT,
			/*data*/bb);
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
 			tdatatype = H5.H5Dget_type(tdataset); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Dcreate() Failed, exception: "+ex;
 			return false;
 		}
 
		byte [][] bab = new byte[nx][maxStr];
 		try {
 			status = H5.H5Dread(tdataset,
 				tdatatype,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5S_ALL,
 				HDF5Constants.H5P_DEFAULT,
 				bab);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dread: failed."; return false;
		}
		for (int ii = 0; ii < nx; ii++) {
			outData[ii] = (new String(bab[ii])).trim();
		}
 		for (j = 0; j < nx; j++)
 		{
			if (!outData[j].equals(data[j])) {
				dbgInfo += "\n* bad data at: ["+j+"]  original: "+data[j]+" read back as "+outData[j];
		//		System.out.println("* bad data at: ["+j+"]  original: "+data[j]+" read back as "+outData[j]);
		//		System.out.flush();
 			}
 		}
 
 		try {
 			H5.H5Tclose(tdatatype);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dclose: failed: "+ex;
			return false;
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
 		dbgInfo += "\nWrite1DStringPadded(): OK ";
		return true;
	}
}
