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

public class TestOpaque  implements TestModule
{
	private String FILE_ROOT = null;


	String dbgInfo;
	boolean testResult = false;

	public TestOpaque()
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
		String desc = "Test OPAQUE datatype, write & read";
		return desc;
	}

	public String getTestDescription() {
		String desc = "Test OPAQUE datatype, write & read";
		return desc;
	}

	public void runTest() {
		testResult = false;
		boolean res;
		int ntests = 1;
		int passed = 0;
		dbgInfo = "\n\n========== Test Write Opaque  ==========";
		if (TestOpaque()) {
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

	private boolean TestOpaque()
	{
		String fileName = FILE_ROOT+"topaque.h5";
		String datasetName = "OpaqueArray";
		int nx = 100;
		int ny = 2;
		int opaquesize = 200;
		int rank = 1;
		int status;
		int i, j;
		int file, dataset;		 /* file and dataset handles */
		int tfile, tdataset;		 /* file and dataset handles */
		int datatype, dataspace;   /* handles */
		int tdatatype, tdataspace;   /* handles */
		long[] dimsf = {2};	 /* dataset dimensions */
		int[][] data = new int[nx][ny];			/* data to write */
		int[][] outData = new int[nx][ny];

		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++)
				data[j][0] = (byte)j;
				data[j][1] = (byte)(99 - j);
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
// or should this be nx * ny??
			datatype = H5.H5Tcreate(HDF5Constants.H5T_OPAQUE, (nx * ny));
			H5.H5Tset_tag(datatype,"test opaque tag");
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
		//	H5.J2C(HDF5CDataTypes.JH5T_NATIVE_OPAQUE),
			datatype,
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5P_DEFAULT,
			data);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dwrite(): failed "+ex;
ex.printStackTrace();
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


// 		tfile = -1;
// 		try {
// 		tfile = H5.H5Fopen(fileName,
//                                 HDF5Constants.H5F_ACC_RDWR,
//                                 HDF5Constants.H5P_DEFAULT);
// 		} catch (HDF5Exception ex) {
// 			dbgInfo += "\n\nH5create() Failed, exception: "+ex;
// 			return false;
// 		}
// 
// 		try {
// 			tdataset = H5.H5Dopen(tfile, datasetName); 
// 		} catch (HDF5Exception ex) {
// 			dbgInfo += "\n\nH5Dcreate() Failed, exception: "+ex;
// 			return false;
// 		}
// 
// 		try {
// 			status = H5.H5Dread(tdataset,
// 				H5.J2C(HDF5CDataTypes.JH5T_NATIVE_INT),
// 				HDF5Constants.H5S_ALL,
// 				HDF5Constants.H5S_ALL,
// 				HDF5Constants.H5P_DEFAULT,
// 				outData);
// 		} catch (Exception ex) {
// 			dbgInfo += "\nH5Dread: failed: "+ex;
// 			return false;
// 		}
// 		for (j = 0; j < nx; j++)
// 		{
// 			for (i = 0; i < ny; i++)  {
// 				if (outData[j*ny+i] != data[j*ny+i]) {
// 					dbgInfo += "\n* bad data at: ["+i+"]["+j+"]  original: "+data[j*ny+i]+" read back as "+outData[j*ny+i];
// 				}
// 			}
// 		}
// 
// 		try {
// 			H5.H5Dclose(tdataset);
// 		} catch (Exception ex) {
// 			dbgInfo += "\nH5Dclose: failed: "+ex;
// 			return false;
// 		}
// 		try {
// 			H5.H5Fclose(tfile);
// 		} catch (HDF5Exception ex) {
// 			dbgInfo += "\nH5Fclose(): FAILED "+ex;
// 			return false;
// 		}
		dbgInfo += "\nWriteOpaque(): OK ";
		return true;
	}

}
