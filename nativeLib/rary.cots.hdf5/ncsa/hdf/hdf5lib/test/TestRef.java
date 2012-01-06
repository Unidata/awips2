
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

public class TestRef  implements TestModule
{
	private String FILE_ROOT = null;


	String dbgInfo;
	boolean testResult = false;

	public TestRef()
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
		String desc = "Test Ref";
		return desc;
	}

	public String getTestDescription() {
		String desc = "Test Ref:  test Region Ref ";
		return desc;
	}

	public void runTest() {
		testResult = false;
		boolean res;
		int ntests = 2;
		int passed = 0;
		dbgInfo = "\n\n========== Test HDF5Ref ";
		if (testHDF5Ref()) {
			passed++;
		}
		dbgInfo = "\n\n========== Test HDF5RefRead ";
		if (testHDF5RefRead()) {
			passed++;
		}
		System.out.println();
		dbgInfo += "\n\n========== Ref tests complete: "+passed+" of "+ntests+" passed  ==========";
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

	private boolean testHDF5Ref()
	{
		String fileName = FILE_ROOT+"ref.h5";
		String dataset1Name = "byteArray";
		String dataset2Name = "refArray";
		int nx = 100;
		int ny = 200;
		int rank = 2;
		int status;
		int i, j;
		int file, dataset1, dataset2;
		int tfile, tdataset1, tdataset2;
		int datatype2, dataspace2;   /* handles */
		int datatype1, dataspace1;   /* handles */
		int dataspace3;
		long[] dimsf = {100, 200};	 /* dataset dimensions */
		long[] rdims = {2};	 /* ref dataset dimensions */
		byte[][] data1 = new byte[nx][ny];			/* data to write */
		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++) {
				data1[j][i] = (byte)(i);
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
			H5.H5Tclose(datatype1);
			H5.H5Dclose(dataset1);
		} catch (Exception ex) {
			dbgInfo += "close: failed: "+ex;
			return false;
		}

		//  Now create a dataset with pointers to regions
		//  in dataset1.


		// Describe the size of the array and create the data space
		// for fixed size dataset.
		dataspace2 = -1;
		try {
			dataspace2 = H5.H5Scopy(dataspace1);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Screate_simple: dataspace2 failed: "+ex; 
			return false;
		}


		long[] offset = new long[2];
		long[] size = new long[2];
		offset[0] = 50;
		offset[1] = 75;
		size[0] = 10;
		size[1] = 20;
		try {
			H5.H5Sselect_hyperslab( dataspace2, 
				HDF5Constants.H5S_SELECT_SET, 
				offset, null, size, null);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dselect_hyperslab failed: "+ex;
			return false;
		}

		// Create a reference to this dataset and region
		byte[][] ref = new byte[2][12];
		try {
			ref[0] = H5.H5Rcreate(file, dataset1Name, HDF5Constants.H5R_DATASET_REGION , dataspace2);
/*
for (int ii = 0; ii < 12; ii++) {
System.out.println("ref[ "+ii+" ] = "+ref[0][ii]);
}
*/
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Rcreate failed: "+ex;
			return false;
		}


		try {
			datatype2 = H5.H5Tcopy(H5.J2C(HDF5CDataTypes.JH5T_STD_REF_DSETREG));
		} catch (Exception ex) {
			dbgInfo += "\nH5Tcopy failed: "+ex;
			return false;
		}

		dataspace3 = -1;
		try {
			dataspace3 = H5.H5Screate_simple(1, rdims, null);
		} catch (Exception ex) { 
			dbgInfo += "\nH5Screate_simple: dataspace3 failed: "+ex; 
			return false;
		}

		// Create a new dataset within the file using defined dataspace and
		// datatype and default dataset creation properties.
		try {
			dataset2 = H5.H5Dcreate(file, dataset2Name, 
				datatype2, dataspace3,
				  HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dcreate() dataset2 Failed, exception: "+ex;
			return false;
		}

		// Write the data to the dataset using default transfer properties.
		try {
			status = H5.H5Dwrite(dataset2,
			datatype2,
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5S_ALL,
			HDF5Constants.H5P_DEFAULT,
			ref);
		} catch (Exception ex) {
			dbgInfo += "\nH5Dwrite(): dataset2 failed "+ex;
			return false;
		}

		try {
			H5.H5Sclose(dataspace2);
			H5.H5Sclose(dataspace3);
			H5.H5Tclose(datatype2);
			H5.H5Dclose(dataset2);
			H5.H5Fclose(file);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nclose(): FAILED "+ex;
			return false;
		}

 
 		dbgInfo += "\nTestRef(): OK ";
 		return true;
 	}

	private boolean testHDF5RefRead()
	{
		String fileName = FILE_ROOT+"ref.h5";
		String dataset1Name = "byteArray";
		String dataset2Name = "refArray";
		int nx = 100;
		int ny = 200;
		int rank = 2;
		int status;
		int i, j;
		int file, dataset1, dataset2;
		int tfile, tdataset1, tdataset2;
		int datatype2, dataspace2;   /* handles */
		int datatype1, dataspace1;   /* handles */
		int dataspace3;
		long[] dimsf = {100, 200};	 /* dataset dimensions */
		long[] rdims = {2};	 /* ref dataset dimensions */
		byte[][] data1 = new byte[nx][ny];			/* data to write */

		byte[][] refs = new byte[2][12];
		// Data  and output buffer initialization.
		for (j = 0; j < nx; j++) {
			for (i = 0; i < ny; i++) {
				data1[j][i] = (byte)(i);
			}
		}


		// Create a new file using H5F_ACC_TRUNC access,
		// default file creation properties, and default file
		// access properties.
		file = -1;
		try {
		file = H5.H5Fopen(fileName,
			HDF5Constants.H5F_ACC_RDONLY,
			HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5open() Failed, exception: "+ex;
			return false;
		}

		try {
			dataset1 = H5.H5Dopen(file, dataset2Name );
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dopen() dataset1 Failed, exception: "+ex;
			return false;
		}

		int cl = -1;
		try {
			datatype1 = H5.H5Dget_type( dataset1 );
			if (H5.H5Tget_class(datatype1) != HDF5Constants.H5T_REFERENCE) {
				dbgInfo += "\nH5Tget_class: FAILED  data is not a reference. "+H5.H5Tget_class(datatype1);
				return false;
			}
		} catch (HDF5Exception ex) {
			dbgInfo += "\n\nH5Dget_type() dataset1 Failed, exception: "+ex;
			return false;
		}

		try {
			status = H5.H5Dread( dataset1, datatype1, 
				HDF5Constants.H5S_ALL, 
				HDF5Constants.H5S_ALL,
				HDF5Constants.H5P_DEFAULT,
				refs);
	        } catch (HDF5Exception ex) {
			dbgInfo += "\nH5Dread (): FAILED "+ex;
			ex.printStackTrace();
		       return false;
		}

/*
		try {
			status = H5.H5Rget_object_type( dataset1, refs[0] );
			if ( status != HDF5Constants.H5G_DATASET ) {
			dbgInfo += "\nH5Rget_object_type (): object is not reference to dataset?";
		       return false;
			}
			
	        } catch (HDF5Exception ex) {
			dbgInfo += "\nH5Rget_object_type (): FAILED "+ex;
			ex.printStackTrace();
		       return false;
		}
*/
		
		// dereference the dataset....
		//byte[] theRef = new byte[12];
		int dsetref = -1;
		try {
			dsetref = H5.H5Rdereference(file,
			HDF5Constants.H5R_DATASET_REGION,refs[0]);
	        } catch (HDF5Exception ex) {
			dbgInfo += "\nH5Rdereference (): FAILED "+ex;
		       return false;
	        }

		int xdims = 0;
		long xd[] = new long[2];
		long xmd[] = new long[2];
		// check the dataspace of the whole dataset
		int dspace = -1;
		try {
			dspace = H5.H5Dget_space( dsetref );
			if (!H5.H5Sis_simple( dspace )) {
				dbgInfo += "\nSpace is not simple? ";
			       return false;
			}
			xdims = H5.H5Sget_simple_extent_ndims( dspace );
			if (xdims != 2)  {
				dbgInfo += "\nSpace is not 2D? "+xdims;
			       return false;
			}
			status = H5.H5Sget_simple_extent_dims( dspace, xd, xmd );
			if ((xd[0] != 100) || (xd[1] != 200) ) {
				dbgInfo += "\n1 Space unexpected dimensions? "+xd[0]+", "+xd[1];
			       return false;
			}
			long start[] = new long[2];
			long end[] = new long[2];
			status = H5.H5Sget_select_bounds( dspace, start, end );
			if ((start[0] != 0) || (start[1] != 0) ||
			(end[0] != 99) || (end[1] != 199) 
			) 
			{
				dbgInfo += "\n2 Space unexpected dimensions? "+start[0]+", "+start[1]+" "+end[0]+", "+end[1];
			       return false;
			}
			
	        } catch (HDF5Exception ex) {
			dbgInfo += "\nH5Dget_space (): FAILED "+ex;
		       return false;
	        }

		int sdims = 0;
		long sd[] = new long[2];
		long smd[] = new long[2];
		// check the dataspace of the selection 
		int sspace = -1;
		try {
			sspace = H5.H5Rget_region( file, 
				HDF5Constants.H5R_DATASET_REGION, refs[0] );
			if (!H5.H5Sis_simple( sspace )) {
				dbgInfo += "\nSpace is not simple? ";
			       return false;
			}
			sdims = H5.H5Sget_simple_extent_ndims( sspace );
			if (sdims != 2)  {
				dbgInfo += "\nSpace is not 2D? "+xdims;
			       return false;
			}
			status = H5.H5Sget_simple_extent_dims( sspace, sd, smd );
			if ((sd[0] != 100) || (sd[1] != 200) ) {
				dbgInfo += "\n3 Space unexpected dimensions? "+sd[0]+", "+sd[1];
			       return false;
			}
			long start[] = new long[2];
			long end[] = new long[2];
			status = H5.H5Sget_select_bounds( sspace, start, end );
			if ((start[0] != 50) || (start[1] != 75) ||
			(end[0] != 59) || (end[1] != 94) 
			) {
				dbgInfo += "\n4 Space unexpected dimensions? "+start[0]+", "+start[1]+" "+end[0]+", "+end[1];
			       return false;
			}
			
	        } catch (HDF5Exception ex) {
			dbgInfo += "\nH5Dget_space (): FAILED "+ex;
		       return false;
	        }

 		dbgInfo += "\nTestRefRead(): OK ";
 		return true;
 	}

}


