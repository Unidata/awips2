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

public class TestPalette  implements TestModule
{
	private String FILE_ROOT = null;


	String dbgInfo;
	boolean testResult = false;

	public TestPalette()
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
		String desc = "Test Palette";
		return desc;
	}

	public String getTestDescription() {
		String desc = "Test Palette:  test Image/Palette attributes";
		return desc;
	}

	public void runTest() {
		testResult = false;
		boolean res;
		int ntests = 1;
		int passed = 0;
		dbgInfo = "\n\n========== Test HDF5Palette ";
		if (testHDF5Palette()) {
			passed++;
		}
		System.out.println();
		dbgInfo += "\n\n========== Palette tests complete: "+passed+" of "+ntests+" passed  ==========";
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

	private boolean testHDF5Palette()
	{
		String fileName = FILE_ROOT+"ice2.h5";
		String dataset1Name = "iceberg_sm1";
		int status;
		int errs = 0;
		int tfile, tdataset1;
		int attr, pal;
		int attr_type, pal_type;
		int attr_space, pal_space;
		int attr_size, pal_size;
		int pal_attr;

		try {
			tfile = H5.H5Fopen(fileName,
				 HDF5Constants.H5F_ACC_RDONLY,
				 HDF5Constants.H5P_DEFAULT);
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Fopen() open Failed, exception: "+ex;
 			return false;
 		}
 
 		try {
 			tdataset1 = H5.H5Dopen(tfile, dataset1Name); 
 		} catch (HDF5Exception ex) {
 			dbgInfo += "\n\nH5Dopen() dataset1 Failed, exception: "+ex;
 			return false;
 		}
 

		attr = -1;
		dbgInfo += "\nCheck for PALETTE attribute";
 		try {
 			attr = H5.H5Aopen_name(tdataset1, "PALETTE");
 		} catch (Exception ex) {
			dbgInfo += "\nH5Aopen_name: PALETTE not found. "+ex; 
			return false;
		}

		dbgInfo += "\nCheck PALETTE attribute type";
 		try {
			attr_type = H5.H5Aget_type( attr );
 		} catch (Exception ex) {
			dbgInfo += "\nH5Aget_type: PALETTE failed. "+ex; 
			return false;
		}
		int palcl = -1;
 		try {
			palcl = H5.H5Tget_class( attr_type );
 		} catch (Exception ex) {
			dbgInfo += "\nH5Aget_class: failed. "+ex; 
			return false;
		}

		if (palcl != HDF5Constants.H5T_REFERENCE) {
			dbgInfo += "\nH5Aget_class: FAILED  PALETTE is not a reference.";
			return false;
		} else {
			dbgInfo += "\nH5Aget_class: OK  PALETTE is a reference.";
		}

		// Really should get number of references -- might be > 1
		//  ds = H5Aget_space(...)
		//  ndims = H5Sget_simple_extent_ndims(..)
		//   ndims should be 1
		//  H5Sget_simple_extent_dims( ...  dims[], ...);
		//  dims[0] > 1, there are 1 or more pal refs...
		dbgInfo += "\nRead the palette reference.";
		byte [] buf = new byte[8];
		try {
			status = H5.H5Aread( attr, attr_type, buf);
	        } catch (HDF5Exception ex) {
			dbgInfo += "\nH5Aread (): FAILED "+ex;
			ex.printStackTrace();
		       return false;
		}

		pal = -1;
		try {
			pal = H5.H5Rdereference(tfile,
			HDF5Constants.H5R_OBJECT,buf);
	        } catch (HDF5Exception ex) {
			dbgInfo += "\nH5Rdereference (): FAILED "+ex;
		       return false;
	        }

		dbgInfo += "\nCheck PALETTE type";
		pal_attr = -1;
 		try {
 			pal_attr = H5.H5Aopen_name(pal, "CLASS");
 		} catch (Exception ex) {
			dbgInfo += "\nH5Aopen_name: palette CLASS not found. "+ex; 
		       return false;
		}

		pal_type = -1;
 		try {
			pal_type = H5.H5Aget_type( pal_attr );
 		} catch (Exception ex) {
			dbgInfo += "\nH5Aget_type: PALETTE failed. "+ex; 
			return false;
		}

		int cl = -1;
 		try {
			cl = H5.H5Tget_class( pal_type);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Dget_class: failed. "+ex; 
			return false;
		}

		if (cl != HDF5Constants.H5T_STRING) {
			dbgInfo += "\nH5Aget_type: FAILED  PALETTE is not a string. ";
			return false;
		}

		int atsize = 0;
 		try {
			atsize =H5.H5Tget_size(pal_type);
 		} catch (Exception ex) {
			dbgInfo += "\nH5Tget_size: failed. "+ex; 
			return false;
		}

		if (atsize <= 0) {
			dbgInfo += "\nH5Tget_size: failed. "; 
			return false;
		}
		
		byte[] pname = new byte[atsize];
		try {
			status = H5.H5Aread( pal_attr, pal_type, pname);
	        } catch (HDF5Exception ex) {
			dbgInfo += "\nH5Aread (): FAILED "+ex;
		       return false;
		}

		String palname = new String(pname);
		palname = palname.trim();

		if (!palname.equals("PALETTE") ) {
			dbgInfo += "\nimage points to something that is not a palette? FAILED ";
			return false;
		}
		//  get other pal attributes, ...

 		try {
			pal_type = H5.H5Dget_type(pal);
	        } catch (HDF5Exception ex) {
			dbgInfo += "\nread palette FAILED "+ex;
		       return false;
		}
		byte[] pd = new byte[256*3];

 		try {
			status = H5.H5Dread(pal, pal_type, 
			HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL, 
			HDF5Constants.H5P_DEFAULT, pd);     
	        } catch (HDF5Exception ex) {
			dbgInfo += "\nread palette FAILED "+ex;
		       return false;
		}

 		try {
			H5.H5Tclose( pal_type );
			H5.H5Aclose( pal_attr );
			H5.H5Dclose( pal );
			H5.H5Tclose (attr_type);
			H5.H5Aclose (attr);
			H5.H5Dclose (tdataset1);
			H5.H5Fclose (tfile);
	        } catch (HDF5Exception ex) {
			dbgInfo += "\nclose FAILED "+ex;
		       return false;
		}
 		dbgInfo += "\n: OK ";
		return true;
	}
}
