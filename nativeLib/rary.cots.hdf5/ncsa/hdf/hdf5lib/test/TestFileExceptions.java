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

public class TestFileExceptions  implements TestModule
{
	private String FILE_ROOT = null;


	String dbgInfo;
	boolean testResult;

	public TestFileExceptions()
	{
		dbgInfo = "\n\nStarting the test";
		testResult = false;
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
		String desc = "Test exceptions for File open write & read";
		return desc;
	}

	public String getTestDescription() {
		String desc = "Test several exceptions on file open ";
		return desc;
	}
	public boolean testPassed()
	{
		return testResult;
	}
	public String getVerboseResult()
	{
		return dbgInfo;
	}

	public void runTest() {
		dbgInfo = "\n\nStarting test run";
		testResult = false;
		boolean res;
		int ntests = 4; /*6*/
		int passed = 0;
		try {
			H5.H5Eclear();
		} catch (HDF5Exception ex) { 
			dbgInfo += "H5Eclear failed? "+ex;
		}
		if (test_is_hdf5()) {
			passed++;
		}
		try {
			H5.H5Eclear();
		} catch (HDF5Exception ex) { 
			dbgInfo += "H5Eclear failed? "+ex;
		}
		if (test_h5fopen() ) {
			passed++;
		}
		try {
			H5.H5Eclear();
		} catch (HDF5Exception ex) { 
			dbgInfo += "H5Eclear failed? "+ex;
		}
		if (test_close() ) {
			passed++;
		}
		try {
			H5.H5Eclear();
		} catch (HDF5Exception ex) { 
			dbgInfo += "H5Eclear failed? "+ex;
		}
		if ( test_protection() ) {
			passed++;
		}
		try {
			H5.H5Eclear();
		} catch (HDF5Exception ex) { 
			dbgInfo += "H5Eclear failed? "+ex;
		}
		/*
		if (javafile() ) {
			passed++;
		}
		try {
			H5.H5Eclear();
		} catch (HDF5Exception ex) { 
			dbgInfo += "H5Eclear failed? "+ex;
		}
		
		if (javafile2()) {
			passed++;
		}
		try {
			H5.H5Eclear();
		} catch (HDF5Exception ex) { 
			dbgInfo += "H5Eclear failed? "+ex;
		}
		*/
		dbgInfo += "\n\n========== file exception tests complete: "+passed+" of "+ntests+" passed  ==========";
		testResult = (passed == ntests);
	}

	private boolean test_is_hdf5()
	{
		String fileName = FILE_ROOT+"never";
		boolean res = false;
		boolean go = false;
		try {
			res = H5.H5Fis_hdf5(fileName);
		} catch (HDF5Exception ex) {
			String s = ex.getMessage();
			dbgInfo += ("\n\nTest OK: is_hdf5 failed with exception: ");
			dbgInfo += ("\n   File: "+fileName+ " "+ ex.getMessage());
			testResult = true;
			go = true;
		}
		if (go == false) {
			dbgInfo += ("\n\nTest FAILED: is_hdf5 succeeded with none existent file, result: "+res);
			testResult = false;
			return testResult;
		}
		
		try {
			H5.H5Eclear();
		} catch (HDF5Exception ex) { 
			dbgInfo += "H5Eclear failed?";
			return false;
		}
		fileName = FILE_ROOT+"notHDF";
		try {
		File f = new File(fileName);

		ObjectOutputStream o = new ObjectOutputStream(new FileOutputStream(f));

		o.writeObject(new Byte((byte)16));
		} catch (Exception e) {
			dbgInfo += ("\n\nTest Failed:  cannot set up file");
			testResult = false;
			return false;
		}
		
		boolean result = false;
		try {
			result = H5.H5Fis_hdf5(fileName);
		} catch (HDF5Exception ex) {
			String s = ex.getMessage();
			dbgInfo += ("\n\nTest FAILED: is_hdf5 failed with exception: ");
			dbgInfo += ("\n   File: "+fileName+ " "+ ex.getMessage());
			testResult = false;
			return false;
		}
		if (result == true) {
			dbgInfo += ("\n\nTest FAILED: is_hdf5 returned true ");
			testResult = false;
			return false;
		}
		dbgInfo += ("\nTest OK: not an HDF-5 file");
		testResult = true;

		fileName = FILE_ROOT+"ishdf5.h5";
		int file_id = -1; 
		try {
			file_id = H5.H5Fcreate(fileName,
			HDF5Constants.H5F_ACC_TRUNC,
			HDF5Constants.H5P_DEFAULT,
                        HDF5Constants.H5P_DEFAULT);
                } catch (HDF5Exception ex) {
                        dbgInfo += "\n\nH5Fcreate() Failed, exception: "+ex.getMessage();
			testResult = false;
                        return false;
                }
		try {
			H5.H5Fclose(file_id);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Aclose(): FAILED "+ex.getMessage();
		}
		try {
			H5.H5Eclear();
		} catch (HDF5Exception ex) { 
			dbgInfo += "H5Eclear failed?";
		}
		result = false;
		try {
			result = H5.H5Fis_hdf5(fileName);
		} catch (HDF5Exception ex) {
			String s = ex.getMessage();
			dbgInfo += ("\n\nTest FAILED: is_hdf5 failed with exception: ");
			dbgInfo += ("\n   File: "+fileName+ " "+ ex.getMessage());
			testResult = false;
			return false;
		}
		if (result == false) {
			dbgInfo += ("\n\nTest FAILED: is_hdf5 returned false ");
			testResult = false;
			return false;
		}
		dbgInfo += ("\nTest OK: is an HDF-5 file");
		testResult = true;
		return true;
	}

	private boolean test_h5fopen()
	{
		String fileName = FILE_ROOT+"never";
		boolean go = false;
		int file_id = -1;
		try {
			file_id = H5.H5Fopen(fileName,
				HDF5Constants.H5F_ACC_RDWR,
				HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			String s = ex.getMessage();
			dbgInfo += ("\n\nTest OK: H5Fopen failed with exception: ");
			dbgInfo += ("\n   File: "+fileName+ " "+ ex.getMessage());
			testResult = true;
			go = true;
		}
		if (go == false) {
			dbgInfo += ("\n\nTest FAILED: H5Fopen succeeded with none existent file, result: "+file_id);
			testResult = false;
			return false;
		}
		
		try {
			H5.H5Eclear();
		} catch (HDF5Exception ex) { 
			dbgInfo += "H5Eclear failed?";
		}
		fileName = FILE_ROOT+"notHDFint";
		dbgInfo = ("\n\n====  Test Exceptions =====");
		try {
		File f = new File(fileName);

		ObjectOutputStream o = new ObjectOutputStream(new FileOutputStream(f));
		o.writeObject(new Integer((int)16));
		} catch (Exception e) {
			dbgInfo += ("\n\nTest Failed:  cannot set up file");
			testResult = false;
			return false;
		}
		
		file_id = -1;
		try {
			file_id = H5.H5Fopen(fileName,
				HDF5Constants.H5F_ACC_RDWR,
				HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			String s = ex.getMessage();
			dbgInfo += ("\n\nTest OK: H5Fopen failed with exception: ");
			dbgInfo += ("\n   File: "+fileName+ " "+ ex.getMessage());
			testResult = true;
		}
		if (file_id >= 0) {
			dbgInfo += ("\n\nTest FAILED: H5Fopen succeeded ");
			testResult = false;
			return false;
		}
		dbgInfo += ("\nTest OK: not an HDF-5 file");
		testResult = true;

		fileName = FILE_ROOT+"ishdf5.h5";
		file_id = -1; 
		try {
			file_id = H5.H5Fcreate(fileName,
			HDF5Constants.H5F_ACC_TRUNC,
			HDF5Constants.H5P_DEFAULT,
                        HDF5Constants.H5P_DEFAULT);
                } catch (HDF5Exception ex) {
                        dbgInfo += "\n\nH5Fcreate() Failed, exception: "+ex.getMessage();
			testResult = false;
                        return false;
                }
		try {
			H5.H5Fclose(file_id);
		} catch (HDF5Exception ex) {
			dbgInfo += "\nH5Aclose(): FAILED "+ex.getMessage();
			return false;
		}
		try {
			H5.H5Eclear();
		} catch (HDF5Exception ex) { dbgInfo += "H5Eclear failed?";}
		file_id = -1;
		try {
			file_id = H5.H5Fopen(fileName,
				HDF5Constants.H5F_ACC_RDWR,
				HDF5Constants.H5P_DEFAULT);
		} catch (HDF5Exception ex) {
			String s = ex.getMessage();
			dbgInfo += ("\n\nTest FAILED: open failed with exception: ");
			dbgInfo += ("\n   File: "+fileName+ " "+ ex.getMessage());
			testResult = false;
			return false;
		}
		if (file_id < 0) {
			dbgInfo += ("\n\nTest FAILED: open returned < 0? ");
			testResult = false;
			return false;
		}
		dbgInfo += ("\nTest OK: is an HDF-5 file");
		testResult = true;
		return true;
	}

private boolean javafile()
{

		String fileName = FILE_ROOT+"java1Byte";
		File f = null;
		try {
		f = new File(fileName);

		ObjectOutputStream o = new ObjectOutputStream(new FileOutputStream(f));
		o.writeObject(new Byte((byte)16));
		o.close();
		} catch (Exception e) {
			return false;
		}
		fileName = FILE_ROOT+"java100x1Byte";
		try {
		f = new File(fileName);

		ObjectOutputStream o = new ObjectOutputStream(new FileOutputStream(f));

		Byte[] bb = new Byte[100];
		for (int i = 0;i < 0;  i++ ) {
			bb[i] = new Byte((byte)16);
		}
		o.writeObject(bb);
		o.close();
		} catch (Exception e) {
			return false;
		}
		fileName = FILE_ROOT+"java1byte";
		try {
		f = new File(fileName);

		ObjectOutputStream o = new ObjectOutputStream(new FileOutputStream(f));

		byte b = (byte)16;
		o.writeByte(b);
		o.close();
		} catch (Exception e) {
			return true;
		}
		fileName = FILE_ROOT+"java100x1byte";
		try {
		f = new File(fileName);

		ObjectOutputStream o = new ObjectOutputStream(new FileOutputStream(f));

		byte[] bbb = new byte[100];
		for (int i = 0;i < 0;  i++ ) {
			bbb[i] = (byte)16;
		}
		o.writeObject(bbb);
		o.close();
		} catch (Exception e) {
			return false;
		}


		fileName = FILE_ROOT+"java2x100x1Byte";
		try {
		f = new File(fileName);

		ObjectOutputStream o = new ObjectOutputStream(new FileOutputStream(f));

		Byte[][] b3 = new Byte[2][];
		Byte[] bb = new Byte[100];
		for (int i = 0;i < 0;  i++ ) {
			bb[i] = new Byte((byte)16);
		}
		b3[0] = bb;
		bb = new Byte[100];
		for (int i = 0;i < 0;  i++ ) {
			bb[i] = new Byte((byte)16);
		}
		b3[1] = bb;
		
		o.writeObject(b3);
		o.close();
		} catch (Exception e) {
			return false;
		}
		fileName = FILE_ROOT+"java2x100x1byte";
		try {
		f = new File(fileName);

		ObjectOutputStream o = new ObjectOutputStream(new FileOutputStream(f));

		byte[][] b4 = new byte[2][];

		byte[] bb = new byte[100];
		for (int i = 0;i < 0;  i++ ) {
			bb[i] = (byte)16;
		}
		b4[0] = bb;
		bb = new byte[100];
		for (int i = 0;i < 0;  i++ ) {
			bb[i] = (byte)16;
		}
		b4[1] = bb;
		o.writeObject(b4);
		o.close();
		} catch (Exception e) {
			return false;
		}
		return true;
}

private boolean test_close()
{
		String fileName = FILE_ROOT+"never";
	int file_id = -1;
	int status = -1;
	boolean go = false;
	try {
		status = H5.H5Fclose(file_id);
	} catch (HDF5Exception ex) {
		String s = ex.getMessage();
		dbgInfo += ("\n\nTest OK: close failed with exception: ");
		dbgInfo += ("\n   File: "+file_id+ " "+ ex.getMessage());
		testResult = true;
		go = true;
	}
	if (go && (status >= 0)) {
		dbgInfo += ("\n\nTest FAILED: close returned exception and >= 0? ");
		testResult = false;
		return false;
	}

	// create, then close twice
	fileName = FILE_ROOT+"testclose.h5";
	file_id = -1; 
	try {
		 file_id = H5.H5Fcreate(fileName,
		HDF5Constants.H5F_ACC_TRUNC,
		HDF5Constants.H5P_DEFAULT,
		HDF5Constants.H5P_DEFAULT);
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nH5Fcreate() Failed, exception: "+ex.getMessage();
		testResult = false;
		return false;
	}
	try {
		H5.H5Eclear();
	} catch (HDF5Exception ex) { 
		dbgInfo += "H5Eclear failed?";
	}
	status = -1;
	try {
		status = H5.H5Fclose(file_id);
	} catch (HDF5Exception ex) {
		dbgInfo += "Test Failed, first close returned exception: "+ex.getMessage();
		testResult = false;
		return false;
	} 
	try {
		H5.H5Eclear();
	} catch (HDF5Exception ex) { 
		dbgInfo += "H5Eclear failed?";
	}
	status = -1;
	try {
		status = H5.H5Fclose(file_id);
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nTest OK, second close returned exception: "+ex.getMessage();
		testResult = true;
		return true;
	}
	if (status >=0 ) {
		dbgInfo += "Test FAILED, second close returned without exception or error ";
		testResult = false;
		return false;
	}
	dbgInfo += "Test FAILED, second close returned -1 but no exception";
	testResult = false;
	return false;
}


private boolean test_protection()
{
	String fileName = FILE_ROOT+"exclusive.HDF5Library"; // 
	int file_id = -1;
	int status = -1;
	boolean go = false;
	try {
		file_id = H5.H5Fcreate(fileName,
		HDF5Constants.H5F_ACC_TRUNC,
		HDF5Constants.H5P_DEFAULT,
		HDF5Constants.H5P_DEFAULT);
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nH5Fcreate() Failed, exception: "+ex.getMessage();
		testResult = false;
		return false;
	}
	try {
		H5.H5Eclear();
	} catch (HDF5Exception ex) { 
		dbgInfo += "H5Eclear failed?";
	}
	status = -1;
	try {
		status = H5.H5Fclose(file_id);
	} catch (HDF5Exception ex) {
		dbgInfo += "Test Failed, first close returned exception: "+ex.getMessage();
		testResult = false;
		return false;
	} 
	try {
		H5.H5Eclear();
	} catch (HDF5Exception ex) { dbgInfo += "H5Eclear failed?";}
	status = -1;
	try {
		file_id = H5.H5Fcreate(fileName,
		HDF5Constants.H5F_ACC_TRUNC,
		HDF5Constants.H5P_DEFAULT,
		HDF5Constants.H5P_DEFAULT);
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nTest Failed, second create failed exception: "+ex.getMessage();
		testResult = false;
		return false;
	}
	try {
		H5.H5Eclear();
	} catch (HDF5Exception ex) { 
		dbgInfo += "H5Eclear failed?";
	}
	status = -1;
	try {
		status = H5.H5Fclose(file_id);
	} catch (HDF5Exception ex) {
		dbgInfo += "Test Failed, first close returned exception: "+ex.getMessage();
		testResult = false;
		return false;
	} 
	try {
		H5.H5Eclear();
	} catch (HDF5Exception ex) { 
		dbgInfo += "H5Eclear failed?";
	}
	file_id = -1;
	try {
		file_id = H5.H5Fcreate(fileName,
		HDF5Constants.H5F_ACC_EXCL,
		HDF5Constants.H5P_DEFAULT,
		HDF5Constants.H5P_DEFAULT);
	} catch (HDF5Exception ex) {
		dbgInfo += "\n\nTest OK, second create failed with exception: "+ex.getMessage();
		testResult = true;
		go = true;
	}
	if (go ) {
		if  (file_id >= 0) {
		dbgInfo += "\n\nTest FAILED, second create had exception and returned >= 0";
		testResult = false;
		return false;
			
		}
	}  else {
		dbgInfo += "\n\nTest FAILED, second create succeeded despite EXCL flag";
		testResult = false;
		return false;
	}

	return true;
}


private boolean javafile2()
{

		String fileName = FILE_ROOT+"java64x64x1float";
		File f = null;

		float[][][] fa = new float[64][][];
		for (int i = 0;i < 64;  i++ ) {
			float[][] r = new float[64][];
			for (int j = 0; j < 64;  j++ ) {
				float c [] = new float[1];
				c[0] = (float)1.0;
				r[j] = c;
			}
			fa[i] = r;
		}
		try {
		f = new File(fileName);

		ObjectOutputStream o = new ObjectOutputStream(new FileOutputStream(f));
		o.writeObject(fa);
		o.close();
		} catch (Exception e) {
			return false;
		}
		fileName = FILE_ROOT+"java64x64float";
		float[][] fb = new float[64][];
		for (int i = 0;i < 64;  i++ ) {
			float[] r = new float[64];
			for (int j = 0; j < 64;  j++ ) {
				r[j] = (float)1.0;
			}
			fb[i] = r;
		}
		try {
		f = new File(fileName);

		ObjectOutputStream o = new ObjectOutputStream(new FileOutputStream(f));
		o.writeObject(fb);
		o.close();
		} catch (Exception e) {
			return false;
		}
	return true;
}
}
