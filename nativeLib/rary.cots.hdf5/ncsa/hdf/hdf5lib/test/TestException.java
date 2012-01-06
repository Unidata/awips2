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

public class TestException  implements TestModule
{
	private String FILE_ROOT = null;


	String dbgInfo;
	boolean testResult = false;

	public TestException()
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
		String desc = "Test Exception";
		return desc;
	}

	public String getTestDescription() {
		String desc = "Test Exceptions:  for exceptions and check stack traces";
		return desc;
	}

	public void runTest() {
		testResult = false;
		boolean res;
		int ntests = 3;
		int passed = 0;
		dbgInfo = "\n\n========== Test HDF5Exception ";
		if (testHDF5Exception()) {
			passed++;
		}
		System.out.println();
		dbgInfo += "\n\n========== Test HDF5LibraryException ";
		if (testHDF5LibraryException()) {
			passed++;
		}
		System.out.println();
		dbgInfo += "\n\n========== Test Force File not found ";
		if ( testNoSuchFile() ) {
			passed++;
		}
		dbgInfo += "\n\n========== Exception tests complete: "+passed+" of "+ntests+" passed  ==========";
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

	private boolean testHDF5Exception()
	{
		try {
			HDF5Exception e1 = new HDF5Exception();
			throw e1;
		} catch (HDF5Exception ex) {
			System.out.println("caught exception after throw...");
			ex.printStackTrace();
			return true;
		}
 		//dbgInfo += "\n: OK ";
		//return true;
	}

	private boolean testHDF5LibraryException()
	{
		try {
			HDF5LibraryException e1 = new HDF5LibraryException();
			throw e1;
		} catch (HDF5LibraryException ex) {
			System.out.println("caught exception after throw...");
			ex.printStackTrace();
			return true;
		}
 		//dbgInfo += "\n: OK ";
		//return true;
	}

	private boolean testNoSuchFile()
	{
		int file_id = -1;
		try {
			file_id = H5.H5Fopen("no-suc-file",
                                HDF5Constants.H5F_ACC_RDWR,
                                HDF5Constants.H5P_DEFAULT);

		} catch (HDF5LibraryException ex) {
			System.out.println(ex.toString());
			ex.printStackTrace();
			return true;
		}
 		//dbgInfo += "\n: OK ";
		return false;
	}

}
