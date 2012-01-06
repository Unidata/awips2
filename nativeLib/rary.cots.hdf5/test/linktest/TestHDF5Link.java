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

/*
 *  This program tests the linking and calling of the HDF-5 library.
 *
 *  The purpose is to help debug the installation of the JHI-5.
 */
import ncsa.hdf.hdf5lib.*;
import ncsa.hdf.hdf5lib.exceptions.*;

public class TestHDF5Link {

	static public void main( String []args ) 
	{

		String libPath = System.getProperty("java.library.path",null);

		if ( libPath == null ) {
			System.out.println("ERROR: No library path set.");
			System.exit(1);
		}
		System.out.println("Searching for libraries in: "+libPath);
		System.out.flush();

		System.out.println("Test load of Java HDF-5 Interface: ");
		try {
			System.loadLibrary("jhdf5");
		} catch (Throwable t1) {
			System.out.println("Exception loading Java HDF-5 Interface: "+t1);
			System.exit(1);
		}
		System.out.println("OK.  (libjhdf5.so)");System.out.flush();

		int status = -1;
		System.out.println("\nTest call to HDF-5 Library:");
		try {
			status = H5.H5open();
			status = H5.H5close();
		} catch (Throwable t3) {
			System.out.println("Exception calling HDF-5 Library: "+t3);
			System.exit(1);
		}
		System.out.println("OK.  (H5.H5open())");System.out.flush();
		System.out.println("\n");System.out.flush();

		System.out.println("Path is Correct: \n\t"+libPath);
		status = -1;
		int fid = -1;
		System.out.println("\nTest call to create file:");
		try {
			status = H5.H5open();
			fid = H5.H5Fcreate("test.h5",0,0,0);
			status = H5.H5Fclose(fid);
			status = H5.H5close();
		} catch (Throwable t3) {
			System.out.println("Exception creating HDF-5 file: "+t3);
			System.exit(1);
		}
		System.out.println("OK.  (H5.H5Fcreate())");System.out.flush();
		System.out.println("\n");System.out.flush();

		System.out.println("Library works");
	}
}
