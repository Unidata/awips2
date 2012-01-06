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
import ncsa.hdf.hdflib.*;

public class TestHDF4Link {

	static public void main( String []args ) 
	{

		String libPath = System.getProperty("java.library.path",null);

		if ( libPath == null ) {
			System.out.println("ERROR: No library path set.");
			System.exit(1);
		}
		System.out.println("Searching for libraries in: "+libPath);
		System.out.flush();

		System.out.println("Test load of Java HDF Interface: ");
		try {
			System.loadLibrary("jhdf");
		} catch (Throwable t1) {
			System.out.println("Exception loading Java HDF Interface: "+t1);
			System.exit(1);
		}
		System.out.println("OK.  (libjhdf.so)");System.out.flush();

		String [] vs = new String[1];
		vs[0] = new String("");
		int vers[] = new int[3];
		boolean ok = false;
		System.out.println("\nTest call to HDF Library:");
		try {
 			ok = HDFLibrary.Hgetlibversion(vers, vs);


		} catch (Throwable t3) {
			System.out.println("Exception calling HDF Library: "+t3);
			System.exit(1);
		}
		System.out.println("OK.  (HDFLibrary.getlibversion "+vers[0]+"."+vers[1]+"."+vers[2]);System.out.flush();
		System.out.println("\n");System.out.flush();

		System.out.println("Path is Correct: \n\t"+libPath);

		System.out.println("Try to create file: \n");

		int res = 0;
		try {
			res = HDFLibrary.Hopen("test.hdf", HDFConstants.DFACC_CREATE);
		} catch (Throwable t4) {
			System.out.println("Exception creating file: "+t4);
			System.exit(1);
		}
		System.out.println("OK.");System.out.flush();
		System.out.println("\n");System.out.flush();
	}
}
