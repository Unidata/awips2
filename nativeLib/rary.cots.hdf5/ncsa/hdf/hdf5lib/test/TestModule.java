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

public interface TestModule
{
	public boolean setUp( String workingDir, boolean cleanFilesAtStart );
	public boolean cleanUp( boolean saveFiles );

	public void runTest();

	public boolean testPassed();
	public String getVerboseResult();

	public String getTestName();
	public String getTestDescription();

}
