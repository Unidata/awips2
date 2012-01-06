/**
 * gov.noaa.nws.ncep.viz.common.ui.FileDateComparator
 * 
 * Date Created: 3-Mar-2010
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.viz.common.ui;

import java.io.File;
import java.util.Comparator;

/**
 * Sorts <code>File<code> objects (in the ascending order) by their last modified date  
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	-----------------------------------
 * 3-Mar-2010     #228        Archana   Initial Creation 
 * 5-Apr-2010     #228        Archana   Added documentation                       
 * </pre>
 * 
 * @author Archana
 * @version 1
 */
public class FileDateComparator implements Comparator<File> {
	
	/**
	 * Sorts the two input files in ascending order by performing 
	 * a case insensitive comparison of their names.
	 * @param The first file to be compared
	 * @param The second file to be compared
	 * @return -1, 0, or 1 - if the last modified date of the first file
	 *          is less than, equal to, or greater than the second respectively.
     *	       
	 */
		
	public int compare(File file1, File file2) {
		
		if(file1.lastModified() < file2.lastModified())
		{
	    		return -1;
		}
		else if(file1.lastModified() > file2.lastModified())
		{
	    		return 1;
		}
		else
		{
	    		return 0;
		}


	}

}
