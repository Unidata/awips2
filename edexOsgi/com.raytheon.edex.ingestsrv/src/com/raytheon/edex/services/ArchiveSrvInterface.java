/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.edex.services;


/**
 * The archive service listens to staging service and moves any files that
 * pass through staging service when the archive mode is on.<br>
 * 
 * This implementation moves the file to the specified archive directory. 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 12/12/2007   561         dfitch      Initial Creation	
 * 
 * </pre>
 * 
 * @author dfitch
 * @version 1
 */
public interface ArchiveSrvInterface {
	
    /**
     * @return the dataType
     */
    public abstract String getPluginName();
    
    /**
     * @param dataType
     *            the dataType to set
     */
    public abstract void setPluginName(String pluginName);
    
    /**
     * @return the archiveDirectoryLocation
     */
    public abstract String getArchiveDirectoryLocation();

    /**
     * @param archiveDirectoryLocation
     *            the archiveDirectoryLocation to set
     */
    public abstract void setArchiveDirectoryLocation(
            String archiveDirectoryLocation);

   /**
    * 
    * @return the status of Archive/TeeMode
    */
    public boolean isTeeModeOn();

    /**
     * @param teeModeOn the teeModeOn to set
     */
    public void setTeeModeOn(boolean teeModeOn);
    
    /**
     * 
     * @return the number of messaged serviced.
     */
    public int getNumMessagesServed();
  
    /**
     * 
     * @return the record locations
     */
    public String getRecordLocations();
    
    /**
     * 
     * @param recordLocations 
     * 		Location to set the record location.
     */
    public void setRecordLocations(String recordLocations);

}
