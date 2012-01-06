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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.edex.core.EdexException;

/**
 * The archive service listens to staging service and moves any files that pass
 * through staging service when the archive mode is on.<br>
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

public class ArchiveSrv implements ArchiveSrvInterface {

    /**
     * Location of the archive.
     */
    private String archiveDirectoryLocation = "";

    /**
     * Status of the Archive/teeMode.
     */
    private boolean teeModeOn = false;

    /**
     * Status of the jmxMode.
     */
    private boolean jmxModeOn = false;

    /**
     * Name describing the data ingesting.
     */
    private String name = null;

    /**
     * could also just use the statistics that are already under
     * org.mule.Statistics in jconsole.
     */
    private int numMessagesServed;

    private int numMessagesCopied;

    private boolean bRegistered = false;

    private String recordLocations = null;

    private boolean multipleLocationsPerFile = false;

    private String locationField = null;

    private Log logger = LogFactory.getLog(getClass());

    /**
     * List of the ArchiveSrvs
     */
    private static List<ArchiveSrv> lstOfArchiveSrv = null;

    private static Map<String, Integer> map = new HashMap<String, Integer>();

    /**
     * Define the datatype and index filtering this instance handles.
     */
    private String pluginName = "";

    /**
     * Constructor.
     */
    public ArchiveSrv() {

        numMessagesServed = numMessagesCopied = 0;
        if (null == lstOfArchiveSrv) {
            lstOfArchiveSrv = new ArrayList<ArchiveSrv>();
        }
        synchronized (lstOfArchiveSrv) {
            lstOfArchiveSrv.add(this);
        }
    }

    public Object process(String filePath) throws EdexException {

        numMessagesServed++;

        if (logger.isInfoEnabled()) {
            logger.info("***** archiveDirectoryLocation="
                    + archiveDirectoryLocation);
        }
        if (teeModeOn) {
            numMessagesCopied++;
            // Copy the filename
            copyFile(archiveDirectoryLocation, filePath);
        }

        return filePath;
    }

    public void dispose() {
        // intentially left empty

    }

    /**
     * @param archiveDirectoryLocation
     *            Location of the Archive Directory
     * @param fileName
     *            File name to be copied
     */
    public static void copyFile(String archiveDirectoryLocation, String fileName) {
        try {
            File file = new File(fileName);
            file = file.getCanonicalFile();

            File archiveDir = new File(archiveDirectoryLocation);
            archiveDir.mkdirs();
            String archiveFileName = archiveDirectoryLocation + file.getName();
            File archiveFile = new File(archiveFileName);
            OutputStream out = new FileOutputStream(archiveFile);
            InputStream is = new FileInputStream(file);
            byte[] bytes = new byte[8 * 1024];

            // Read in the bytes
            int numRead = 0;
            while (0 <= (numRead = is.read(bytes))) {
                out.write(bytes, 0, numRead);
            }
            // Close the input stream and return bytes
            is.close();
            out.close();

            // Get the last modified time
            long modifiedTime = file.lastModified();
            // 0L is returned if the file does not exist

            // Set the last modified time
            archiveFile.setLastModified(modifiedTime);

        } catch (Exception bland) {
            bland.printStackTrace();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.services.ArchiveTeeSrvInterface#getArchiveDirectoryLocation
     * ()
     */
    public String getArchiveDirectoryLocation() {
        return archiveDirectoryLocation;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.services.ArchiveTeeSrvInterface#setArchiveDirectoryLocation
     * (java.lang.String)
     */
    public void setArchiveDirectoryLocation(String archiveDirectoryLocation) {
        if (archiveDirectoryLocation.endsWith("NAME")) {
            archiveDirectoryLocation = archiveDirectoryLocation.substring(0,
                    archiveDirectoryLocation.length() - "NAME".length());

            archiveDirectoryLocation = archiveDirectoryLocation + "/"
                    + ((name == null) ? pluginName : name) + "/";
        }
        this.archiveDirectoryLocation = archiveDirectoryLocation;

    }

    /**
     * @return the teeModeOn
     */
    public boolean isTeeModeOn() {
        return teeModeOn;
    }

    /**
     * @param teeModeOn
     *            the teeModeOn to set
     */
    public void setTeeModeOn(boolean teeModeOn) {
        this.teeModeOn = teeModeOn;
    }

    /**
     * @return the lstOfArchiveSrv
     */
    public static List<ArchiveSrv> getLstOfArchiveSrv() {
        return lstOfArchiveSrv;
    }

    /**
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * @param name
     *            the name to set
     */
    public void setName(String name) {
        if (null == this.name) {
            this.name = name;
        }

    }

    /**
     * @return the numMessagesServed
     */
    public int getNumMessagesServed() {
        return numMessagesServed;
    }

    /**
     * @return the numMessagesCopied
     */
    public int getNumMessagesCopied() {
        return numMessagesCopied;
    }

    /**
     * @return the jmxModeOn
     */
    public boolean isJmxModeOn() {

        return jmxModeOn;

    }

    /**
     * @return the recordLocations
     */
    public String getRecordLocations() {
        return recordLocations;
    }

    /**
     * @param recordLocations
     *            the recordLocations to set
     */
    public void setRecordLocations(String recordLocations) {
        this.recordLocations = recordLocations;
    }

    /**
     * @return the multipleLocationsPerFile
     */
    public boolean isMultipleLocationsPerFile() {
        return multipleLocationsPerFile;
    }

    /**
     * @param multipleLocationsPerFile
     *            the multipleLocationsPerFile to set
     */
    public void setMultipleLocationsPerFile(boolean multipleLocationsPerFile) {
        this.multipleLocationsPerFile = multipleLocationsPerFile;
    }

    /**
     * @return the locationField
     */
    public String getLocationField() {
        return locationField;
    }

    /**
     * @param locationField
     *            the locationField to set
     */
    public void setLocationField(String locationField) {
        this.locationField = locationField;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.services.ArchiveSrvInterface#getPluginName()
     */
    public String getPluginName() {
        return pluginName;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.services.ArchiveSrvInterface#setPluginName(java.lang
     * .String)
     */
    public void setPluginName(String pluginName) {
        this.pluginName = pluginName;

        if (null == this.name) {
            this.name = pluginName;
        }

    }
}
