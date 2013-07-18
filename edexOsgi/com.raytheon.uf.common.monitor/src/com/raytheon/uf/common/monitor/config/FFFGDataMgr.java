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
package com.raytheon.uf.common.monitor.config;

import java.io.File;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.TimeZone;

import com.raytheon.uf.common.monitor.config.FFFGXmlMgr.FFFGXmlType;
import com.raytheon.uf.common.monitor.xml.FFFGBasinIdXML;
import com.raytheon.uf.common.monitor.xml.FFFGDataXML;
import com.raytheon.uf.common.monitor.xml.FFFGSourceItemXML;
import com.raytheon.uf.common.monitor.xml.FFFGSourceXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * 
 * Singleton data manager for FFFG.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 10, 2010 #4517      lvenable    Initial creation
 * Jun 17, 2013 #2085      njensen     Double checked locking of instance
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class FFFGDataMgr {
    /**
     * Class instance.
     */
    private static FFFGDataMgr classInstance;

    /**
     * Master XML manager.
     */
    private FFFGXmlMgr masterXmlMgr = null;

    /**
     * User XML manager.
     */
    private FFFGXmlMgr userXmlMgr = null;

    /**
     * Master XML file name.
     */
    private final String fffgMasterFilename = "FFFGMasterData.xml";

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFFGDataMgr.class);

    /**
     * Private constructor.
     */
    private FFFGDataMgr() {
        readInMasterXmlManager();
    }

    /**
     * Get an instance of this class.
     * 
     * @return An instance of this class.
     */
    public static FFFGDataMgr getInstance() {
        if (classInstance == null) {
            synchronized (FFFGDataMgr.class) {
                if (classInstance == null) {
                    classInstance = new FFFGDataMgr();
                }
            }
        }

        return classInstance;
    }

    /**
     * Return an updated instance of this class that also re-reads in the data.
     * 
     * @return An updated instance of this class.
     */
    public static FFFGDataMgr getUpdatedInstance() {
        if (classInstance == null) {
            classInstance = getInstance();
        } else {
            classInstance.readInMasterXmlManager();
        }

        return classInstance;
    }

    /**
     * Read in the master XML manager.
     */
    private void readInMasterXmlManager() {
        masterXmlMgr = new FFFGXmlMgr(fffgMasterFilename,
                getFFFGDataFilePath(), FFFGXmlType.MASTER);
        try {
            masterXmlMgr.readThresholdXml();
        } catch (Exception ex) {
            ex.printStackTrace();
            masterXmlMgr = null;
        }
    }

    /**
     * Get the XML data path.
     * 
     * @return The XML data path.
     */
    public String getFFFGDataFilePath() {
        String fs = String.valueOf(File.separatorChar);
        StringBuilder sb = new StringBuilder();

        sb.append("monitoring").append(fs);
        sb.append("fffg").append(fs);

        return sb.toString();
    }

    /**
     * Load in a new user XML file.
     * 
     * @param filename
     *            File name.
     */
    public void loadUserFFFGData(String filename) {
        userXmlMgr = new FFFGXmlMgr(filename, getFFFGDataFilePath(),
                FFFGXmlType.USER);
        try {
            userXmlMgr.readThresholdXml();
        } catch (Exception ex) {
            ex.printStackTrace();
            userXmlMgr = null;
        }
    }

    /**
     * Check if the FFFG is expired.
     * 
     * @return True if the data is expired, false if the data is still valid.
     */
    public boolean isExpired() {
        if ((masterXmlMgr == null) || (masterXmlMgr.getXMLData() == null)) {
            return true;
        }

        long expTime = masterXmlMgr.getXMLData().getExpTimeInMillis();
        if (Calendar.getInstance(TimeZone.getTimeZone("GMT")).getTimeInMillis() < expTime) {
            return false;
        }

        // if expiration time is set to 0 then the forcing persists until
        // changed.
        if (expTime == 0) {
            return false;
        }

        return true;
    }

    /**
     * Get the file name of the user data.
     * 
     * @return The file name of the user data.
     */
    public String getUserFileName() {
        if (userXmlMgr == null) {
            return null;
        }

        return userXmlMgr.getDataFileName();
    }

    /**
     * Update and save the user XML data.
     * 
     * @param expTime
     *            Expiration time.
     * @param srcCompData
     *            Array of source component data.
     */
    public void saveUpdateUserXML(long expTime,
            ArrayList<SourceCompData> srcCompData,
            ArrayList<ArrayList<FFFGBasinIdXML>> basinList) {
        if (userXmlMgr == null) {
            System.out.println("User XML manager is null...");
            return;
        }

        FFFGDataXML newXmlData = new FFFGDataXML();
        newXmlData.setExpTimeInMillis(expTime);
        createSourceXml(newXmlData, srcCompData, basinList);
        userXmlMgr.setDataXML(newXmlData);

        userXmlMgr.saveFFFGDataXml();
    }

    /**
     * Update and save the user XML data to a new file name.
     * 
     * @param newFileName
     *            New file anem.
     * @param expTime
     *            Expiration time.
     * @param srcCompData
     *            Array of source component data.
     */
    public void saveAsUpdateUserXML(String newFileName, long expTime,
            ArrayList<SourceCompData> srcCompData,
            ArrayList<ArrayList<FFFGBasinIdXML>> basinList) {
        if (userXmlMgr == null) {
            userXmlMgr = new FFFGXmlMgr(newFileName, getFFFGDataFilePath(),
                    FFFGXmlType.USER);
        }

        FFFGDataXML newXmlData = new FFFGDataXML();
        newXmlData.setExpTimeInMillis(expTime);
        createSourceXml(newXmlData, srcCompData, basinList);

        userXmlMgr.setDataXML(newXmlData);
        userXmlMgr.saveFFFGDataXmlAs(newFileName);
    }

    /**
     * Update the master XML data with the provided data.
     * 
     * @param expTime
     *            Expiration time.
     * @param srcCompData
     *            Array of source component data.
     */
    public void updateMasterXML(long expTime,
            ArrayList<SourceCompData> srcCompData,
            ArrayList<ArrayList<FFFGBasinIdXML>> basinList) {
        FFFGDataXML newXmlData = new FFFGDataXML();

        newXmlData.setExpTimeInMillis(expTime);

        createSourceXml(newXmlData, srcCompData, basinList);

        masterXmlMgr.setDataXML(newXmlData);
        masterXmlMgr.saveFFFGDataXml();
    }

    /**
     * Create the Source XML data from the Source component data provided.
     * 
     * @param newXmlData
     *            New FFFGDataXML data.
     * @param srcCompData
     *            Array of source component data.
     */
    private void createSourceXml(FFFGDataXML newXmlData,
            ArrayList<SourceCompData> srcCompData,
            ArrayList<ArrayList<FFFGBasinIdXML>> basinList) {
        ArrayList<FFFGSourceXML> sources = new ArrayList<FFFGSourceXML>();
        FFFGSourceXML srcXML;

        /*
         * Loop over the source data and create the source XML and the source
         * items XML.
         */
        int idx = 0;
        for (SourceCompData scd : srcCompData) {
            ArrayList<FFFGBasinIdXML> list = basinList.get(idx);
            srcXML = new FFFGSourceXML();
            srcXML.setAreaFFGValue(scd.getAreaFFGValue());
            srcXML.setSourceName(scd.getSourceName());

            // Loop and create an array of source items
            ArrayList<ValueNameIdData> cntyBasinData = scd.getCountyBasinData();
            ArrayList<FFFGSourceItemXML> sourceItems = new ArrayList<FFFGSourceItemXML>();
            FFFGSourceItemXML srcItemXML;

            for (ValueNameIdData data : cntyBasinData) {
                srcItemXML = new FFFGSourceItemXML();
                srcItemXML.setType(data.getType().name());
                srcItemXML.setName(data.getName());
                srcItemXML.setId(data.getId());
                srcItemXML.setValue(data.getValue());

                sourceItems.add(srcItemXML);

                // list the basins in this county
                for (FFFGBasinIdXML basin : list) {
                    srcXML.addBasin(basin);
                }
            }

            // Add the array of source items to the source XML.
            srcXML.setSourceItems(sourceItems);

            // Add the FFFGSourceXML to the array.
            sources.add(srcXML);
            idx++;
        }

        newXmlData.setSourceData(sources);
    }

    /**
     * Adjust the value passed in depending on the source, county, and basin
     * information.
     * 
     * @param value
     *            Value to be adjusted.
     * @param sourceName
     *            Source name of the value.
     * @param basinPfaf
     *            Basin pfaf of the value.
     * @param countyGID
     *            County GID of the value.
     * @return The adjusted value.
     */
    public float adjustValue(float value, String sourceName, Long basinPfaf,
            Long countyGID) {
        return (float) adjustValue((double) value, sourceName, basinPfaf,
                countyGID);
    }

    /**
     * Adjust the value passed in depending on the source, county, and basin
     * information.
     * 
     * @param value
     *            Value to be adjusted.
     * @param sourceName
     *            Source name of the value.
     * @param basinPfaf
     *            Basin pfaf of the value.
     * @param countyGID
     *            County GID of the value.
     * @return The adjusted value.
     */
    public double adjustValue(double value, String sourceName, Long basinPfaf,
            Long countyGID) {
        // Check to see if the XML manager or the data is null.
        if ((masterXmlMgr == null) || (masterXmlMgr.getXMLData() == null)) {
            return value;
        }

        // Check to see if the source is even available.
        if (masterXmlMgr.getXMLData().containsSource(sourceName) == false) {
            return value;
        }

        // Check basin
        FFFGSourceXML srcXML = masterXmlMgr.getXMLData().getSourceData(
                sourceName);

        if (srcXML.containsID(basinPfaf) == true) {
            return adjustValue(value, srcXML.getAssociatedValue(basinPfaf));
        }

        // Check county
        if (srcXML.containsID(countyGID) == true) {
            return adjustValue(value, srcXML.getAssociatedValue(countyGID));
        }

        // Check areaFFG
        if ((srcXML.getAreaFFGValue() != null)
                && (srcXML.getAreaFFGValue().length() > 0)) {
            return adjustValue(value, srcXML.getAreaFFGValue());
        }

        return value;
    }

    public boolean isForced(String sourceName, Long id) {
        if (masterXmlMgr != null) {
            FFFGDataXML fffgData = masterXmlMgr.getXMLData();
            if (fffgData == null) {
                return false;
            }
            FFFGSourceXML srcXML = fffgData.getSourceData(sourceName);
            if (srcXML == null) {
                return false;
            }
            // Check areaFFG
            if ((srcXML.getAreaFFGValue() != null)
                    && (srcXML.getAreaFFGValue().length() > 0)
                    && (isExpired() == false)) {
                return true;
            }
            if (srcXML.containsID(id) && (isExpired() == false)) {
                return true;
            }
        }

        return false;
    }

    /**
     * Adjust the value passed in based on the string value passed in. If the
     * string value starts with a "+" the string value will be added to the
     * value. If the string value starts with a "-" the string value will be
     * subtracted from the value. Otherwise the value is replaced by the value
     * in the string value.
     * 
     * @param value
     *            Value
     * @param strValue
     *            String value.
     * @return The adjusted value.
     */
    private double adjustValue(double value, String strValue) {
        double newValue = value;

        if (strValue.startsWith("+") || strValue.startsWith("-")) {
            newValue = value + Double.valueOf(strValue);
        } else {
            newValue = Double.valueOf(strValue);
        }

        return newValue;
    }

    /**
     * Get the FFFG master file name.
     * 
     * @return The master file name.
     */
    public String getFFFGMasterFileName() {
        return fffgMasterFilename;
    }

    /**
     * Get the user XML data as an array of source component data.
     * 
     * @return An array of source component data.
     */
    public ArrayList<SourceCompData> getUserXMLData() {
        ArrayList<SourceCompData> srcDataArray = new ArrayList<SourceCompData>();

        SourceCompData srcCompData;
        if ((userXmlMgr == null) || (userXmlMgr.getXMLData() == null)) {
            return null;
        }
        ArrayList<FFFGSourceXML> sources = userXmlMgr.getXMLData().getSources();

        for (FFFGSourceXML srcXML : sources) {
            srcCompData = new SourceCompData();
            srcCompData.setSourceName(srcXML.getSourceName());
            srcCompData.setAreaFFGValue(srcXML.getAreaFFGValue());
            srcCompData.setCountyBasinData(srcXML.getCountyBasinData());

            srcDataArray.add(srcCompData);
        }

        return srcDataArray;
    }

    /**
     * Get the Master XML data as an array of source component data.
     * 
     * @return An array of source component data.
     */
    public ArrayList<SourceCompData> getMasterXMLData() {
        ArrayList<SourceCompData> srcDataArray = new ArrayList<SourceCompData>();

        SourceCompData srcCompData;
        ArrayList<FFFGSourceXML> sources = null;

        try {
            sources = masterXmlMgr.getXMLData().getSources();
        } catch (Exception e) {
            statusHandler
                    .handle(Priority.DEBUG, "No FFFG Data sources in file");
        }

        if (sources != null) {
            for (FFFGSourceXML srcXML : sources) {
                srcCompData = new SourceCompData();
                srcCompData.setSourceName(srcXML.getSourceName());
                srcCompData.setAreaFFGValue(srcXML.getAreaFFGValue());
                srcCompData.setCountyBasinData(srcXML.getCountyBasinData());

                srcDataArray.add(srcCompData);
            }
        }

        return srcDataArray;
    }

    /**
     * Check if the deleted user file is the user file that is loaded. If so
     * then set the XML to null as the file no longer exists.
     * 
     * @param deleteFileName
     *            The file name that was deleted.
     */
    public void deleteUserXMLData(String deleteFileName) {
        if (deleteFileName.compareTo(getUserFileName()) == 0) {
            userXmlMgr = null;
        }
    }

    /**
     * Print the data associated with the type passed in.
     * 
     * @param xmlType
     *            XML data type.
     */
    public void printData(FFFGXmlType xmlType) {
        if (xmlType == FFFGXmlType.MASTER) {
            masterXmlMgr.printData();
        } else {
            userXmlMgr.printData();
        }
    }

    /**
     * @param source
     * @param pfaf
     * @return
     */
    public boolean isBasinForced(String sourceName, long id) {

        if ((masterXmlMgr != null) && (sourceName != null)) {

            try {
                FFFGSourceXML srcXML = masterXmlMgr.getXMLData().getSourceData(
                        sourceName);

                if (srcXML == null) {
                    return false;
                }

                if ((srcXML.getAreaFFGValue() != null)
                        && (srcXML.getAreaFFGValue().length() > 0)
                        && (isExpired() == false)) {
                    return true;
                }

                if (srcXML.containsID(id) && (isExpired() == false)) {
                    return true;
                }
            } catch (NullPointerException npe) {

            }
        }

        return false;
    }

    /**
     * Are any forcings configured?
     * 
     * @return true if forcings are configured
     */
    public boolean isForcingConfigured() {
        return masterXmlMgr.isForcingConfigured();
    }
}
