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
package com.raytheon.uf.common.dataplugin.gfe.request;

import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Request object to run ifpnetCDF.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 24, 2010            dgilling     Initial creation
 * Mar 11, 2013  #1759     dgilling     Re-factoring to use fields instead
 *                                      of an argument string.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

@DynamicSerialize
public class ExecuteIfpNetCDFGridRequest extends AbstractGfeRequest {

    @DynamicSerializeElement
    private String outputFilename;

    @DynamicSerializeElement
    private List<String> parmList;

    @DynamicSerializeElement
    private DatabaseID databaseID;

    @DynamicSerializeElement
    private String startTime;

    @DynamicSerializeElement
    private String endTime;

    @DynamicSerializeElement
    private String mask;

    @DynamicSerializeElement
    private boolean geoInfo;

    @DynamicSerializeElement
    private boolean compressFile;

    @DynamicSerializeElement
    private String configFileName;

    @DynamicSerializeElement
    private int compressFileFactor;

    @DynamicSerializeElement
    private boolean trim;

    @DynamicSerializeElement
    private boolean krunch;

    @DynamicSerializeElement
    private String userID;

    @DynamicSerializeElement
    private String logFileName;

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("ExecuteIfpNetCDFGridRequest [outputFilename=");
        builder.append(outputFilename);
        builder.append(", parmList=");
        builder.append(parmList);
        builder.append(", databaseID=");
        builder.append(databaseID);
        builder.append(", startTime=");
        builder.append(startTime);
        builder.append(", endTime=");
        builder.append(endTime);
        builder.append(", mask=");
        builder.append(mask);
        builder.append(", geoInfo=");
        builder.append(geoInfo);
        builder.append(", compressFile=");
        builder.append(compressFile);
        builder.append(", configFileName=");
        builder.append(configFileName);
        builder.append(", compressFileFactor=");
        builder.append(compressFileFactor);
        builder.append(", trim=");
        builder.append(trim);
        builder.append(", krunch=");
        builder.append(krunch);
        builder.append(", userID=");
        builder.append(userID);
        builder.append(", logFileName=");
        builder.append(logFileName);
        builder.append("]");
        return builder.toString();
    }

    public String getOutputFilename() {
        return outputFilename;
    }

    public void setOutputFilename(String outputFilename) {
        this.outputFilename = outputFilename;
    }

    public List<String> getParmList() {
        return parmList;
    }

    public void setParmList(List<String> parmList) {
        this.parmList = parmList;
    }

    public DatabaseID getDatabaseID() {
        return databaseID;
    }

    public void setDatabaseID(DatabaseID databaseID) {
        this.databaseID = databaseID;
    }

    public String getStartTime() {
        return startTime;
    }

    public void setStartTime(String startTime) {
        this.startTime = startTime;
    }

    public String getEndTime() {
        return endTime;
    }

    public void setEndTime(String endTime) {
        this.endTime = endTime;
    }

    public String getMask() {
        return mask;
    }

    public void setMask(String mask) {
        this.mask = mask;
    }

    public boolean isGeoInfo() {
        return geoInfo;
    }

    public void setGeoInfo(boolean geoInfo) {
        this.geoInfo = geoInfo;
    }

    public boolean isCompressFile() {
        return compressFile;
    }

    public void setCompressFile(boolean compressFile) {
        this.compressFile = compressFile;
    }

    public String getConfigFileName() {
        return configFileName;
    }

    public void setConfigFileName(String configFileName) {
        this.configFileName = configFileName;
    }

    public int getCompressFileFactor() {
        return compressFileFactor;
    }

    public void setCompressFileFactor(int compressFileFactor) {
        this.compressFileFactor = compressFileFactor;
    }

    public boolean isTrim() {
        return trim;
    }

    public void setTrim(boolean trim) {
        this.trim = trim;
    }

    public boolean isKrunch() {
        return krunch;
    }

    public void setKrunch(boolean krunch) {
        this.krunch = krunch;
    }

    public String getUserID() {
        return userID;
    }

    public void setUserID(String userID) {
        this.userID = userID;
    }

    public String getLogFileName() {
        return logFileName;
    }

    public void setLogFileName(String logFileName) {
        this.logFileName = logFileName;
    }
}
