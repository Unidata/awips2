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
 * Request object to run iscMosaic.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 20, 2010            dgilling     Initial creation
 * Mar 12, 2013  #1759     dgilling     Re-factoring to use fields instead
 *                                      of an argument string.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

@DynamicSerialize
public class ExecuteIscMosaicRequest extends AbstractGfeRequest {

    @DynamicSerializeElement
    private String userID;

    @DynamicSerializeElement
    private DatabaseID databaseID;

    @DynamicSerializeElement
    private List<String> parmsToProcess;

    @DynamicSerializeElement
    private boolean blankOtherPeriods;

    @DynamicSerializeElement
    private String startTime;

    @DynamicSerializeElement
    private String endTime;

    @DynamicSerializeElement
    private String altMask;

    @DynamicSerializeElement
    private boolean replaceOnly;

    @DynamicSerializeElement
    private boolean eraseFirst;

    @DynamicSerializeElement
    private String announce;

    @DynamicSerializeElement
    private boolean renameWE;

    @DynamicSerializeElement
    private boolean iscSends;

    @DynamicSerializeElement
    private List<String> inFiles;

    @DynamicSerializeElement
    private boolean ignoreMask;

    @DynamicSerializeElement
    private boolean adjustTranslate;

    @DynamicSerializeElement
    private boolean deleteInput;

    @DynamicSerializeElement
    private List<String> parmsToIgnore;

    @DynamicSerializeElement
    private float gridDelay;

    @DynamicSerializeElement
    private String logFileName;

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("ExecuteIscMosaicRequest [workstationID=");
        builder.append(workstationID);
        builder.append(", siteID=");
        builder.append(siteID);
        builder.append(", userID=");
        builder.append(userID);
        builder.append(", databaseID=");
        builder.append(databaseID);
        builder.append(", parmsToProcess=");
        builder.append(parmsToProcess);
        builder.append(", blankOtherPeriods=");
        builder.append(blankOtherPeriods);
        builder.append(", startTime=");
        builder.append(startTime);
        builder.append(", endTime=");
        builder.append(endTime);
        builder.append(", altMask=");
        builder.append(altMask);
        builder.append(", replaceOnly=");
        builder.append(replaceOnly);
        builder.append(", eraseFirst=");
        builder.append(eraseFirst);
        builder.append(", announce=");
        builder.append(announce);
        builder.append(", renameWE=");
        builder.append(renameWE);
        builder.append(", iscSends=");
        builder.append(iscSends);
        builder.append(", inFiles=");
        builder.append(inFiles);
        builder.append(", ignoreMask=");
        builder.append(ignoreMask);
        builder.append(", adjustTranslate=");
        builder.append(adjustTranslate);
        builder.append(", deleteInput=");
        builder.append(deleteInput);
        builder.append(", parmsToIgnore=");
        builder.append(parmsToIgnore);
        builder.append(", gridDelay=");
        builder.append(gridDelay);
        builder.append(", logFileName=");
        builder.append(logFileName);
        builder.append("]");
        return builder.toString();
    }

    public String getUserID() {
        return userID;
    }

    public void setUserID(String userID) {
        this.userID = userID;
    }

    public DatabaseID getDatabaseID() {
        return databaseID;
    }

    public void setDatabaseID(DatabaseID databaseID) {
        this.databaseID = databaseID;
    }

    public List<String> getParmsToProcess() {
        return parmsToProcess;
    }

    public void setParmsToProcess(List<String> parmsToProcess) {
        this.parmsToProcess = parmsToProcess;
    }

    public boolean isBlankOtherPeriods() {
        return blankOtherPeriods;
    }

    public void setBlankOtherPeriods(boolean blankOtherPeriods) {
        this.blankOtherPeriods = blankOtherPeriods;
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

    public String getAltMask() {
        return altMask;
    }

    public void setAltMask(String altMask) {
        this.altMask = altMask;
    }

    public boolean isReplaceOnly() {
        return replaceOnly;
    }

    public void setReplaceOnly(boolean replaceOnly) {
        this.replaceOnly = replaceOnly;
    }

    public boolean isEraseFirst() {
        return eraseFirst;
    }

    public void setEraseFirst(boolean eraseFirst) {
        this.eraseFirst = eraseFirst;
    }

    public String getAnnounce() {
        return announce;
    }

    public void setAnnounce(String announce) {
        this.announce = announce;
    }

    public boolean isRenameWE() {
        return renameWE;
    }

    public void setRenameWE(boolean renameWE) {
        this.renameWE = renameWE;
    }

    public boolean isIscSends() {
        return iscSends;
    }

    public void setIscSends(boolean iscSends) {
        this.iscSends = iscSends;
    }

    public List<String> getInFiles() {
        return inFiles;
    }

    public void setInFiles(List<String> inFiles) {
        this.inFiles = inFiles;
    }

    public boolean isIgnoreMask() {
        return ignoreMask;
    }

    public void setIgnoreMask(boolean ignoreMask) {
        this.ignoreMask = ignoreMask;
    }

    public boolean isAdjustTranslate() {
        return adjustTranslate;
    }

    public void setAdjustTranslate(boolean adjustTranslate) {
        this.adjustTranslate = adjustTranslate;
    }

    public boolean isDeleteInput() {
        return deleteInput;
    }

    public void setDeleteInput(boolean deleteInput) {
        this.deleteInput = deleteInput;
    }

    public List<String> getParmsToIgnore() {
        return parmsToIgnore;
    }

    public void setParmsToIgnore(List<String> parmsToIgnore) {
        this.parmsToIgnore = parmsToIgnore;
    }

    public float getGridDelay() {
        return gridDelay;
    }

    public void setGridDelay(float gridDelay) {
        this.gridDelay = gridDelay;
    }

    public void setLogFileName(String logFileName) {
        this.logFileName = logFileName;
    }

    public String getLogFileName() {
        return logFileName;
    }

}
