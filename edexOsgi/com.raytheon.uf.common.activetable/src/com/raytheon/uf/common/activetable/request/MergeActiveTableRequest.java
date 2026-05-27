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
package com.raytheon.uf.common.activetable.request;

import java.util.Map;

import com.raytheon.uf.common.activetable.ActiveTableMode;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * Request to merge in a provided collection of new active table entries to the
 * specified active table using the legacy MergeVTEC logic. Used by CLI
 * utilities ingestAT/MergeVTEC for active table sharing.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 13, 2010            wldougher     Initial creation
 * Feb 13, 2013   1447     dgilling     Added additional fields to 
 *                                      better support VTEC table sharing.
 * 
 * </pre>
 * 
 * @author wldougher
 * @version 1.0
 */

@DynamicSerialize
public class MergeActiveTableRequest implements IServerRequest {

    @DynamicSerializeElement
    private Map<String, Object>[] incomingRecords;

    @DynamicSerializeElement
    private String site;

    @DynamicSerializeElement
    private ActiveTableMode tableName;

    @DynamicSerializeElement
    private float timeOffset;

    @DynamicSerializeElement
    private String xmlSource;

    @DynamicSerializeElement
    private boolean fromIngestAT;

    @DynamicSerializeElement
    private boolean makeBackups;

    /**
     * No argument constructor. Not intended to be used by anyone, except
     * DynamicSerialize.
     */
    public MergeActiveTableRequest() {
        this(null, ActiveTableMode.PRACTICE, null, 0.0f, null, false, true);
    }

    /**
     * Builds a MergeActiveTableRequest.
     * 
     * @param incomingRecords
     *            Array containing active table entries in the python-style dict
     *            or Map format.
     * @param tableName
     *            Table the new records will be merged into.
     * @param site
     *            3-char site id to perform the merge operation as.
     * @param timeOffset
     *            For DRT; the number of seconds away from current time to use
     *            as the base time for the merge.
     * @param xmlSource
     *            MHS XML data that contains who sent these active table
     *            records.
     * @param fromIngestAT
     *            Whether to run ingestAT or MergeVTEC to perform the merge.
     * @param makeBackups
     *            Whether to save a backup copy of the active table prior to the
     *            merge operation.
     */
    public MergeActiveTableRequest(Map<String, Object>[] incomingRecords,
            ActiveTableMode tableName, String site, float timeOffset,
            String xmlSource, boolean fromIngestAT, boolean makeBackups) {
        this.incomingRecords = incomingRecords;
        this.tableName = tableName;
        this.site = site;
        this.timeOffset = timeOffset;
        this.xmlSource = xmlSource;
        this.fromIngestAT = fromIngestAT;
        this.makeBackups = makeBackups;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("MergeActiveTableRequest [incomingRecords=");
        builder.append(incomingRecords);
        builder.append(", tableName=");
        builder.append(tableName);
        builder.append(", site=");
        builder.append(site);
        builder.append(", timeOffset=");
        builder.append(timeOffset);
        builder.append(", xmlSource=");
        builder.append(xmlSource);
        builder.append(", fromIngestAT=");
        builder.append(fromIngestAT);
        builder.append(", makeBackups=");
        builder.append(makeBackups);
        builder.append("]");
        return builder.toString();
    }

    public Map<String, Object>[] getIncomingRecords() {
        return incomingRecords;
    }

    public void setIncomingRecords(Map<String, Object>[] incomingRecords) {
        this.incomingRecords = incomingRecords;
    }

    public ActiveTableMode getTableName() {
        return tableName;
    }

    public void setTableName(ActiveTableMode tableName) {
        this.tableName = tableName;
    }

    public float getTimeOffset() {
        return timeOffset;
    }

    public void setTimeOffset(float timeOffset) {
        this.timeOffset = timeOffset;
    }

    public String getXmlSource() {
        return xmlSource;
    }

    public void setXmlSource(String xmlSource) {
        this.xmlSource = xmlSource;
    }

    public void setFromIngestAT(boolean fromIngestAT) {
        this.fromIngestAT = fromIngestAT;
    }

    public boolean isFromIngestAT() {
        return fromIngestAT;
    }

    public void setMakeBackups(boolean makeBackups) {
        this.makeBackups = makeBackups;
    }

    public boolean isMakeBackups() {
        return makeBackups;
    }

    public void setSite(String site) {
        this.site = site;
    }

    public String getSite() {
        return site;
    }
}
