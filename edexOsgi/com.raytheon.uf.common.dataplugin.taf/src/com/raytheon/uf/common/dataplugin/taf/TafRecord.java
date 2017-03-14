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

package com.raytheon.uf.common.dataplugin.taf;

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Record implementation for taf plugin
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Feb 14, 2007 139         bphillip    Initial Creation
 * Jun 21, 2007 180         bphillip    Updated to use new plugin pattern
 * Nov 29, 2007 472         jkorman     Added IDecoderGettable interface.
 * Apr 04, 2013 1846        bkowal      Added an index on refTime and
 *                                      forecastTime
 * Apr 12, 2013 1857        bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 1869        bsteffen    Remove dataURI column from
 *                                      PluginDataObject.
 * Aug 30, 2013 2298        rjpeter     Make getPluginName abstract
 * Nov 01, 2013 2361        njensen     Remove XML annotations
 * Feb 10, 2014 2777        rferrel     Assign parent id when setting ChangeGroup.
 * Feb 11, 2014 2784        rferrel     Remove override of setIdentifier.
 * May 15, 2014 3002        bgonzale    Moved to com.raytheon.uf.common.dataplugin.taf.
 * Oct 10, 2014 3722        mapeters    Removed dataURI column.
 * Apr 01, 2015 3722        rjpeter     Made dataURI fields required, changed amd and corIndicator to boolean.
 * Sep 21, 2015 4890        rferrel     Removal of Change Group.
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "tafseq")
// TODO: remove issue_time/issue_timeString from table, same as reftime in
// different format
@Table(name = TafRecord.PLUGIN_NAME, uniqueConstraints = { @UniqueConstraint(name = "uk_taf_datauri_fields", columnNames = {
        "reftime", "stationid", "corindicator", "amdindicator",
        "issue_timestring" }) })
@org.hibernate.annotations.Table(appliesTo = TafRecord.PLUGIN_NAME, indexes = { @Index(name = "taf_refTimeIndex", columnNames = { "refTime" }) })
@DynamicSerialize
public class TafRecord extends PluginDataObject implements ISpatialEnabled {

    private static final long serialVersionUID = 1L;

    public static final String PLUGIN_NAME = "taf";

    @DynamicSerializeElement
    @Column
    private String wmoHeader;

    @DynamicSerializeElement
    @Column(length = 1024)
    private String tafText;

    // Station Identifier for the data
    @DynamicSerializeElement
    @Column(nullable = false)
    @Index(name = "taf_stationIndex")
    @DataURI(position = 1)
    private String stationId;

    @DynamicSerializeElement
    @DataURI(position = 2)
    @Column(nullable = false)
    private boolean corIndicator;

    @DynamicSerializeElement
    @Column(nullable = false)
    @DataURI(position = 3)
    private boolean amdIndicator;

    /** Issue date */
    @DynamicSerializeElement
    @Column
    private Date issue_time;

    /** Issue date string */
    @DynamicSerializeElement
    @Column(nullable = false)
    @DataURI(position = 4)
    private String issue_timeString;

    /** Bulletin issuance time */
    @DynamicSerializeElement
    @Column
    private Date bulletin_time;

    /** Any remarks contained in the TAF record */
    @DynamicSerializeElement
    @Column
    private String remarks;

    @ManyToOne
    @PrimaryKeyJoinColumn
    @DynamicSerializeElement
    protected ObStation location;

    public TafRecord() {
    }

    /**
     * Constructs a taf record from a dataURI
     * 
     * @param uri
     *            The dataURI
     * @param tableDef
     *            The table definition associated with this class
     */
    public TafRecord(String uri) {
        super(uri);
    }

    /**
     * Get the WMO header for the enclosing WMO message.
     * 
     * @return The wmoHeader.
     */
    public String getWmoHeader() {
        return wmoHeader;
    }

    /**
     * Set the WMO header for the enclosing WMO message.
     * 
     * @param wmoHeader
     *            The WMOHeader to set.
     */
    public void setWmoHeader(String wmoHeader) {
        this.wmoHeader = wmoHeader;
    }

    /**
     * Get the text of this terminal forecast.
     * 
     * @return The terminal forecast text.
     */
    public String getTafText() {
        return tafText;
    }

    /**
     * Set the text of this terminal forecast.
     * 
     * @param tafText
     *            The terminal forecast text.
     */
    public void setTafText(String tafText) {
        this.tafText = tafText;
    }

    /**
     * 
     * @return
     */
    public String getStationId() {
        return stationId;
    }

    /**
     * 
     * @param stationID
     */
    public void setStationId(String stationID) {
        stationId = stationID;
    }

    /**
     * 
     * @return the corIndicator
     */
    public boolean getCorIndicator() {
        return corIndicator;
    }

    /**
     * 
     * @param corIndicator
     *            the corIndicator to set
     */
    public void setCorIndicator(boolean corIndicator) {
        this.corIndicator = corIndicator;
    }

    /**
     * 
     * @return the amdIndicator
     */
    public boolean getAmdIndicator() {
        return amdIndicator;
    }

    /**
     * 
     * @param amdIndicator
     *            the amdIndicator to set
     */
    public void setAmdIndicator(boolean amdIndicator) {
        this.amdIndicator = amdIndicator;
    }

    /**
     * 
     * @return the bulletin_time
     */
    public Date getBulletin_time() {
        return bulletin_time;
    }

    /**
     * 
     * @param bulletin_time
     *            the bulletin_time to set
     */
    public void setBulletin_time(Date bulletin_time) {
        this.bulletin_time = bulletin_time;
    }

    /**
     * @return the issue_time
     */
    public Date getIssue_time() {
        return issue_time;
    }

    /**
     * @param issue_time
     *            the issue_time to set
     */
    public void setIssue_time(Date issue_time) {
        this.issue_time = issue_time;
    }

    /**
     * @return the issue_timeString
     */
    public String getIssue_timeString() {
        return issue_timeString;
    }

    /**
     * @param issue_timeString
     *            the issue_time to set
     */
    public void setIssue_timeString(String issue_timeString) {
        this.issue_timeString = issue_timeString;
    }

    /**
     * @return the remarks
     */
    public String getRemarks() {
        return remarks;
    }

    /**
     * @param remarks
     *            the remarks to set
     */
    public void setRemarks(String remarks) {
        this.remarks = remarks;
    }

    @Override
    public ObStation getSpatialObject() {
        return location;
    }

    public ObStation getLocation() {
        return location;
    }

    public void setLocation(ObStation location) {
        this.location = location;
    }

    /**
     * Returns the hashCode for this object. This implementation returns the
     * hashCode of the generated dataURI.
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = (prime * result)
                + ((getDataURI() == null) ? 0 : getDataURI().hashCode());
        return result;
    }

    /**
     * Checks if this record is equal to another by checking the generated
     * dataURI.
     * 
     * @param obj
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        TafRecord other = (TafRecord) obj;
        if (getDataURI() == null) {
            if (other.getDataURI() != null) {
                return false;
            }
        } else if (!getDataURI().equals(other.getDataURI())) {
            return false;
        }
        return true;
    }

    @Override
    public String getPluginName() {
        return PLUGIN_NAME;
    }
}
