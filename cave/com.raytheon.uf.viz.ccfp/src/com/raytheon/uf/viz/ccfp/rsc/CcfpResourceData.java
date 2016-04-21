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
package com.raytheon.uf.viz.ccfp.rsc;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.ccfp.CcfpRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * 
 * ResourceData for CCFP data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 22, 2009 3072       bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class CcfpResourceData extends AbstractRequestableResourceData {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CcfpResourceData.class);

    // This flag determnies if we draw lines and polygons
    @XmlAttribute
    private boolean displayArea = true;

    // This flag is used to determine if we dipslat movement arrows
    @XmlAttribute
    private boolean displayMovement = true;

    // This flag determines if we display text boxes
    @XmlAttribute
    private boolean displayText = true;

    // Filter by coverage
    @XmlAttribute
    private int coverageFilter = 0;

    @XmlAttribute
    private int validDuration = 0;

    @Override
    public boolean equals(Object obj) {
        if (!super.equals(obj)) {
            return false;
        }

        if (obj instanceof CcfpResourceData == false) {
            return false;
        }

        CcfpResourceData other = (CcfpResourceData) obj;

        if (other.coverageFilter != this.coverageFilter) {
            return false;
        }

        if (other.validDuration != this.validDuration) {
            return false;
        }

        if (other.displayText != this.displayText) {
            return false;
        }

        if (other.displayArea != this.displayArea) {
            return false;
        }

        if (other.displayMovement != this.displayMovement) {
            return false;
        }

        return true;
    }

    /**
     * Overriden to make sure the datatimes we get are applicable for our
     * duration
     */
    @Override
    public DataTime[] getAvailableTimes() throws VizException {
        DataTime[] originalTimes = super.getAvailableTimes();
        ArrayList<DataTime> newTimes = new ArrayList<DataTime>();
        for (DataTime time : originalTimes) {
            if (durationMatches(time)) {
                newTimes.add(time);
            }
        }
        return newTimes.toArray(new DataTime[newTimes.size()]);
    }

    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {
        CcfpResource nr = new CcfpResource(this, loadProperties);
        for (PluginDataObject o : objects) {
            if (o instanceof CcfpRecord) {
                CcfpRecord rec = (CcfpRecord) o;
                nr.addRecord(rec);
            } else {
                statusHandler.handle(Priority.PROBLEM,
                        "Received wrong type of data.  Got: " + o.getClass()
                                + " Expected: " + CcfpRecord.class);
            }
        }
        return nr;
    }

    /**
     * Checks if a DataTime's valid period matches the duration of the resource
     * data
     * 
     * @param time
     *            the time to check
     * @return true if the durations are equal, otherwise false
     */
    protected boolean durationMatches(DataTime time) {
        return time.getValidPeriod().getDuration() == validDuration * 1000;
    }

    public boolean isDisplayArea() {
        return displayArea;
    }

    public void setDisplayArea(boolean displayArea) {
        this.displayArea = displayArea;
    }

    public boolean isDisplayMovement() {
        return displayMovement;
    }

    public void setDisplayMovement(boolean displayMovement) {
        this.displayMovement = displayMovement;
    }

    public boolean isDisplayText() {
        return displayText;
    }

    public void setDisplayText(boolean displayText) {
        this.displayText = displayText;
    }

    public int getCoverageFilter() {
        return coverageFilter;
    }

    public void setCoverageFilter(int coverageFilter) {
        this.coverageFilter = coverageFilter;
    }

    public int getValidDuration() {
        return validDuration;
    }

    public void setValidDuration(int validDuration) {
        this.validDuration = validDuration;
    }

}
