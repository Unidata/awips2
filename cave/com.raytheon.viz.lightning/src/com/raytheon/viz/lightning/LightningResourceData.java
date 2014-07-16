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
package com.raytheon.viz.lightning;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.binlightning.BinLightningRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * Implements persistable lightning resource properties and factory
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 18, 2009            chammack     Initial creation
 * Feb 27, 2013 DCS 152    jgerth       Support for WWLLN and multiple sources
 * Jun 19, 2014  3214      bclement     added pulse and cloud flash support
 * Jul 07, 2014  3333       bclement    removed plotLightSource field
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class LightningResourceData extends AbstractRequestableResourceData {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(LightningResourceData.class);

    @XmlAttribute
    private boolean handlingPositiveStrikes = true;

    @XmlAttribute
    private boolean handlingNegativeStrikes = true;

    @XmlAttribute
    private boolean handlingCloudFlashes = false;

    @XmlAttribute
    private boolean handlingPulses = false;

    @XmlAttribute
    private int countPosition = 0;
    
    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects) {
        LightningResource rsc = new LightningResource(this, loadProperties,
                countPosition);
        List<BinLightningRecord> records = new ArrayList<BinLightningRecord>(
                objects.length);
        for (PluginDataObject pdo : objects) {
            if (pdo instanceof BinLightningRecord) {
                records.add((BinLightningRecord) pdo);
            } else {
                statusHandler.handle(Priority.PROBLEM,
                        "Received wrong type of data.  Got: " + pdo.getClass()
                                + " Expected: " + BinLightningRecord.class);
            }
        }
        rsc.addRecords(records);

        return rsc;
    }

    @Override
    public boolean isUpdatingOnMetadataOnly() {
    	if (this.isUpdatingOnMetadataOnly == false)
    		return false;
        return true;
    }

    @Override
    public boolean isRetrieveData() {
        return true;
    }

    /**
     * @return the handlingPositiveStrikes
     */
    public boolean isHandlingPositiveStrikes() {
        return handlingPositiveStrikes;
    }

    /**
     * @param handlingPositiveStrikes
     *            the handlingPositiveStrikes to set
     */
    public void setHandlingPositiveStrikes(boolean handlingPositiveStrikes) {
        this.handlingPositiveStrikes = handlingPositiveStrikes;
    }

    /**
     * @return the handlingNegativeStrikes
     */
    public boolean isHandlingNegativeStrikes() {
        return handlingNegativeStrikes;
    }

    /**
     * @param handlingNegativeStrikes
     *            the handlingNegativeStrikes to set
     */
    public void setHandlingNegativeStrikes(boolean handlingNegativeStrikes) {
        this.handlingNegativeStrikes = handlingNegativeStrikes;
    }

    /**
     * @return the handlingCloudFlashes
     */
    public boolean isHandlingCloudFlashes() {
        return handlingCloudFlashes;
    }

    /**
     * @param handlingCloudFlashes
     *            the handlingCloudFlashes to set
     */
    public void setHandlingCloudFlashes(boolean handlingCloudFlashes) {
        this.handlingCloudFlashes = handlingCloudFlashes;
    }

    /**
     * @return the handlingPulses
     */
    public boolean isHandlingPulses() {
        return handlingPulses;
    }

    /**
     * @see #isHandlingCloudFlashes()
     * @see #isHandlingNegativeStrikes()
     * @see #isHandlingPositiveStrikes()
     * @see #isHandlingPulses()
     * @return true if resource data handles exactly one type of data
     */
    public boolean isExclusiveForType() {
        boolean[] bools = { isHandlingCloudFlashes(),
                isHandlingNegativeStrikes(), isHandlingPositiveStrikes(),
                isHandlingPulses() };
        boolean handlingZero = true;
        boolean handlingMoreThanOne = false;
        for (boolean handlingSomething : bools) {
            if (handlingSomething) {
                if (handlingZero) {
                    handlingZero = false;
                } else {
                    handlingMoreThanOne = true;
                    break;
                }
            }
        }
        return !handlingZero && !handlingMoreThanOne;
    }

    /**
     * @param handlingPulses
     *            the handlingPulses to set
     */
    public void setHandlingPulses(boolean handlingPulses) {
        this.handlingPulses = handlingPulses;
    }

    /**
     * @return countPosition
     *            the countPosition to get - JJG
     */
    public int getCountPosition() {
    	return countPosition;
    }

    /**
     * @param countPosition
     *            the countPosition to set - JJG
     */
    public void setCountPosition(int countPosition) {
        this.countPosition = countPosition;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + countPosition;
        result = prime * result + (handlingCloudFlashes ? 1231 : 1237);
        result = prime * result + (handlingNegativeStrikes ? 1231 : 1237);
        result = prime * result + (handlingPositiveStrikes ? 1231 : 1237);
        result = prime * result + (handlingPulses ? 1231 : 1237);
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        LightningResourceData other = (LightningResourceData) obj;
        if (countPosition != other.countPosition)
            return false;
        if (handlingCloudFlashes != other.handlingCloudFlashes)
            return false;
        if (handlingNegativeStrikes != other.handlingNegativeStrikes)
            return false;
        if (handlingPositiveStrikes != other.handlingPositiveStrikes)
            return false;
        if (handlingPulses != other.handlingPulses)
            return false;
        return true;
    }

}
