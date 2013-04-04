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
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
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
    private String plotLightSource = "";

    @XmlAttribute
    private int countPosition = 0;
    
    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects) {
    	// jjg add
    	String ls = "";
    	if (this.metadataMap.containsKey("lightSource"))
    		ls = this.metadataMap.get("lightSource").getConstraintValue();
    	else if (!plotLightSource.isEmpty()) {
    		ls = plotLightSource;
    		RequestConstraint lsrc = new RequestConstraint(ls, ConstraintType.EQUALS);
    		this.metadataMap.put("lightSource", lsrc);
    	}
    	int pa = 0;
    	if (countPosition != 0)
    		pa = countPosition;
    	// end
    	LightningResource rsc = new LightningResource(this, loadProperties, ls, pa);
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
     * @return plotLightSource
     *            the lightSource to get - JJG
     */
    public String getPlotLightSource() {
    	return plotLightSource;
    }
    
    /**
     * @param plotLightSource
     *            the lightSource to set - JJG
     */
    public void setPlotLightSource(String plotLightSource) {
        this.plotLightSource = plotLightSource;
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

    @Override
    public boolean equals(Object obj) {
        // TODO Auto-generated method stub
        if (!super.equals(obj)) {
            return false;
        }

        if (obj instanceof LightningResourceData == false) {
            return false;
        }

        LightningResourceData other = (LightningResourceData) obj;
        return (this.handlingNegativeStrikes == other.handlingNegativeStrikes && this.handlingPositiveStrikes == other.handlingPositiveStrikes &&
        		this.plotLightSource == other.plotLightSource && this.countPosition == other.countPosition);
    }

}
