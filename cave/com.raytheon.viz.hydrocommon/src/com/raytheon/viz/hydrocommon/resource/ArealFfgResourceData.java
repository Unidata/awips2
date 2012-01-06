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
package com.raytheon.viz.hydrocommon.resource;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.hydrocommon.data.ArealData;

/**
 * Displays the Areal FFG Data from the Contingency Value table
 * in IHFS.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 22, 2011            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class ArealFfgResourceData extends AbstractResourceData {
    /** List of ArealData objects */
    private List<ArealData> arealDataList = new ArrayList<ArealData>();
    
    /** The data duration */
    private int duration;
    
    /** The data resolution Basin, County, Zone */
    private String resolution;
    
    /** The data reference time */
    private Date dataDate;
    
    /** Display the value */
    private boolean displayValues = true;
    
    /** Display the id */
    private boolean displayIds = true;
    
    /** Data change flag */
    private boolean dataChanged = true;
    
    /**
     * Constructor
     * 
     * @param arealDataList
     */
    public ArealFfgResourceData(List<ArealData> arealDataList, int duration, 
            String resolution, Date dataDate, boolean displayValues, boolean displayIds) {
        this.arealDataList = arealDataList;
        this.duration = duration;
        this.resolution = resolution;
        this.dataDate = dataDate;
        this.displayIds = displayIds;
        this.displayValues = displayValues;
    }
    
    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractResourceData#construct(com.raytheon
     * .uf.viz.core.rsc.LoadProperties,
     * com.raytheon.uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public AbstractVizResource<?, ?> construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        return new ArealFfgResource(this, loadProperties);
    }

    /**
     * @return the arealDataList
     */
    public List<ArealData> getArealDataList() {
        return arealDataList;
    }

    /**
     * @param arealDataList the arealDataList to set
     */
    public void setArealDataList(List<ArealData> arealDataList) {
        this.arealDataList = arealDataList;
    }

    /**
     * @param duration the duration to set
     */
    public void setDuration(int duration) {
        this.duration = duration;
    }

    /**
     * @return the duration
     */
    public int getDuration() {
        return duration;
    }

    /**
     * @return the resolution
     */
    public String getResolution() {
        return resolution;
    }

    /**
     * @param resolution the resolution to set
     */
    public void setResolution(String resolution) {
        this.resolution = resolution;
    }

    /**
     * @return the dataDate
     */
    public Date getDataDate() {
        return dataDate;
    }

    /**
     * @param dataDate the dataDate to set
     */
    public void setDataDate(Date dataDate) {
        this.dataDate = dataDate;
    }

    /**
     * @return the displayValues
     */
    public boolean isDisplayValues() {
        return displayValues;
    }

    /**
     * @param displayValues the displayValues to set
     */
    public void setDisplayValues(boolean displayValues) {
        this.displayValues = displayValues;
        dataChanged = true;
    }

    /**
     * @return the displayIds
     */
    public boolean isDisplayIds() {
        return displayIds;
    }

    /**
     * @param displayIds the displayIds to set
     */
    public void setDisplayIds(boolean displayIds) {
        this.displayIds = displayIds;
        dataChanged = true;
    }
    
    public boolean isDataChanged() {
        return dataChanged;
    }
    
    public void setDataChanged(boolean dataChanged) {
        this.dataChanged = dataChanged;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractResourceData#update(java.lang.Object
     * )
     */
    @Override
    public void update(Object updateData) {
        // TODO Auto-generated method stub

    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((arealDataList == null) ? 0 : arealDataList.hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractResourceData#equals(java.lang.Object
     * )
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        return true;
    }

}
