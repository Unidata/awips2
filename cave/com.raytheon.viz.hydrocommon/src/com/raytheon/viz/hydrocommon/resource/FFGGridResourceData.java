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

import java.util.Date;

import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.hydrocommon.constants.FFGConstants.ResolutionLevel;

/**
 * FFG Resource Data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 14, 2009 2256       mpduff     Initial creation.  Moved here
 *                                    for additional functionality.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@XmlType(name = "ffgGridResourceData")
public class FFGGridResourceData extends AbstractResourceData {

    private int duration;

    private GridRecord gridRecord;

    private ResolutionLevel resolution;

    private Date dataDate;

    public FFGGridResourceData() {

    }

    public FFGGridResourceData(int duration, GridRecord gr,
            ResolutionLevel resolution, Date dataDate) {
        this.setDuration(duration);
        this.setGridRecord(gr);
        this.setResolution(resolution);
        this.setDataDate(dataDate);
    }

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

    /* (non-Javadoc)
     * @see com.raytheon.uf.viz.core.rsc.AbstractResourceData#construct(com.raytheon.uf.viz.core.rsc.LoadProperties, com.raytheon.uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public AbstractVizResource<?, ?> construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {

        return new FFGGridResource(this, loadProperties);
    }

    /* (non-Javadoc)
     * @see com.raytheon.uf.viz.core.rsc.AbstractResourceData#update(java.lang.Object)
     */
    @Override
    public void update(Object updateData) {
        // TODO Auto-generated method stub

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
     * @param gribRecord the gribRecord to set
     */
    public void setGridRecord(GridRecord gridRecord) {
        this.gridRecord = gridRecord;
    }

    /**
     * @return the gribRecord
     */
    public GridRecord getGridRecord() {
        return gridRecord;
    }

    /**
     * @param resolution the resolution to set
     */
    public void setResolution(ResolutionLevel resolution) {
        this.resolution = resolution;
    }

    /**
     * @return the resolution
     */
    public ResolutionLevel getResolution() {
        return resolution;
    }

    /**
     * @param dataDate the dataDate to set
     */
    public void setDataDate(Date dataDate) {
        this.dataDate = dataDate;
    }

    /**
     * @return the dataDate
     */
    public Date getDataDate() {
        return dataDate;
    }
}
