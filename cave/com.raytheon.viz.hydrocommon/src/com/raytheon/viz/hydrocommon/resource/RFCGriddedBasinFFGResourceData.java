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
import java.util.List;

import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.constants.FFGConstants.ResolutionLevel;

/**
 * RFC Gridded Basin FFG Display
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 24, 2011            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class RFCGriddedBasinFFGResourceData extends AbstractResourceData {
    private ResolutionLevel resolution;

    private int duration;

    private Date dataDate;

    private GridRecord gr;

    public RFCGriddedBasinFFGResourceData(int duration, GridRecord gr,
            ResolutionLevel resolution, Date dataDate) {
        this.duration = duration;
        this.gr = gr;
        this.resolution = resolution;
        this.dataDate = dataDate;
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
        String userId = System.getProperty("user.name");
        List<Colorvalue> ffgColorList = HydroDisplayManager.getInstance().getFFGColorMap(userId, "FFG", duration);
        
        return new RFCGriddedBasinFFGResource(this, loadProperties, ffgColorList);
    }

    /**
     * @return the resolution
     */
    public ResolutionLevel getResolution() {
        return resolution;
    }

    /**
     * @param resolution the resolution to set
     */
    public void setResolution(ResolutionLevel resolution) {
        this.resolution = resolution;
    }

    /**
     * @return the duration
     */
    public int getDuration() {
        return duration;
    }

    /**
     * @param duration the duration to set
     */
    public void setDuration(int duration) {
        this.duration = duration;
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
     * @return the gr
     */
    public GridRecord getGr() {
        return gr;
    }

    /**
     * @param gr the gr to set
     */
    public void setGr(GridRecord gr) {
        this.gr = gr;
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
        result = prime * result + ((gr == null) ? 0 : gr.hashCode());
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
