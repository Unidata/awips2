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
package com.raytheon.viz.mpe.ui.rsc;

import javax.measure.unit.NonSI;
import javax.measure.unit.Unit;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.mpe.ui.MPEPlotType;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 6, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class MPEPlotGriddedResourceData extends AbstractMPEGriddedResourceData {

    @XmlElement
    private MPEPlotType plotType;

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
        switch (plotType) {
        case T_24hGRID_PRECIP:
        case T_6hGRID_PRECIP:
            // TODO: Replace PlotGriddedPrecipResource.java
            break;
        case T_maxGRID_TEMP:
        case T_minGRID_TEMP:
        case T_sixhGRID_TEMP:
            // TODO: Replace PlotGriddedTempResource.java
            break;
        case T_6hGRID_FREEZL:
            // TODO: Replace PlotGriddedFreezeResource.java
            break;
        case T_24hMAREAPRECIP:
        case T_6hMAREA_PRECIP:
            // TODO: Replace PlotMeanAreaPreipResource.java
            break;
        case T_6hMAREA_FREEZL:
            // TODO: Replace PlotMeanAreaFreezeResource.java
            break;
        case T_sixhMAREA_TEMP:
            // TODO: Replace PlotMeanAreaTempResource.java
            break;
        default:
            throw new VizException("Unknown MPEPlotType: " + plotType);
        }
        return null;
    }

    /**
     * @return the plotType
     */
    public MPEPlotType getPlotType() {
        return plotType;
    }

    /**
     * @param plotType
     *            the plotType to set
     */
    public void setPlotType(MPEPlotType plotType) {
        this.plotType = plotType;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.mpe.ui.rsc.AbstractMPEGriddedResourceData#getDurationInHours
     * ()
     */
    @Override
    public int getDurationInHours() {
        return plotType != null ? plotType.getDurationInHrs() : 1;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.mpe.ui.rsc.AbstractMPEGriddedResourceData#getCvUseString
     * ()
     */
    @Override
    public String getCvUseString() {
        return plotType != null ? plotType.getCvUse() : null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.mpe.ui.rsc.AbstractMPEGriddedResourceData#getDataUnits()
     */
    @Override
    public Unit<?> getDataUnits() {
        switch (plotType) {
        case T_24hGRID_PRECIP:
        case T_6hGRID_PRECIP:
        case T_24hMAREAPRECIP:
        case T_6hMAREA_PRECIP:
            return NonSI.INCH.divide(100.0);
        case T_6hGRID_FREEZL:
        case T_6hMAREA_FREEZL:
            return NonSI.FOOT.divide(100.0);
        case T_minGRID_TEMP:
        case T_maxGRID_TEMP:
        case T_sixhGRID_TEMP:
        case T_sixhMAREA_TEMP:
            return NonSI.FAHRENHEIT.divide(100.0);
        }
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.mpe.ui.rsc.AbstractMPEGriddedResourceData#getDisplayUnits
     * ()
     */
    @Override
    public Unit<?> getDisplayUnits() {
        switch (plotType) {
        case T_24hGRID_PRECIP:
        case T_6hGRID_PRECIP:
        case T_24hMAREAPRECIP:
        case T_6hMAREA_PRECIP:
            return NonSI.INCH;
        case T_6hGRID_FREEZL:
        case T_6hMAREA_FREEZL:
            return NonSI.FOOT;
        case T_minGRID_TEMP:
        case T_maxGRID_TEMP:
        case T_sixhGRID_TEMP:
        case T_sixhMAREA_TEMP:
            return NonSI.FAHRENHEIT;
        }
        return null;
    }

}
