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
 */
package com.raytheon.uf.viz.monitor.snow.resources;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.monitor.snow.SnowMonitor;

/**
 * SnowResourceData
 * 
 * Implements empty display for SNOW
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date             Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    July 20, 2010    4891        skorolev    Initial Creation.
 * 
 * </pre>
 * 
 * @author skorolev
 * @version 1
 */
@XmlAccessorType(XmlAccessType.NONE)
public class SnowResourceData extends AbstractResourceData {
    @XmlAttribute
    protected String plotSource = "SNOW Table";

    protected SnowMonitor monitor;

    /**
     * @return plotSource
     */
    public String getPlotSource() {
        return plotSource;
    }

    /**
     * @param plotSource
     *            the plotSource to set
     */
    public void setPlotSource(String plotSource) {
        this.plotSource = plotSource;
    }

    @Override
    public boolean equals(Object obj) {

        if (obj instanceof SnowResourceData == false) {
            return false;
        }

        SnowResourceData other = (SnowResourceData) obj;

        if (this.plotSource != null && other.plotSource == null) {
            return false;
        } else if (this.plotSource == null && other.plotSource != null) {
            return false;
        } else if (this.plotSource != null
                && this.plotSource.equals(other.plotSource) == false) {
            return false;
        }

        return false;
    }

    /** Get the Snow monitor **/
    protected SnowMonitor getSnowMonitor() {
        if (monitor == null) {
            monitor = SnowMonitor.getInstance();
        }
        return monitor;
    }

    @Override
    public AbstractVizResource<?, ?> construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        SnowResource snowResource = new SnowResource(this, loadProperties);
        this.getSnowMonitor().addSnowResourceListener(snowResource);
        return snowResource;
    }

    @Override
    public void update(Object updateData) {
        // TODO Auto-generated method stub

    }

}
