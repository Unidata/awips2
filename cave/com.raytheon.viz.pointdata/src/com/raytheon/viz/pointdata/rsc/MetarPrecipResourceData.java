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
package com.raytheon.viz.pointdata.rsc;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 19, 2011            bsteffen     Initial creation
 * Jan 10, 2019 DCS 20579  MPorricelli Modified to handle ice accum
 * Aug 08  2019 DR 21515   MPorricelli Add differentiation between 
 *                                     precip and ice accum fields
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class MetarPrecipResourceData extends AbstractRequestableResourceData {

    @XmlAttribute
    private int duration;

    @XmlAttribute
    protected String plotSource = null;

    @XmlAttribute
    private String ptype;

    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {
        return new MetarPrecipResource(this, loadProperties);
    }

    public int getDuration() {
        return duration;
    }

    public void setDuration(int duration) {
        this.duration = duration;
    }

    /**
     * @return the plotSource
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

    public String getPtype() {
        return ptype;
    }

    public void setPtype(String ptype) {
        this.ptype = ptype;
    }

    @Override
    public boolean isUpdatingOnMetadataOnly() {
        return true;
    }

    @Override
    public boolean isRetrieveData() {
        return false;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + duration;
        result = prime * result + ((ptype == null) ? 0 : ptype.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        MetarPrecipResourceData other = (MetarPrecipResourceData) obj;
        if (ptype != other.ptype) {
            return false;
        }
        if (duration != other.duration)
            return false;
        return true;
    }

}
