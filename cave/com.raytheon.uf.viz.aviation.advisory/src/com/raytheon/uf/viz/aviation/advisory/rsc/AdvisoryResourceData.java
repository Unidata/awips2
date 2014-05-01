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
package com.raytheon.uf.viz.aviation.advisory.rsc;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.viz.aviation.advisory.adapter.AbstractAdvisoryDataAdapter;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;

/**
 * 
 * Resource Data for Advisories which consist of a simple outLine data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 1, 2009            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class AdvisoryResourceData extends AbstractRequestableResourceData {

    @XmlElement
    private AbstractAdvisoryDataAdapter dataAdapter;

    @XmlAttribute
    private String name;

    @XmlAttribute
    private String colorString;

    @XmlAttribute
    private boolean enableNonstandardInspect = false;

    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {
        AdvisoryResource nr = new AdvisoryResource(this, loadProperties);
        if (colorString != null) {
            nr.getCapability(ColorableCapability.class).setColorAsString(
                    colorString);
        }
        for (PluginDataObject o : objects) {
            nr.addRecord(o);
        }
        return nr;
    }

    public AbstractAdvisoryDataAdapter getDataAdapter() {
        return dataAdapter;
    }

    public void setDataAdapter(AbstractAdvisoryDataAdapter dataAdapter) {
        this.dataAdapter = dataAdapter;
    }

    public void setEnableNonstandardInspect(boolean enableNonstandardInspect) {
        this.enableNonstandardInspect = enableNonstandardInspect;
    }

    public boolean isEnableNonstandardInspect() {
        return enableNonstandardInspect;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public void setColorString(String colorString) {
        this.colorString = colorString;
    }

    public String getColorString() {
        return colorString;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result
                + ((colorString == null) ? 0 : colorString.hashCode());
        result = prime * result
                + ((dataAdapter == null) ? 0 : dataAdapter.hashCode());
        result = prime * result + (enableNonstandardInspect ? 1231 : 1237);
        result = prime * result + ((name == null) ? 0 : name.hashCode());
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
        AdvisoryResourceData other = (AdvisoryResourceData) obj;
        if (colorString == null) {
            if (other.colorString != null)
                return false;
        } else if (!colorString.equals(other.colorString))
            return false;
        if (dataAdapter == null) {
            if (other.dataAdapter != null)
                return false;
        } else if (!dataAdapter.equals(other.dataAdapter))
            return false;
        if (enableNonstandardInspect != other.enableNonstandardInspect)
            return false;
        if (name == null) {
            if (other.name != null)
                return false;
        } else if (!name.equals(other.name))
            return false;
        return true;
    }

}
