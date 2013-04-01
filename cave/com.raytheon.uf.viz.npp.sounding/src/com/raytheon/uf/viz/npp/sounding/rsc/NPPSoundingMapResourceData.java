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
package com.raytheon.uf.viz.npp.sounding.rsc;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.npp.AbstractNppResourceData;

/**
 * Resource data for availability map of npp soundings
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 14, 2013            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class NPPSoundingMapResourceData extends AbstractNppResourceData {

    @XmlElement
    private Class<? extends AbstractNPPNSharpResourceData> nsharpResourceData;

    @XmlElement
    private String resourceName;

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData#
     * constructResource(com.raytheon.uf.viz.core.rsc.LoadProperties,
     * com.raytheon.uf.viz.core.rsc.PluginDataObject[])
     */
    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {
        NPPSoundingMapResource resource = new NPPSoundingMapResource(this,
                loadProperties);
        if (objects instanceof PluginDataObject[]) {
            resource.addRecords((PluginDataObject[]) objects);
        }
        return resource;
    }

    /**
     * @return the nsharpResourceData
     */
    public Class<? extends AbstractNPPNSharpResourceData> getNsharpResourceData() {
        return nsharpResourceData;
    }

    /**
     * @param nsharpResourceData
     *            the nsharpResourceData to set
     */
    public void setNsharpResourceData(
            Class<? extends AbstractNPPNSharpResourceData> nsharpResourceData) {
        this.nsharpResourceData = nsharpResourceData;
    }

    /**
     * @return the resourceName
     */
    public String getResourceName() {
        return resourceName;
    }

    /**
     * @param resourceName
     *            the resourceName to set
     */
    public void setResourceName(String resourceName) {
        this.resourceName = resourceName;
    }

    public AbstractNPPNSharpResourceData newNsharpResourceData() {
        try {
            return getNsharpResourceData().newInstance();
        } catch (Exception e) {
            throw new RuntimeException("Unable to create new instance of: "
                    + getNsharpResourceData());
        }
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
        result = prime
                * result
                + ((nsharpResourceData == null) ? 0 : nsharpResourceData
                        .hashCode());
        result = prime * result
                + ((resourceName == null) ? 0 : resourceName.hashCode());
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
        NPPSoundingMapResourceData other = (NPPSoundingMapResourceData) obj;
        if (nsharpResourceData == null) {
            if (other.nsharpResourceData != null)
                return false;
        } else if (!nsharpResourceData.equals(other.nsharpResourceData))
            return false;
        if (resourceName == null) {
            if (other.resourceName != null)
                return false;
        } else if (!resourceName.equals(other.resourceName))
            return false;
        return true;
    }

}
