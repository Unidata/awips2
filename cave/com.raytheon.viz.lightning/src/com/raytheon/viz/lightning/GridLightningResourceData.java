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

import java.util.Arrays;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * Configuration data needed to render lightning as grids
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 7, 2014  3333      bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class GridLightningResourceData extends LightningResourceData {

    @XmlAttribute
    /* defaults to 40km if not specified in bundle */
    private int kmResolution = 40;

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.lightning.LightningResourceData#constructResource(com
     * .raytheon.uf.viz.core.rsc.LoadProperties,
     * com.raytheon.uf.common.dataplugin.PluginDataObject[])
     */
    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects) {
        GridLightningResource rval = new GridLightningResource(this,
                loadProperties);
        rval.add(Arrays.asList(objects));
        return rval;
    }

    /**
     * @return the kmResolution
     */
    public int getKmResolution() {
        return kmResolution;
    }

    /**
     * @param kmResolution
     *            the kmResolution to set
     */
    public void setKmResolution(int kmResolution) {
        this.kmResolution = kmResolution;
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
        result = prime * result + kmResolution;
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
        GridLightningResourceData other = (GridLightningResourceData) obj;
        if (kmResolution != other.kmResolution)
            return false;
        return true;
    }

}
