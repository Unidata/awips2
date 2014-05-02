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

import java.awt.Rectangle;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;


import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.uf.common.mpe.util.XmrgFile;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.GetColorValues;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 16, 2011            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "xmrgResourceData")
@XmlType(name = "xmrgResourceData", namespace = "com.raytheon.viz.mpe.ui.dialogs.postanalysis.PAXmrgResourceData")
public class PAXmrgResourceData extends AbstractResourceData {
    @XmlElement
    private XmrgFile xmrgfile;

    private float[] data;

    private Rectangle extent;

    @XmlElement
    private String cvUse = "FFG";

    @XmlElement
    private int accumInterval = 0;

    private List<Colorvalue> colorList;

    //no-arg default constructor
    public PAXmrgResourceData() {

    }
    
    public PAXmrgResourceData(XmrgFile xmrgfile, String cvUse) {
        this.xmrgfile = xmrgfile;
        this.cvUse = cvUse;
    }

    public PAXmrgResourceData(float[] data, Rectangle extent, String cvUse) {
        this.data = data;
        this.extent = extent;
        this.cvUse = cvUse;
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
        String cvu = null;
        if (cvUse.equalsIgnoreCase("MULTIHOUR")
                || cvUse.equalsIgnoreCase("xmrg")) {
            cvu = "XMRG";
        } else {
            cvu = cvUse;
        }

        if (colorList == null) {
            String user_id = System.getProperty("user.name");
            int duration = 1;
            colorList = GetColorValues.get_colorvalues(user_id,
                    HydroDisplayManager.MPE_APPLICATION_NAME, cvu, duration,
                    "E", HydroDisplayManager.getInstance()
                            .getNamedColorUseSetList());
        }

        if (xmrgfile == null) {
            return new PAXmrgResource(this, cvUse, data, colorList, extent);
        } else {
            return new PAXmrgResource(this, cvUse, xmrgfile, colorList);
        }
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

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        MPEDisplayManager dm = MPEDisplayManager.getCurrent();
        final int prime = 31;
        int result = 1;
        result = prime * result + ((dm == null) ? 0 : dm.hashCode());
        return result;
    }

    /**
     * @param data
     *            the data to set
     */
    public void setData(float[] data) {
        this.data = data;
    }

    /**
     * @return the data
     */
    public float[] getData() {
        return data;
    }

    /**
     * @param extent
     *            the extent to set
     */
    public void setExtent(Rectangle extent) {
        this.extent = extent;
    }

    /**
     * @return the extent
     */
    public Rectangle getExtent() {
        return extent;
    }

    public String getCvUse() {
        return cvUse;
    }

    public void setCvUse(String cvUse) {
        this.cvUse = cvUse;
    }

    public int getAccumInterval() {
        return accumInterval;
    }

    public void setAccumInterval(int accumInterval) {
        this.accumInterval = accumInterval;
    }

    public List<Colorvalue> getColorList() {
        return colorList;
    }

    public void setColorList(List<Colorvalue> colorList) {
        this.colorList = colorList;
    }
}
