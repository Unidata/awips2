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
import com.raytheon.viz.hydrocommon.constants.FFGConstants.ResolutionLevel;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.GetColorValues;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 27, 2009            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "xmrgResourceData")
@XmlType(name = "xmrgResourceData", namespace = "com.raytheon.viz.hydrocommon.resource.XmrgResourceData")
public class XmrgResourceData extends AbstractResourceData {

	@XmlElement
    private XmrgFile xmrgfile;
    
	private HydroDisplayManager dm = HydroDisplayManager.getInstance();
    
	@XmlElement
    private int duration = 3600;
    
	@XmlElement
    private ResolutionLevel resolution;
    
    private float[] data;
    
    private Rectangle extent;

	@XmlElement
	private String cv_use = "FFG";

	@XmlElement
	private int accumInterval = 0;

	private List<Colorvalue> colorList;
    
    public XmrgResourceData() {
        
    }
    
    public XmrgResourceData(XmrgFile xmrgfile, int duration, ResolutionLevel resolution) {
        this.xmrgfile = xmrgfile;
        this.dm = HydroDisplayManager.getInstance();
        this.duration = duration;
        this.resolution = resolution;
    }
    
    public XmrgResourceData(float[] data, int duration, ResolutionLevel resolution, Rectangle extent) {
        this.dm = HydroDisplayManager.getInstance();
        this.duration = duration;
        this.resolution = resolution;
        this.data = data;
        this.extent = extent;
    }
    
    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractResourceData#construct(com.raytheon
     * .uf.viz.core.comm.LoadProperties,
     * com.raytheon.uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public AbstractVizResource<?, ?> construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {

		String cvu = null;
		if (cv_use.equalsIgnoreCase("MULTIHOUR")
				|| cv_use.equalsIgnoreCase("xmrg")) {
			cvu = "XMRG";
		} else {
			cvu = cv_use;
		}

		if (colorList == null) {
			String user_id = System.getProperty("user.name");
			colorList = GetColorValues.get_colorvalues(user_id,
					HydroDisplayManager.HYDRO_APPLICATION_NAME, cvu, duration,
					"E", HydroDisplayManager.getInstance().getNamedColorUseSetList());
		}

        if (xmrgfile == null) {
			return new XmrgResource(this, cv_use, accumInterval, data,
					colorList, extent);
        } else {
			return new XmrgResource(this, cv_use, accumInterval, xmrgfile,
					colorList);
        }
    }

    /**
     * @return the xmrgfile
     */
    public XmrgFile getXmrgfile() {
        return xmrgfile;
    }

    /**
     * @param xmrgfile the xmrgfile to set
     */
    public void setXmrgfile(XmrgFile xmrgfile) {
        this.xmrgfile = xmrgfile;
    }

    /**
     * @return the dm
     */
    public HydroDisplayManager getDm() {
        return dm;
    }

    /**
     * @param dm the dm to set
     */
    public void setDm(HydroDisplayManager dm) {
        this.dm = dm;
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
        final int prime = 31;
        int result = 1;
        result = prime * result + ((dm == null) ? 0 : dm.hashCode());
        return result;
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
     * @param data the data to set
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
     * @param extent the extent to set
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

	public String getCv_use() {
		return cv_use;
	}

	public void setCv_use(String cv_use) {
		this.cv_use = cv_use;
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
