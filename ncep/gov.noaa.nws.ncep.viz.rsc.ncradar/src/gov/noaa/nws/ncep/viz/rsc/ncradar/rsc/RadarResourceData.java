/*
 * gov.noaa.nws.ncep.viz.rsc.ncradar.rsc.RadarResourceData
 * 
 * 12-08-2011
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.viz.rsc.ncradar.rsc;

import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.rsc.ncradar.rsc.image.RadarRadialResource;
import gov.noaa.nws.ncep.viz.rsc.ncradar.rsc.image.RadarRasterResource;
import gov.noaa.nws.ncep.viz.ui.display.ColorBarFromColormap;

import java.util.HashSet;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.radar.util.RadarInfo;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.NoDataAvailableException;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.radar.interrogators.IRadarInterrogator;
import gov.noaa.nws.ncep.viz.rsc.ncradar.rsc.AbstractRadarResource;
import gov.noaa.nws.ncep.viz.rsc.ncradar.rsc.RadarProductFactory;

/**
 * Provides the metadata and constructor for Radar
 * 
 * This class is based on Raytheon's code.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 12/08/2011      #541       S. Gurung   Initial creation
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name="NC-RadarResourceData")
public class RadarResourceData extends AbstractNatlCntrsRequestableResourceData {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(RadarResourceData.class);

    @XmlAttribute
    protected String pointID = "";

    // This might be better as an enumeration, currently "CZ-Pg" triggers
    // Composite Reflectivity to display as graphics rather than image and
    // "SRM" causes Velocity to do SRM stuff
    @XmlAttribute
    protected String mode = "";

    // Will only display the most recently ingested tilt for any time
    @XmlAttribute
    protected boolean latest = false;

    @XmlAttribute
    protected boolean rangeRings = true;
    
    @XmlElement
    private String colorMapName;

	@XmlElement
    private ColorBarFromColormap colorBar;

	@XmlElement
	private Float alpha;

	@XmlElement
	private Float brightness;
	
	@XmlElement
	private Float contrast;
	
	public Float getAlpha() {
		return alpha;
	}

	public void setAlpha(Float alpha) {
		this.alpha = alpha;
	}

	public Float getBrightness() {
		return brightness;
	}

	public void setBrightness(Float brightness) {
		this.brightness = brightness;
	}

	public Float getContrast() {
		return contrast;
	}

	public void setContrast(Float contrast) {
		this.contrast = contrast;
	}

	public RadarResourceData() {
        nameGenerator = new RadarNameGenerator();
    }

    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {
        AbstractVizResource<?, ?> rrd = null;
        String format = null;
        int productCode = -1;
        RequestConstraint productCodeConstraint = metadataMap
                .get("productCode");
        if (productCodeConstraint != null
                && productCodeConstraint.getConstraintType() == ConstraintType.EQUALS) {
            try {
                productCode = Integer.parseInt(productCodeConstraint
                        .getConstraintValue());
            } catch (NumberFormatException e) {
                // It was a good effort but it just wasn't meant to be.
            }
        }
        if (AbstractRadarResource.infoDict != null) {
            RadarInfo info = AbstractRadarResource.infoDict
                    .getInfo(productCode);
            if (info != null) {
                format = info.getFormat();
            }
        }
        if (format != null && productCode != -1) {
        	 IRadarInterrogator interrogator = RadarProductFactory.buildInterrogator(productCode, format);

		     if (!"".equals(format)) {
		         rrd = RadarProductFactory.buildResource(this, loadProperties,
		                 interrogator, productCode, format);
		         /*for (PluginDataObject p : objects) {
		             ((AbstractRadarResource<?>) rrd).addRecord(p);
		         }*/
		     }
        	if ("Raster".equals(format)) {
                   rrd = new RadarRasterResource(this, loadProperties, interrogator);
            } else if ("Radial".equals(format)) {
                    rrd = new RadarRadialResource(this, loadProperties, interrogator);
            }
        } else if ("".equals(format)) {
            statusHandler.handle(Priority.ERROR,
                    "There is not format defined in radarInfo.txt");
        } else {
            throw new NoDataAvailableException();
        }
        return rrd;
    }

    /**
     * @return the pointID
     */
    public String getPointID() {
        return pointID;
    }

    /**
     * @param pointID
     *            the pointID to set
     */
    public void setPointID(String pointID) {
        this.pointID = pointID;
    }

    /**
     * @return the latest
     */
    public boolean isLatest() {
        return latest;
    }

    /**
     * @param latest
     *            the latest to set
     */
    public void setLatest(boolean latest) {
        this.latest = latest;
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
        result = prime * result + (latest ? 1231 : 1237);
        result = prime * result + ((mode == null) ? 0 : mode.hashCode());
        result = prime * result + ((pointID == null) ? 0 : pointID.hashCode());
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
        RadarResourceData other = (RadarResourceData) obj;
        if (latest != other.latest)
            return false;
        if (mode == null) {
            if (other.mode != null)
                return false;
        } else if (!mode.equals(other.mode))
            return false;
        if (pointID == null) {
            if (other.pointID != null)
                return false;
        } else if (!pointID.equals(other.pointID))
            return false;
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData#
     * getAvailableTimes()
     */
    @Override
    public DataTime[] getAvailableTimes() throws VizException {
        DataTime[] all = super.getAvailableTimes();
        if (latest) {
            Set<DataTime> times = new HashSet<DataTime>();
            for (DataTime time : all) {
                time.setLevelValue(null);
                times.add(time);
            }
            all = times.toArray(new DataTime[0]);
        }
        return all;
    }

    public String getColorMapName() {
		return colorMapName;
	}

	public void setColorMapName(String colorMapName) {
		this.colorMapName = colorMapName;
	}
	
	
    public ColorBarFromColormap getColorBar() {
		return colorBar;
	}

	public void setColorBar(ColorBarFromColormap cBar) {
		this.colorBar = cBar;
	}		
}

