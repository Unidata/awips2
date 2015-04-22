package gov.noaa.nws.ncep.viz.overlays.resources;


import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ProgressiveDisclosureProperties;

import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.common.RGBColorAdapter;
import gov.noaa.nws.ncep.viz.common.StringListAdapter;
import gov.noaa.nws.ncep.viz.common.staticPointDataSource.IStaticPointDataSource.StaticPointDataSourceType;
import gov.noaa.nws.ncep.viz.common.ui.Markers.MarkerState;
import gov.noaa.nws.ncep.viz.common.ui.Markers.MarkerTextSize;
import gov.noaa.nws.ncep.viz.common.ui.Markers.MarkerType;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ----------- ---------- ----------- --------------------------
 * 07/03/13     #1010     ghull       Initial creation
 * 
 * </pre>
 *  * 
 * @author ghull
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name="PointOverlayResourceData")
public class PointOverlayResourceData extends AbstractNatlCntrsResourceData 
		implements INatlCntrsResourceData  { 
	
	// ex. LPI, SPI, NCEP DB TBL,.... Stations files, common_obs_spatial table
    @XmlElement
    private StaticPointDataSourceType sourceType = null;

    @XmlElement
    private String sourceName = null; // the filename for LPI/SPI, the tablename for ncep stns tbles

    // source-specific info needed to create the PointDataSource
    @XmlElement
	@XmlJavaTypeAdapter(StringListAdapter.class)
	private ArrayList<String> sourceParams;

    // one or more fields from the source data used to create the label.
    // (Currently only one is supported.) 
//	@XmlElement
//	@XmlJavaTypeAdapter(StringListAdapter.class)
//	private ArrayList<String>  fieldNames;

// the name of an adapter class used to create the label from 1 or more 'fields'    
//    @XmlElement
//    private String labelCreator 

    /** The human readable name for the legend */
    @XmlElement
    private String mapName = null;
    
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB color = new RGB(255, 0, 0);
	
    @XmlElement
    private MarkerState markerState;

    @XmlElement
    private MarkerType markerType; 
    
    @XmlElement
    private Float markerSize; 

    @XmlElement
    private Integer markerWidth;

    @XmlElement
    private MarkerTextSize markerTextSize;

    // same as values in ProgressiveDisclosureProperties
    // this value represents the maxMapWidth of the screen in km  
    @XmlElement
    private Integer maxSymbolDisplayWidth;

    @XmlElement
    private Integer maxLabelDisplayWidth;
    
    public PointOverlayResourceData() {
        super();
    }

    @Override
    public PointOverlayResource constructResource(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        return new PointOverlayResource(this, loadProperties);
    }

    /**
     * @return the mapName
     */
    public String getMapName() {
        return mapName;
    }

    /**
     * @param mapName
     *            the mapName to set
     */
    public void setMapName(String mapName) {
        this.mapName = mapName;
    }
	
    public RGB getColor	() {
		return color;
	}

	public void setColor(RGB color) {
		this.color = color;
		this.legendColor = color;
	}

	public MarkerState getMarkerState() {
		return markerState;
	}

	public void setMarkerState(MarkerState markerState) {
		this.markerState = markerState;
	}

	public MarkerType getMarkerType() {
		return markerType;
	}

	public void setMarkerType(MarkerType markerType) {
		this.markerType = markerType;
	}

	public float getMarkerSize() {
		return markerSize;
	}

	public void setMarkerSize(float markerSize) {
		this.markerSize = markerSize;
	}

	public int getMarkerWidth() {
		return markerWidth;
	}

	public void setMarkerWidth(int markerWidth) {
		this.markerWidth = markerWidth;
	}

	public MarkerTextSize getMarkerTextSize() {
		return markerTextSize;
	}

	public void setMarkerTextSize(MarkerTextSize markerTextSize) {
		this.markerTextSize = markerTextSize;
	}
	
	public StaticPointDataSourceType getSourceType() {
		return sourceType;
	}

	public void setSourceType(StaticPointDataSourceType sourceType) {
		this.sourceType = sourceType;
	}

	public String getSourceName() {
		return sourceName;
	}

	public void setSourceName(String sourceName) {
		this.sourceName = sourceName;
	}

	public ArrayList<String> getSourceParams() {
		return sourceParams;
	}

	public void setSourceParams(ArrayList<String> sp) {
		this.sourceParams = sp;
	}

//	public ArrayList<String> getFieldNames() {
//		return fieldNames;
//	}
//
//	public void setFieldNames(ArrayList<String> fieldNames) {
//		this.fieldNames = fieldNames;
//	}

	public Integer getMaxSymbolDisplayWidth() {
		return maxSymbolDisplayWidth;
	}

	public void setMaxSymbolDisplayWidth(Integer maxSymbolDisplayWidth) {
		this.maxSymbolDisplayWidth = maxSymbolDisplayWidth;
	}

	public Integer getMaxLabelDisplayWidth() {
		return maxLabelDisplayWidth;
	}

	public void setMaxLabelDisplayWidth(Integer maxLabelDisplayWidth) {
		this.maxLabelDisplayWidth = maxLabelDisplayWidth;
	}

	public void setMarkerSize(Float markerSize) {
		this.markerSize = markerSize;
	}

	public void setMarkerWidth(Integer markerWidth) {
		this.markerWidth = markerWidth;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((color == null) ? 0 : color.hashCode());
		result = prime * result
				+ ((sourceParams == null) ? 0 : sourceParams.hashCode());
		result = prime * result + ((mapName == null) ? 0 : mapName.hashCode());
		result = prime * result
				+ ((sourceName == null) ? 0 : sourceName.hashCode());
		result = prime * result
				+ ((sourceType == null) ? 0 : sourceType.hashCode());
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
		PointOverlayResourceData other = (PointOverlayResourceData) obj;
		if (color == null) {
			if (other.color != null)
				return false;
		} else if (!color.equals(other.color))
			return false;
		if (sourceParams == null) {
			if (other.sourceParams != null)
				return false;
		} else if (!sourceParams.equals(other.sourceParams))
			return false;
		if (mapName == null) {
			if (other.mapName != null)
				return false;
		} else if (!mapName.equals(other.mapName))
			return false;
		if (sourceName == null) {
			if (other.sourceName != null)
				return false;
		} else if (!sourceName.equals(other.sourceName))
			return false;
		if (sourceType != other.sourceType)
			return false;
		return true;
	}
}
