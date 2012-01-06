package gov.noaa.nws.ncep.viz.overlays.resources;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter; 

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;

import gov.noaa.nws.ncep.viz.overlays.IScaleOverlayResourceData.ScaleModel;
import gov.noaa.nws.ncep.viz.overlays.IScaleOverlayResourceData.ScalePosition;
import gov.noaa.nws.ncep.viz.overlays.IScaleOverlayResourceData.ScaleTextSize;
import gov.noaa.nws.ncep.viz.overlays.IScaleOverlayResourceData.ScaleUnit;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.common.RGBColorAdapter;

/**
 * Resource to draw Scale
 * 
 * Derived from LatLonOverlayResourceData
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06 Oct 2010  311        B. Hebbard  Initial creation (from LatLonOverlayResourceData)
 * 
 * </pre>
 * 
 * This class is copied over from com.raytheon.viz.core.rsc.DbMapResourceData
 * 
 * @author bhebbard
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name="NC-ScaleOverlayResourceData")
public class ScaleOverlayResourceData extends AbstractNatlCntrsResourceData {

    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB color = new RGB(0, 255, 0);

    @XmlElement
    private Integer scaleModel = 0;
	
    @XmlElement
    private Integer scalePosition = 0;
	
    @XmlElement
    private Integer scaleUnit = 0;

    @XmlElement
    private Integer scaleIntervalMode = 0; 
    
    @XmlElement
    private Integer scaleIntervalValue = 0; 

    @XmlElement
    private Integer scaleLatMode = 0; 
    
    @XmlElement
    private Integer scaleEffectiveLatitudeValue = 0; 

    @XmlElement
    private Integer scaleTextFont = 0;

    @XmlElement
    private Integer scaleTextSize = 0;

    @XmlElement
    private Integer scaleTextStyle = 0;    

    public ScaleOverlayResourceData() {
        super();
        this.nameGenerator = new AbstractNameGenerator() {
            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
                //setColor (new RGB(0,255,0));
                return "Distance Scale";
            }
        };
        setColor (new RGB(0,255,0));
    }
    
	@Override
	public ScaleOverlayResource constructResource(LoadProperties loadProperties,
			IDescriptor descriptor) throws VizException {
		ScaleOverlayResource resource = new ScaleOverlayResource(this, loadProperties);
        return resource;
	}

	public RGB getColor() {
		return color;
	}

	public void setColor(RGB _color) {
		color = _color; 
		this.legendColor = color;
	}

    public Integer getScaleModel() {
		return scaleModel;
	}

	public void setScaleModel(Integer scaleModel) {
		this.scaleModel = scaleModel;
	}

	public Integer getScaleLatMode() {
		return scaleLatMode;
	}

	public void setScaleLatMode(Integer scaleLatMode) {
		this.scaleLatMode = scaleLatMode;
	}

	public Integer getScaleEffectiveLatitudeValue() {
		return scaleEffectiveLatitudeValue;
	}

	public void setScaleEffectiveLatitudeValue(Integer scaleEffectiveLatitudeValue) {
		this.scaleEffectiveLatitudeValue = scaleEffectiveLatitudeValue;
	}

	public Integer getScalePosition() {
		return scalePosition;
	}

	public void setScalePosition(Integer scalePosition) {
		this.scalePosition = scalePosition;
	}

	public Integer getScaleUnit() {
		return scaleUnit;
	}

	public void setScaleUnit(Integer scaleUnit) {
		this.scaleUnit = scaleUnit;
	}

	public Integer getScaleIntervalMode() {
		return scaleIntervalMode;
	}

	public void setScaleIntervalMode(Integer scaleIntervalMode) {
		this.scaleIntervalMode = scaleIntervalMode;
	}

	public Integer getScaleIntervalValue() {
		return scaleIntervalValue;
	}

	public void setScaleIntervalValue(Integer scaleIntervalValue) {
		this.scaleIntervalValue = scaleIntervalValue;
	}

	public Integer getScaleTextFont() {
		return scaleTextFont;
	}

	public void setScaleTextFont(Integer scaleTextFont) {
		this.scaleTextFont = scaleTextFont;
	}

	public Integer getScaleTextSize() {
		return scaleTextSize;
	}

	public void setScaleTextSize(Integer scaleTextSize) {
		this.scaleTextSize = scaleTextSize;
	}

	public Integer getScaleTextStyle() {
		return scaleTextStyle;
	}

	public void setScaleTextStyle(Integer scaleTextStyle) {
		this.scaleTextStyle = scaleTextStyle;
	}

	@Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || obj instanceof ScaleOverlayResourceData == false) {
            return false;
        }
        ScaleOverlayResourceData other = (ScaleOverlayResourceData) obj;

        // TODO : complete this...
        
//        if (this.labelField != null && other.labelField == null) {
//            return false;
//        } else if (this.labelField == null && other.labelField != null) {
//            return false;
//        } else if (this.labelField != null
//                && this.labelField.equals(other.labelField) == false) {
//            return false;
//        }

        return true;
    }

    @Override
    public String toString() {
    	return "ScaleOverlayResource";  // what's this used for?
    }
    
}
