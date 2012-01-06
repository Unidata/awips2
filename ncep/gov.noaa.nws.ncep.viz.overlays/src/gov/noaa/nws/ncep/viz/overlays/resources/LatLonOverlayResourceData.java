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

import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.common.RGBColorAdapter;

/**
 * Resource to draw Latitude and Longitude Lines
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * July 20, 2009           mgao        Initial creation
 * Aug 03, 2009            ghull       rm 'Attr' getter methods
 * Aug 06, 2009            ghull       construct() -> constructResource()
 * Apr 14  2010    #259    ghull       set legendColor from color
 * 
 * </pre>
 * 
 * This class is copied over from com.raytheon.viz.core.rsc.DbMapResourceData
 * 
 * @author mgao
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name="NC-LatLonOverlayResourceData")
public class LatLonOverlayResourceData extends AbstractNatlCntrsResourceData {

    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB color = new RGB(255, 0, 0);
	
    @XmlElement
    private int lineWidth = 1;

    @XmlElement
    private int latitudeInterval = 5; 
    
    @XmlElement
    private int longitudeInterval = 5; 

    @XmlElement
    private LineStyle lineStyle;

    public LatLonOverlayResourceData() {
        super();
        this.nameGenerator = new AbstractNameGenerator() {
            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
                return "Latitude/Longitude";
            }
        };
    }
    
	@Override
	public LatLonOverlayResource constructResource(LoadProperties loadProperties,
			IDescriptor descriptor) throws VizException {
		LatLonOverlayResource resource = new LatLonOverlayResource(this, loadProperties);
        return resource;
	}

	public int getLatitudeInterval() {
		return latitudeInterval;
	}

	public LineStyle getLineStyle() {
		return lineStyle;
	}

	public int getLineWidth() {
		return lineWidth;
	}

	public int getLongitudeInterval() {
		return longitudeInterval;
	}

	public void setLatitudeInterval(int _latitudeInterval) {
		latitudeInterval = _latitudeInterval;
	}

	public void setLineStyle(LineStyle _lineStyle) {
		lineStyle = _lineStyle; 
	}

	public void setLineWidth(int _lineWidth) {
		lineWidth = _lineWidth; 
	}

	public void setLongitudeInterval(int _longitudeInterval) {
		longitudeInterval = _longitudeInterval; 
	}

	public RGB getColor() {
		return color;
	}

	public void setColor(RGB _color) {
		color = _color; 
		this.legendColor = color;
	}

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || obj instanceof LatLonOverlayResourceData == false) {
            return false;
        }
        LatLonOverlayResourceData other = (LatLonOverlayResourceData) obj;


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
    	return "LatLonResource";  // what's this used for?
    }
    
}
