package gov.noaa.nws.ncep.viz.rsc.wavesat.rsc;

import java.util.Date;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter; 

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;

import gov.noaa.nws.ncep.viz.common.RGBColorAdapter;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.ui.display.ColorBar;

/**
 * Resource data for WaveSat data.
 * 
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  09/21/2011    #248      Greg Hull    Initial creation.
 *  
 * </pre>
 * 
 * @author ghull
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name="NC-WaveSatResourceData")
public class WaveSatResourceData extends AbstractNatlCntrsRequestableResourceData 
               implements INatlCntrsResourceData {
	
    @XmlElement
    private ColorBar colorBar = null;

//    @XmlElement
//    private String satelliteId = null;
	    
    @XmlElement
    private Integer timeDisplayInterval = null; // in minutes
    
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB timeDisplayColor;
	    
    // NOT Implementing this
//	@XmlElement
//    private boolean displayValues = true; // if false display a symbol instead ("X" implemented as dflt symbol)
//
//    @XmlElement
//    private String symbolName = "ASTERISK"; // TODO : implement

    @XmlElement
    private String fontName = "Times";

    @XmlElement
    private Integer fontSize = 12;

	public WaveSatResourceData() {
        super();
        this.nameGenerator = new AbstractNameGenerator() {
            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
            	return "Significant Wave Height ("+getSatelliteName() +")";
            }
        };

        if( colorBar == null ) {
        	colorBar = new ColorBar();
        	colorBar.addColorBarInterval( 0.0f, Float.POSITIVE_INFINITY, 
        								new RGB(0,255,0) );
        }
    }
	
	@Override
	public void update(Object updateData) {
	}

    public int getSatelliteId() {
    	if( getMetadataMap().containsKey("satelliteId") ) {
    		return Integer.parseInt( getMetadataMap().get("satelliteId").getConstraintValue() );
    	}
    	else if( getMetadataMap().containsKey("said") ) {
    		return Integer.parseInt( getMetadataMap().get("said").getConstraintValue() );
    	}
		return 0;
	}

    public String getSatelliteName() {
    	if( getSatelliteId() == 260 ) {
    		return "Jason-1";
    	}
    	else if( getSatelliteId() == 261 ) {
    		return "Jason-2";
    	}
    	else if( getSatelliteId() == 60 ) {
    		return "Envisat";
    	}
    	else {
    		return "SatelliteId="+Integer.toString( getSatelliteId() );
    	}
  
    }
    
//	public void setSatelliteId(String satelliteId) {
//		this.satelliteId = satelliteId;
//	}

	public RGB getTimeDisplayColor() {
		return timeDisplayColor;
	}

	public void setTimeDisplayColor(RGB timeDisplayColor) {
		this.timeDisplayColor = timeDisplayColor;
	}

	public Integer getTimeDisplayInterval() {
		return timeDisplayInterval;
	}

	public void setTimeDisplayInterval(Integer timeDisplayInterval) {
		this.timeDisplayInterval = timeDisplayInterval;
	}

    public String getFontName() {
		return fontName;
	}

	public void setFontName(String fontName) {
		this.fontName = fontName;
	}

    public Integer getFontSize() {
		return fontSize;
	}

	public void setFontSize(Integer fontSize) {
		this.fontSize = fontSize;
	}

	public ColorBar getColorBar() {
		return colorBar;
	}

	public void setColorBar(ColorBar colorBar) {
		this.colorBar = colorBar;
	}
    
    @Override
	protected AbstractVizResource<?, ?> constructResource(
			LoadProperties loadProperties, PluginDataObject[] objects)
			throws VizException {
		return new WaveSatResource(this, loadProperties);
	}
    
    @Override
    public boolean equals(Object obj) {
        if( !super.equals(obj) ) {
            return false;
        }

        if (obj instanceof WaveSatResourceData == false) {
            return false;
        }

        WaveSatResourceData other = (WaveSatResourceData) obj;
        
        if( !colorBar.equals( other.colorBar ) ||
 //       	!satelliteId.equals( other.satelliteId ) ||
        	!fontName.equals( other.fontName ) ||
        	 fontSize != other.fontSize ||
        	!timeDisplayColor.equals( other.timeDisplayColor ) ||
        	timeDisplayInterval != other.timeDisplayInterval ) {
        	return false;
        }
        
        return true;    
    }
}
