package gov.noaa.nws.ncep.viz.rsc.lightning.rsc;

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
 * Resource data for Lightning data.
 * 
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  04/10/2010    #257     	Greg Hull    Initial creation.
 *  04/11/2010    #259      Greg Hull    Added ColorBar
 *  04/22/2011    #439      Greg Hull    show strike count in legend
 *  
 * </pre>
 * 
 * @author ghull
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name="LightningResourceData")
public class LightningResourceData extends AbstractNatlCntrsRequestableResourceData 
               implements INatlCntrsResourceData {
			
    @XmlElement
    private Boolean enablePositiveStrikes=true;
    
	@XmlElement
    private Boolean enableNegativeStrikes=true;

	@XmlElement
	private Integer positiveSymbolSize=3;

	@XmlElement
	private Integer negativeSymbolSize=3;

	@XmlElement
	private ColorBar colorBar = null;
	
    // if true this will cause the strikes to be colored useing the colorBar and the intensity
    // values. This will also mean that the timeMatching is changed so that only data for the given 
    // frame times will show in a frame.
    @XmlElement
    private Boolean colorByIntensity=false;
    
    public Boolean getColorByIntensity() {
		return colorByIntensity;
	}

	public void setColorByIntensity(Boolean colorByIntensity) {
		this.colorByIntensity = colorByIntensity;
		this.timeMatchMethod = ( colorByIntensity ? TimeMatchMethod.CLOSEST_BEFORE_OR_AFTER :
			                                        TimeMatchMethod.BEFORE_OR_EQUAL );
	}

	public Boolean getEnablePositiveStrikes() {
		return enablePositiveStrikes;
	}

	public void setEnablePositiveStrikes(Boolean enablePositiveStrikes) {
		this.enablePositiveStrikes = enablePositiveStrikes;
	}

	public Boolean getEnableNegativeStrikes() {
		return enableNegativeStrikes;
	}

	public void setEnableNegativeStrikes(Boolean enableNegativeStrikes) {
		this.enableNegativeStrikes = enableNegativeStrikes;
	}
    
	public ColorBar getColorBar() {
		return colorBar;
	}

	public void setColorBar(ColorBar colorBar) {
		this.colorBar = colorBar;
	}

	public Integer getPositiveSymbolSize() {
		return positiveSymbolSize;
	}

	public void setPositiveSymbolSize(Integer positiveSymbolSize) {
		this.positiveSymbolSize = positiveSymbolSize;
	}

	public Integer getNegativeSymbolSize() {
		return negativeSymbolSize;
	}

	public void setNegativeSymbolSize(Integer negativeSymbolSize) {
		this.negativeSymbolSize = negativeSymbolSize;
	}

	// base the lineWidth off of the symbolSize. The symbol size is from
	// 1 to 10 and the line width from 
	public Float getLineWidth() {		
		return positiveSymbolSize.floatValue()/3;
	}

    public LightningResourceData() {
        super();
        this.nameGenerator = new AbstractNameGenerator() {
            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
            	if( resource instanceof LightningResource ) {
            		return "Lightning ("+
            		    ((LightningResource)resource).getNumDisplayedStrikes() + " Strikes)";
            	}
            	else return "Lightning";
            }
        };
        if( colorBar == null ) {
        	colorBar = new ColorBar();
        	if( colorByIntensity ) {
        		colorBar.addColorBarInterval( Float.NEGATIVE_INFINITY, 0.0f, 
        				new RGB(255,0,0) );
        		colorBar.addColorBarInterval( 0.0f, Float.POSITIVE_INFINITY, 
        				new RGB(0,0,255) );
        	}
        	else {
        		colorBar.addColorBarInterval( 0.0f, 10f, new RGB(255,0,0) );
        		colorBar.addColorBarInterval( 10f, 30f, new RGB(0,255,0) );
        		colorBar.addColorBarInterval( 30f, 60f, new RGB(0,0,255) );
        	}
        }

        // This is done to force all of the data before the frame end time to be matched to the
        // frame. A strike will be displayed on multiple frames.
        timeMatchMethod = TimeMatchMethod.BEFORE_OR_EQUAL;
    }
	
	@Override
	public void update(Object updateData) {
		// TODO Auto-generated method stub
	}

    @Override
	protected AbstractVizResource<?, ?> constructResource(
			LoadProperties loadProperties, PluginDataObject[] objects)
			throws VizException {
		return new LightningResource(this, loadProperties);
	}
	
    @Override
    public boolean equals(Object obj) {
        if( !(obj instanceof LightningResourceData) ) {
            return false;
        }

        if( !super.equals(obj) ) {
            return false;
        }

        LightningResourceData other = (LightningResourceData) obj;
        
        if( enablePositiveStrikes != other.enablePositiveStrikes ||
            enableNegativeStrikes != other.enableNegativeStrikes ||
            positiveSymbolSize    != other.positiveSymbolSize ||
            negativeSymbolSize    != other.negativeSymbolSize ) {
        	return false;
        }
        if( colorByIntensity != other.colorByIntensity ) {
        	return false;
        }
        if( (colorBar == null && other.colorBar != null ) ||
            (colorBar != null && other.colorBar == null ) ) {
            	return false;
        }
        if( !colorBar.equals( other.colorBar ) ) {
        	return false;
        }
     	
        return true;
    }

}
