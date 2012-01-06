package gov.noaa.nws.ncep.viz.rsc.ffg.rsc;

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
 * Resource data for FFG data.
 * 
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  04/01/2009             	Greg Hull    Initial creation for to10.
 *  06/18/2009    115      	Greg Hull    Integrate with AbstractNatlCntrsResource
 *  07/01/2009	  134		M. Li		 Replace ffgCountyZone.stn with ffgZones.xml
 *  08/12/2009              M. Li	     Initial creation/migration to to11
 *  08/18/2009    147       Greg Hull    timeInterval moved to Abstract
 *  03/24/2010    259       Greg Hull    Add colorBar
 *  
 * </pre>
 * 
 * @author mli
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name="NC-FFGResourceData")
public class FFGResourceData extends AbstractNatlCntrsRequestableResourceData 
               implements INatlCntrsResourceData {
	
    @XmlElement
    private ColorBar colorBar = null;
	    
    @XmlElement
    private boolean displayValues = true; // if false display a symbol instead ("X" implemented as dflt symbol)

    public static enum FfgParam {
    	FF01, FF03, FF06, FF12, FF24
    }
    
    @XmlElement
    private FfgParam ffgParam = FfgParam.FF01; 

    @XmlElement
    private String symbolName = "ASTERISK"; // TODO : implement

    public FFGResourceData() {
        super();
        this.nameGenerator = new AbstractNameGenerator() {
            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
        		return "FFG-"+ffgParam.toString().substring("FF".length()) + "HR";
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
		// TODO Auto-generated method stub
	}

    public void setDisplayValues(boolean dv) {
        this.displayValues = dv;
    }

    public boolean getDisplayValues() {
        return this.displayValues;
    }

    public void setSymbolName(String snStr ) {
        this.symbolName = snStr;
    }

    public String getSymbolName() {
        return this.symbolName;
    }
    
    public void setFfgParam( FfgParam p) {
        this.ffgParam = p;
    }

    public FfgParam getFfgParam() {
        return this.ffgParam;
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
		return new FFGResource(this, loadProperties);
	}
    
    @Override
    public boolean equals(Object obj) {
        if (!super.equals(obj)) {
            return false;
        }

        if (obj instanceof FFGResourceData == false) {
            return false;
        }

        FFGResourceData other = (FFGResourceData) obj;

        // NOTE; The resource attributes are compared in the base class so se don't
        // need to check them here.

        if (this.ffgParam != other.ffgParam) {
            return false;
        }

        if( this.displayValues != other.displayValues ) {
        	return false;
        }

        return true;    
    }
}
