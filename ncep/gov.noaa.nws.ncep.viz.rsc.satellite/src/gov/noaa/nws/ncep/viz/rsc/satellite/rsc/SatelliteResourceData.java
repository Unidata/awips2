package gov.noaa.nws.ncep.viz.rsc.satellite.rsc;

import java.text.ParseException;
import java.text.ParsePosition;

import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.ui.display.ColorBarFromColormap;

/**
 * Resource data for satellite data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * July 10, 2009           mgao		   Initial creation
 * July 31, 2009           ghull       to11 migration
 * Aug  26, 2009           ghull       Integrate with AbstractNatlCntrsResource
 * Apr  15, 2010   #259    ghull       Added ColorBar
 * 
 * This class is copied from com.raytheon.viz.satellite.rsc.SatResourceData
 * for To 11 integration
 * 
 * </pre>
 * 
 * @author mgao
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name="NcSatelliteResourceData")
public class SatelliteResourceData extends AbstractNatlCntrsRequestableResourceData {

	enum SatelliteType {
		GINI, MCIDAS
	}
	
	@XmlElement
	private Float alpha;

	@XmlElement
	private Float brightness;
	
	@XmlElement
	private Float contrast;
	
	@XmlElement
    private String colorMapName;
	
	@XmlElement
    private ColorBarFromColormap colorBar;

	@XmlElement
	private SatelliteType satelliteType;
	
	@XmlElement
	private String displayUnitStr;
	
	private Unit<?> displayUnit;
	
	private AbstractVizResource<?, ?> satRsc = null;
	
    public SatelliteResourceData() {
        super();
        
        displayUnit = null;
        displayUnitStr = null;
        
        this.nameGenerator = new AbstractNameGenerator() {
            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
            	return ((AbstractSatelliteResource)resource).getLegendString();
            	
//            	String satName = metadataMap.get("satelliteName").getConstraintValue();
//            	String imgType="";
//            	if( satType == SatelliteType.GINI ) {
//            		imgType = metadataMap.get("physicalElement").getConstraintValue();
//            	}
//            	else if( satType == SatelliteType.MCIDAS ) {
//            		imgType = metadataMap.get("imageType").getConstraintValue();
//            	}
//            	String areaName = metadataMap.get("areaName").getConstraintValue();
//            	
//            	return satName+"-"+imgType +" ("+satType.toString() +")";            	
            }
        };
        
    }

	
    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects) {
//        records = new SatelliteRecord[objects.length];
//        for (int i = 0; i < objects.length; i++) {
//            records[i] = (SatelliteRecord) objects[i];
//        }
    	if( satelliteType == SatelliteType.GINI ) {
    		satRsc = new GiniSatResource(this, loadProperties); 
            return satRsc;
    	}
    	else if( satelliteType == SatelliteType.MCIDAS ) {
    		satRsc = new McidasSatResource(this, loadProperties);
    		return satRsc;
    	}
    	else {
    		System.out.println("Unrecognized satellite type: "+satelliteType.toString() );
    		return null;
    	}
    
    }

	public AbstractVizResource<?, ?> getResource() {
		return satRsc;
	}


	public Unit<?> getDisplayUnit() {
		if( displayUnit == null ) {
			setDisplayUnitStr( displayUnitStr );
		}
		return displayUnit;
	}
	
	public String getDisplayUnitStr() {
		return displayUnitStr;
	}

	public void setDisplayUnitStr(String dispUnitStr) {
		this.displayUnitStr = dispUnitStr;

		if( displayUnit == null ) {
			if( displayUnitStr != null ) {
	            try {
	            	displayUnit = UnitFormat.getUCUMInstance().parseSingleUnit(
	            			 displayUnitStr, new ParsePosition(0));
	            } catch (ParseException e) {
	                System.out.println("Unable parse display units : " + displayUnitStr );
	            }
			}
		}
	}
	
	public String getColorMapName() {
		return colorMapName;
	}

	public void setColorMapName(String cmapName) {
		colorMapName = cmapName;
	}
	
    public ColorBarFromColormap getColorBar() {
		return colorBar;
	}

	public void setColorBar(ColorBarFromColormap cBar) {
		this.colorBar = cBar;
	}
	
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


	@Override
    public boolean equals(Object obj) {
        if (!super.equals(obj)) {
            return false;
        }
        if (obj instanceof SatelliteResourceData == false) {
            return false;
        }

        SatelliteResourceData other = (SatelliteResourceData) obj;

        if (this.colorMapName != null && other.colorMapName == null) {
            return false;
        } else if (this.colorMapName == null && other.colorMapName != null) {
            return false;
        } else if (this.colorMapName != null
                && this.colorMapName.equals(other.colorMapName) == false) {
            return false;
        }
        
        if (this.displayUnitStr != null && other.displayUnitStr == null) {
            return false;
        } else if (this.displayUnitStr == null && other.displayUnitStr != null) {
            return false;
        } else if (this.displayUnitStr != null
                && this.displayUnitStr.equals(other.displayUnitStr) == false) {
            return false;
        }
        
        if( this.satelliteType != other.satelliteType ) {
        	return false;
        }
        
        if ( (this.alpha != null && other.alpha == null)
        		|| (this.alpha == null && other.alpha != null)
        		|| (this.alpha != null && this.alpha.equals(other.alpha) == false)){
        	return false;
        	
	    }
        	
        if ( (this.brightness != null && other.brightness == null)
        		|| (this.brightness == null && other.brightness != null)
        		|| (this.brightness != null && this.brightness.equals(other.brightness) == false)){
        	return false;
        	
	    }
        
        if ( (this.contrast != null && other.contrast == null)
        		|| (this.contrast == null && other.contrast != null)
        		|| (this.contrast != null && this.contrast.equals(other.contrast) == false)){
        	return false;
        	
	    }
        
        
        return true;
    }

}
