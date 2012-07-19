package gov.noaa.nws.ncep.viz.rsc.warn.rsc;

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

import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.attributes.RGBColorAdapter;
import gov.noaa.nws.ncep.viz.resources.misc.IMiscResourceData;
import gov.noaa.nws.ncep.viz.resources.misc.IMiscResourceData.EditElement;

/**
 * WarnResourceData - Resource Data Convective Watch data.
 * 
 *  This code has been developed by the SIB for use in the AWIPS2 system.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 14 Apr 2010  ###        Uma Josyula  Initial creation.

 * 
 * </pre>
 * 
 * @author ujosyula 
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name="NC-WarnResourceData")
public  class WarnResourceData extends AbstractNatlCntrsRequestableResourceData

 implements   INatlCntrsResourceData, IMiscResourceData { 

    @XmlElement
    protected String sourceName;

    
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB color;  //  resource legend color only
     
    @XmlElement
    protected boolean thunderstormEnable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB     thunderstormColor;
    @XmlElement
    protected int     thunderstormSymbolWidth;
    @XmlElement
    protected int     thunderstormSymbolSize;

    @XmlElement    
    protected boolean tornadoEnable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB     tornadoColor;
    @XmlElement
    protected int     tornadoSymbolWidth;
    @XmlElement
    protected int     tornadoSymbolSize;
    
    @XmlElement
    protected boolean flashFloodEnable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB     flashFloodColor;
    @XmlElement
    protected int     flashFloodSymbolWidth;  
    @XmlElement
    protected int     flashFloodSymbolSize;  
        
    @XmlElement
    protected boolean timeEnable;
    @XmlElement
    protected boolean countyNameEnable;
    @XmlElement
    protected boolean countyOutlineEnable;
    @XmlElement
    protected boolean stormBasedPolygonEnable;   

	/**
     * Create a Warning (WARN) resource.
     * 
     * @throws VizException
     */
    public WarnResourceData() throws VizException {
        super();
        this.nameGenerator = new AbstractNameGenerator() {
            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
                if( sourceName != null ) {
                    return sourceName;
                }
                return "Warning";
            }

        };
        color = new RGB(155, 155, 155);;
    }
    
    @Override
    public boolean isEventResource() {
    	return true;
    }
    
    public String getSourceName() {
        return sourceName;
    }

    public RGB getColor() {
        return color;
    }

    public void setColor(RGB color) {
        this.color = color;
    }

	@Override
	public MiscRscAttrs getMiscResourceAttrs() {
		MiscRscAttrs attrs = new MiscRscAttrs( 5 );
	
		attrs.addAttr( new MiscResourceAttr( "thunderstormEnable", "Thunderstorm",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "thunderstormColor", "", 
				EditElement.COLOR_SELECTOR, 2 ));
		attrs.addAttr( new MiscResourceAttr( "thunderstormSymbolWidth", "Symbol Width", 
				EditElement.SPINNER, 3 ));	
		attrs.addAttr( new MiscResourceAttr( "thunderstormSymbolSize", "Symbol Size", 
				EditElement.SPINNER, 4 ));	
		attrs.addAttr( new MiscResourceAttr( "tornadoEnable", "Tornado",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "tornadoColor", "", 
				EditElement.COLOR_SELECTOR, 2 ));
		attrs.addAttr( new MiscResourceAttr( "tornadoSymbolWidth", "Symbol Width", 
				EditElement.SPINNER, 3 ));		
		attrs.addAttr( new MiscResourceAttr( "tornadoSymbolSize", "Symbol Size", 
				EditElement.SPINNER, 4 ));
		attrs.addAttr( new MiscResourceAttr( "flashFloodEnable", "FlashFlood",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "flashFloodColor", "", 
				EditElement.COLOR_SELECTOR, 2 ));
		attrs.addAttr( new MiscResourceAttr( "flashFloodSymbolWidth", "Symbol Width", 
				EditElement.SPINNER, 3 ));
		attrs.addAttr( new MiscResourceAttr( "flashFloodSymbolSize", "Symbol Size", 
				EditElement.SPINNER, 4 ));
		
		
		
		attrs.addAttr( new MiscResourceAttr( null, null, 
			   EditElement.SEPARATOR, 1 ));
		
		//TODO:  Improve appearance -- Make columns have more uniform width?
		attrs.addAttr( new MiscResourceAttr( "timeEnable", "Time",
				EditElement.CHECK_BOX, 1 ));
		attrs.addAttr( new MiscResourceAttr( "countyNameEnable", "County Name",
				EditElement.CHECK_BOX, 2 ));
		attrs.addAttr( new MiscResourceAttr( "countyOutlineEnable", "County Outline",
				EditElement.CHECK_BOX, 3 ));
		attrs.addAttr( new MiscResourceAttr( "stormBasedPolygonEnable", "Storm-Based Polygon",
				EditElement.CHECK_BOX, 4));				
		

		return attrs;
	}



    public boolean getThunderstormEnable() {
		return thunderstormEnable;
	}

	public void setThunderstormEnable(boolean thunderstormEnable) {
		this.thunderstormEnable = thunderstormEnable;
	}

	public RGB getThunderstormColor() {
		return thunderstormColor;
	}

	public void setThunderstormColor(RGB thunderstormColor) {
		this.thunderstormColor = thunderstormColor;
	}

	public int getThunderstormSymbolWidth() {
		return thunderstormSymbolWidth;
	}

	public void setThunderstormSymbolWidth(int thunderstormSymbolWidth) {
		this.thunderstormSymbolWidth = thunderstormSymbolWidth;
	}


	public int getThunderstormSymbolSize() {
		return thunderstormSymbolSize;
	}

	public void setThunderstormSymbolSize(int thunderstormSymbolSize) {
		this.thunderstormSymbolSize = thunderstormSymbolSize;
	}

	public boolean getTornadoEnable() {
		return tornadoEnable;
	}

	public void setTornadoEnable(boolean tornadoEnable) {
		this.tornadoEnable = tornadoEnable;
	}

	public RGB getTornadoColor() {
		return tornadoColor;
	}

	public void setTornadoColor(RGB tornadoColor) {
		this.tornadoColor = tornadoColor;
	}

	public int getTornadoSymbolWidth() {
		return tornadoSymbolWidth;
	}

	public void setTornadoSymbolWidth(int tornadoSymbolWidth) {
		this.tornadoSymbolWidth = tornadoSymbolWidth;
	}

	public int getTornadoSymbolSize() {
		return tornadoSymbolSize;
	}

	public void setTornadoSymbolSize(int tornadoSymbolSize) {
		this.tornadoSymbolSize = tornadoSymbolSize;
	}

	public boolean getFlashFloodEnable() {
		return flashFloodEnable;
	}

	public void setFlashFloodEnable(boolean flashFloodEnable) {
		this.flashFloodEnable = flashFloodEnable;
	}

	public RGB getFlashFloodColor() {
		return flashFloodColor;
	}

	public void setFlashFloodColor(RGB flashFloodColor) {
		this.flashFloodColor = flashFloodColor;
	}

	public int getFlashFloodSymbolWidth() {
		return flashFloodSymbolWidth;
	}

	public void setFlashFloodSymbolWidth(int flashFloodSymbolWidth) {
		this.flashFloodSymbolWidth = flashFloodSymbolWidth;
	}

	public int getFlashFloodSymbolSize() {
		return flashFloodSymbolSize;
	}

	public void setFlashFloodSymbolSize(int flashFloodSymbolSize) {
		this.flashFloodSymbolSize = flashFloodSymbolSize;
	}

	public boolean getTimeEnable() {
		return timeEnable;
	}

	public void setTimeEnable(boolean timeEnable) {
		this.timeEnable = timeEnable;
	}

	public boolean getCountyNameEnable() {
		return countyNameEnable;
	}

	public void setCountyNameEnable(boolean countyNameEnable) {
		this.countyNameEnable = countyNameEnable;
	}

	public boolean getCountyOutlineEnable() {
		return countyOutlineEnable;
	}

	public void setCountyOutlineEnable(boolean countyOutlineEnable) {
		this.countyOutlineEnable = countyOutlineEnable;
	}

	public boolean getStormBasedPolygonEnable() {
		return stormBasedPolygonEnable;
	}

	public void setStormBasedPolygonEnable(boolean stormBasedPolygonEnable) {
		this.stormBasedPolygonEnable = stormBasedPolygonEnable;
	}


	
	@Override
	protected AbstractVizResource<?, ?> constructResource(
			LoadProperties loadProperties, PluginDataObject[] objects)
			throws VizException {
		return new WarnResource( this, loadProperties );
	}

	}