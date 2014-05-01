package gov.noaa.nws.ncep.viz.rsc.nonconvsigmet.rsc;

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
import gov.noaa.nws.ncep.viz.resources.misc.IMiscResourceData;

/**
 * NonConvSigmetResourceData - Resource Data NonConvective SIGMET data.
 * 
 *  This code has been developed by the SIB for use in the AWIPS2 system.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20 Jul 2009  132        B. Hebbard  Initial creation.
 * 08/10/09     migration  G. Hull     Migrate to to11
 * 01 Dec 2009  migration  B. Hebbard  Migrate to11d3->d6
 * 23 Apr 2010  245        Greg Hull   Change SLIDER_TEXTs to SPINNERs
 * 
 * </pre>
 * 
 * @author bhebbard 
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name="NC-NonConvSigmetResourceData")
public class NonConvSigmetResourceData extends AbstractNatlCntrsRequestableResourceData implements
        INatlCntrsResourceData, IMiscResourceData { 

    @XmlElement
    protected String sourceName;

//    @XmlElement
//    protected int timeInterval;
    
    @XmlElement
    protected boolean icingEnable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB     icingColor;
    @XmlElement
    protected int     icingLineWidth;
    @XmlElement
	protected int     icingSymbolWidth;
    @XmlElement
	protected int     icingSymbolSize;

    @XmlElement    
    protected boolean turbulenceEnable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB     turbulenceColor;
    @XmlElement
    protected int     turbulenceLineWidth;
    @XmlElement
    protected int     turbulenceSymbolWidth;
    @XmlElement
    protected int     turbulenceSymbolSize;
    
    @XmlElement
    protected boolean duststormEnable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB     duststormColor;
    @XmlElement
    protected int     duststormLineWidth;
    @XmlElement
    protected int     duststormSymbolWidth;
    @XmlElement
    protected int     duststormSymbolSize;
    
    @XmlElement
    protected boolean volcanicAshEnable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB     volcanicAshColor;
    @XmlElement
    protected int     volcanicAshLineWidth;
    @XmlElement
    protected int     volcanicAshSymbolWidth;
    @XmlElement
    protected int     volcanicAshSymbolSize;
    
    @XmlElement
    protected boolean symbolEnable;
    @XmlElement
    protected boolean timeEnable;
    @XmlElement
    protected boolean nameNumberEnable;
    @XmlElement
    protected boolean flightLevelEnable;
    @XmlElement
    protected boolean novemberEnable;
    @XmlElement
    protected boolean oscarEnable;
    @XmlElement
    protected boolean papaEnable;
    @XmlElement
    protected boolean quebecEnable;
    @XmlElement
    protected boolean romeoEnable;
    @XmlElement
    protected boolean uniformEnable;
    @XmlElement
    protected boolean victorEnable;
    @XmlElement
    protected boolean whiskeyEnable;
    @XmlElement
    protected boolean xrayEnable;
    @XmlElement
    protected boolean yankeeEnable;

    /**
     * Create a NonConvective SIGMET resource.
     * 
     * @throws VizException
     */
    public NonConvSigmetResourceData() throws VizException {
        super();
        this.nameGenerator = new AbstractNameGenerator() {
            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
                if( sourceName != null ) {
                    return sourceName;
                }
                return "Non-Convective Sigmet";
            }

        };
    }
    
    @Override
    public boolean isEventResource() {
    	return true;
    }
    
    public String getSourceName() {
        return sourceName;
    }

	public boolean getIcingEnable() {
		return icingEnable;
	}

	public void setIcingEnable(boolean icingEnable) {
		this.icingEnable = icingEnable;
	}

	public RGB getIcingColor() {
		return icingColor;
	}

	public void setIcingColor(RGB icingColor) {
		this.icingColor = icingColor;
	}

	public int getIcingLineWidth() {
		return icingLineWidth;
	}

	public void setIcingLineWidth(int icingLineWidth) {
		this.icingLineWidth = icingLineWidth;
	}

	public int getIcingSymbolWidth() {
		return icingSymbolWidth;
	}

	public void setIcingSymbolWidth(int icingSymbolWidth) {
		this.icingSymbolWidth = icingSymbolWidth;
	}

	public int getIcingSymbolSize() {
		return icingSymbolSize;
	}

	public void setIcingSymbolSize(int icingSymbolSize) {
		this.icingSymbolSize = icingSymbolSize;
	}

	public boolean getTurbulenceEnable() {
		return turbulenceEnable;
	}

	public void setTurbulenceEnable(boolean turbulenceEnable) {
		this.turbulenceEnable = turbulenceEnable;
	}

	public RGB getTurbulenceColor() {
		return turbulenceColor;
	}

	public void setTurbulenceColor(RGB turbulenceColor) {
		this.turbulenceColor = turbulenceColor;
	}

	public int getTurbulenceLineWidth() {
		return turbulenceLineWidth;
	}

	public void setTurbulenceLineWidth(int turbulenceLineWidth) {
		this.turbulenceLineWidth = turbulenceLineWidth;
	}

	public int getTurbulenceSymbolWidth() {
		return turbulenceSymbolWidth;
	}

	public void setTurbulenceSymbolWidth(int turbulenceSymbolWidth) {
		this.turbulenceSymbolWidth = turbulenceSymbolWidth;
	}

	public int getTurbulenceSymbolSize() {
		return turbulenceSymbolSize;
	}

	public void setTurbulenceSymbolSize(int turbulenceSymbolSize) {
		this.turbulenceSymbolSize = turbulenceSymbolSize;
	}

	public boolean getDuststormEnable() {
		return duststormEnable;
	}

	public void setDuststormEnable(boolean duststormEnable) {
		this.duststormEnable = duststormEnable;
	}

	public RGB getDuststormColor() {
		return duststormColor;
	}

	public void setDuststormColor(RGB duststormColor) {
		this.duststormColor = duststormColor;
	}

	public int getDuststormLineWidth() {
		return duststormLineWidth;
	}

	public void setDuststormLineWidth(int duststormLineWidth) {
		this.duststormLineWidth = duststormLineWidth;
	}

	public int getDuststormSymbolWidth() {
		return duststormSymbolWidth;
	}

	public void setDuststormSymbolWidth(int duststormSymbolWidth) {
		this.duststormSymbolWidth = duststormSymbolWidth;
	}

	public int getDuststormSymbolSize() {
		return duststormSymbolSize;
	}

	public void setDuststormSymbolSize(int duststormSymbolSize) {
		this.duststormSymbolSize = duststormSymbolSize;
	}

	public boolean getVolcanicAshEnable() {
		return volcanicAshEnable;
	}

	public void setVolcanicAshEnable(boolean volcanicAshEnable) {
		this.volcanicAshEnable = volcanicAshEnable;
	}

	public RGB getVolcanicAshColor() {
		return volcanicAshColor;
	}

	public void setVolcanicAshColor(RGB volcanicAshColor) {
		this.volcanicAshColor = volcanicAshColor;
	}

	public int getVolcanicAshLineWidth() {
		return volcanicAshLineWidth;
	}

	public void setVolcanicAshLineWidth(int volcanicAshLineWidth) {
		this.volcanicAshLineWidth = volcanicAshLineWidth;
	}

	public int getVolcanicAshSymbolWidth() {
		return volcanicAshSymbolWidth;
	}

	public void setVolcanicAshSymbolWidth(int volcanicAshSymbolWidth) {
		this.volcanicAshSymbolWidth = volcanicAshSymbolWidth;
	}

	public int getVolcanicAshSymbolSize() {
		return volcanicAshSymbolSize;
	}

	public void setVolcanicAshSymbolSize(int volcanicAshSymbolSize) {
		this.volcanicAshSymbolSize = volcanicAshSymbolSize;
	}

	public boolean getSymbolEnable() {
		return symbolEnable;
	}

	public void setSymbolEnable(boolean symbolEnable) {
		this.symbolEnable = symbolEnable;
	}

	public boolean getNameNumberEnable() {
		return nameNumberEnable;
	}

	public void setNameNumberEnable(boolean nameNumberEnable) {
		this.nameNumberEnable = nameNumberEnable;
	}

	public boolean getFlightLevelEnable() {
		return flightLevelEnable;
	}

	public void setFlightLevelEnable(boolean flightLevelEnable) {
		this.flightLevelEnable = flightLevelEnable;
	}

	public boolean getNovemberEnable() {
		return novemberEnable;
	}

	public void setNovemberEnable(boolean novemberEnable) {
		this.novemberEnable = novemberEnable;
	}

	public boolean getOscarEnable() {
		return oscarEnable;
	}

	public void setOscarEnable(boolean oscarEnable) {
		this.oscarEnable = oscarEnable;
	}

	public boolean getPapaEnable() {
		return papaEnable;
	}

	public void setPapaEnable(boolean papaEnable) {
		this.papaEnable = papaEnable;
	}

	public boolean getQuebecEnable() {
		return quebecEnable;
	}

	public void setQuebecEnable(boolean quebecEnable) {
		this.quebecEnable = quebecEnable;
	}

	public boolean getRomeoEnable() {
		return romeoEnable;
	}

	public void setRomeoEnable(boolean romeoEnable) {
		this.romeoEnable = romeoEnable;
	}

	public boolean getUniformEnable() {
		return uniformEnable;
	}

	public void setUniformEnable(boolean uniformEnable) {
		this.uniformEnable = uniformEnable;
	}

	public boolean getVictorEnable() {
		return victorEnable;
	}

	public void setVictorEnable(boolean victorEnable) {
		this.victorEnable = victorEnable;
	}

	public boolean getWhiskeyEnable() {
		return whiskeyEnable;
	}

	public void setWhiskeyEnable(boolean whiskeyEnable) {
		this.whiskeyEnable = whiskeyEnable;
	}

	public boolean getXrayEnable() {
		return xrayEnable;
	}

	public void setXrayEnable(boolean xrayEnable) {
		this.xrayEnable = xrayEnable;
	}

	public boolean getYankeeEnable() {
		return yankeeEnable;
	}

	public void setYankeeEnable(boolean yankeeEnable) {
		this.yankeeEnable = yankeeEnable;
	}

	public boolean getTimeEnable() {
		return timeEnable;
	}

	public void setTimeEnable(boolean timeEnable) {
		this.timeEnable = timeEnable;
	}

	@Override
	public MiscRscAttrs getMiscResourceAttrs() {
		MiscRscAttrs attrs = new MiscRscAttrs( 5 );
	
		attrs.addAttr( new MiscResourceAttr( "icingEnable", "Icing",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "icingColor", "", 
				EditElement.COLOR_SELECTOR, 2 ));
		attrs.addAttr( new MiscResourceAttr( "icingLineWidth", "Line Width", 
				EditElement.SPINNER, 3 ));
		attrs.addAttr( new MiscResourceAttr( "icingSymbolWidth", "Symbol Width", 
				EditElement.SPINNER, 4 ));
		attrs.addAttr( new MiscResourceAttr( "icingSymbolSize", "Symbol Size", 
				EditElement.SPINNER, 5 ));
		
		attrs.addAttr( new MiscResourceAttr( "turbulenceEnable", "Turbulence",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "turbulenceColor", "", 
				EditElement.COLOR_SELECTOR, 2 ));
		attrs.addAttr( new MiscResourceAttr( "turbulenceLineWidth", "Line Width", 
				EditElement.SPINNER, 3 ));
		attrs.addAttr( new MiscResourceAttr( "turbulenceSymbolWidth", "Symbol Width", 
				EditElement.SPINNER, 4 ));
		attrs.addAttr( new MiscResourceAttr( "turbulenceSymbolSize", "Symbol Size", 
				EditElement.SPINNER, 5 ));
		
		attrs.addAttr( new MiscResourceAttr( "duststormEnable", "Dust/Sandstorm",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "duststormColor", "", 
				EditElement.COLOR_SELECTOR, 2 ));
		attrs.addAttr( new MiscResourceAttr( "duststormLineWidth", "Line Width", 
				EditElement.SPINNER, 3 ));
		attrs.addAttr( new MiscResourceAttr( "duststormSymbolWidth", "Symbol Width", 
				EditElement.SPINNER, 4 ));
		attrs.addAttr( new MiscResourceAttr( "duststormSymbolSize", "Symbol Size", 
				EditElement.SPINNER, 5 ));
		
		attrs.addAttr( new MiscResourceAttr( "volcanicAshEnable", "Volcanic Ash",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "volcanicAshColor", "", 
				EditElement.COLOR_SELECTOR, 2 ));
		attrs.addAttr( new MiscResourceAttr( "volcanicAshLineWidth", "Line Width", 
				EditElement.SPINNER, 3 ));
		attrs.addAttr( new MiscResourceAttr( "volcanicAshSymbolWidth", "Symbol Width", 
				EditElement.SPINNER, 4 ));
		attrs.addAttr( new MiscResourceAttr( "volcanicAshSymbolSize", "Symbol Size", 
				EditElement.SPINNER, 5 ));		
		
		attrs.addAttr( new MiscResourceAttr( null, null, 
			   EditElement.SEPARATOR, 1 ));
		
		//TODO:  Improve appearance -- Make columns have more uniform width?
		attrs.addAttr( new MiscResourceAttr( "symbolEnable", "Symbol",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "timeEnable", "Time         ",
				EditElement.CHECK_BOX, 2 ));		
		attrs.addAttr( new MiscResourceAttr( "nameNumberEnable", "Name/Number",
				EditElement.CHECK_BOX, 3 ));		
		attrs.addAttr( new MiscResourceAttr( "flightLevelEnable", "Flight Levels",
				EditElement.CHECK_BOX, 4 ));		
		
		attrs.addAttr( new MiscResourceAttr( null, null, 
			   EditElement.SEPARATOR, 1 ));

		//TODO:  Improve appearance -- Make columns have more uniform width?
		attrs.addAttr( new MiscResourceAttr( "novemberEnable", "November",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "oscarEnable", "Oscar",
				EditElement.CHECK_BOX, 2 ));		
		attrs.addAttr( new MiscResourceAttr( "papaEnable", "Papa",
				EditElement.CHECK_BOX, 3 ));		
		attrs.addAttr( new MiscResourceAttr( "quebecEnable", "Quebec",
				EditElement.CHECK_BOX, 4 ));		
		attrs.addAttr( new MiscResourceAttr( "romeoEnable", "Romeo",
				EditElement.CHECK_BOX, 5 ));		
		attrs.addAttr( new MiscResourceAttr( "uniformEnable", "Uniform",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "victorEnable", "Victor",
				EditElement.CHECK_BOX, 2 ));		
		attrs.addAttr( new MiscResourceAttr( "whiskeyEnable", "Whiskey",
				EditElement.CHECK_BOX, 3 ));		
		attrs.addAttr( new MiscResourceAttr( "xrayEnable", "Xray",
				EditElement.CHECK_BOX, 4 ));		
		attrs.addAttr( new MiscResourceAttr( "yankeeEnable", "Yankee",
				EditElement.CHECK_BOX, 5 ));		

		return attrs;
	}

	@Override
	protected AbstractVizResource<?, ?> constructResource(
			LoadProperties loadProperties, PluginDataObject[] objects)
			throws VizException {
		return new NonConvSigmetResource( this, loadProperties );
	}

	}