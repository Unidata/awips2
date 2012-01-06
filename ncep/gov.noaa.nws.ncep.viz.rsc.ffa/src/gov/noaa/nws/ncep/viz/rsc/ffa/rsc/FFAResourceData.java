package gov.noaa.nws.ncep.viz.rsc.ffa.rsc;

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
//import gov.noaa.nws.ncep.viz.resources.misc.IMiscResourceData.EditElement;

/**
 * FFAResourceData - Resource Data.
 * 
 *  This code has been developed by the SIB for use in the AWIPS2 system.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 21 June 2010  254        M. Gao		Initial creation.

 * 
 * </pre>
 * 
 * @author mgao 
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name="NC-FFAResourceData")
public  class FFAResourceData extends AbstractNatlCntrsRequestableResourceData

 implements   INatlCntrsResourceData, IMiscResourceData { 

    @XmlElement
    protected String sourceName;

    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB color;  //  resource legend color only
     
    @XmlElement
    protected boolean flashFloodAdvisoryEnable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB flashFloodAdvisoryColor;  //  color for Flash Flood Advisory
    @XmlElement
    protected int     flashFloodAdvisorySymbolWidth;
    @XmlElement
    protected int     flashFloodAdvisorySymbolSize;

    @XmlElement    
    protected boolean flashFloodWarningEnable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB flashFloodWarningColor;  //  color for Flash Flood Warning 
    @XmlElement
    protected int     flashFloodWarningSymbolWidth;
    @XmlElement
    protected int     flashFloodWarningSymbolSize;
    
    @XmlElement
    protected boolean flashFloodWatchEnable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB flashFloodWatchColor;  //  color for Flash Flood Watch
    @XmlElement
    protected int     flashFloodWatchSymbolWidth;  
    @XmlElement
    protected int     flashFloodWatchSymbolSize;  
        
    @XmlElement
    protected boolean flashFloodStatementEnable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB flashFloodStatementColor;  //  color for Flash Flood Statement
    @XmlElement
    protected int     flashFloodStatementSymbolWidth;  
    @XmlElement
    protected int     flashFloodStatementSymbolSize;  
        
    @XmlElement
    protected boolean timeEnable;
    @XmlElement
    protected boolean countyOrZoneNameEnable;
    @XmlElement
    protected boolean immediateCauseEnable;
    @XmlElement
    protected boolean outlineEnable;   

    @XmlElement
    protected float		outlineLineWidth;  
	@XmlElement
    protected double	symbolSizeScale;  
    
	/**
     * Create a Warning (WARN) resource.
     * 
     * @throws VizException
     */
    public FFAResourceData() throws VizException {
        super();
        this.nameGenerator = new AbstractNameGenerator() {
            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
                if( sourceName != null ) {
                    return sourceName;
                }
                return "Flash Flood";
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
		MiscRscAttrs attrs = new MiscRscAttrs( 4 );
	
		attrs.addAttr( new MiscResourceAttr( "flashFloodAdvisoryEnable", "Flash Flood Advisory",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "flashFloodAdvisoryColor", "", 
				EditElement.COLOR_SELECTOR, 2 ));
		attrs.addAttr( new MiscResourceAttr( "flashFloodAdvisorySymbolWidth", "Outline Width", 
				EditElement.SPINNER, 3 ));	
		attrs.addAttr( new MiscResourceAttr( "flashFloodAdvisorySymbolSize", "Symbol Size", 
				EditElement.SPINNER, 4 ));	
		
		attrs.addAttr( new MiscResourceAttr( "flashFloodWarningEnable", "Flash Flood Warning",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "flashFloodWarningColor", "", 
				EditElement.COLOR_SELECTOR, 2 ));
		attrs.addAttr( new MiscResourceAttr( "flashFloodWarningSymbolWidth", "Outline Width", 
				EditElement.SPINNER, 3 ));		
		attrs.addAttr( new MiscResourceAttr( "flashFloodWarningSymbolSize", "Symbol Size", 
				EditElement.SPINNER, 4 ));
		
		attrs.addAttr( new MiscResourceAttr( "flashFloodWatchEnable", "Flash Flood Watch",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "flashFloodWatchColor", "", 
				EditElement.COLOR_SELECTOR, 2 ));
		attrs.addAttr( new MiscResourceAttr( "flashFloodWatchSymbolWidth", "Outline Width", 
				EditElement.SPINNER, 3 ));
		attrs.addAttr( new MiscResourceAttr( "flashFloodWatchSymbolSize", "Symbol Size", 
				EditElement.SPINNER, 4 ));
		
		attrs.addAttr( new MiscResourceAttr( "flashFloodStatementEnable", "Flash Flood Statement",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "flashFloodStatementColor", "", 
				EditElement.COLOR_SELECTOR, 2 ));
		attrs.addAttr( new MiscResourceAttr( "flashFloodStatementSymbolWidth", "Outline Width", 
				EditElement.SPINNER, 3 ));
		attrs.addAttr( new MiscResourceAttr( "flashFloodStatementSymbolSize", "Symbol Size", 
				EditElement.SPINNER, 4 ));
		
		attrs.addAttr( new MiscResourceAttr( null, null, EditElement.SEPARATOR, 1 ));
		
		//TODO:  Improve appearance -- Make columns have more uniform width?
		attrs.addAttr( new MiscResourceAttr( "timeEnable", "Time",
				EditElement.CHECK_BOX, 1 ));
		attrs.addAttr( new MiscResourceAttr( "countyOrZoneNameEnable", "County/Zone Name",
				EditElement.CHECK_BOX, 1));
		attrs.addAttr( new MiscResourceAttr( "immediateCauseEnable", "Immediate Cause",
				EditElement.CHECK_BOX, 1));
		attrs.addAttr( new MiscResourceAttr( "outlineEnable", "Outline",
				EditElement.CHECK_BOX, 1));				
		

		return attrs;
	}

	@Override
	protected AbstractVizResource<?, ?> constructResource(
			LoadProperties loadProperties, PluginDataObject[] objects)
			throws VizException {
		return new FFAResource( this, loadProperties );
	}

	/*
	 * Flash Flood Advisory
	 */
	public boolean getFlashFloodAdvisoryEnable() {
		return flashFloodAdvisoryEnable;
	}
	public void setFlashFloodAdvisoryEnable(boolean flashFloodAdvisoryEnable) {
		this.flashFloodAdvisoryEnable = flashFloodAdvisoryEnable;
	}

	public RGB getFlashFloodAdvisoryColor() {
		return flashFloodAdvisoryColor;
	}
	public void setFlashFloodAdvisoryColor(RGB flashFloodAdvisoryColor) {
		this.flashFloodAdvisoryColor = flashFloodAdvisoryColor;
	}

	public int getFlashFloodAdvisorySymbolWidth() {
		return flashFloodAdvisorySymbolWidth;
	}
	public void setFlashFloodAdvisorySymbolWidth(int flashFloodAdvisorySymbolWidth) {
		this.flashFloodAdvisorySymbolWidth = flashFloodAdvisorySymbolWidth;
	}

	public int getFlashFloodAdvisorySymbolSize() {
		return flashFloodAdvisorySymbolSize;
	}
	public void setFlashFloodAdvisorySymbolSize(int flashFloodAdvisorySymbolSize) {
		this.flashFloodAdvisorySymbolSize = flashFloodAdvisorySymbolSize;
	}

	/*
	 * Flash Flood Warning
	 */
	public boolean getFlashFloodWarningEnable() {
		return flashFloodWarningEnable;
	}
	public void setFlashFloodWarningEnable(boolean flashFloodWarningEnable) {
		this.flashFloodWarningEnable = flashFloodWarningEnable;
	}

	public RGB getFlashFloodWarningColor() {
		return flashFloodWarningColor;
	}
	public void setFlashFloodWarningColor(RGB flashFloodWarningColor) {
		this.flashFloodWarningColor = flashFloodWarningColor;
	}

	public int getFlashFloodWarningSymbolWidth() {
		return flashFloodWarningSymbolWidth;
	}
	public void setFlashFloodWarningSymbolWidth(int flashFloodWarningSymbolWidth) {
		this.flashFloodWarningSymbolWidth = flashFloodWarningSymbolWidth;
	}

	public int getFlashFloodWarningSymbolSize() {
		return flashFloodWarningSymbolSize;
	}
	public void setFlashFloodWarningSymbolSize(int flashFloodWarningSymbolSize) {
		this.flashFloodWarningSymbolSize = flashFloodWarningSymbolSize;
	}

	/*
	 * Flash Flood Watch
	 */
	public boolean getFlashFloodWatchEnable() {
		return flashFloodWatchEnable;
	}
	public void setFlashFloodWatchEnable(boolean flashFloodWatchEnable) {
		this.flashFloodWatchEnable = flashFloodWatchEnable;
	}

	public RGB getFlashFloodWatchColor() {
		return flashFloodWatchColor;
	}
	public void setFlashFloodWatchColor(RGB flashFloodWatchColor) {
		this.flashFloodWatchColor = flashFloodWatchColor;
	}

	public int getFlashFloodWatchSymbolWidth() {
		return flashFloodWatchSymbolWidth;
	}
	public void setFlashFloodWatchSymbolWidth(int flashFloodWatchSymbolWidth) {
		this.flashFloodWatchSymbolWidth = flashFloodWatchSymbolWidth;
	}

	public int getFlashFloodWatchSymbolSize() {
		return flashFloodWatchSymbolSize;
	}
	public void setFlashFloodWatchSymbolSize(int flashFloodWatchSymbolSize) {
		this.flashFloodWatchSymbolSize = flashFloodWatchSymbolSize;
	}

	/*
	 * Flash Flood Statement
	 */
	public boolean getFlashFloodStatementEnable() {
		return flashFloodStatementEnable;
	}
	public void setFlashFloodStatementEnable(boolean flashFloodStatementEnable) {
		this.flashFloodStatementEnable = flashFloodStatementEnable;
	}

	public RGB getFlashFloodStatementColor() {
		return flashFloodStatementColor;
	}
	public void setFlashFloodStatementColor(RGB flashFloodStatementColor) {
		this.flashFloodStatementColor = flashFloodStatementColor;
	}

	public int getFlashFloodStatementSymbolWidth() {
		return flashFloodStatementSymbolWidth;
	}
	public void setFlashFloodStatementSymbolWidth(int flashFloodStatementSymbolWidth) {
		this.flashFloodStatementSymbolWidth = flashFloodStatementSymbolWidth;
	}

	public int getFlashFloodStatementSymbolSize() {
		return flashFloodStatementSymbolSize;
	}
	public void setFlashFloodStatementSymbolSize(int flashFloodStatementSymbolSize) {
		this.flashFloodStatementSymbolSize = flashFloodStatementSymbolSize;
	}

	/*
	 * Miscellaneous attributes
	 */
	public boolean getTimeEnable() {
		return timeEnable;
	}
	public void setTimeEnable(boolean timeEnable) {
		this.timeEnable = timeEnable;
	}

	public boolean getCountyOrZoneNameEnable() {
		return countyOrZoneNameEnable;
	}
	public void setCountyOrZoneNameEnable(boolean countyOrZoneNameEnable) {
		this.countyOrZoneNameEnable = countyOrZoneNameEnable;
	}

	public boolean getImmediateCauseEnable() {
		return immediateCauseEnable;
	}
	public void setImmediateCauseEnable(boolean immediateCauseEnable) {
		this.immediateCauseEnable = immediateCauseEnable;
	}

	public boolean getOutlineEnable() {
		return outlineEnable;
	}
	public void setOutlineEnable(boolean outlineEnable) {
		this.outlineEnable = outlineEnable;
	}

	/*
	 * the symbol of the label's attributes
	 */
    public float getOutlineLineWidth() {
		return outlineLineWidth;
	}
	public void setOutlineLineWidth(float outlineLineWidth) {
		this.outlineLineWidth = outlineLineWidth;
	}

	public double getSymbolSizeScale() {
		return symbolSizeScale;
	}
	public void setSymbolSizeScale(double symbolSizeScale) {
		this.symbolSizeScale = symbolSizeScale;
	}
}