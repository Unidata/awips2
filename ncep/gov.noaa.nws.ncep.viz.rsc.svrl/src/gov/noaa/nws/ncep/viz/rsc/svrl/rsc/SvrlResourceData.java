package gov.noaa.nws.ncep.viz.rsc.svrl.rsc;

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
import gov.noaa.nws.ncep.viz.resources.misc.IMiscResourceData.MiscResourceAttr;

@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name="NC-SvrlResourceData")

public class SvrlResourceData  extends AbstractNatlCntrsRequestableResourceData
 implements   INatlCntrsResourceData, IMiscResourceData { 

    @XmlElement
    protected String sourceName;

//    @XmlElement
//    protected int timeInterval;
    
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
    protected boolean Watchxxx0Enable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB     Watchxxx0Color;
    @XmlElement
    protected int     Watchxxx0SymbolWidth;
    @XmlElement
    protected int     Watchxxx0SymbolSize;

    @XmlElement    
    protected boolean Watchxxx1Enable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB     Watchxxx1Color;
    @XmlElement
    protected int     Watchxxx1SymbolWidth;
    @XmlElement
    protected int     Watchxxx1SymbolSize;

    
    @XmlElement    
    protected boolean Watchxxx2Enable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB     Watchxxx2Color;
    @XmlElement
    protected int     Watchxxx2SymbolWidth;
    @XmlElement
    protected int     Watchxxx2SymbolSize;

    @XmlElement    
    protected boolean Watchxxx3Enable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB     Watchxxx3Color;
    @XmlElement
    protected int     Watchxxx3SymbolWidth;
    @XmlElement
    protected int     Watchxxx3SymbolSize;

    @XmlElement    
    protected boolean Watchxxx4Enable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB     Watchxxx4Color;
    @XmlElement
    protected int     Watchxxx4SymbolWidth;
    @XmlElement
    protected int     Watchxxx4SymbolSize;

    @XmlElement    
    protected boolean Watchxxx5Enable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB     Watchxxx5Color;
    @XmlElement
    protected int     Watchxxx5SymbolWidth;
    @XmlElement
    protected int     Watchxxx5SymbolSize;

    @XmlElement    
    protected boolean Watchxxx6Enable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB     Watchxxx6Color;
    @XmlElement
    protected int     Watchxxx6SymbolWidth;
    @XmlElement
    protected int     Watchxxx6SymbolSize;

    
    @XmlElement    
    protected boolean Watchxxx7Enable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB     Watchxxx7Color;
    @XmlElement
    protected int     Watchxxx7SymbolWidth;
    @XmlElement
    protected int     Watchxxx7SymbolSize;

    @XmlElement    
    protected boolean Watchxxx8Enable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB     Watchxxx8Color;
    @XmlElement
    protected int     Watchxxx8SymbolWidth;
    @XmlElement
    protected int     Watchxxx8SymbolSize;

    @XmlElement    
    protected boolean Watchxxx9Enable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB     Watchxxx9Color;
    @XmlElement
    protected int     Watchxxx9SymbolWidth;
    @XmlElement
    protected int     Watchxxx9SymbolSize;

        
    @XmlElement
    protected boolean watchBoxTimeEnable;
    @XmlElement
    protected boolean watchBoxLabelEnable;
    @XmlElement
    protected boolean watchBoxNumberEnable;
    @XmlElement
    protected boolean colorCodeEnable;
    @XmlElement
    protected boolean watchBoxOutlineEnable;
   

	/**
     * Create a Convective Watch (WTCH) resource.
     * 
     * @throws VizException
     */
    public SvrlResourceData() throws VizException {
        super();
        this.nameGenerator = new AbstractNameGenerator() {
            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
                if( sourceName != null ) {
                    return sourceName;
                }
                return "SVRL";
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
		attrs.addAttr( new MiscResourceAttr( "thunderstormSymbolWidth", "Line Width", 
				EditElement.SPINNER, 3 ));	
		attrs.addAttr( new MiscResourceAttr( "thunderstormSymbolSize", "Symbol Size", 
				EditElement.SPINNER, 4 ));	
		
		attrs.addAttr( new MiscResourceAttr( "tornadoEnable", "Tornado",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "tornadoColor", "", 
				EditElement.COLOR_SELECTOR, 2 ));		
		attrs.addAttr( new MiscResourceAttr( "tornadoSymbolWidth", "Line Width", 
				EditElement.SPINNER, 3 ));		
		attrs.addAttr( new MiscResourceAttr( "tornadoSymbolSize", "Symbol Size", 
				EditElement.SPINNER, 4 ));
		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx0Enable", "Watchxxx0",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx0Color", "", 
				EditElement.COLOR_SELECTOR, 2 ));		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx0SymbolWidth", "Line Width", 
				EditElement.SPINNER, 3 ));		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx0SymbolSize", "Symbol Size", 
				EditElement.SPINNER, 4 ));
		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx1Enable", "Watchxxx1",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx1Color", "", 
				EditElement.COLOR_SELECTOR, 2 ));		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx1SymbolWidth", "Line Width", 
				EditElement.SPINNER, 3 ));		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx1SymbolSize", "Symbol Size", 
				EditElement.SPINNER, 4 ));	
		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx2Enable", "Watchxxx2",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx2Color", "", 
				EditElement.COLOR_SELECTOR, 2 ));		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx2SymbolWidth", "Line Width", 
				EditElement.SPINNER, 3 ));		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx2SymbolSize", "Symbol Size", 
				EditElement.SPINNER, 4 ));	
				
		attrs.addAttr( new MiscResourceAttr( "Watchxxx3Enable", "Watchxxx3",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx3Color", "", 
				EditElement.COLOR_SELECTOR, 2 ));		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx3SymbolWidth", "Line Width", 
				EditElement.SPINNER, 3 ));		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx3SymbolSize", "Symbol Size", 
				EditElement.SPINNER, 4 ));
		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx4Enable", "Watchxxx4",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx4Color", "", 
				EditElement.COLOR_SELECTOR, 2 ));		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx4SymbolWidth", "Line Width", 
				EditElement.SPINNER, 3 ));		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx4SymbolSize", "Symbol Size", 
				EditElement.SPINNER, 4 ));
		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx5Enable", "Watchxxx5",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx5Color", "", 
				EditElement.COLOR_SELECTOR, 2 ));		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx5SymbolWidth", "Line Width", 
				EditElement.SPINNER, 3 ));		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx5SymbolSize", "Symbol Size", 
				EditElement.SPINNER, 4 ));
		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx6Enable", "Watchxxx6",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx6Color", "", 
				EditElement.COLOR_SELECTOR, 2 ));		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx6SymbolWidth", "Line Width", 
				EditElement.SPINNER, 3 ));		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx6SymbolSize", "Symbol Size", 
				EditElement.SPINNER, 4 ));
		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx7Enable", "Watchxxx7",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx7Color", "", 
				EditElement.COLOR_SELECTOR, 2 ));		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx7SymbolWidth", "Line Width", 
				EditElement.SPINNER, 3 ));		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx7SymbolSize", "Symbol Size", 
				EditElement.SPINNER, 4 ));
		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx8Enable", "Watchxxx8",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx8Color", "", 
				EditElement.COLOR_SELECTOR, 2 ));		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx8SymbolWidth", "Line Width", 
				EditElement.SPINNER, 3 ));		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx8SymbolSize", "Symbol Size", 
				EditElement.SPINNER, 4 ));
		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx9Enable", "Watchxxx9",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx9Color", "", 
				EditElement.COLOR_SELECTOR, 2 ));		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx9SymbolWidth", "Line Width", 
				EditElement.SPINNER, 3 ));		
		attrs.addAttr( new MiscResourceAttr( "Watchxxx9SymbolSize", "Symbol Size", 
				EditElement.SPINNER, 4 ));	
		
		attrs.addAttr( new MiscResourceAttr( null, null, 
			   EditElement.SEPARATOR, 1 ));
		
		//TODO:  Improve appearance -- Make columns have more uniform width?
		attrs.addAttr( new MiscResourceAttr( "watchBoxTimeEnable", "Time",
				EditElement.CHECK_BOX, 1 ));
		attrs.addAttr( new MiscResourceAttr( "watchBoxLabelEnable", "County",
				EditElement.CHECK_BOX, 1 ));
//		attrs.addAttr( new MiscResourceAttr( "watchBoxNumberEnable", "Watch number",
//				EditElement.CHECK_BOX, 1 ));
		attrs.addAttr( new MiscResourceAttr( "colorCodeEnable", "Color Code",
				EditElement.CHECK_BOX, 1 ));
		attrs.addAttr( new MiscResourceAttr( "watchBoxOutlineEnable", "Outline",
				EditElement.CHECK_BOX, 1 ));

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
	
	public boolean getWatchxxx0Enable() {
		return Watchxxx0Enable;
	}

	public void setWatchxxx0Enable(boolean watchxxx0Enable) {
		Watchxxx0Enable = watchxxx0Enable;
	}

	public RGB getWatchxxx0Color() {
		return Watchxxx0Color;
	}

	public void setWatchxxx0Color(RGB watchxxx0Color) {
		Watchxxx0Color = watchxxx0Color;
	}

	public int getWatchxxx0SymbolWidth() {
		return Watchxxx0SymbolWidth;
	}

	public void setWatchxxx0SymbolWidth(int watchxxx0SymbolWidth) {
		Watchxxx0SymbolWidth = watchxxx0SymbolWidth;
	}

	public int getWatchxxx0SymbolSize() {
		return Watchxxx0SymbolSize;
	}

	public void setWatchxxx0SymbolSize(int watchxxx0SymbolSize) {
		Watchxxx0SymbolSize = watchxxx0SymbolSize;
	}

	public boolean getWatchxxx1Enable() {
		return Watchxxx1Enable;
	}

	public void setWatchxxx1Enable(boolean watchxxx1Enable) {
		Watchxxx1Enable = watchxxx1Enable;
	}

	public RGB getWatchxxx1Color() {
		return Watchxxx1Color;
	}

	public void setWatchxxx1Color(RGB watchxxx1Color) {
		Watchxxx1Color = watchxxx1Color;
	}

	public int getWatchxxx1SymbolWidth() {
		return Watchxxx1SymbolWidth;
	}

	public void setWatchxxx1SymbolWidth(int watchxxx1SymbolWidth) {
		Watchxxx1SymbolWidth = watchxxx1SymbolWidth;
	}

	public int getWatchxxx1SymbolSize() {
		return Watchxxx1SymbolSize;
	}

	public void setWatchxxx1SymbolSize(int watchxxx1SymbolSize) {
		Watchxxx1SymbolSize = watchxxx1SymbolSize;
	}

	public boolean getWatchxxx2Enable() {
		return Watchxxx2Enable;
	}

	public void setWatchxxx2Enable(boolean watchxxx2Enable) {
		Watchxxx2Enable = watchxxx2Enable;
	}

	public RGB getWatchxxx2Color() {
		return Watchxxx2Color;
	}

	public void setWatchxxx2Color(RGB watchxxx2Color) {
		Watchxxx2Color = watchxxx2Color;
	}

	public int getWatchxxx2SymbolWidth() {
		return Watchxxx2SymbolWidth;
	}

	public void setWatchxxx2SymbolWidth(int watchxxx2SymbolWidth) {
		Watchxxx2SymbolWidth = watchxxx2SymbolWidth;
	}

	public int getWatchxxx2SymbolSize() {
		return Watchxxx2SymbolSize;
	}

	public void setWatchxxx2SymbolSize(int watchxxx2SymbolSize) {
		Watchxxx2SymbolSize = watchxxx2SymbolSize;
	}


	public boolean getWatchxxx3Enable() {
		return Watchxxx3Enable;
	}

	public void setWatchxxx3Enable(boolean watchxxx3Enable) {
		Watchxxx3Enable = watchxxx3Enable;
	}

	public RGB getWatchxxx3Color() {
		return Watchxxx3Color;
	}

	public void setWatchxxx3Color(RGB watchxxx3Color) {
		Watchxxx3Color = watchxxx3Color;
	}

	public int getWatchxxx3SymbolWidth() {
		return Watchxxx3SymbolWidth;
	}

	public void setWatchxxx3SymbolWidth(int watchxxx3SymbolWidth) {
		Watchxxx3SymbolWidth = watchxxx3SymbolWidth;
	}

	public int getWatchxxx3SymbolSize() {
		return Watchxxx3SymbolSize;
	}

	public void setWatchxxx3SymbolSize(int watchxxx3SymbolSize) {
		Watchxxx3SymbolSize = watchxxx3SymbolSize;
	}

	public boolean getWatchxxx4Enable() {
		return Watchxxx4Enable;
	}

	public void setWatchxxx4Enable(boolean watchxxx4Enable) {
		Watchxxx4Enable = watchxxx4Enable;
	}

	public RGB getWatchxxx4Color() {
		return Watchxxx4Color;
	}

	public void setWatchxxx4Color(RGB watchxxx4Color) {
		Watchxxx4Color = watchxxx4Color;
	}

	public int getWatchxxx4SymbolWidth() {
		return Watchxxx4SymbolWidth;
	}

	public void setWatchxxx4SymbolWidth(int watchxxx4SymbolWidth) {
		Watchxxx4SymbolWidth = watchxxx4SymbolWidth;
	}

	public int getWatchxxx4SymbolSize() {
		return Watchxxx4SymbolSize;
	}

	public void setWatchxxx4SymbolSize(int watchxxx4SymbolSize) {
		Watchxxx4SymbolSize = watchxxx4SymbolSize;
	}

	public boolean getWatchxxx5Enable() {
		return Watchxxx5Enable;
	}

	public void setWatchxxx5Enable(boolean watchxxx5Enable) {
		Watchxxx5Enable = watchxxx5Enable;
	}

	public RGB getWatchxxx5Color() {
		return Watchxxx5Color;
	}

	public void setWatchxxx5Color(RGB watchxxx5Color) {
		Watchxxx5Color = watchxxx5Color;
	}

	public int getWatchxxx5SymbolWidth() {
		return Watchxxx5SymbolWidth;
	}

	public void setWatchxxx5SymbolWidth(int watchxxx5SymbolWidth) {
		Watchxxx5SymbolWidth = watchxxx5SymbolWidth;
	}

	public int getWatchxxx5SymbolSize() {
		return Watchxxx5SymbolSize;
	}

	public void setWatchxxx5SymbolSize(int watchxxx5SymbolSize) {
		Watchxxx5SymbolSize = watchxxx5SymbolSize;
	}

	public boolean getWatchxxx6Enable() {
		return Watchxxx6Enable;
	}

	public void setWatchxxx6Enable(boolean watchxxx6Enable) {
		Watchxxx6Enable = watchxxx6Enable;
	}

	public RGB getWatchxxx6Color() {
		return Watchxxx6Color;
	}

	public void setWatchxxx6Color(RGB watchxxx6Color) {
		Watchxxx6Color = watchxxx6Color;
	}

	public int getWatchxxx6SymbolWidth() {
		return Watchxxx6SymbolWidth;
	}

	public void setWatchxxx6SymbolWidth(int watchxxx6SymbolWidth) {
		Watchxxx6SymbolWidth = watchxxx6SymbolWidth;
	}

	public int getWatchxxx6SymbolSize() {
		return Watchxxx6SymbolSize;
	}

	public void setWatchxxx6SymbolSize(int watchxxx6SymbolSize) {
		Watchxxx6SymbolSize = watchxxx6SymbolSize;
	}

	public boolean getWatchxxx7Enable() {
		return Watchxxx7Enable;
	}

	public void setWatchxxx7Enable(boolean watchxxx7Enable) {
		Watchxxx7Enable = watchxxx7Enable;
	}

	public RGB getWatchxxx7Color() {
		return Watchxxx7Color;
	}

	public void setWatchxxx7Color(RGB watchxxx7Color) {
		Watchxxx7Color = watchxxx7Color;
	}

	public int getWatchxxx7SymbolWidth() {
		return Watchxxx7SymbolWidth;
	}

	public void setWatchxxx7SymbolWidth(int watchxxx7SymbolWidth) {
		Watchxxx7SymbolWidth = watchxxx7SymbolWidth;
	}

	public int getWatchxxx7SymbolSize() {
		return Watchxxx7SymbolSize;
	}

	public void setWatchxxx7SymbolSize(int watchxxx7SymbolSize) {
		Watchxxx7SymbolSize = watchxxx7SymbolSize;
	}

	public boolean getWatchxxx8Enable() {
		return Watchxxx8Enable;
	}

	public void setWatchxxx8Enable(boolean watchxxx8Enable) {
		Watchxxx8Enable = watchxxx8Enable;
	}

	public RGB getWatchxxx8Color() {
		return Watchxxx8Color;
	}

	public void setWatchxxx8Color(RGB watchxxx8Color) {
		Watchxxx8Color = watchxxx8Color;
	}

	public int getWatchxxx8SymbolWidth() {
		return Watchxxx8SymbolWidth;
	}

	public void setWatchxxx8SymbolWidth(int watchxxx8SymbolWidth) {
		Watchxxx8SymbolWidth = watchxxx8SymbolWidth;
	}

	public int getWatchxxx8SymbolSize() {
		return Watchxxx8SymbolSize;
	}

	public void setWatchxxx8SymbolSize(int watchxxx8SymbolSize) {
		Watchxxx8SymbolSize = watchxxx8SymbolSize;
	}
	public boolean getWatchxxx9Enable() {
		return Watchxxx9Enable;
	}

	public void setWatchxxx9Enable(boolean watchxxx9Enable) {
		Watchxxx9Enable = watchxxx9Enable;
	}

	public RGB getWatchxxx9Color() {
		return Watchxxx9Color;
	}

	public void setWatchxxx9Color(RGB watchxxx9Color) {
		Watchxxx9Color = watchxxx9Color;
	}

	public int getWatchxxx9SymbolWidth() {
		return Watchxxx9SymbolWidth;
	}

	public void setWatchxxx9SymbolWidth(int watchxxx9SymbolWidth) {
		Watchxxx9SymbolWidth = watchxxx9SymbolWidth;
	}

	public int getWatchxxx9SymbolSize() {
		return Watchxxx9SymbolSize;
	}

	public void setWatchxxx9SymbolSize(int watchxxx9SymbolSize) {
		Watchxxx9SymbolSize = watchxxx9SymbolSize;
	}

	public boolean getWatchBoxLabelEnable() {
		return watchBoxLabelEnable;
	}

	public void setWatchBoxLabelEnable(boolean watchBoxLabelEnable) {
		this.watchBoxLabelEnable = watchBoxLabelEnable;
	}

	public boolean getWatchBoxOutlineEnable() {
		return watchBoxOutlineEnable;
	}

	public void setWatchBoxOutlineEnable(boolean watchBoxOutlineEnable) {
		this.watchBoxOutlineEnable = watchBoxOutlineEnable;
	}

	public boolean getWatchBoxTimeEnable() {
		return watchBoxTimeEnable;
	}

	public void setWatchBoxTimeEnable(boolean watchBoxTimeEnable) {
		this.watchBoxTimeEnable = watchBoxTimeEnable;
	}
	public boolean getWatchBoxNumberEnable() {
		return watchBoxNumberEnable;
	}

	public void setWatchBoxNumberEnable(boolean watchBoxNumberEnable) {
		this.watchBoxNumberEnable = watchBoxNumberEnable;
	}

	public boolean getColorCodeEnable() {
		return colorCodeEnable;
	}

	public void setColorCodeEnable(boolean colorCodeEnable) {
		this.colorCodeEnable = colorCodeEnable;
	}


	
	
	@Override
	protected AbstractVizResource<?, ?> constructResource(
			LoadProperties loadProperties, PluginDataObject[] objects)
			throws VizException {
		return new SvrlResource( this, loadProperties );
	}

	}