package gov.noaa.nws.ncep.viz.rsc.wtch.rsc;
        
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


@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name="NC-WtchResourceData")
public class WtchResourceData  extends AbstractNatlCntrsRequestableResourceData
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
    protected int     thunderstormLineWidth;

    @XmlElement    
    protected boolean tornadoEnable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB     tornadoColor;
    @XmlElement
    protected int     tornadoLineWidth;
    
    @XmlElement    
    protected boolean statusEnable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB     statusColor;
    @XmlElement
    protected int     statusLineWidth;
    
    @XmlElement    
    protected boolean watchxxx0Enable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB     watchxxx0Color;
    @XmlElement
    protected int     watchxxx0LineWidth;
	
    @XmlElement    
    protected boolean watchxxx1Enable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB     watchxxx1Color;
    @XmlElement
    protected int     watchxxx1LineWidth;
	
    @XmlElement    
    protected boolean watchxxx2Enable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB     watchxxx2Color;
    @XmlElement
    protected int     watchxxx2LineWidth;
	
    @XmlElement    
    protected boolean watchxxx3Enable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB     watchxxx3Color;
    @XmlElement
    protected int     watchxxx3LineWidth;
	
    @XmlElement    
    protected boolean watchxxx4Enable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB     watchxxx4Color;
    @XmlElement
    protected int     watchxxx4LineWidth;
	
    @XmlElement    
    protected boolean watchxxx5Enable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB     watchxxx5Color;
    @XmlElement
    protected int     watchxxx5LineWidth;
	
    @XmlElement    
    protected boolean watchxxx6Enable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB     watchxxx6Color;
    @XmlElement
    protected int     watchxxx6LineWidth;
	
    @XmlElement    
    protected boolean watchxxx7Enable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB     watchxxx7Color;
    @XmlElement
    protected int     watchxxx7LineWidth;
	
    @XmlElement    
    protected boolean watchxxx8Enable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB     watchxxx8Color;
    @XmlElement
    protected int     watchxxx8LineWidth;
	
    @XmlElement    
    protected boolean watchxxx9Enable;
    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB     watchxxx9Color;
    @XmlElement
    protected int     watchxxx9LineWidth;
	
    @XmlElement
    protected boolean watchBoxTimeEnable;
    @XmlElement
    protected boolean statusLinesTimeEnable;
    @XmlElement
    protected boolean watchBoxNumberEnable;
    @XmlElement
    protected boolean statusLinesNumberEnable;

	@XmlElement
    protected boolean colorCodeEnable;
    @XmlElement
    protected boolean mostRecentStatusEnable;

	/**
     * Create a Watch (WTCH) resource.
     * 
     * @throws VizException
     */
    public WtchResourceData() throws VizException {
        super();
        this.nameGenerator = new AbstractNameGenerator() {
            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
                if( sourceName != null ) {
                    return sourceName;
                }
                return "Watch";
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
		MiscRscAttrs attrs = new MiscRscAttrs( 3 );

		attrs.addAttr( new MiscResourceAttr( "thunderstormEnable", "Thunderstorm",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "thunderstormColor", "", 
				EditElement.COLOR_SELECTOR, 2 ));
		attrs.addAttr( new MiscResourceAttr( "thunderstormLineWidth", "Line Width", 
				EditElement.SPINNER, 3 ));	
		
		attrs.addAttr( new MiscResourceAttr( "tornadoEnable", "Tornado",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "tornadoColor", "", 
				EditElement.COLOR_SELECTOR, 2 ));
		attrs.addAttr( new MiscResourceAttr( "tornadoLineWidth", "Line Width", 
				EditElement.SPINNER, 3 ));	
		
		attrs.addAttr( new MiscResourceAttr( "statusEnable", "Status",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "statusColor", "", 
				EditElement.COLOR_SELECTOR, 2 ));
		attrs.addAttr( new MiscResourceAttr( "statusLineWidth", "Line Width", 
				EditElement.SPINNER, 3 ));	
		
		attrs.addAttr( new MiscResourceAttr( "watchxxx0Enable", "Watch xxx0",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "watchxxx0Color", "", 
				EditElement.COLOR_SELECTOR, 2 ));
		attrs.addAttr( new MiscResourceAttr( "watchxxx0LineWidth", "Line Width", 
				EditElement.SPINNER, 3 ));	
		
		attrs.addAttr( new MiscResourceAttr( "watchxxx1Enable", "Watch xxx1",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "watchxxx1Color", "", 
				EditElement.COLOR_SELECTOR, 2 ));
		attrs.addAttr( new MiscResourceAttr( "watchxxx1LineWidth", "Line Width", 
				EditElement.SPINNER, 3 ));	
		
		attrs.addAttr( new MiscResourceAttr( "watchxxx2Enable", "Watch xxx2",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "watchxxx2Color", "", 
				EditElement.COLOR_SELECTOR, 2 ));
		attrs.addAttr( new MiscResourceAttr( "watchxxx2LineWidth", "Line Width", 
				EditElement.SPINNER, 3 ));	
		
		attrs.addAttr( new MiscResourceAttr( "watchxxx3Enable", "Watch xxx3",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "watchxxx3Color", "", 
				EditElement.COLOR_SELECTOR, 2 ));
		attrs.addAttr( new MiscResourceAttr( "watchxxx3LineWidth", "Line Width", 
				EditElement.SPINNER, 3 ));	
		
		attrs.addAttr( new MiscResourceAttr( "watchxxx4Enable", "Watch xxx4",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "watchxxx4Color", "", 
				EditElement.COLOR_SELECTOR, 2 ));
		attrs.addAttr( new MiscResourceAttr( "watchxxx4LineWidth", "Line Width", 
				EditElement.SPINNER, 3 ));	
		
		attrs.addAttr( new MiscResourceAttr( "watchxxx5Enable", "Watch xxx5",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "watchxxx5Color", "", 
				EditElement.COLOR_SELECTOR, 2 ));
		attrs.addAttr( new MiscResourceAttr( "watchxxx5LineWidth", "Line Width", 
				EditElement.SPINNER, 3 ));	
		
		attrs.addAttr( new MiscResourceAttr( "watchxxx6Enable", "Watch xxx6",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "watchxxx6Color", "", 
				EditElement.COLOR_SELECTOR, 2 ));
		attrs.addAttr( new MiscResourceAttr( "watchxxx6LineWidth", "Line Width", 
				EditElement.SPINNER, 3 ));	
		
		attrs.addAttr( new MiscResourceAttr( "watchxxx7Enable", "Watch xxx7",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "watchxxx7Color", "", 
				EditElement.COLOR_SELECTOR, 2 ));
		attrs.addAttr( new MiscResourceAttr( "watchxxx7LineWidth", "Line Width", 
				EditElement.SPINNER, 3 ));	
		
		attrs.addAttr( new MiscResourceAttr( "watchxxx8Enable", "Watch xxx8",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "watchxxx8Color", "", 
				EditElement.COLOR_SELECTOR, 2 ));
		attrs.addAttr( new MiscResourceAttr( "watchxxx8LineWidth", "Line Width", 
				EditElement.SPINNER, 3 ));	
		
		attrs.addAttr( new MiscResourceAttr( "watchxxx9Enable", "Watch xxx9",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "watchxxx9Color", "", 
				EditElement.COLOR_SELECTOR, 2 ));
		attrs.addAttr( new MiscResourceAttr( "watchxxx9LineWidth", "Line Width", 
				EditElement.SPINNER, 3 ));	
		
		//TODO:  Improve appearance -- Make columns have more uniform width?
		attrs.addAttr( new MiscResourceAttr( "watchBoxTimeEnable", "Watch Box Time",
				EditElement.CHECK_BOX, 1 ));
		attrs.addAttr( new MiscResourceAttr( "statusLinesTimeEnable", "Status Lines Time",
				EditElement.CHECK_BOX, 1 ));
		attrs.addAttr( new MiscResourceAttr( "watchBoxNumberEnable", "Watch Box Number",
				EditElement.CHECK_BOX, 1 ));	
		attrs.addAttr( new MiscResourceAttr( "statusLinesNumberEnable", "Status Lines Number",
				EditElement.CHECK_BOX, 1 ));	
		attrs.addAttr( new MiscResourceAttr( "colorCodeEnable", "Color Code",
				EditElement.CHECK_BOX, 1 ));
		attrs.addAttr( new MiscResourceAttr( "mostRecentStatusEnable", "Most Recent Status Line",
				EditElement.CHECK_BOX, 1 ));
		
		return attrs;
	}

	@Override
	protected AbstractVizResource<?, ?> constructResource(
			LoadProperties loadProperties, PluginDataObject[] objects)
			throws VizException {
		return new WtchResource(this, loadProperties);
	}

	/**
	 * all setters and getters go here
	 */
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

	public int getThunderstormLineWidth() {
		return thunderstormLineWidth;
	}

	public void setThunderstormLineWidth(int thunderstormLineWidth) {
		this.thunderstormLineWidth = thunderstormLineWidth;
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

	public int getTornadoLineWidth() {
		return tornadoLineWidth;
	}

	public void setTornadoLineWidth(int tornadoLineWidth) {
		this.tornadoLineWidth = tornadoLineWidth;
	}

	public boolean getStatusEnable() {
		return statusEnable;
	}

	public void setStatusEnable(boolean statusEnable) {
		this.statusEnable = statusEnable;
	}

	public RGB getStatusColor() {
		return statusColor;
	}

	public void setStatusColor(RGB statusColor) {
		this.statusColor = statusColor;
	}

	public int getStatusLineWidth() {
		return statusLineWidth;
	}

	public void setStatusLineWidth(int statusLineWidth) {
		this.statusLineWidth = statusLineWidth;
	}

	public boolean getWatchxxx0Enable() {
		return watchxxx0Enable;
	}

	public void setWatchxxx0Enable(boolean watchxxx0Enable) {
		this.watchxxx0Enable = watchxxx0Enable;
	}

	public RGB getWatchxxx0Color() {
		return watchxxx0Color;
	}

	public void setWatchxxx0Color(RGB watchxxx0Color) {
		this.watchxxx0Color = watchxxx0Color;
	}

	public int getWatchxxx0LineWidth() {
		return watchxxx0LineWidth;
	}

	public void setWatchxxx0LineWidth(int watchxxx0LineWidth) {
		this.watchxxx0LineWidth = watchxxx0LineWidth;
	}

	public boolean getWatchxxx1Enable() {
		return watchxxx1Enable;
	}

	public void setWatchxxx1Enable(boolean watchxxx1Enable) {
		this.watchxxx1Enable = watchxxx1Enable;
	}

	public RGB getWatchxxx1Color() {
		return watchxxx1Color;
	}

	public void setWatchxxx1Color(RGB watchxxx1Color) {
		this.watchxxx1Color = watchxxx1Color;
	}

	public int getWatchxxx1LineWidth() {
		return watchxxx1LineWidth;
	}

	public void setWatchxxx1LineWidth(int watchxxx1LineWidth) {
		this.watchxxx1LineWidth = watchxxx1LineWidth;
	}

	public boolean getWatchxxx2Enable() {
		return watchxxx2Enable;
	}

	public void setWatchxxx2Enable(boolean watchxxx2Enable) {
		this.watchxxx2Enable = watchxxx2Enable;
	}

	public RGB getWatchxxx2Color() {
		return watchxxx2Color;
	}

	public void setWatchxxx2Color(RGB watchxxx2Color) {
		this.watchxxx2Color = watchxxx2Color;
	}

	public int getWatchxxx2LineWidth() {
		return watchxxx2LineWidth;
	}

	public void setWatchxxx2LineWidth(int watchxxx2LineWidth) {
		this.watchxxx2LineWidth = watchxxx2LineWidth;
	}

	public boolean getWatchxxx3Enable() {
		return watchxxx3Enable;
	}

	public void setWatchxxx3Enable(boolean watchxxx3Enable) {
		this.watchxxx3Enable = watchxxx3Enable;
	}

	public RGB getWatchxxx3Color() {
		return watchxxx3Color;
	}

	public void setWatchxxx3Color(RGB watchxxx3Color) {
		this.watchxxx3Color = watchxxx3Color;
	}

	public int getWatchxxx3LineWidth() {
		return watchxxx3LineWidth;
	}

	public void setWatchxxx3LineWidth(int watchxxx3LineWidth) {
		this.watchxxx3LineWidth = watchxxx3LineWidth;
	}

	public boolean getWatchxxx4Enable() {
		return watchxxx4Enable;
	}

	public void setWatchxxx4Enable(boolean watchxxx4Enable) {
		this.watchxxx4Enable = watchxxx4Enable;
	}

	public RGB getWatchxxx4Color() {
		return watchxxx4Color;
	}

	public void setWatchxxx4Color(RGB watchxxx4Color) {
		this.watchxxx4Color = watchxxx4Color;
	}

	public int getWatchxxx4LineWidth() {
		return watchxxx4LineWidth;
	}

	public void setWatchxxx4LineWidth(int watchxxx4LineWidth) {
		this.watchxxx4LineWidth = watchxxx4LineWidth;
	}

	public boolean getWatchxxx5Enable() {
		return watchxxx5Enable;
	}

	public void setWatchxxx5Enable(boolean watchxxx5Enable) {
		this.watchxxx5Enable = watchxxx5Enable;
	}

	public RGB getWatchxxx5Color() {
		return watchxxx5Color;
	}

	public void setWatchxxx5Color(RGB watchxxx5Color) {
		this.watchxxx5Color = watchxxx5Color;
	}

	public int getWatchxxx5LineWidth() {
		return watchxxx5LineWidth;
	}

	public void setWatchxxx5LineWidth(int watchxxx5LineWidth) {
		this.watchxxx5LineWidth = watchxxx5LineWidth;
	}

	public boolean getWatchxxx6Enable() {
		return watchxxx6Enable;
	}

	public void setWatchxxx6Enable(boolean watchxxx6Enable) {
		this.watchxxx6Enable = watchxxx6Enable;
	}

	public RGB getWatchxxx6Color() {
		return watchxxx6Color;
	}

	public void setWatchxxx6Color(RGB watchxxx6Color) {
		this.watchxxx6Color = watchxxx6Color;
	}

	public int getWatchxxx6LineWidth() {
		return watchxxx6LineWidth;
	}

	public void setWatchxxx6LineWidth(int watchxxx6LineWidth) {
		this.watchxxx6LineWidth = watchxxx6LineWidth;
	}

	public boolean getWatchxxx7Enable() {
		return watchxxx7Enable;
	}

	public void setWatchxxx7Enable(boolean watchxxx7Enable) {
		this.watchxxx7Enable = watchxxx7Enable;
	}

	public RGB getWatchxxx7Color() {
		return watchxxx7Color;
	}

	public void setWatchxxx7Color(RGB watchxxx7Color) {
		this.watchxxx7Color = watchxxx7Color;
	}

	public int getWatchxxx7LineWidth() {
		return watchxxx7LineWidth;
	}

	public void setWatchxxx7LineWidth(int watchxxx7LineWidth) {
		this.watchxxx7LineWidth = watchxxx7LineWidth;
	}

	public boolean getWatchxxx8Enable() {
		return watchxxx8Enable;
	}

	public void setWatchxxx8Enable(boolean watchxxx8Enable) {
		this.watchxxx8Enable = watchxxx8Enable;
	}

	public RGB getWatchxxx8Color() {
		return watchxxx8Color;
	}

	public void setWatchxxx8Color(RGB watchxxx8Color) {
		this.watchxxx8Color = watchxxx8Color;
	}

	public int getWatchxxx8LineWidth() {
		return watchxxx8LineWidth;
	}

	public void setWatchxxx8LineWidth(int watchxxx8LineWidth) {
		this.watchxxx8LineWidth = watchxxx8LineWidth;
	}

	public boolean getWatchxxx9Enable() {
		return watchxxx9Enable;
	}

	public void setWatchxxx9Enable(boolean watchxxx9Enable) {
		this.watchxxx9Enable = watchxxx9Enable;
	}

	public RGB getWatchxxx9Color() {
		return watchxxx9Color;
	}

	public void setWatchxxx9Color(RGB watchxxx9Color) {
		this.watchxxx9Color = watchxxx9Color;
	}

	public int getWatchxxx9LineWidth() {
		return watchxxx9LineWidth;
	}

	public void setWatchxxx9LineWidth(int watchxxx9LineWidth) {
		this.watchxxx9LineWidth = watchxxx9LineWidth;
	}

	public boolean getWatchBoxTimeEnable() {
		return watchBoxTimeEnable;
	}

	public void setWatchBoxTimeEnable(boolean watchBoxTimeEnable) {
		this.watchBoxTimeEnable = watchBoxTimeEnable;
	}

	public boolean getStatusLinesTimeEnable() {
		return statusLinesTimeEnable;
	}

	public void setStatusLinesTimeEnable(boolean statusLinesTimeEnable) {
		this.statusLinesTimeEnable = statusLinesTimeEnable;
	}

	public boolean getWatchBoxNumberEnable() {
		return watchBoxNumberEnable;
	}

	public void setWatchBoxNumberEnable(boolean watchBoxNumberEnable) {
		this.watchBoxNumberEnable = watchBoxNumberEnable;
	}

	public boolean getStatusLinesNumberEnable() {
		return statusLinesNumberEnable;
	}

	public void setStatusLinesNumberEnable(boolean statusLinesNumberEnable) {
		this.statusLinesNumberEnable = statusLinesNumberEnable;
	}

	public boolean getColorCodeEnable() {
		return colorCodeEnable;
	}

	public void setColorCodeEnable(boolean colorCodeEnable) {
		this.colorCodeEnable = colorCodeEnable;
	}

	public boolean getMostRecentStatusEnable() {
		return mostRecentStatusEnable;
	}

	public void setMostRecentStatusEnable(boolean mostRecentStatusEnable) {
		this.mostRecentStatusEnable = mostRecentStatusEnable;
	}

	
}
