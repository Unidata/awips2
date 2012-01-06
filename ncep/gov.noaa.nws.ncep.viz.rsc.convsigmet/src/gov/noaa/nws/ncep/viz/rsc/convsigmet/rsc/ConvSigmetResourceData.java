package gov.noaa.nws.ncep.viz.rsc.convsigmet.rsc;

import gov.noaa.nws.ncep.viz.common.RGBColorAdapter;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.misc.IMiscResourceData;

import java.util.Date;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;


/**
 * ConvSigmetResource - Display Convective SIGMET data.
 * 
 *  This code has been developed by the SIB for use in the AWIPS2 system.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 16 Jun 2009  95         B. Hebbard  Initial creation.
 * 17 Jun 2009  115        G. Hull     Integrate with INatlCntrsResouce
 * 02 Jul 2009	134		   M. Li	   Use vors.xml
 * 10 Aug 2009             B. Hebbard  Convert to TO11 structure
 * 23 Apr 2010  245        Greg Hull   Change SLIDER_TEXTs to SPINNERs
 * 
 * </pre>
 * 
 * @author bhebbard 
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name="NC-ConvSigmetResourceData")
public class ConvSigmetResourceData extends AbstractNatlCntrsRequestableResourceData
                        implements IMiscResourceData, INatlCntrsResourceData { 

    @XmlElement
    protected String sourceName;

    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB color;  //  resource legend color only
    
    @XmlElement
    protected boolean hour0Enable;
    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB     hour0Color;
    @XmlElement
    protected int     hour0LineWidth;

    @XmlElement
    protected boolean hour1Enable;
    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB     hour1Color;
    @XmlElement
    protected int     hour1LineWidth;

    @XmlElement
    protected boolean hour2Enable;
    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB     hour2Color;
    @XmlElement
    protected int     hour2LineWidth;

    @XmlElement
    protected boolean outlookEnable;
    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB     outlookColor;
    @XmlElement
    protected int     outlookLineWidth;
    
    @XmlElement
    protected boolean hour0sequenceIdEnable;
    @XmlElement
    protected boolean hour1sequenceIdEnable;
    @XmlElement
    protected boolean hour2sequenceIdEnable;
    @XmlElement
    protected boolean timeEnable;
    @XmlElement
    protected boolean motionEnable;
    @XmlElement
    protected boolean flightLevelEnable;
    @XmlElement
    protected boolean intensityEnable;
    
    //  ------------------------------------------------------------

    private static final RGB    RESOURCE_LEGEND_COLOR = new RGB(155, 155, 155);
    
    /**
     * Create a Convective SIGMET resource.
     * 
     * @throws VizException
     */
    public ConvSigmetResourceData() throws VizException {
        super();
        this.nameGenerator = new AbstractNameGenerator() {
            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
                if( sourceName != null ) {
                    return sourceName;
                }
                return "Convective SIGMET";
            }
        };
        color = RESOURCE_LEGEND_COLOR;
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

    public boolean getHour0Enable() {
		return hour0Enable;
	}

	public void setHour0Enable(boolean hour0Enable) {
		this.hour0Enable = hour0Enable;
	}

	public RGB getHour0Color() {
		return hour0Color;
	}

	public void setHour0Color(RGB hour0Color) {
		this.hour0Color = hour0Color;
	}

	public int getHour0LineWidth() {
		return hour0LineWidth;
	}

	public void setHour0LineWidth(int hour0LineWidth) {
		this.hour0LineWidth = hour0LineWidth;
	}

	public boolean getHour1Enable() {
		return hour1Enable;
	}

	public void setHour1Enable(boolean hour1Enable) {
		this.hour1Enable = hour1Enable;
	}

	public RGB getHour1Color() {
		return hour1Color;
	}

	public void setHour1Color(RGB hour1Color) {
		this.hour1Color = hour1Color;
	}

	public int getHour1LineWidth() {
		return hour1LineWidth;
	}

	public void setHour1LineWidth(int hour1LineWidth) {
		this.hour1LineWidth = hour1LineWidth;
	}

	public boolean getHour2Enable() {
		return hour2Enable;
	}

	public void setHour2Enable(boolean hour2Enable) {
		this.hour2Enable = hour2Enable;
	}

	public RGB getHour2Color() {
		return hour2Color;
	}

	public void setHour2Color(RGB hour2Color) {
		this.hour2Color = hour2Color;
	}

	public int getHour2LineWidth() {
		return hour2LineWidth;
	}

	public void setHour2LineWidth(int hour2LineWidth) {
		this.hour2LineWidth = hour2LineWidth;
	}

	public boolean getOutlookEnable() {
		return outlookEnable;
	}

	public void setOutlookEnable(boolean outlookEnable) {
		this.outlookEnable = outlookEnable;
	}

	public RGB getOutlookColor() {
		return outlookColor;
	}

	public void setOutlookColor(RGB outlookColor) {
		this.outlookColor = outlookColor;
	}

	public int getOutlookLineWidth() {
		return outlookLineWidth;
	}

	public void setOutlookLineWidth(int outlookLineWidth) {
		this.outlookLineWidth = outlookLineWidth;
	}

	public boolean getHour0sequenceIdEnable() {
		return hour0sequenceIdEnable;
	}

	public void setHour0sequenceIdEnable(boolean hour0sequenceIdEnable) {
		this.hour0sequenceIdEnable = hour0sequenceIdEnable;
	}

	public boolean getHour1sequenceIdEnable() {
		return hour1sequenceIdEnable;
	}

	public void setHour1sequenceIdEnable(boolean hour1sequenceIdEnable) {
		this.hour1sequenceIdEnable = hour1sequenceIdEnable;
	}

	public boolean getHour2sequenceIdEnable() {
		return hour2sequenceIdEnable;
	}

	public void setHour2sequenceIdEnable(boolean hour2sequenceIdEnable) {
		this.hour2sequenceIdEnable = hour2sequenceIdEnable;
	}

	public boolean getTimeEnable() {
		return timeEnable;
	}

	public void setTimeEnable(boolean timeEnable) {
		this.timeEnable = timeEnable;
	}

	public boolean getMotionEnable() {
		return motionEnable;
	}

	public void setMotionEnable(boolean motionEnable) {
		this.motionEnable = motionEnable;
	}

	public boolean getFlightLevelEnable() {
		return flightLevelEnable;
	}

	public void setFlightLevelEnable(boolean flightLevelEnable) {
		this.flightLevelEnable = flightLevelEnable;
	}

	public boolean getIntensityEnable() {
		return intensityEnable;
	}

	public void setIntensityEnable(boolean intensityEnable) {
		this.intensityEnable = intensityEnable;
	}

	@Override
	public MiscRscAttrs getMiscResourceAttrs() {
		MiscRscAttrs attrs = new MiscRscAttrs( 3 );
	
		attrs.addAttr( new MiscResourceAttr( "hour0Enable", "Initial (0-hour)",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "hour0Color", "", 
				EditElement.COLOR_SELECTOR, 2 ));
		attrs.addAttr( new MiscResourceAttr( "hour0LineWidth", "Line Width", 
				EditElement.SPINNER, 3 ));				
		attrs.addAttr( new MiscResourceAttr( "hour1Enable", "1-hour",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "hour1Color", "", 
				EditElement.COLOR_SELECTOR, 2 ));
		attrs.addAttr( new MiscResourceAttr( "hour1LineWidth", "Line Width", 
				EditElement.SPINNER, 3 ));				
		attrs.addAttr( new MiscResourceAttr( "hour2Enable", "2-hour",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "hour2Color", "", 
				EditElement.COLOR_SELECTOR, 2 ));
		attrs.addAttr( new MiscResourceAttr( "hour2LineWidth", "Line Width", 
				EditElement.SPINNER, 3 ));				
		attrs.addAttr( new MiscResourceAttr( "outlookEnable", "Outlook",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "outlookColor", "", 
				EditElement.COLOR_SELECTOR, 2 ));
		attrs.addAttr( new MiscResourceAttr( "outlookLineWidth", "Line Width", 
				EditElement.SPINNER, 3 ));				
		
		attrs.addAttr( new MiscResourceAttr( null, null, 
			   EditElement.SEPARATOR, 1 ));
		
		attrs.addAttr( new MiscResourceAttr( "hour0sequenceIdEnable", "Sequence ID (0-hour)",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "timeEnable", "Time",
				EditElement.CHECK_BOX, 1 ));
		
		attrs.addAttr( new MiscResourceAttr( "motionEnable", "Direction/speed",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "flightLevelEnable", "Flight Level",
				EditElement.CHECK_BOX, 1 ));
		attrs.addAttr( new MiscResourceAttr( "intensityEnable", "Intensity",
				EditElement.CHECK_BOX, 1 ));
		attrs.addAttr( new MiscResourceAttr( "hour1sequenceIdEnable", "Sequence ID (1-hour)",
				EditElement.CHECK_BOX, 1 ));
		attrs.addAttr( new MiscResourceAttr( "hour2sequenceIdEnable", "Sequence ID (2-hour)",
				EditElement.CHECK_BOX, 1 ));

		return attrs;
	}	

	@Override
	protected AbstractVizResource<?, ?> constructResource(
			LoadProperties loadProperties,
			com.raytheon.uf.common.dataplugin.PluginDataObject[] objects)
			throws VizException {
		return new ConvSigmetResource( this, loadProperties );
	}

}