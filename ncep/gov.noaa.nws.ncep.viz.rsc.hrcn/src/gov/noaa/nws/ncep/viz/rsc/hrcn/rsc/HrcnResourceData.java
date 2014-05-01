package gov.noaa.nws.ncep.viz.rsc.hrcn.rsc;

import gov.noaa.nws.ncep.viz.common.RGBColorAdapter;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.misc.IMiscResourceData;
import gov.noaa.nws.ncep.viz.resources.misc.IMiscResourceData.EditElement;
import gov.noaa.nws.ncep.viz.resources.misc.IMiscResourceData.MiscResourceAttr;

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
 * HrcnResource - Display Tropical Cyclone data
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04 Oct 2011  466        B. Hebbard  Initial creation.
 * 
 * </pre>
 * 
 * @author bhebbard 
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name="NC-HrcnResourceData")
public class HrcnResourceData extends AbstractNatlCntrsRequestableResourceData
                        implements IMiscResourceData, INatlCntrsResourceData { 

    @XmlElement
    protected String sourceName;

    @XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB color;  //  resource legend color only

    @XmlElement
    protected boolean hurricaneEnable;
    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB     hurricaneColor;
    @XmlElement
    protected int     hurricaneSymbolWidth;
    @XmlElement
    protected int     hurricaneSymbolSize;

    @XmlElement
    protected boolean tropicalStormEnable;
    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB     tropicalStormColor;
    @XmlElement
    protected int     tropicalStormSymbolWidth;
    @XmlElement
    protected int     tropicalStormSymbolSize;

    @XmlElement
    protected boolean tropicalDepressionEnable;
    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB     tropicalDepressionColor;
    @XmlElement
    protected int     tropicalDepressionSymbolWidth;
    @XmlElement
    protected int     tropicalDepressionSymbolSize;

    @XmlElement
    protected boolean motionDirectionEnable;
    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB     motionDirectionColor;
    @XmlElement
    protected int     motionDirectionArrowWidth;
    @XmlElement
    protected int     motionDirectionArrowSize;
    @XmlElement
    protected int     motionDirectionArrowHeadSize;

    @XmlElement
    protected boolean timeEnable;
    @XmlElement
    protected boolean namePressureEnable;
    @XmlElement
    protected boolean motionSpeedEnable;
    @XmlElement
    protected boolean windSeaFtRadiiEnable;
	
    @XmlElement
    protected boolean forecastPosHour00;
    @XmlElement
    protected boolean forecastPosHour06;
    @XmlElement
    protected boolean forecastPosHour12;
    @XmlElement
    protected boolean forecastPosHour18;
    @XmlElement
    protected boolean forecastPosHour24;
    @XmlElement
    protected boolean forecastPosHour30;
    @XmlElement
    protected boolean forecastPosHour36;
    @XmlElement
    protected boolean forecastPosHour42;
    @XmlElement
    protected boolean forecastPosHour48;
    @XmlElement
    protected boolean forecastPosHour54;
    @XmlElement
    protected boolean forecastPosHour60;
    @XmlElement
    protected boolean forecastPosHour66;
    @XmlElement
    protected boolean forecastPosHour72;
    @XmlElement
    protected boolean forecastPosHour78;
    @XmlElement
    protected boolean forecastPosHour84;
    @XmlElement
    protected boolean forecastPosHour90;
    @XmlElement
    protected boolean forecastPosHour96;
    @XmlElement
    protected boolean forecastPosHour120;
    
    //  ------------------------------------------------------------

    private static final RGB    RESOURCE_LEGEND_COLOR = new RGB(155, 155, 155);
    
    /**
     * Create a HRCN resource.
     * 
     * @throws VizException
     */
    public HrcnResourceData() throws VizException {
        super();
        this.nameGenerator = new AbstractNameGenerator() {
            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
                if( sourceName != null ) {
                    return sourceName;
                }
                return "HRCN";
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

	public boolean getHurricaneEnable() {
		return hurricaneEnable;
	}

	public void setHurricaneEnable(boolean hurricaneEnable) {
		this.hurricaneEnable = hurricaneEnable;
	}

	public RGB getHurricaneColor() {
		return hurricaneColor;
	}

	public void setHurricaneColor(RGB hurricaneColor) {
		this.hurricaneColor = hurricaneColor;
	}

	public int getHurricaneSymbolWidth() {
		return hurricaneSymbolWidth;
	}

	public void setHurricaneSymbolWidth(int hurricaneSymbolWidth) {
		this.hurricaneSymbolWidth = hurricaneSymbolWidth;
	}

	public int getHurricaneSymbolSize() {
		return hurricaneSymbolSize;
	}

	public void setHurricaneSymbolSize(int hurricaneSymbolSize) {
		this.hurricaneSymbolSize = hurricaneSymbolSize;
	}

	public boolean getTropicalStormEnable() {
		return tropicalStormEnable;
	}

	public void setTropicalStormEnable(boolean tropicalStormEnable) {
		this.tropicalStormEnable = tropicalStormEnable;
	}

	public RGB getTropicalStormColor() {
		return tropicalStormColor;
	}

	public void setTropicalStormColor(RGB tropicalStormColor) {
		this.tropicalStormColor = tropicalStormColor;
	}

	public int getTropicalStormSymbolWidth() {
		return tropicalStormSymbolWidth;
	}

	public void setTropicalStormSymbolWidth(int tropicalStormSymbolWidth) {
		this.tropicalStormSymbolWidth = tropicalStormSymbolWidth;
	}

	public int getTropicalStormSymbolSize() {
		return tropicalStormSymbolSize;
	}

	public void setTropicalStormSymbolSize(int tropicalStormSymbolSize) {
		this.tropicalStormSymbolSize = tropicalStormSymbolSize;
	}

	public boolean getTropicalDepressionEnable() {
		return tropicalDepressionEnable;
	}

	public void setTropicalDepressionEnable(boolean tropicalDepressionEnable) {
		this.tropicalDepressionEnable = tropicalDepressionEnable;
	}

	public RGB getTropicalDepressionColor() {
		return tropicalDepressionColor;
	}

	public void setTropicalDepressionColor(RGB tropicalDepressionColor) {
		this.tropicalDepressionColor = tropicalDepressionColor;
	}

	public int getTropicalDepressionSymbolWidth() {
		return tropicalDepressionSymbolWidth;
	}

	public void setTropicalDepressionSymbolWidth(int tropicalDepressionSymbolWidth) {
		this.tropicalDepressionSymbolWidth = tropicalDepressionSymbolWidth;
	}

	public int getTropicalDepressionSymbolSize() {
		return tropicalDepressionSymbolSize;
	}

	public void setTropicalDepressionSymbolSize(int tropicalDepressionSymbolSize) {
		this.tropicalDepressionSymbolSize = tropicalDepressionSymbolSize;
	}

	public boolean getMotionDirectionEnable() {
		return motionDirectionEnable;
	}

	public void setMotionDirectionEnable(boolean motionDirectionEnable) {
		this.motionDirectionEnable = motionDirectionEnable;
	}

	public RGB getMotionDirectionColor() {
		return motionDirectionColor;
	}

	public void setMotionDirectionColor(RGB motionDirectionColor) {
		this.motionDirectionColor = motionDirectionColor;
	}

	public int getMotionDirectionArrowWidth() {
		return motionDirectionArrowWidth;
	}

	public void setMotionDirectionArrowWidth(int motionDirectionArrowWidth) {
		this.motionDirectionArrowWidth = motionDirectionArrowWidth;
	}

	public int getMotionDirectionArrowSize() {
		return motionDirectionArrowSize;
	}

	public void setMotionDirectionArrowSize(int motionDirectionArrowSize) {
		this.motionDirectionArrowSize = motionDirectionArrowSize;
	}

	public int getMotionDirectionArrowHeadSize() {
		return motionDirectionArrowHeadSize;
	}

	public void setMotionDirectionArrowHeadSize(int motionDirectionArrowHeadSize) {
		this.motionDirectionArrowHeadSize = motionDirectionArrowHeadSize;
	}

	public boolean getTimeEnable() {
		return timeEnable;
	}

	public void setTimeEnable(boolean timeEnable) {
		this.timeEnable = timeEnable;
	}

	public boolean getNamePressureEnable() {
		return namePressureEnable;
	}

	public void setNamePressureEnable(boolean namePressureEnable) {
		this.namePressureEnable = namePressureEnable;
	}

	public boolean getMotionSpeedEnable() {
		return motionSpeedEnable;
	}

	public void setMotionSpeedEnable(boolean motionSpeedEnable) {
		this.motionSpeedEnable = motionSpeedEnable;
	}

	public boolean getWindSeaFtRadiiEnable() {
		return windSeaFtRadiiEnable;
	}

	public void setWindSeaFtRadiiEnable(boolean windSeaFtRadiiEnable) {
		this.windSeaFtRadiiEnable = windSeaFtRadiiEnable;
	}

	public boolean getForecastPosHour00() {
		return forecastPosHour00;
	}

	public void setForecastPosHour00(boolean forecastPosHour00) {
		this.forecastPosHour00 = forecastPosHour00;
	}

	public boolean getForecastPosHour06() {
		return forecastPosHour06;
	}

	public void setForecastPosHour06(boolean forecastPosHour06) {
		this.forecastPosHour06 = forecastPosHour06;
	}

	public boolean getForecastPosHour12() {
		return forecastPosHour12;
	}

	public void setForecastPosHour12(boolean forecastPosHour12) {
		this.forecastPosHour12 = forecastPosHour12;
	}

	public boolean getForecastPosHour18() {
		return forecastPosHour18;
	}

	public void setForecastPosHour18(boolean forecastPosHour18) {
		this.forecastPosHour18 = forecastPosHour18;
	}

	public boolean getForecastPosHour24() {
		return forecastPosHour24;
	}

	public void setForecastPosHour24(boolean forecastPosHour24) {
		this.forecastPosHour24 = forecastPosHour24;
	}

	public boolean getForecastPosHour30() {
		return forecastPosHour30;
	}

	public void setForecastPosHour30(boolean forecastPosHour30) {
		this.forecastPosHour30 = forecastPosHour30;
	}

	public boolean getForecastPosHour36() {
		return forecastPosHour36;
	}

	public void setForecastPosHour36(boolean forecastPosHour36) {
		this.forecastPosHour36 = forecastPosHour36;
	}

	public boolean getForecastPosHour42() {
		return forecastPosHour42;
	}

	public void setForecastPosHour42(boolean forecastPosHour42) {
		this.forecastPosHour42 = forecastPosHour42;
	}

	public boolean getForecastPosHour48() {
		return forecastPosHour48;
	}

	public void setForecastPosHour48(boolean forecastPosHour48) {
		this.forecastPosHour48 = forecastPosHour48;
	}

	public boolean getForecastPosHour54() {
		return forecastPosHour54;
	}

	public void setForecastPosHour54(boolean forecastPosHour54) {
		this.forecastPosHour54 = forecastPosHour54;
	}

	public boolean getForecastPosHour60() {
		return forecastPosHour60;
	}

	public void setForecastPosHour60(boolean forecastPosHour60) {
		this.forecastPosHour60 = forecastPosHour60;
	}

	public boolean getForecastPosHour66() {
		return forecastPosHour66;
	}

	public void setForecastPosHour66(boolean forecastPosHour66) {
		this.forecastPosHour66 = forecastPosHour66;
	}

	public boolean getForecastPosHour72() {
		return forecastPosHour72;
	}

	public void setForecastPosHour72(boolean forecastPosHour72) {
		this.forecastPosHour72 = forecastPosHour72;
	}

	public boolean getForecastPosHour78() {
		return forecastPosHour78;
	}

	public void setForecastPosHour78(boolean forecastPosHour78) {
		this.forecastPosHour78 = forecastPosHour78;
	}

	public boolean getForecastPosHour84() {
		return forecastPosHour84;
	}

	public void setForecastPosHour84(boolean forecastPosHour84) {
		this.forecastPosHour84 = forecastPosHour84;
	}

	public boolean getForecastPosHour90() {
		return forecastPosHour90;
	}

	public void setForecastPosHour90(boolean forecastPosHour90) {
		this.forecastPosHour90 = forecastPosHour90;
	}

	public boolean getForecastPosHour96() {
		return forecastPosHour96;
	}

	public void setForecastPosHour96(boolean forecastPosHour96) {
		this.forecastPosHour96 = forecastPosHour96;
	}

	public boolean getForecastPosHour120() {
		return forecastPosHour120;
	}

	public void setForecastPosHour120(boolean forecastPosHour120) {
		this.forecastPosHour120 = forecastPosHour120;
	}

	public static RGB getResourceLegendColor() {
		return RESOURCE_LEGEND_COLOR;
	}

	public void setSourceName(String sourceName) {
		this.sourceName = sourceName;
	}

	@Override
	public MiscRscAttrs getMiscResourceAttrs() {
		MiscRscAttrs attrs = new MiscRscAttrs( 6 );

		attrs.addAttr(new MiscResourceAttr("hurricaneEnable", "Hurricane",
				EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("hurricaneColor", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("", "",
				EditElement.LABEL, 3));		
		attrs.addAttr(new MiscResourceAttr("hurricaneSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("hurricaneSymbolSize",
				"Symbol Size", EditElement.SPINNER, 5));

		attrs.addAttr(new MiscResourceAttr("tropicalStormEnable",
				"Tropical Storm", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("tropicalStormColor", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("","", EditElement.LABEL, 3));
		attrs.addAttr(new MiscResourceAttr("tropicalStormSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("tropicalStormSymbolSize",
				"Symbol Size", EditElement.SPINNER, 5));

		attrs.addAttr(new MiscResourceAttr("tropicalDepressionEnable",
				"Tropical Depression", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("tropicalDepressionColor", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("", "",
				EditElement.LABEL, 3));
		attrs.addAttr(new MiscResourceAttr("tropicalDepressionSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("tropicalDepressionSymbolSize",
				"Symbol Size", EditElement.SPINNER, 5));

		attrs.addAttr(new MiscResourceAttr("motionDirectionEnable", "MotionDirection",
				EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("motionDirectionColor", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("", "",
				EditElement.LABEL, 3));		
		attrs.addAttr(new MiscResourceAttr("motionDirectionArrowWidth",
				"Arrow Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("motionDirectionArrowSize",
				"Arrow Size", EditElement.SPINNER, 5));
		attrs.addAttr(new MiscResourceAttr("motionDirectionArrowHeadSize",
				"Arrow Head Size", EditElement.SPINNER, 6));
		
		attrs.addAttr( new MiscResourceAttr( null, null, 
			   EditElement.SEPARATOR, 1 ));
		
		attrs.addAttr( new MiscResourceAttr( "timeEnable", "Time",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "namePressureEnable", "Name/Pressure",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "motionSpeedEnable", "Motion Speed",
				EditElement.CHECK_BOX, 1 ));		
		attrs.addAttr( new MiscResourceAttr( "windSeaFtRadiiEnable", "Wind/Sea Ft Radii",
				EditElement.CHECK_BOX, 1 ));
		attrs.addAttr( new MiscResourceAttr( "forecastPosHour00", " 00 Hr Forecast Pos",
				EditElement.CHECK_BOX, 1 ));
		attrs.addAttr( new MiscResourceAttr( "forecastPosHour06", " 06 Hr Forecast Pos",
				EditElement.CHECK_BOX, 1 ));
		attrs.addAttr( new MiscResourceAttr( "forecastPosHour12", " 12 Hr Forecast Pos",
				EditElement.CHECK_BOX, 1 ));
		attrs.addAttr( new MiscResourceAttr( "forecastPosHour18", " 18 Hr Forecast Pos",
				EditElement.CHECK_BOX, 1 ));
		attrs.addAttr( new MiscResourceAttr( "forecastPosHour24", " 24 Hr Forecast Pos",
				EditElement.CHECK_BOX, 1 ));
		attrs.addAttr( new MiscResourceAttr( "forecastPosHour30", " 30 Hr Forecast Pos",
				EditElement.CHECK_BOX, 1 ));
		attrs.addAttr( new MiscResourceAttr( "forecastPosHour36", " 36 Hr Forecast Pos",
				EditElement.CHECK_BOX, 1 ));
		attrs.addAttr( new MiscResourceAttr( "forecastPosHour42", " 42 Hr Forecast Pos",
				EditElement.CHECK_BOX, 1 ));
		attrs.addAttr( new MiscResourceAttr( "forecastPosHour48", " 48 Hr Forecast Pos",
				EditElement.CHECK_BOX, 1 ));
		attrs.addAttr( new MiscResourceAttr( "forecastPosHour54", " 54 Hr Forecast Pos",
				EditElement.CHECK_BOX, 1 ));
		attrs.addAttr( new MiscResourceAttr( "forecastPosHour60", " 60 Hr Forecast Pos",
				EditElement.CHECK_BOX, 1 ));
		attrs.addAttr( new MiscResourceAttr( "forecastPosHour66", " 66 Hr Forecast Pos",
				EditElement.CHECK_BOX, 1 ));
		attrs.addAttr( new MiscResourceAttr( "forecastPosHour72", " 72 Hr Forecast Pos",
				EditElement.CHECK_BOX, 1 ));
		attrs.addAttr( new MiscResourceAttr( "forecastPosHour78", " 78 Hr Forecast Pos",
				EditElement.CHECK_BOX, 1 ));
		attrs.addAttr( new MiscResourceAttr( "forecastPosHour84", " 84 Hr Forecast Pos",
				EditElement.CHECK_BOX, 1 ));
		attrs.addAttr( new MiscResourceAttr( "forecastPosHour90", " 90 Hr Forecast Pos",
				EditElement.CHECK_BOX, 1 ));
		attrs.addAttr( new MiscResourceAttr( "forecastPosHour96", " 96 Hr Forecast Pos",
				EditElement.CHECK_BOX, 1 ));
		attrs.addAttr( new MiscResourceAttr( "forecastPosHour120", "120 Hr Forecast Pos",
				EditElement.CHECK_BOX, 1 ));

		return attrs;
	}	

	@Override
	protected AbstractVizResource<?, ?> constructResource(
			LoadProperties loadProperties,
			com.raytheon.uf.common.dataplugin.PluginDataObject[] objects)
			throws VizException {
		return new HrcnResource( this, loadProperties );
	}

}