/**
 * StormTrackResourceData
 * Date created: October 11, 2011
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.viz.rsc.stormtrack.rsc;

import java.util.ArrayList;
import java.util.List;

import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.misc.IMiscResourceData;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.time.DataTime;

import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.attributes.RGBColorAdapter;
import gov.noaa.nws.ncep.viz.resources.misc.IMiscResourceData;
import gov.noaa.nws.ncep.viz.resources.misc.IMiscResourceData.EditElement;
import gov.noaa.nws.ncep.viz.resources.misc.IMiscResourceData.MiscResourceAttr;
import gov.noaa.nws.ncep.viz.resources.misc.IMiscResourceData.MiscRscAttrs;
import gov.noaa.nws.ncep.viz.ui.display.ColorBar;
import gov.noaa.nws.ncep.gempak.parameters.colorbar.ColorBarAnchorLocation;
import gov.noaa.nws.ncep.gempak.parameters.colorbar.ColorBarOrientation;

import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

/**
 * <pre>
 * StormTrackResourceData - Creates and updates the elements of the Storm Track edit dialog. 
 * 
  * SOFTWARE HISTORY
 *    Date                Ticket#     Engineer         Description
 * ------------          ------------   ------------- --------------------------
 * 11-Oct-2011                           sgilbert          Initial creation.
 * 25 Oct 2011                   bhebbard          Add TD/Gale/TS/Hurricane attributes
 * 01 Oct 2012             #860	         ghull             replace hardcoded modelNames
 * 
 * </pre>
 * @author sgilbert
 *
 */
@SuppressWarnings("unused")
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "StormTrackResourceData")
public class StormTrackResourceData  extends	AbstractNatlCntrsRequestableResourceData implements IMiscResourceData,
INatlCntrsResourceData{

	ColorBar windSpeedColorBar = new ColorBar(); {
		windSpeedColorBar.setAnchorLoc( ColorBarAnchorLocation.LowerLeft );
		windSpeedColorBar.setOrientation( ColorBarOrientation.Vertical );
		windSpeedColorBar.setLengthAsRatio( .3f );
		windSpeedColorBar.setWidthInPixels( 10 );
		windSpeedColorBar.setDrawToScale( false );
		windSpeedColorBar.addColorBarInterval( 0.0f, 34.0f,  new RGB( 0,0,0 ) );
		windSpeedColorBar.addColorBarInterval( 34.0f, 48.0f, new RGB( 0,0,0 ) );
		windSpeedColorBar.addColorBarInterval( 48.0f, 64.0f, new RGB( 0,0,0 ) );
		windSpeedColorBar.addColorBarInterval( 64.0f, 200.0f, new RGB( 0,0,0 ) );
		// units currently not supported by the ColorBar but set this anyway 
		windSpeedColorBar.setDataUnits( NonSI.KNOT );
		windSpeedColorBar.setNumDecimals(1);
	}	
	
	protected Boolean[] windSpeedCatEnable = new Boolean[] { true, true, true, true };
	
//	@XmlElement
//	@XmlJavaTypeAdapter(RGBColorAdapter.class)
//	protected RGB color;
	
	@Override
	public RGB getLegendColor() {		
		return getModel01Color();
	}

	public static class ModelDisplayAttrs {
		String  modelName="";
		String  cycloneID="";
		Boolean enabled=false;
		RGB     color;
		Integer lineWidth;
		Float   symbolSize;
//		double   symbolScale;
	}

	final static int MAX_NUM_MODELS = 20;
	private ModelDisplayAttrs[] modelDisplayAttrs = new ModelDisplayAttrs[MAX_NUM_MODELS];
	
	public ModelDisplayAttrs[] getModelDisplayAttributes() {
		return modelDisplayAttrs;
	}

	// the times in the db have the forecast hours set. This works and is needed for the forecast version of the storm track 
	// resource but this is a problem for the regular (ie legacy version) that generates the timeline from the cycle times.
	// In this case we will return the available times list with only the cycle times.
	//
	@Override
	public List<DataTime> getAvailableDataTimes( ) {

		List<DataTime> times = super.getAvailableDataTimes(); 
		if( isForecastResource() ) {
			return times;
		}
		else {
			List<DataTime> refTimes = new ArrayList<DataTime>();
			for( DataTime dt : times ) {
				dt = new DataTime( dt.getRefTime() );
				if( !refTimes.contains( dt ) ) {
					refTimes.add( dt );
				}
			}
			return refTimes;
		}
	}
	
	//------------------------------------------------------

	@XmlElement
	protected boolean colorCodeByWindSpeed; 

	@XmlElement
	protected boolean drawBeginDateTime;
	
	@XmlElement
	protected boolean drawForecastHour;

	@XmlElement
	protected boolean drawPressure;

	@XmlElement
	protected boolean drawWindSpeed;
	
	@XmlElement
	protected boolean drawModelName;

	@XmlElement
	protected boolean drawCycloneID;

	@XmlElement
	protected boolean drawMarker;

	// only draw the given forecast hour
	@XmlElement
	protected boolean forecastHourEnable;

	@XmlElement
	protected Integer forecastHour;
	
	public boolean getForecastHourEnable() {
		return forecastHourEnable;
	}
	
	public void setForecastHourEnable(boolean forecastHourEnable) {
		this.forecastHourEnable = forecastHourEnable;
	}
	
	public Integer getForecastHour() {
		return forecastHour;
	}

	public void setForecastHour(Integer forecastHour) {
		this.forecastHour = forecastHour;
	}

	@XmlElement
	protected String legend="StormTrack";

	public String getLegend() {
		return legend;
	}

	public void setLegend(String legend) {
		this.legend = legend;
	}
	
	public StormTrackResourceData() throws VizException {
		super();
		this.nameGenerator = new AbstractNameGenerator() {
			@Override
			public String getName(AbstractVizResource<?, ?> resource) {
				return legend;
			}
		};
	}	

//	@Override
	protected AbstractVizResource<?, ?> constructResource(
			LoadProperties loadProperties,
			PluginDataObject[] objects){
		StormTrackResource thisResource = new StormTrackResource(this,loadProperties);
		return thisResource;
	}

	@Override
	public MiscRscAttrs getMiscResourceAttrs() {
		MiscRscAttrs attrs = new MiscRscAttrs(9);

		attrs.addAttr(new MiscResourceAttr(null,
				"Model Name", EditElement.LABEL, 1));			

		attrs.addAttr(new MiscResourceAttr(null,
				"  Line Width", EditElement.LABEL, 3));			
	
		attrs.addAttr(new MiscResourceAttr(null,
				"Marker Size", EditElement.LABEL, 4));			

		attrs.addAttr(new MiscResourceAttr(null,
				"Model Name", EditElement.LABEL, 6));			

		attrs.addAttr(new MiscResourceAttr(null,
				"Line Width     ", EditElement.LABEL, 8));			

		attrs.addAttr(new MiscResourceAttr(null,
				"Marker Size", EditElement.LABEL, 9));			

		if( !getModel01Name().isEmpty() ) {
			attrs.addAttr(new MiscResourceAttr("model01Enable",
					getModel01Name(), EditElement.CHECK_BOX, 1));
			attrs.addAttr(new MiscResourceAttr("model01Color", "",
					EditElement.COLOR_SELECTOR, 2));
			attrs.addAttr( new MiscResourceAttr("model01LineWidth",
					"", EditElement.SPINNER, 3) );
			attrs.addAttr(new MiscResourceAttr("model01SymbolSize",
					"", EditElement.SPINNER, 4));
		}
	
		if( !getModel11Name().isEmpty() ) {
			attrs.addAttr(new MiscResourceAttr(null,
					null, EditElement.VERTICAL_SEPARATOR, 5));			
			attrs.addAttr(new MiscResourceAttr("model11Enable",
					getModel11Name(), EditElement.CHECK_BOX, 6));
			attrs.addAttr(new MiscResourceAttr("model11Color", "",
					EditElement.COLOR_SELECTOR, 7));
			attrs.addAttr(new MiscResourceAttr("model11LineWidth",
					"", EditElement.SPINNER, 8));
			attrs.addAttr(new MiscResourceAttr("model11SymbolSize",
					"", EditElement.SPINNER, 9));
		}

		if( !getModel02Name().isEmpty() ) {		
			attrs.addAttr(new MiscResourceAttr("model02Enable",
					getModel02Name(), EditElement.CHECK_BOX, 1));
			attrs.addAttr(new MiscResourceAttr("model02Color", "",
					EditElement.COLOR_SELECTOR, 2));
			attrs.addAttr(new MiscResourceAttr("model02LineWidth",
					"", EditElement.SPINNER, 3));
			attrs.addAttr(new MiscResourceAttr("model02SymbolSize",
					"", EditElement.SPINNER, 4));
		}

		if( !getModel12Name().isEmpty() ) {		
			attrs.addAttr(new MiscResourceAttr(null,
					null, EditElement.VERTICAL_SEPARATOR, 5));			
			attrs.addAttr(new MiscResourceAttr("model12Enable",
					getModel12Name(), EditElement.CHECK_BOX, 6));
			attrs.addAttr(new MiscResourceAttr("model12Color", "",
					EditElement.COLOR_SELECTOR, 7));
			attrs.addAttr(new MiscResourceAttr("model12LineWidth",
					"", EditElement.SPINNER, 8));
			attrs.addAttr(new MiscResourceAttr("model12SymbolSize",
					"", EditElement.SPINNER, 9));
		}

		if( !getModel03Name().isEmpty() ) {		
			attrs.addAttr(new MiscResourceAttr("model03Enable",
					getModel03Name(), EditElement.CHECK_BOX, 1));
			attrs.addAttr(new MiscResourceAttr("model03Color", "",
					EditElement.COLOR_SELECTOR, 2));
			attrs.addAttr(new MiscResourceAttr("model03LineWidth",
					"", EditElement.SPINNER, 3));
			attrs.addAttr(new MiscResourceAttr("model03SymbolSize",
					"", EditElement.SPINNER, 4));
		}

		if( !getModel13Name().isEmpty() ) {		
			attrs.addAttr(new MiscResourceAttr(null,
					null, EditElement.VERTICAL_SEPARATOR, 5));			
			attrs.addAttr(new MiscResourceAttr("model13Enable",
					getModel13Name(), EditElement.CHECK_BOX, 6));
			attrs.addAttr(new MiscResourceAttr("model13Color", "",
					EditElement.COLOR_SELECTOR, 7));
			attrs.addAttr(new MiscResourceAttr("model13LineWidth",
					"", EditElement.SPINNER, 8));
			attrs.addAttr(new MiscResourceAttr("model13SymbolSize",
					"", EditElement.SPINNER, 9));
		}

		if( !getModel04Name().isEmpty() ) {		
			attrs.addAttr(new MiscResourceAttr("model04Enable",
					getModel04Name(), EditElement.CHECK_BOX, 1));
			attrs.addAttr(new MiscResourceAttr("model04Color", "",
					EditElement.COLOR_SELECTOR, 2));
			attrs.addAttr(new MiscResourceAttr("model04LineWidth",
					"", EditElement.SPINNER, 3));
			attrs.addAttr(new MiscResourceAttr("model04SymbolSize",
					"", EditElement.SPINNER, 4));
		}

		if( !getModel14Name().isEmpty() ) {		
			attrs.addAttr(new MiscResourceAttr(null,
					null, EditElement.VERTICAL_SEPARATOR, 5));			
			attrs.addAttr(new MiscResourceAttr("model14Enable",
					getModel14Name(), EditElement.CHECK_BOX, 6));
			attrs.addAttr(new MiscResourceAttr("model14Color", "",
					EditElement.COLOR_SELECTOR, 7));
			attrs.addAttr(new MiscResourceAttr("model14LineWidth",
					"", EditElement.SPINNER, 8));
			attrs.addAttr(new MiscResourceAttr("model14SymbolSize",
					"", EditElement.SPINNER, 9));
		}


		if( !getModel05Name().isEmpty() ) {		
			attrs.addAttr(new MiscResourceAttr("model05Enable",
					getModel05Name(), EditElement.CHECK_BOX, 1));
			attrs.addAttr(new MiscResourceAttr("model05Color", "",
					EditElement.COLOR_SELECTOR, 2));
			attrs.addAttr(new MiscResourceAttr("model05LineWidth",
					"", EditElement.SPINNER, 3));
			attrs.addAttr(new MiscResourceAttr("model05SymbolSize",
					"", EditElement.SPINNER, 4));
		}

		if( !getModel15Name().isEmpty() ) {		
			attrs.addAttr(new MiscResourceAttr(null,
					null, EditElement.VERTICAL_SEPARATOR, 5));			
			attrs.addAttr(new MiscResourceAttr("model15Enable",
					getModel15Name(), EditElement.CHECK_BOX, 6));
			attrs.addAttr(new MiscResourceAttr("model15Color", "",
					EditElement.COLOR_SELECTOR, 7));
			attrs.addAttr(new MiscResourceAttr("model15LineWidth",
					"", EditElement.SPINNER, 8));
			attrs.addAttr(new MiscResourceAttr("model15SymbolSize",
					"", EditElement.SPINNER, 9));
		}

		if( !getModel06Name().isEmpty() ) {
			attrs.addAttr(new MiscResourceAttr("model06Enable",
					getModel06Name(), EditElement.CHECK_BOX, 1));
			attrs.addAttr(new MiscResourceAttr("model06Color", "",
					EditElement.COLOR_SELECTOR, 2));
			attrs.addAttr(new MiscResourceAttr("model06LineWidth",
					"", EditElement.SPINNER, 3));
			attrs.addAttr(new MiscResourceAttr("model06SymbolSize",
					"", EditElement.SPINNER, 4));
		}

		if( !getModel16Name().isEmpty() ) {		
			attrs.addAttr(new MiscResourceAttr(null,
					null, EditElement.VERTICAL_SEPARATOR, 5));			
			attrs.addAttr(new MiscResourceAttr("model16Enable",
					getModel16Name(), EditElement.CHECK_BOX, 6));
			attrs.addAttr(new MiscResourceAttr("model16Color", "",
					EditElement.COLOR_SELECTOR, 7));
			attrs.addAttr(new MiscResourceAttr("model16LineWidth",
					"", EditElement.SPINNER, 8));
			attrs.addAttr(new MiscResourceAttr("model16SymbolSize",
					"", EditElement.SPINNER, 9));
		}

		if( !getModel07Name().isEmpty() ) {		
			attrs.addAttr(new MiscResourceAttr("model07Enable",
					getModel07Name(), EditElement.CHECK_BOX, 1));
			attrs.addAttr(new MiscResourceAttr("model07Color", "",
					EditElement.COLOR_SELECTOR, 2));
			attrs.addAttr(new MiscResourceAttr("model07LineWidth",
					"", EditElement.SPINNER, 3));
			attrs.addAttr(new MiscResourceAttr("model07SymbolSize",
					"", EditElement.SPINNER, 4));
		}
	
		if( !getModel17Name().isEmpty() ) {		
			attrs.addAttr(new MiscResourceAttr(null,
					null, EditElement.VERTICAL_SEPARATOR, 5));			
			attrs.addAttr(new MiscResourceAttr("model17Enable",
					getModel17Name(), EditElement.CHECK_BOX, 6));
			attrs.addAttr(new MiscResourceAttr("model17Color", "",
					EditElement.COLOR_SELECTOR, 7));
			attrs.addAttr(new MiscResourceAttr("model17LineWidth",
					"", EditElement.SPINNER, 8));
			attrs.addAttr(new MiscResourceAttr("model17SymbolSize",
					"", EditElement.SPINNER, 9));
		}

		if( !getModel08Name().isEmpty() ) {		
			attrs.addAttr(new MiscResourceAttr("model08Enable",
					getModel08Name(), EditElement.CHECK_BOX, 1));
			attrs.addAttr(new MiscResourceAttr("model08Color", "",
					EditElement.COLOR_SELECTOR, 2));
			attrs.addAttr(new MiscResourceAttr("model08LineWidth",
					"", EditElement.SPINNER, 3));
			attrs.addAttr(new MiscResourceAttr("model08SymbolSize",
					"", EditElement.SPINNER, 4));
		}

		if( !getModel18Name().isEmpty() ) {		
			attrs.addAttr(new MiscResourceAttr(null,
					null, EditElement.VERTICAL_SEPARATOR, 5));			
			attrs.addAttr(new MiscResourceAttr("model18Enable",
					getModel18Name(), EditElement.CHECK_BOX, 6));
			attrs.addAttr(new MiscResourceAttr("model18Color", "",
					EditElement.COLOR_SELECTOR, 7));
			attrs.addAttr(new MiscResourceAttr("model18LineWidth",
					"", EditElement.SPINNER, 8));
			attrs.addAttr(new MiscResourceAttr("model18SymbolSize",
					"", EditElement.SPINNER, 9));
		}

		if( !getModel09Name().isEmpty() ) {		
			attrs.addAttr(new MiscResourceAttr("model09Enable",
					getModel09Name(), EditElement.CHECK_BOX, 1));
			attrs.addAttr(new MiscResourceAttr("model09Color", "",
					EditElement.COLOR_SELECTOR, 2));
			attrs.addAttr(new MiscResourceAttr("model09LineWidth",
					"", EditElement.SPINNER, 3));
			attrs.addAttr(new MiscResourceAttr("model09SymbolSize",
					"", EditElement.SPINNER, 4));
		}

		if( !getModel19Name().isEmpty() ) {		
			attrs.addAttr(new MiscResourceAttr(null,
					null, EditElement.VERTICAL_SEPARATOR, 5));			
			attrs.addAttr(new MiscResourceAttr("model19Enable",
					getModel19Name(), EditElement.CHECK_BOX, 6));
			attrs.addAttr(new MiscResourceAttr("model19Color", "",
					EditElement.COLOR_SELECTOR, 7));
			attrs.addAttr(new MiscResourceAttr("model19LineWidth",
					"", EditElement.SPINNER, 8));
			attrs.addAttr(new MiscResourceAttr("model19SymbolSize",
					"", EditElement.SPINNER, 9));
		}
	
		if( !getModel10Name().isEmpty() ) {		
			attrs.addAttr(new MiscResourceAttr("model10Enable",
					getModel10Name(), EditElement.CHECK_BOX, 1));
			attrs.addAttr(new MiscResourceAttr("model10Color", "",
					EditElement.COLOR_SELECTOR, 2));
			attrs.addAttr(new MiscResourceAttr("model10LineWidth",
					"", EditElement.SPINNER, 3));
			attrs.addAttr(new MiscResourceAttr("model10SymbolSize",
					"", EditElement.SPINNER, 4));
		}

		if( !getModel20Name().isEmpty() ) {
			attrs.addAttr(new MiscResourceAttr(null,
					null, EditElement.VERTICAL_SEPARATOR, 5));			
			attrs.addAttr(new MiscResourceAttr("model20Enable",
					getModel20Name(), EditElement.CHECK_BOX, 6));
			attrs.addAttr(new MiscResourceAttr("model20Color", "",
					EditElement.COLOR_SELECTOR, 7));
			attrs.addAttr(new MiscResourceAttr("model20LineWidth",
					"", EditElement.SPINNER, 8));
			attrs.addAttr(new MiscResourceAttr("model20SymbolSize",
					"", EditElement.SPINNER, 9));
		}

		attrs.addAttr(new MiscResourceAttr(null, null,
				EditElement.SEPARATOR, 1 ));	

		attrs.addAttr(new MiscResourceAttr("colorCodeByWindSpeed", "Color Code by",
				EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr(null,
				"Wind Speed", EditElement.LABEL, 2));			

		attrs.addAttr(new MiscResourceAttr("forecastHourEnable",
				"Only Draw\nForecast Hour", EditElement.CHECK_BOX, 6));
		MiscResourceAttr fcstHrSpnr = new MiscResourceAttr("forecastHour",
				"", EditElement.SPINNER, 7);
		fcstHrSpnr.setSpinnerControls(0, 0, 384, 3, 12 );
		attrs.addAttr( fcstHrSpnr ); 
	
		attrs.addAttr(new MiscResourceAttr(null, null,
				EditElement.SEPARATOR, 1));	

		// 2 LABELS to avoid forcing column 1 to be too wide
		attrs.addAttr(new MiscResourceAttr(null,
				"Wind Speed", EditElement.LABEL, 1));			

//		attrs.addAttr(new MiscResourceAttr("colorCodeByWindSpeed", "Color Code",
//				EditElement.CHECK_BOX, 6));
//		attrs.addAttr(new MiscResourceAttr(null,
//				"by Wind Speed", EditElement.LABEL, 7));			

		attrs.addAttr(new MiscResourceAttr(null,
				"Category", EditElement.LABEL, 1));			

		attrs.addAttr(new MiscResourceAttr(null,
				"Upper Limit (kt)", EditElement.LABEL, 3));			
	
		attrs.addAttr(new MiscResourceAttr(null,
				"Enable Display of:", EditElement.LABEL, 6));			


		attrs.addAttr(new MiscResourceAttr("tropDepressionEnable",
				"Tropical     \nDepression", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("tropDepressionColor", "",
				EditElement.COLOR_SELECTOR, 2));
		MiscResourceAttr maxWindSpeedSpnr = new MiscResourceAttr("tropDepressionUpperLimit",
				"", EditElement.SPINNER, 3);
		maxWindSpeedSpnr.setSpinnerControls(1, 0, 2500, 10, 100 );
		attrs.addAttr( maxWindSpeedSpnr ); 


		attrs.addAttr(new MiscResourceAttr("drawBeginDateTime", "Begin Date/Time",
				EditElement.CHECK_BOX, 6));
	

		attrs.addAttr(new MiscResourceAttr("drawForecastHour", "Forecast Hr",
				EditElement.CHECK_BOX, 8));	


		attrs.addAttr(new MiscResourceAttr("galeEnable",
				"Gale", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("galeColor", "",
				EditElement.COLOR_SELECTOR, 2));
		maxWindSpeedSpnr = new MiscResourceAttr("galeUpperLimit",
				"", EditElement.SPINNER, 3);
		maxWindSpeedSpnr.setSpinnerControls(1, 0, 2500, 10, 100 );

		attrs.addAttr( maxWindSpeedSpnr ); 
	
		attrs.addAttr(new MiscResourceAttr("drawPressure", "Pressure",
				EditElement.CHECK_BOX, 6));	

		attrs.addAttr(new MiscResourceAttr("drawWindSpeed", "Wind Speed",
				EditElement.CHECK_BOX, 8));	

		attrs.addAttr(new MiscResourceAttr("tropStormEnable",
				"Tropical\nStorm  ", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("tropStormColor", "",
				EditElement.COLOR_SELECTOR, 2));
		maxWindSpeedSpnr = new MiscResourceAttr("tropStormUpperLimit",
				"", EditElement.SPINNER, 3);
		maxWindSpeedSpnr.setSpinnerControls(1, 0, 2500, 10, 100 );
		attrs.addAttr(maxWindSpeedSpnr );

		attrs.addAttr(new MiscResourceAttr("drawModelName", "Model Name",
				EditElement.CHECK_BOX, 6));	

		attrs.addAttr(new MiscResourceAttr("drawCycloneID", "Cyclone ID",
				EditElement.CHECK_BOX, 8));	
	
		attrs.addAttr(new MiscResourceAttr("hurricaneEnable",
				"Hurricane", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("hurricaneColor", "",
				EditElement.COLOR_SELECTOR, 2));
		maxWindSpeedSpnr = new MiscResourceAttr("hurricaneUpperLimit",
				"", EditElement.SPINNER, 3);
		maxWindSpeedSpnr.setSpinnerControls(1, 0, 2500, 10, 100 );;
		attrs.addAttr(maxWindSpeedSpnr );

		attrs.addAttr(new MiscResourceAttr("drawMarker", "Marker",
				EditElement.CHECK_BOX, 6));			

		return attrs;
	}

	public boolean getColorCodeByWindSpeed() {
		return colorCodeByWindSpeed;
	}

	public void setColorCodeByWindSpeed(boolean colorCodeByWindSpeed) {
		this.colorCodeByWindSpeed = colorCodeByWindSpeed;
	}
	
	public Boolean[] getWindSpeedCatEnable() {
		return windSpeedCatEnable;		
	}

	@XmlElement
	public boolean getTropDepressionEnable() {
		return windSpeedCatEnable[0];
	}

	public void setTropDepressionEnable(boolean tropDepressionEnable) {
		windSpeedCatEnable[0] = tropDepressionEnable;
	}

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	public RGB getTropDepressionColor() {
		return windSpeedColorBar.getRGB(0);
	}

	public void setTropDepressionColor(RGB tropDepressionColor) {
		windSpeedColorBar.setRGB(0, tropDepressionColor );
	}
	
	@XmlElement
	public float getTropDepressionUpperLimit() {
		return windSpeedColorBar.getIntervalMax(0);
	}

	public void setTropDepressionUpperLimit(float tropDepressionUpperLimit) {
		windSpeedColorBar.setIntervalMax(0, tropDepressionUpperLimit);
	}

	@XmlElement
	public boolean getGaleEnable() {
		return windSpeedCatEnable[1];
	}

	public void setGaleEnable(boolean galeEnable) {
		windSpeedCatEnable[1] = galeEnable;
	}

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	public RGB getGaleColor() {
		return windSpeedColorBar.getRGB(1);
	}
	
	public void setGaleColor(RGB galeColor) {
		windSpeedColorBar.setRGB(1,galeColor);
	}

	@XmlElement
	public float getGaleUpperLimit() {
		return windSpeedColorBar.getIntervalMax(1);
	}

	public void setGaleUpperLimit(float galeUpperLimit) {
		windSpeedColorBar.setIntervalMax(1, galeUpperLimit);
	}

	@XmlElement
	public boolean getTropStormEnable() {
		return windSpeedCatEnable[2];
	}

	public void setTropStormEnable(boolean tropStormEnable) {
		windSpeedCatEnable[2] = tropStormEnable;
	}
	
	public RGB getTropStormColor() {
		return windSpeedColorBar.getRGB(2);
	}

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	public void setTropStormColor(RGB tropStormColor) {
		windSpeedColorBar.setRGB(2, tropStormColor);
	}

	@XmlElement
	public float getTropStormUpperLimit() {
		return windSpeedColorBar.getIntervalMax(2);
	}

	public void setTropStormUpperLimit(float tropStormUpperLimit) {
		windSpeedColorBar.setIntervalMax(2, tropStormUpperLimit);	}

	@XmlElement
	public boolean getHurricaneEnable() {
		return windSpeedCatEnable[3];
	}
	
	public void setHurricaneEnable(boolean hurricaneEnable) {
		windSpeedCatEnable[3] = hurricaneEnable;
	}

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	public RGB getHurricaneColor() {
		return windSpeedColorBar.getRGB(3);
	}

	public void setHurricaneColor(RGB hurricaneColor) {
		windSpeedColorBar.setRGB(3, hurricaneColor);
	}

	@XmlElement
	public float getHurricaneUpperLimit() {
		return windSpeedColorBar.getIntervalMax(3);
	}	
	
	public void setHurricaneUpperLimit(float hurricaneUpperLimit) {
		windSpeedColorBar.setIntervalMax(3, hurricaneUpperLimit);
	}

	public ColorBar getWindSpeedColorBar( ) {
		return windSpeedColorBar;
	}

	public boolean getDrawBeginDateTime() {
		return drawBeginDateTime;
	}

	public void setDrawBeginDateTime(boolean drawBeginDateTime) {
		this.drawBeginDateTime = drawBeginDateTime;
	}

	public boolean getDrawForecastHour() {
		return drawForecastHour;
	}

	public void setDrawForecastHour(boolean drawForecaseHour) {
		this.drawForecastHour = drawForecaseHour;
	}

	public boolean getDrawPressure() {
		return drawPressure;
	}

	public void setDrawPressure(boolean drawPressure) {
		this.drawPressure = drawPressure;
	}

	public boolean getDrawWindSpeed() {
		return drawWindSpeed;
	}

	public void setDrawWindSpeed(boolean drawWindSpeed) {
		this.drawWindSpeed = drawWindSpeed;
	}

	public boolean getDrawModelName() {
		return drawModelName;
	}

	public void setDrawModelName(boolean drawModelName) {
		this.drawModelName = drawModelName;
	}

	public boolean getDrawCycloneID() {
		return drawCycloneID;
	}

	public void setDrawCycloneID(boolean drawCycloneID) {
		this.drawCycloneID = drawCycloneID;
	}

	public boolean getDrawMarker() {
		return drawMarker;
	}

	public void setDrawMarker(boolean drawMarker) {
		this.drawMarker = drawMarker;
	}


	// 
	public Object getModelDisplayAttribute( int m, String attrName ) {
		if( modelDisplayAttrs[m-1] == null ) {
			modelDisplayAttrs[m-1] = new ModelDisplayAttrs();
	}
		if( attrName.equalsIgnoreCase("modelName") ) {
			return modelDisplayAttrs[m-1].modelName;			
	}
		else if( attrName.equalsIgnoreCase("enabled") ) {
			return modelDisplayAttrs[m-1].enabled;			
	}
		else if( attrName.equalsIgnoreCase("color") ) {
			return modelDisplayAttrs[m-1].color;			
	}
		else if( attrName.equalsIgnoreCase("lineWidth") ) {
			return modelDisplayAttrs[m-1].lineWidth;			
	}
		else if( attrName.equalsIgnoreCase("symbolSize") ) {
			return modelDisplayAttrs[m-1].symbolSize;			
	}
		else {
			System.out.println("Sanity Check: Unrecognized attribut name for StormTrackResource");
			return null;
	}
	}

	// I'm going to cheat a little and key off of the attr class instead of passing in a string
	public void setModelDisplayAttribute( int m, Object attrVal ) {
		if( modelDisplayAttrs[m-1] == null ) {
			modelDisplayAttrs[m-1] = new ModelDisplayAttrs();
	}
		if( attrVal instanceof String ) {
			modelDisplayAttrs[m-1].modelName = (String)attrVal;	
	}
		else if( attrVal instanceof Boolean ) {
			modelDisplayAttrs[m-1].enabled = (Boolean)attrVal;	
	}
		else if( attrVal instanceof RGB ) {
			modelDisplayAttrs[m-1].color = (RGB)attrVal;	
	}
		else if( attrVal instanceof Integer ) {
			modelDisplayAttrs[m-1].lineWidth = (Integer)attrVal;	
	}
		else if( attrVal instanceof Float ) {
			modelDisplayAttrs[m-1].symbolSize = (Float)attrVal;	
	}
	}

	@XmlElement
	public String getModel01Name() {
		return (String) getModelDisplayAttribute(1,"modelName");
	}
	@XmlElement
	public String getModel02Name() {
		return (String) getModelDisplayAttribute(2,"modelName");
	}
	@XmlElement
	public String getModel03Name() {
		return (String) getModelDisplayAttribute(3,"modelName");
	}
	@XmlElement
	public String getModel04Name() {
		return (String) getModelDisplayAttribute(4,"modelName");
	}
	@XmlElement
	public String getModel05Name() {
		return (String) getModelDisplayAttribute(5,"modelName");
	}
	@XmlElement
	public String getModel06Name() {
		return (String) getModelDisplayAttribute(6,"modelName");
	}
	@XmlElement
	public String getModel07Name() {
		return (String) getModelDisplayAttribute(7,"modelName");
	}
	@XmlElement
	public String getModel08Name() {
		return (String) getModelDisplayAttribute(8,"modelName");
	}
	@XmlElement
	public String getModel09Name() {
		return (String) getModelDisplayAttribute(9,"modelName");
	}
	@XmlElement
	public String getModel10Name() {
		return (String) getModelDisplayAttribute(10,"modelName");
	}
	@XmlElement
	public String getModel11Name() {
		return (String) getModelDisplayAttribute(11,"modelName");
	}
	@XmlElement
	public String getModel12Name() {
		return (String) getModelDisplayAttribute(12,"modelName");
	}
	@XmlElement
	public String getModel13Name() {
		return (String) getModelDisplayAttribute(13,"modelName");
	}
	@XmlElement
	public String getModel14Name() {
		return (String) getModelDisplayAttribute(14,"modelName");
	}
	@XmlElement
	public String getModel15Name() {
		return (String) getModelDisplayAttribute(15,"modelName");
	}
	@XmlElement
	public String getModel16Name() {
		return (String) getModelDisplayAttribute(16,"modelName");
	}
	@XmlElement
	public String getModel17Name() {
		return (String) getModelDisplayAttribute(17,"modelName");
	}
	@XmlElement
	public String getModel18Name() {
		return (String) getModelDisplayAttribute(18,"modelName");
	}
	@XmlElement
	public String getModel19Name() {
		return (String) getModelDisplayAttribute(19,"modelName");
	}
	@XmlElement
	public String getModel20Name() {
		return (String) getModelDisplayAttribute(20,"modelName");
	}

	public void setModel01Name( String m ) {
		setModelDisplayAttribute(1,m);
	}
	public void setModel02Name( String m ) {
		setModelDisplayAttribute(2,m);
	}
	public void setModel03Name( String m ) {
		setModelDisplayAttribute(3,m);
	}
	public void setModel04Name( String m ) {
		setModelDisplayAttribute(4,m);
	}
	public void setModel05Name( String m ) {
		setModelDisplayAttribute(5,m);
	}
	public void setModel06Name( String m ) {
		setModelDisplayAttribute(6,m);
	}
	public void setModel07Name( String m ) {
		setModelDisplayAttribute(7,m);
	}
	public void setModel08Name( String m ) {
		setModelDisplayAttribute(8,m);
	}
	public void setModel09Name( String m ) {
		setModelDisplayAttribute(9,m);
	}
	public void setModel10Name( String m ) {
		setModelDisplayAttribute(10,m);
	}
	public void setModel11Name( String m ) {
		setModelDisplayAttribute(11,m);
	}
	public void setModel12Name( String m ) {
		setModelDisplayAttribute(12,m);
	}
	public void setModel13Name( String m ) {
		setModelDisplayAttribute(13,m);
	}
	public void setModel14Name( String m ) {
		setModelDisplayAttribute(14,m);
	}
	public void setModel15Name( String m ) {
		setModelDisplayAttribute(15,m);
	}
	public void setModel16Name( String m ) {
		setModelDisplayAttribute(16,m);
	}
	public void setModel17Name( String m ) {
		setModelDisplayAttribute(17,m);
	}
	public void setModel18Name( String m ) {
		setModelDisplayAttribute(18,m);
	}
	public void setModel19Name( String m ) {
		setModelDisplayAttribute(19,m);
	}
	public void setModel20Name( String m ) {
		setModelDisplayAttribute(20,m);
	}


	@XmlElement
	public Boolean getModel01Enable() {
		return (Boolean) getModelDisplayAttribute(1,"enabled");
	}
	@XmlElement
	public Boolean getModel02Enable() {
		return (Boolean) getModelDisplayAttribute(2,"enabled");
	}
	@XmlElement
	public Boolean getModel03Enable() {
		return (Boolean) getModelDisplayAttribute(3,"enabled");
	}
	@XmlElement
	public Boolean getModel04Enable() {
		return (Boolean) getModelDisplayAttribute(4,"enabled");
	}
	@XmlElement
	public Boolean getModel05Enable() {
		return (Boolean) getModelDisplayAttribute(5,"enabled");
	}
	@XmlElement
	public Boolean getModel06Enable() {
		return (Boolean) getModelDisplayAttribute(6,"enabled");
	}
	@XmlElement
	public Boolean getModel07Enable() {
		return (Boolean) getModelDisplayAttribute(7,"enabled");
	}
	@XmlElement
	public Boolean getModel08Enable() {
		return (Boolean) getModelDisplayAttribute(8,"enabled");
	}
	@XmlElement
	public Boolean getModel09Enable() {
		return (Boolean) getModelDisplayAttribute(9,"enabled");
	}
	@XmlElement
	public Boolean getModel10Enable() {
		return (Boolean) getModelDisplayAttribute(10,"enabled");
	}
	@XmlElement
	public Boolean getModel11Enable() {
		return (Boolean) getModelDisplayAttribute(11,"enabled");
	}
	@XmlElement
	public Boolean getModel12Enable() {
		return (Boolean) getModelDisplayAttribute(12,"enabled");
	}
	@XmlElement
	public Boolean getModel13Enable() {
		return (Boolean) getModelDisplayAttribute(13,"enabled");
	}
	@XmlElement
	public Boolean getModel14Enable() {
		return (Boolean) getModelDisplayAttribute(14,"enabled");
	}
	@XmlElement
	public Boolean getModel15Enable() {
		return (Boolean) getModelDisplayAttribute(15,"enabled");
	}
	@XmlElement
	public Boolean getModel16Enable() {
		return (Boolean) getModelDisplayAttribute(16,"enabled");
	}
	@XmlElement
	public Boolean getModel17Enable() {
		return (Boolean) getModelDisplayAttribute(17,"enabled");
	}
	@XmlElement
	public Boolean getModel18Enable() {
		return (Boolean) getModelDisplayAttribute(18,"enabled");
	}
	@XmlElement
	public Boolean getModel19Enable() {
		return (Boolean) getModelDisplayAttribute(19,"enabled");
	}
	@XmlElement
	public Boolean getModel20Enable() {
		return (Boolean) getModelDisplayAttribute(20,"enabled");
	}

	public void setModel01Enable( Boolean e ) {
		setModelDisplayAttribute(1,e);
	}
	public void setModel02Enable( Boolean e ) {
		setModelDisplayAttribute(2,e);
	}
	public void setModel03Enable( Boolean e ) {
		setModelDisplayAttribute(3,e);
	}
	public void setModel04Enable( Boolean e ) {
		setModelDisplayAttribute(4,e);
	}
	public void setModel05Enable( Boolean e ) {
		setModelDisplayAttribute(5,e);
	}
	public void setModel06Enable( Boolean e ) {
		setModelDisplayAttribute(6,e);
	}
	public void setModel07Enable( Boolean e ) {
		setModelDisplayAttribute(7,e);
	}
	public void setModel08Enable( Boolean e ) {
		setModelDisplayAttribute(8,e);
	}
	public void setModel09Enable( Boolean e ) {
		setModelDisplayAttribute(9,e);
	}
	public void setModel10Enable( Boolean e ) {
		setModelDisplayAttribute(10,e);
	}
	public void setModel11Enable( Boolean e ) {
		setModelDisplayAttribute(11,e);
	}
	public void setModel12Enable( Boolean e ) {
		setModelDisplayAttribute(12,e);
	}
	public void setModel13Enable( Boolean e ) {
		setModelDisplayAttribute(13,e);
	}
	public void setModel14Enable( Boolean e ) {
		setModelDisplayAttribute(14,e);
	}
	public void setModel15Enable( Boolean e ) {
		setModelDisplayAttribute(15,e);
	}
	public void setModel16Enable( Boolean e ) {
		setModelDisplayAttribute(16,e);
	}
	public void setModel17Enable( Boolean e ) {
		setModelDisplayAttribute(17,e);
	}
	public void setModel18Enable( Boolean e ) {
		setModelDisplayAttribute(18,e);
	}
	public void setModel19Enable( Boolean e ) {
		setModelDisplayAttribute(19,e);
	}
	public void setModel20Enable( Boolean e ) {
		setModelDisplayAttribute(20,e);
	}


	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	public RGB getModel01Color() {
		return (RGB) getModelDisplayAttribute(1,"color");
	}
	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	public RGB getModel02Color() {
		return (RGB) getModelDisplayAttribute(2,"color");
	}
	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	public RGB getModel03Color() {
		return (RGB) getModelDisplayAttribute(3,"color");
	}
	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	public RGB getModel04Color() {
		return (RGB) getModelDisplayAttribute(4,"color");
	}
	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	public RGB getModel05Color() {
		return (RGB) getModelDisplayAttribute(5,"color");
	}
	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	public RGB getModel06Color() {
		return (RGB) getModelDisplayAttribute(6,"color");
	}
	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	public RGB getModel07Color() {
		return (RGB) getModelDisplayAttribute(7,"color");
	}
	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	public RGB getModel08Color() {
		return (RGB) getModelDisplayAttribute(8,"color");
	}
	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	public RGB getModel09Color() {
		return (RGB) getModelDisplayAttribute(9,"color");
	}
	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	public RGB getModel10Color() {
		return (RGB) getModelDisplayAttribute(10,"color");
	}
	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	public RGB getModel11Color() {
		return (RGB) getModelDisplayAttribute(11,"color");
	}
	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	public RGB getModel12Color() {
		return (RGB) getModelDisplayAttribute(12,"color");
	}
	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	public RGB getModel13Color() {
		return (RGB) getModelDisplayAttribute(13,"color");
	}
	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	public RGB getModel14Color() {
		return (RGB) getModelDisplayAttribute(14,"color");
	}
	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	public RGB getModel15Color() {
		return (RGB) getModelDisplayAttribute(15,"color");
	}
	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	public RGB getModel16Color() {
		return (RGB) getModelDisplayAttribute(16,"color");
	}
	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	public RGB getModel17Color() {
		return (RGB) getModelDisplayAttribute(17,"color");
	}
	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	public RGB getModel18Color() {
		return (RGB) getModelDisplayAttribute(18,"color");
	}
	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	public RGB getModel19Color() {
		return (RGB) getModelDisplayAttribute(19,"color");
	}
	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	public RGB getModel20Color() {
		return (RGB) getModelDisplayAttribute(20,"color");
	}


	public void setModel01Color( RGB rgb ) {
		setModelDisplayAttribute(1,rgb);
	}
	public void setModel02Color( RGB rgb ) {
		setModelDisplayAttribute(2,rgb);
	}
	public void setModel03Color( RGB rgb ) {
		setModelDisplayAttribute(3,rgb);
	}
	public void setModel04Color( RGB rgb ) {
		setModelDisplayAttribute(4,rgb);
	}
	public void setModel05Color( RGB rgb ) {
		setModelDisplayAttribute(5,rgb);
	}
	public void setModel06Color( RGB rgb ) {
		setModelDisplayAttribute(6,rgb);
	}
	public void setModel07Color( RGB rgb ) {
		setModelDisplayAttribute(7,rgb);
	}
	public void setModel08Color( RGB rgb ) {
		setModelDisplayAttribute(8,rgb);
	}
	public void setModel09Color( RGB rgb ) {
		setModelDisplayAttribute(9,rgb);
	}
	public void setModel10Color( RGB rgb ) {
		setModelDisplayAttribute(10,rgb);
	}
	public void setModel11Color( RGB rgb ) {
		setModelDisplayAttribute(11,rgb);
	}
	public void setModel12Color( RGB rgb ) {
		setModelDisplayAttribute(12,rgb);
	}
	public void setModel13Color( RGB rgb ) {
		setModelDisplayAttribute(13,rgb);
	}
	public void setModel14Color( RGB rgb ) {
		setModelDisplayAttribute(14,rgb);
	}
	public void setModel15Color( RGB rgb ) {
		setModelDisplayAttribute(15,rgb);
	}
	public void setModel16Color( RGB rgb ) {
		setModelDisplayAttribute(16,rgb);
	}
	public void setModel17Color( RGB rgb ) {
		setModelDisplayAttribute(17,rgb);
	}
	public void setModel18Color( RGB rgb ) {
		setModelDisplayAttribute(18,rgb);
	}
	public void setModel19Color( RGB rgb ) {
		setModelDisplayAttribute(19,rgb);
	}
	public void setModel20Color( RGB rgb ) {
		setModelDisplayAttribute(20,rgb);
	}


	@XmlElement
	public Integer getModel01LineWidth() {
		return (Integer) getModelDisplayAttribute(1,"lineWidth");
	}
	@XmlElement
	public Integer getModel02LineWidth() {
		return (Integer) getModelDisplayAttribute(2,"lineWidth");
	}
	@XmlElement
	public Integer getModel03LineWidth() {
		return (Integer) getModelDisplayAttribute(3,"lineWidth");
	}
	@XmlElement
	public Integer getModel04LineWidth() {
		return (Integer) getModelDisplayAttribute(4,"lineWidth");
	}
	@XmlElement
	public Integer getModel05LineWidth() {
		return (Integer) getModelDisplayAttribute(5,"lineWidth");
	}
	@XmlElement
	public Integer getModel06LineWidth() {
		return (Integer) getModelDisplayAttribute(6,"lineWidth");
	}
	@XmlElement
	public Integer getModel07LineWidth() {
		return (Integer) getModelDisplayAttribute(7,"lineWidth");
	}
	@XmlElement
	public Integer getModel08LineWidth() {
		return (Integer) getModelDisplayAttribute(8,"lineWidth");
	}
	@XmlElement
	public Integer getModel09LineWidth() {
		return (Integer) getModelDisplayAttribute(9,"lineWidth");
	}
	@XmlElement
	public Integer getModel10LineWidth() {
		return (Integer) getModelDisplayAttribute(10,"lineWidth");
	}
	@XmlElement
	public Integer getModel11LineWidth() {
		return (Integer) getModelDisplayAttribute(11,"lineWidth");
	}
	@XmlElement
	public Integer getModel12LineWidth() {
		return (Integer) getModelDisplayAttribute(12,"lineWidth");
	}
	@XmlElement
	public Integer getModel13LineWidth() {
		return (Integer) getModelDisplayAttribute(13,"lineWidth");
	}
	@XmlElement
	public Integer getModel14LineWidth() {
		return (Integer) getModelDisplayAttribute(14,"lineWidth");
	}
	@XmlElement
	public Integer getModel15LineWidth() {
		return (Integer) getModelDisplayAttribute(15,"lineWidth");
	}
	@XmlElement
	public Integer getModel16LineWidth() {
		return (Integer) getModelDisplayAttribute(16,"lineWidth");
	}
	@XmlElement
	public Integer getModel17LineWidth() {
		return (Integer) getModelDisplayAttribute(17,"lineWidth");
	}
	@XmlElement
	public Integer getModel18LineWidth() {
		return (Integer) getModelDisplayAttribute(18,"lineWidth");
	}
	@XmlElement
	public Integer getModel19LineWidth() {
		return (Integer) getModelDisplayAttribute(19,"lineWidth");
	}
	@XmlElement
	public Integer getModel20LineWidth() {
		return (Integer) getModelDisplayAttribute(20,"lineWidth");
	}

	public void setModel01LineWidth( Integer lw ) {
		setModelDisplayAttribute(1,lw);
	}
	public void setModel02LineWidth( Integer lw ) {
		setModelDisplayAttribute(2,lw);
	}
	public void setModel03LineWidth( Integer lw ) {
		setModelDisplayAttribute(3,lw);
	}
	public void setModel04LineWidth( Integer lw ) {
		setModelDisplayAttribute(4,lw);
	}
	public void setModel05LineWidth( Integer lw ) {
		setModelDisplayAttribute(5,lw);
	}
	public void setModel06LineWidth( Integer lw ) {
		setModelDisplayAttribute(6,lw);
	}
	public void setModel07LineWidth( Integer lw ) {
		setModelDisplayAttribute(7,lw);
	}
	public void setModel08LineWidth( Integer lw ) {
		setModelDisplayAttribute(8,lw);
	}
	public void setModel09LineWidth( Integer lw ) {
		setModelDisplayAttribute(9,lw);
	}
	public void setModel10LineWidth( Integer lw ) {
		setModelDisplayAttribute(10,lw);
	}
	public void setModel11LineWidth( Integer lw ) {
		setModelDisplayAttribute(11,lw);
	}
	public void setModel12LineWidth( Integer lw ) {
		setModelDisplayAttribute(12,lw);
	}
	public void setModel13LineWidth( Integer lw ) {
		setModelDisplayAttribute(13,lw);
	}
	public void setModel14LineWidth( Integer lw ) {
		setModelDisplayAttribute(14,lw);
	}
	public void setModel15LineWidth( Integer lw ) {
		setModelDisplayAttribute(15,lw);
	}
	public void setModel16LineWidth( Integer lw ) {
		setModelDisplayAttribute(16,lw);
	}
	public void setModel17LineWidth( Integer lw ) {
		setModelDisplayAttribute(17,lw);
	}
	public void setModel18LineWidth( Integer lw ) {
		setModelDisplayAttribute(18,lw);
	}
	public void setModel19LineWidth( Integer lw ) {
		setModelDisplayAttribute(19,lw);
	}
	public void setModel20LineWidth( Integer lw ) {
		setModelDisplayAttribute(20,lw);
	}


	@XmlElement
	public Float getModel01SymbolSize() {
		return (Float) getModelDisplayAttribute(1,"symbolSize");
	}
	@XmlElement
	public Float getModel02SymbolSize() {
		return (Float) getModelDisplayAttribute(2,"symbolSize");
	}
	@XmlElement
	public Float getModel03SymbolSize() {
		return (Float) getModelDisplayAttribute(3,"symbolSize");
	}
	@XmlElement
	public Float getModel04SymbolSize() {
		return (Float) getModelDisplayAttribute(4,"symbolSize");
	}
	@XmlElement
	public Float getModel05SymbolSize() {
		return (Float) getModelDisplayAttribute(5,"symbolSize");
	}
	@XmlElement
	public Float getModel06SymbolSize() {
		return (Float) getModelDisplayAttribute(6,"symbolSize");
	}
	@XmlElement
	public Float getModel07SymbolSize() {
		return (Float) getModelDisplayAttribute(7,"symbolSize");
	}
	@XmlElement
	public Float getModel08SymbolSize() {
		return (Float) getModelDisplayAttribute(8,"symbolSize");
	}
	@XmlElement
	public Float getModel09SymbolSize() {
		return (Float) getModelDisplayAttribute(9,"symbolSize");
	}
	@XmlElement
	public Float getModel10SymbolSize() {
		return (Float) getModelDisplayAttribute(10,"symbolSize");
	}
	@XmlElement
	public Float getModel11SymbolSize() {
		return (Float) getModelDisplayAttribute(11,"symbolSize");
	}
	@XmlElement
	public Float getModel12SymbolSize() {
		return (Float) getModelDisplayAttribute(12,"symbolSize");
	}
	@XmlElement
	public Float getModel13SymbolSize() {
		return (Float) getModelDisplayAttribute(13,"symbolSize");
	}
	@XmlElement
	public Float getModel14SymbolSize() {
		return (Float) getModelDisplayAttribute(14,"symbolSize");
	}
	@XmlElement
	public Float getModel15SymbolSize() {
		return (Float) getModelDisplayAttribute(15,"symbolSize");
	}
	@XmlElement
	public Float getModel16SymbolSize() {
		return (Float) getModelDisplayAttribute(16,"symbolSize");
	}
	@XmlElement
	public Float getModel17SymbolSize() {
		return (Float) getModelDisplayAttribute(17,"symbolSize");
	}
	@XmlElement
	public Float getModel18SymbolSize() {
		return (Float) getModelDisplayAttribute(18,"symbolSize");
	}
	@XmlElement
	public Float getModel19SymbolSize() {
		return (Float) getModelDisplayAttribute(19,"symbolSize");
	}
	@XmlElement
	public Float getModel20SymbolSize() {
		return (Float) getModelDisplayAttribute(20,"symbolSize");
	}

	public void setModel01SymbolSize( Float ss ) {
		setModelDisplayAttribute(1,ss);
	}
	public void setModel02SymbolSize( Float ss ) {
		setModelDisplayAttribute(2,ss);
	}
	public void setModel03SymbolSize( Float ss ) {
		setModelDisplayAttribute(3,ss);
	}
	public void setModel04SymbolSize( Float ss ) {
		setModelDisplayAttribute(4,ss);
	}
	public void setModel05SymbolSize( Float ss ) {
		setModelDisplayAttribute(5,ss);
	}
	public void setModel06SymbolSize( Float ss ) {
		setModelDisplayAttribute(6,ss);
	}
	public void setModel07SymbolSize( Float ss ) {
		setModelDisplayAttribute(7,ss);
	}
	public void setModel08SymbolSize( Float ss ) {
		setModelDisplayAttribute(8,ss);
	}
	public void setModel09SymbolSize( Float ss ) {
		setModelDisplayAttribute(9,ss);
	}
	public void setModel10SymbolSize( Float ss ) {
		setModelDisplayAttribute(10,ss);
	}
	public void setModel11SymbolSize( Float ss ) {
		setModelDisplayAttribute(11,ss);
	}
	public void setModel12SymbolSize( Float ss ) {
		setModelDisplayAttribute(12,ss);
	}
	public void setModel13SymbolSize( Float ss ) {
		setModelDisplayAttribute(13,ss);
	}
	public void setModel14SymbolSize( Float ss ) {
		setModelDisplayAttribute(14,ss);
	}
	public void setModel15SymbolSize( Float ss ) {
		setModelDisplayAttribute(15,ss);
	}
	public void setModel16SymbolSize( Float ss ) {
		setModelDisplayAttribute(16,ss);
	}
	public void setModel17SymbolSize( Float ss ) {
		setModelDisplayAttribute(17,ss);
	}
	public void setModel18SymbolSize( Float ss ) {
		setModelDisplayAttribute(18,ss);
	}
	public void setModel19SymbolSize( Float ss ) {
		setModelDisplayAttribute(19,ss);
	}
	public void setModel20SymbolSize( Float ss ) {
		setModelDisplayAttribute(20,ss);
	}
}
