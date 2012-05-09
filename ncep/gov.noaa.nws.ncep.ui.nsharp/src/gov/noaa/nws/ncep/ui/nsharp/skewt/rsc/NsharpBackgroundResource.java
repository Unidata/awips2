/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.skewt.rsc.NsharpSkewTBackgroundResource
 * 
 * This java class performs the NSHARP NsharpSkewTBackgroundResource functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 03/23/2010	229			Chin Chen	Initial coding
 * 03/09/2011               Chin Chen   Updated for R1G2-9
 * 06/14/2011   11-5        Chin Chen   migration
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp.skewt.rsc;

import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.NsharpWxMath;
import gov.noaa.nws.ncep.ui.nsharp.palette.NsharpPaletteWindow;
import gov.noaa.nws.ncep.ui.nsharp.skewt.NsharpSkewTDescriptor;
import gov.noaa.nws.ncep.ui.nsharp.skewt.NsharpSkewTEditor;
import gov.noaa.nws.ncep.ui.nsharp.skewt.bkgd.NsharpHodoBackground;
import gov.noaa.nws.ncep.ui.nsharp.skewt.bkgd.NsharpIcingBackground;
import gov.noaa.nws.ncep.ui.nsharp.skewt.bkgd.NsharpInsetBackground;
import gov.noaa.nws.ncep.ui.nsharp.skewt.bkgd.NsharpSkewTBackground;
import gov.noaa.nws.ncep.ui.nsharp.skewt.bkgd.NsharpTurbulenceBackground;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.common.sounding.WxMath;
import com.vividsolutions.jts.geom.Coordinate;

public class NsharpBackgroundResource extends
AbstractVizResource<NsharpBkgResourceData, NsharpSkewTDescriptor> {
	private static final UnitConverter celciusToFahrenheit = SI.CELSIUS.getConverterTo(NonSI.FAHRENHEIT);
	private static final UnitConverter celciusToKelvin = SI.CELSIUS.getConverterTo(SI.KELVIN);
	private String sTemperatureC= "";
	private String sTemperatureF = "";
	private String sThetaInK = "";
	private String sWThetaInK = "";
	private String sEThetaInK="";
	private String sMixingRatio = "";
	private String sPressure = "";
	private double dPressure;	
	private String sWindSpeed ="";
	private String sWindDirection="";
	private NsharpSkewTBackground skewTBackground;
	private NsharpIcingBackground icingBackground;
	private NsharpTurbulenceBackground turbBackground;
	private NsharpHodoBackground hodoBackground;
	private NsharpInsetBackground thetaEHeightBackground;
	private NsharpInsetBackground srWindsBackground;
	private NsharpInsetBackground dataTimelineBackground;
	private NsharpInsetBackground stormSlinkyBackground;
	private NsharpInsetBackground srWindVectorBackground;
	private NsharpInsetBackground psblWatchTypeBackground;
	private NsharpInsetBackground thetaEPresureBackground;
	private NsharpInsetBackground dataPanel1Background;
	private NsharpInsetBackground dataPanel2Background;
	private NsharpInsetBackground dataPanel3Background;
	private NsharpInsetBackground dataPanel4Background;
	private NsharpInsetBackground stationIdBackground;	
	private NsharpInsetBackground colorNotationsBackground;
	private NsharpInsetBackground verticalWindBackground;
	private NsharpInsetBackground windMotionBackground;
	private int currentGraphMode= NsharpConstants.GRAPH_SKEWT;
    
    public int getCurrentGraphMode() {
		return currentGraphMode;
	}

	public void setCurrentGraphMode(int currentGraphMode) {
		this.currentGraphMode = currentGraphMode;
		NsharpSkewTEditor editor = NsharpSkewTEditor.getActiveNsharpEditor();
		if (editor != null) {
			editor.refresh();
		}
	}
	

	@Override
	public void setDescriptor(NsharpSkewTDescriptor descriptor) {
		super.setDescriptor(descriptor);
		//System.out.println("NsharpBackgroundResource setDescriptor called");
    }
	/* #10438
	public float getSmSpd() {
		return smSpd;
	}

	public void setSmSpd(float smSpd) {
		this.smSpd = smSpd;
	}

	public float getSmDir() {
		return smDir;
	}

	public void setSmDir(float smDir) {
		this.smDir = smDir;
	}*/

	public String getSWindSpeed() {
		return sWindSpeed;
	}

	public String getSWindDirection() {
		return sWindDirection;
	}

	public String getSEThetaInK() {
		return sEThetaInK;
	}

	public String getSWThetaInK() {
		return sWThetaInK;
	}

	public String getSMixingRatio() {
		return sMixingRatio;
	}

	public double getDPressure() {
		return dPressure;
	}

	public String getSTemperatureC() {
		return sTemperatureC;
	}

	public String getSTemperatureF() {
		return sTemperatureF;
	}

	public String getSThetaInK() {
		return sThetaInK;
	}

	public String getSPressure() {
		return sPressure;
	}
	
	public NsharpInsetBackground getWindMotionBackground() {
		return windMotionBackground;
	}

	public NsharpInsetBackground getVerticalWindBackground() {
		return verticalWindBackground;
	}
	public NsharpInsetBackground getColorNotationsBackground() {
		return colorNotationsBackground;
	}

	public NsharpInsetBackground getDataPanel1Background() {
		return dataPanel1Background;
	}

	public NsharpInsetBackground getDataPanel2Background() {
		return dataPanel2Background;
	}

	public NsharpInsetBackground getDataPanel3Background() {
		return dataPanel3Background;
	}

	public NsharpInsetBackground getDataPanel4Background() {
		return dataPanel4Background;
	}

	public NsharpInsetBackground getThetaEPresureBackground() {
		return thetaEPresureBackground;
	}

	public NsharpInsetBackground getPsblWatchTypeBackground() {
		return psblWatchTypeBackground;
	}

	public NsharpInsetBackground getSrWindVectorBackground() {
		return srWindVectorBackground;
	}

	public NsharpInsetBackground getStormSlinkyBackground() {
		return stormSlinkyBackground;
	}

	public NsharpInsetBackground getDataTimelineBackground() {
		return dataTimelineBackground;
	}

	public NsharpInsetBackground getStationIdBackground() {
		return stationIdBackground;
	}

	public NsharpInsetBackground getSrWindsBackground() {
		return srWindsBackground;
	}

	/**
	 * @return the skewTBackground
	 */
	public NsharpSkewTBackground getSkewTBackground() {
		return skewTBackground;
	}

	/**
	 * @return the hodoBackground
	 */
	public NsharpHodoBackground getHodoBackground() {
		return hodoBackground;
	}



	public NsharpTurbulenceBackground getTurbBackground() {
		return turbBackground;
	}

	public NsharpIcingBackground getIcingBackground() {
		return icingBackground;
	}

	public NsharpInsetBackground getThetaEHeightBackground() {
		return thetaEHeightBackground;
	}

	public NsharpBackgroundResource(NsharpBkgResourceData rscData,
			LoadProperties loadProperties, NsharpSkewTDescriptor desc) {
		super(rscData, loadProperties);
		System.out.println("NsharpBackgroundResource  constructor called");
		skewTBackground = new NsharpSkewTBackground(desc);
		hodoBackground = new NsharpHodoBackground(desc);
		icingBackground = new NsharpIcingBackground(desc);
		turbBackground = new NsharpTurbulenceBackground(desc);
		thetaEHeightBackground = new NsharpInsetBackground(new Rectangle(NsharpConstants.THETAH_REC_X_ORIG,NsharpConstants.THETAH_REC_Y_ORIG,
        		NsharpConstants.THETAH_REC_WIDTH,NsharpConstants.THETAH_REC_HEIGHT));
		srWindsBackground = new NsharpInsetBackground(new Rectangle(NsharpConstants.SRWINDS_REC_X_ORIG,NsharpConstants.SRWINDS_REC_Y_ORIG,
        		NsharpConstants.SRWINDS_REC_WIDTH,NsharpConstants.SRWINDS_REC_HEIGHT));
		dataTimelineBackground = new NsharpInsetBackground(new Rectangle(NsharpConstants.DATA_TIMELINE_REC_X_ORIG,NsharpConstants.DATA_TIMELINE_REC_Y_ORIG,
        		NsharpConstants.DATA_TIMELINE_REC_WIDTH,NsharpConstants.DATA_TIMELINE_REC_HEIGHT));
		stationIdBackground = new NsharpInsetBackground(new Rectangle(NsharpConstants.STATION_ID_REC_X_ORIG,NsharpConstants.STATION_ID_REC_Y_ORIG,
        		NsharpConstants.STATION_ID_REC_WIDTH,NsharpConstants.STATION_ID_REC_HEIGHT));
		stormSlinkyBackground = new NsharpInsetBackground(new Rectangle(NsharpConstants.STORMSLINKY_REC_X_ORIG,NsharpConstants.STORMSLINKY_REC_Y_ORIG,
        		NsharpConstants.STORMSLINKY_REC_WIDTH,NsharpConstants.STORMSLINKY_REC_HEIGHT)); 
		srWindVectorBackground = new NsharpInsetBackground(new Rectangle(NsharpConstants.SRWINDVTRS_REC_X_ORIG,NsharpConstants.SRWINDVTRS_REC_Y_ORIG,
				NsharpConstants.SRWINDVTRS_REC_WIDTH,NsharpConstants.SRWINDVTRS_REC_HEIGHT));
		psblWatchTypeBackground = new NsharpInsetBackground(new Rectangle(NsharpConstants.PSBLWATCH_REC_X_ORIG,NsharpConstants.PSBLWATCH_REC_Y_ORIG,
				NsharpConstants.PSBLWATCH_REC_WIDTH,NsharpConstants.PSBLWATCH_REC_HEIGHT));//NsharpPsblWatchTypeBackground();
		thetaEPresureBackground = new NsharpInsetBackground(new Rectangle(NsharpConstants.THETAP_REC_X_ORIG,NsharpConstants.THETAP_REC_Y_ORIG,
				NsharpConstants.THETAP_REC_WIDTH,NsharpConstants.THETAP_REC_HEIGHT));
		dataPanel1Background = new NsharpInsetBackground(new Rectangle(NsharpConstants.DATAPANEL1_REC_X_ORIG,NsharpConstants.DATAPANEL1_REC_Y_ORIG,
				NsharpConstants.DATAPANEL1_REC_WIDTH,NsharpConstants.DATAPANEL1_REC_HEIGHT));
		dataPanel2Background = new NsharpInsetBackground(new Rectangle(NsharpConstants.DATAPANEL2_REC_X_ORIG,NsharpConstants.DATAPANEL2_REC_Y_ORIG,
				NsharpConstants.DATAPANEL2_REC_WIDTH,NsharpConstants.DATAPANEL2_REC_HEIGHT));
		//dataPanel3Background = new NsharpInsetBackground(new Rectangle(NsharpConstants.DATAPANEL3_REC_X_ORIG,NsharpConstants.DATAPANEL3_REC_Y_ORIG,
		//		NsharpConstants.DATAPANEL3_REC_WIDTH,NsharpConstants.DATAPANEL3_REC_HEIGHT));
		dataPanel4Background = new NsharpInsetBackground(new Rectangle(NsharpConstants.DATAPANEL4_REC_X_ORIG,NsharpConstants.DATAPANEL4_REC_Y_ORIG,
				NsharpConstants.DATAPANEL4_REC_WIDTH,NsharpConstants.DATAPANEL4_REC_HEIGHT));
		colorNotationsBackground = new NsharpInsetBackground(new Rectangle(NsharpConstants.COLOR_NOTATION_REC_X_ORIG,NsharpConstants.COLOR_NOTATION_REC_Y_ORIG,
				NsharpConstants.COLOR_NOTATION_REC_WIDTH,NsharpConstants.COLOR_NOTATION_REC_HEIGHT));
		verticalWindBackground = new NsharpInsetBackground(new Rectangle(NsharpConstants.VERTICAL_WIND_X_ORIG,NsharpConstants.VERTICAL_WIND_Y_ORIG,
				NsharpConstants.VERTICAL_WIND_WIDTH,NsharpConstants.VERTICAL_WIND_HEIGHT));
		
		windMotionBackground = new NsharpInsetBackground(new Rectangle(NsharpConstants.WIND_MOTION_REC_X_ORIG,NsharpConstants.WIND_MOTION_REC_Y_ORIG,
				NsharpConstants.WIND_MOTION_REC_WIDTH,NsharpConstants.WIND_MOTION_REC_HEIGHT));
		currentGraphMode = NsharpPaletteWindow.getCurrentGraphMode();
	}
	// NOTE:::Chin. It should only be called on new displays. Descriptor is passed to here as it is needed for
	// creating SkewT and Hodo background in NsharpBackgroundResource constructor(). 
	//NsharpBackgroundResource's own setDescriptor() will be called to set descriptor, but it is a bit too late when
	// within "constructor()".
    public static NsharpBackgroundResource createSkewTBkGResource(NsharpSkewTDescriptor  desc) {
    	//System.out.println("NsharpBackgroundResource createSkewTBkGResource called");
        
        LoadProperties loadProperties = new LoadProperties();
        ColorableCapability colorable1 = new ColorableCapability();
        colorable1.setColor(NsharpConstants.backgroundColor);
        loadProperties.getCapabilities().addCapability(colorable1);
        return new NsharpBackgroundResource(new NsharpBkgResourceData(desc),
                loadProperties, desc);
    }

	@Override
	protected void disposeInternal() {
		//System.out.println("NsharpBackgroundResource disposeInternal called");
		skewTBackground.disposeInternal();
		hodoBackground.disposeInternal();
		icingBackground.disposeInternal();
		turbBackground.disposeInternal();
		hodoBackground=null;
		skewTBackground=null;
		//Chin Note: currently, all other backgrounds (all are NsharpInsetBackground) do not have to call disposeInternal()
		psblWatchTypeBackground=null;
		thetaEHeightBackground=null;
		srWindsBackground=null;
		dataTimelineBackground=null;
		stormSlinkyBackground=null;
		srWindVectorBackground=null;
		thetaEPresureBackground=null;
		dataPanel1Background=null;
		dataPanel2Background=null;
		dataPanel3Background=null;
		dataPanel4Background=null;
		stationIdBackground=null;	
		colorNotationsBackground=null;
		verticalWindBackground=null;
		windMotionBackground=null;
	}

	@Override
	protected void initInternal(IGraphicsTarget target) throws VizException {
		//System.out.println("NsharpBackgroundResource initInternal called");
		skewTBackground.initInternal(target);
		hodoBackground.initInternal(target);
		icingBackground.initInternal(target);
		turbBackground.initInternal(target);
		//Chin Note: currently, all other backgrounds (all are NsharpInsetBackground) do not have to call initInternal()
	}

	@Override
	protected void paintInternal(IGraphicsTarget target,
			PaintProperties paintProps) throws VizException {
		if(currentGraphMode== NsharpConstants.GRAPH_SKEWT)
			skewTBackground.paint(target, paintProps);
		else if(currentGraphMode == NsharpConstants.GRAPH_ICING)
			icingBackground.paint(target, paintProps);
		else if(currentGraphMode == NsharpConstants.GRAPH_TURB)
			turbBackground.paint(target, paintProps);
		else
			//default
			skewTBackground.paint(target, paintProps);
		hodoBackground.paint(target, paintProps);
		thetaEHeightBackground.paint(target, paintProps);
		srWindsBackground.paint(target, paintProps);
		dataTimelineBackground.paint(target, paintProps);
		stationIdBackground.paint(target, paintProps);
		stormSlinkyBackground.paint(target, paintProps);
		srWindVectorBackground.paint(target, paintProps);
		psblWatchTypeBackground.paint(target, paintProps);
		thetaEPresureBackground.paint(target, paintProps);
		dataPanel1Background.paint(target, paintProps);
		dataPanel2Background.paint(target, paintProps);
		//dataPanel3Background.paint(target, paintProps);
		dataPanel4Background.paint(target, paintProps);
		colorNotationsBackground.paint(target, paintProps);
		verticalWindBackground.paint(target, paintProps);
		//TBD windMotionBackground.paint(target, paintProps);
	}

	/*
	 * 
	 * (non-Javadoc)
	 * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#inspect(com.raytheon.uf.common.geospatial.ReferencedCoordinate)
	*  Chin:::this may not be used any more....
	 */
	
	@Override
	public String inspect(ReferencedCoordinate rCoord) throws VizException {
		//System.out.print("NsharpSkewTBKResource ::: inspect entered!!!!!\n");
		Coordinate c;
		try {
			c = rCoord.asPixel(this.getDescriptor().getGridGeometry());
			//DECP NsharpPaletteWindow win = NsharpPaletteWindow.getAccess();
			//if(win == null)
			//	return "";

			if (skewTBackground.contains(c)) {
				c = NsharpWxMath.reverseSkewTXY(skewTBackground.getWorld()
						.unMap(c.x, c.y));
				double p_mb = c.y;
				double t_C = c.x; // Celsius
				double t_F = celciusToFahrenheit.convert(c.x);
				double theta = celciusToKelvin.convert(WxMath.theta(p_mb, t_C,
						1000));
				double wtheta = celciusToKelvin.convert(WxMath.thetaw(p_mb,
						t_C, t_C));
				double etheta = celciusToKelvin.convert(WxMath.thetae(p_mb,
						t_C, t_C));
				double mixRatio = WxMath.mixingRatio(p_mb, t_C);
				dPressure = p_mb;

				sPressure = String.format(
						"%.0f mb",p_mb, NsharpConstants.THETA_SYMBOL);
				sTemperatureC = String.format("%4.1f%cC",
						t_C, NsharpConstants.DEGREE_SYMBOL);
				sTemperatureF = String.format("%4.1f%cF",
						t_F,NsharpConstants.DEGREE_SYMBOL);

				sThetaInK = String.format("%c=%.0fK",
						NsharpConstants.THETA_SYMBOL,theta);
				sWThetaInK = String.format("%cw=%.0fK",
						NsharpConstants.THETA_SYMBOL,wtheta);
				sEThetaInK = String.format("%ce=%.0fK",
						NsharpConstants.THETA_SYMBOL,etheta);
				sMixingRatio = String.format("%.0fg/Kg",mixRatio);
			}
			if (hodoBackground.contains(c)) {

				c = hodoBackground.getWorld().unMap(c.x, c.y);
				c = WxMath.speedDir((float) c.x, (float) c.y);


				sWindDirection = String.format("%.0f%c", c.y, NsharpConstants.DEGREE_SYMBOL);
				sWindSpeed = String.format("%.0f Knots (%.0f m/s)",c.x, c.x * NsharpConstants.KnotsToMetersPerSecond /*metersPerSecondToKnots.convert(c.x)*/);

				// 10438 smSpd = (float) c.x;
				//smDir = (float) c.y;
			}
			

		} catch (Exception e) {
			UFStatus.getHandler().handle(Priority.PROBLEM, "Exception translating coordinate", e);
		}
		return "";
	}
	public String updateDynamicData(Coordinate c) throws VizException {
		
		try {
			//System.out.println(" updateDynamicData entered!!!!!C.x="+c.x + " c.y="+c.y);

			if (skewTBackground.contains(c)) {
				c = NsharpWxMath.reverseSkewTXY(skewTBackground.getWorld()
						.unMap(c.x, c.y));
				double p_mb = c.y;
				double t_C = c.x; // Celsius
				double t_F = celciusToFahrenheit.convert(c.x);
				double theta = celciusToKelvin.convert(WxMath.theta(p_mb, t_C,
						1000));
				double wtheta = celciusToKelvin.convert(WxMath.thetaw(p_mb,
						t_C, t_C));
				double etheta = celciusToKelvin.convert(WxMath.thetae(p_mb,
						t_C, t_C));
				double mixRatio = WxMath.mixingRatio(p_mb, t_C);
				dPressure = p_mb;

				sPressure = String.format(
						"%.0f mb",p_mb, NsharpConstants.THETA_SYMBOL);
				sTemperatureC = String.format("%.0f%cC",
						t_C, NsharpConstants.DEGREE_SYMBOL);
				sTemperatureF = String.format("%.0f%cF",
						t_F,NsharpConstants.DEGREE_SYMBOL);

				sThetaInK = String.format("%c=%.0fK",
						NsharpConstants.THETA_SYMBOL,theta);
				sWThetaInK = String.format("%cw=%.0fK",
						NsharpConstants.THETA_SYMBOL,wtheta);
				sEThetaInK = String.format("%ce=%.0fK",
						NsharpConstants.THETA_SYMBOL,etheta);
				sMixingRatio = String.format("%.0fg/Kg",mixRatio);
			}
			if (hodoBackground.contains(c)) {

				c = hodoBackground.getWorld().unMap(c.x, c.y);
				c = WxMath.speedDir((float) c.x, (float) c.y);


				sWindDirection = String.format("%.0f%c", c.y, NsharpConstants.DEGREE_SYMBOL);
				sWindSpeed = String.format("%.0f Knots (%.0f m/s)",c.x, c.x * NsharpConstants.KnotsToMetersPerSecond /*metersPerSecondToKnots.convert(c.x)*/);

				//#10438 smSpd = (float) c.x;
				//smDir = (float) c.y;
			}
			

		} catch (Exception e) {
			UFStatus.getHandler().handle(Priority.PROBLEM, "Exception translating coordinate", e);
		}
		return "";
	}
}
