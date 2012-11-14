package gov.noaa.nws.ncep.ui.nsharp.display.rsc;
/**
 * 
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 04/23/2012	229			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.NsharpGraphProperty;
import gov.noaa.nws.ncep.ui.nsharp.NsharpShapeAndLineProperty;
import gov.noaa.nws.ncep.ui.nsharp.NsharpSoundingElementStateProperty;
import gov.noaa.nws.ncep.ui.nsharp.NsharpStationStateProperty;
import gov.noaa.nws.ncep.ui.nsharp.NsharpTimeLineStateProperty;
import gov.noaa.nws.ncep.ui.nsharp.background.NsharpHodoPaneBackground;
import gov.noaa.nws.ncep.ui.nsharp.display.NsharpHodoPaneDescriptor;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.common.sounding.WxMath;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.PointStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.core.graphing.WGraphics;
import com.sun.jna.ptr.FloatByReference;
import com.vividsolutions.jts.geom.Coordinate;

public class NsharpHodoPaneResource extends NsharpAbstractPaneResource{
	private static final int HODO_NORMAL	=		0;
	//private static int HODO_EFFECTIVE=		1; not used in BigNsharp source code
	private static final int HODO_STORMRELATIVE=	2;
	private static final int HODO_BNDRY=			3;
	private static final int HODO_MEANWIND=		4;
	private int currentHodoWindMode = HODO_MEANWIND;
	private NsharpHodoPaneBackground hodoBackground=null;
	private String sWindSpeed ="";
	private String sWindDirection="";
	private List<NsharpShapeAndLineProperty>hodoWindRscShapeList  = new ArrayList<NsharpShapeAndLineProperty>();
	private IWireframeShape hodoWindMotionBoxShape = null;
	private boolean cursorInHodo=false;
	private Integer markerWidth = 1;
	private List<NsharpTimeLineStateProperty> timeLineStateList;
    private List<NsharpStationStateProperty> stnStateList;
	private IFont fixedFont;
	private int hodoWidth = NsharpConstants.HODO_WIDTH;
	private int hodoHeight = NsharpConstants.HODO_HEIGHT;
	//private int hodoXOrig = NsharpConstants.HODO_X_ORIG;
	//private int hodoYOrig = NsharpConstants.HODO_Y_ORIG;
	//private int hodoXEnd = NsharpConstants.HODO_X_END;
	//private int hodoYEnd = NsharpConstants.HODO_Y_ORIG+  NsharpConstants.HODO_HEIGHT;
	//private Coordinate hodoHouseC = new Coordinate(NsharpConstants.HODO_CENTER_X_, NsharpConstants.HODO_CENTER_Y_);
	private float xRatio=1;
	private float yRatio=1;
	public NsharpHodoPaneResource(AbstractResourceData resourceData,
			LoadProperties loadProperties, NsharpHodoPaneDescriptor desc) {
		super(resourceData, loadProperties, desc);
		
		hodoBackground = new NsharpHodoPaneBackground((NsharpHodoPaneDescriptor)descriptor); 
	}

	private void createRscHodoWindShape(WGraphics world, List<NcSoundingLayer> soundingLays, RGB incolor){

		Coordinate c0 = null;
		Coordinate c1;
		NsharpShapeAndLineProperty shNcolor;
		IWireframeShape shapeR=null, shapeG=null, shapeY=null, shapeC=null, shapeV=null, shapeIn=null;
		if(incolor == null){
			//creating regular Hodo shape with 5 colors
			shNcolor = new NsharpShapeAndLineProperty();
			shapeR = target.createWireframeShape(false,descriptor );
			shNcolor.setShape(shapeR);
			shapeR.allocate(soundingLays.size()*2);
			shNcolor.getLp().setLineColor(NsharpConstants.color_red);
			hodoWindRscShapeList.add(shNcolor);
			shNcolor = new NsharpShapeAndLineProperty();
			shapeG= target.createWireframeShape(false,descriptor );
			shNcolor.setShape(shapeG);
			shapeG.allocate(soundingLays.size()*2);
			shNcolor.getLp().setLineColor(NsharpConstants.color_green);
			hodoWindRscShapeList.add(shNcolor);
			shNcolor = new NsharpShapeAndLineProperty();
			shapeY=  target.createWireframeShape(false,descriptor );
			shNcolor.setShape(shapeY);
			shapeY.allocate(soundingLays.size()*2);
			shNcolor.getLp().setLineColor(NsharpConstants.color_yellow);
			hodoWindRscShapeList.add(shNcolor);
			shNcolor = new NsharpShapeAndLineProperty();
			shapeC=  target.createWireframeShape(false,descriptor );
			shNcolor.setShape(shapeC);
			shapeC.allocate(soundingLays.size()*2);
			shNcolor.getLp().setLineColor(NsharpConstants.color_cyan);
			hodoWindRscShapeList.add(shNcolor);
			shNcolor = new NsharpShapeAndLineProperty();
			shapeV =  target.createWireframeShape(false,descriptor );
			shNcolor.setShape(shapeV);
			shapeV.allocate(soundingLays.size()*2);
			shNcolor.getLp().setLineColor(NsharpConstants.color_violet);
			hodoWindRscShapeList.add(shNcolor);
		}
		else{
			shNcolor = new NsharpShapeAndLineProperty();
			shapeIn = target.createWireframeShape(false,descriptor );
			shNcolor.setShape(shapeIn);
			shapeIn.allocate(soundingLays.size()*2);
			shNcolor.getLp().setLineColor(incolor);
			hodoWindRscShapeList.add(shNcolor);
		}


		for (NcSoundingLayer layer : soundingLays){
			if(layer.getPressure() < 100 || layer.getWindSpeed() <0)
				continue;
			float wspd = layer.getWindSpeed();
			float wdir = layer.getWindDirection();
			c1 = WxMath.uvComp(wspd, wdir);
			if (c0 != null) {
				double [][] lines = {{world.mapX(c0.x), world.mapY(c0.y)},{world
					.mapX(c1.x), world.mapY(c1.y)}};
				if(incolor == null){
					if(layer.getGeoHeight() <nsharpNative.nsharpLib.msl(3000)){
						//red
						shapeR.addLineSegment(lines);
					}
					else if(layer.getGeoHeight() < nsharpNative.nsharpLib.msl(6000))
						shapeG.addLineSegment(lines);
					else if(layer.getGeoHeight() < nsharpNative.nsharpLib.msl(9000))
						shapeY.addLineSegment(lines);
					else if(layer.getGeoHeight() < nsharpNative.nsharpLib.msl(12000))
						shapeC.addLineSegment(lines);
					else
						shapeV.addLineSegment(lines);            	
				}
				else
					shapeIn.addLineSegment(lines);
			}

			c0 = c1;
		}
		if(incolor == null){
			shapeR.compile();
			shapeG.compile();
			shapeY.compile();
			shapeV.compile();
			shapeC.compile();
		}
		else
			shapeIn.compile();

	}
	public void createRscHodoWindShapeAll(){
		if(target == null || rscHandler ==null || soundingLys==null || hodoWindRscShapeList == null)
			return;
		if(hodoWindRscShapeList.size()>0){
			for(NsharpShapeAndLineProperty shapeColor: hodoWindRscShapeList){
				shapeColor.getShape().dispose();
			}
			hodoWindRscShapeList.clear();
		}
		world = hodoBackground.computeWorld();
		boolean compareStnIsOn = rscHandler.isCompareStnIsOn();
		int currentTimeLineStateListIndex = rscHandler.getCurrentTimeLineStateListIndex();
		boolean compareTmIsOn = rscHandler.isCompareTmIsOn();
		boolean overlayIsOn = rscHandler.isOverlayIsOn();
		int currentStnStateListIndex = rscHandler.getCurrentStnStateListIndex();
		stnStateList = rscHandler.getStnStateList();
		timeLineStateList = rscHandler.getTimeLineStateList();
		List<List<NsharpSoundingElementStateProperty>> stnTimeTable = rscHandler.getStnTimeTable();
		HashMap<String, List<NcSoundingLayer>> dataTimelineSndLysListMap = rscHandler.getDataTimelineSndLysListMap();
		if(compareStnIsOn && currentTimeLineStateListIndex >=0){
			int colorIndex =NsharpConstants.LINE_COMP1;
			for(NsharpStationStateProperty elm: stnStateList) {
				if(elm.stnState == NsharpConstants.State.ACTIVE && 
						stnTimeTable.get(stnStateList.indexOf(elm)).get(currentTimeLineStateListIndex).elementState == NsharpConstants.State.AVAIL){
					List<NcSoundingLayer> soundingLayeys = dataTimelineSndLysListMap.get(stnTimeTable.get(stnStateList.indexOf(elm)).get(currentTimeLineStateListIndex).elementDescription);
					RGB color = linePropertyMap.get(NsharpConstants.lineNameArray[colorIndex]).getLineColor();
					colorIndex++; 
					if(colorIndex > NsharpConstants.LINE_COMP10)
						colorIndex =NsharpConstants.LINE_COMP1;
					createRscHodoWindShape(world, soundingLayeys, color);
				}
			}
		}
		else if(compareTmIsOn && currentStnStateListIndex >=0 ){
			int colorIndex =NsharpConstants.LINE_COMP1;
			for(NsharpTimeLineStateProperty elm: timeLineStateList) {
				if(elm.timeState == NsharpConstants.State.ACTIVE && 
						stnTimeTable.get(currentStnStateListIndex).get(timeLineStateList.indexOf(elm)).elementState == NsharpConstants.State.AVAIL){
					List<NcSoundingLayer> soundingLayeys = dataTimelineSndLysListMap.get(stnTimeTable.get(currentStnStateListIndex).get(timeLineStateList.indexOf(elm)).elementDescription);
					RGB color = linePropertyMap.get(NsharpConstants.lineNameArray[colorIndex]).getLineColor();
					colorIndex++;
					if(colorIndex > NsharpConstants.LINE_COMP10)
						colorIndex =NsharpConstants.LINE_COMP1;
					createRscHodoWindShape(world, soundingLayeys, color);
				}
			}
		}
		else if(overlayIsOn == true ){
			previousSoundingLys = rscHandler.getPreviousSoundingLys();
			createRscHodoWindShape(world, this.soundingLys, linePropertyMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_OVERLAY1]).getLineColor());
			if(previousSoundingLys!=null && !soundingLys.equals( previousSoundingLys))
				createRscHodoWindShape(world, this.previousSoundingLys, linePropertyMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_OVERLAY2]).getLineColor());
		}
		else {
			createRscHodoWindShape(world, this.soundingLys, null);
		}
		
		//for future createHodoWindMotionBoxShape();

	}
	@SuppressWarnings("unused")
	private void createHodoWindMotionBoxShape(/*WGraphics world*/){
		hodoWindMotionBoxShape = target.createWireframeShape(false,descriptor );
		hodoWindMotionBoxShape.allocate(12);
		double xOri = NsharpConstants.WIND_MOTION_REC_X_ORIG;
    	double yOri = NsharpConstants.WIND_MOTION_REC_Y_ORIG;
    	double xEnd= NsharpConstants.WIND_MOTION_REC_X_ORIG + NsharpConstants.WIND_MOTION_REC_WIDTH;
    	double yEnd = NsharpConstants.WIND_MOTION_REC_Y_ORIG + NsharpConstants.WIND_MOTION_REC_HEIGHT;
    	double ygap = NsharpConstants.WIND_MOTION_REC_HEIGHT/3;
    	double [][] lines1 = {{xOri, yOri},{xEnd, yOri}};
    	hodoWindMotionBoxShape.addLineSegment(lines1);
    	double [][] lines2 = {{xOri, yOri+ygap},{xEnd, yOri+ygap}};
    	hodoWindMotionBoxShape.addLineSegment(lines2);
    	double [][] lines3 = {{xOri, yOri+2*ygap},{xEnd, yOri+2*ygap}};
    	hodoWindMotionBoxShape.addLineSegment(lines3);
    	double [][] lines4 = {{xOri, yEnd},{xEnd, yEnd}};
    	hodoWindMotionBoxShape.addLineSegment(lines4);
    	double [][] lines5 = {{xOri, yOri},{xOri, yEnd}};
    	hodoWindMotionBoxShape.addLineSegment(lines5);
    	double [][] lines6 = {{xEnd, yOri},{xEnd, yEnd}};
    	hodoWindMotionBoxShape.addLineSegment(lines6);
	}
	private void plotHodoEditPoints(IGraphicsTarget target, 
			 RGB color) throws VizException {

		Coordinate c1;
		for (NcSoundingLayer layer : this.soundingLys){
			if(layer.getPressure() < 100 || layer.getWindSpeed() <0)
				continue;
			float wspd = layer.getWindSpeed();
			float wdir = layer.getWindDirection();
			c1 = WxMath.uvComp(wspd, wdir);
			target.drawPoint(world
					.mapX(c1.x), world.mapY(c1.y), 0.0, color, PointStyle.CIRCLE);


		}
	}
	/*
	 * smvtype: 1: small circle, 2 large circle, 3: square
	 */
	@SuppressWarnings("deprecation")
	public void plotNsharpHodoVectors(IGraphicsTarget target, double zoomLevel,
			 GC gc,  boolean printEvent) throws VizException {
		double radiusUnit = 5* xRatio;
		//System.out.println("radiusUnit="+radiusUnit+" xRatio="+xRatio+ " zoomLevel="+zoomLevel);
		//NsharpParametersSelectionConfigDialog configD = NsharpParametersSelectionConfigDialog.getAccess();
		Coordinate c;
		FloatByReference value1= new FloatByReference(-999);
		FloatByReference value2= new FloatByReference(-999);
		FloatByReference wdir= new FloatByReference(-999);
		FloatByReference wspd= new FloatByReference(-999);
		FloatByReference Surfpressure = new FloatByReference(-999);
		String textStr;

		//plot  Mean Wind Vector, yellow square, by default plot it
		if(((graphConfigProperty != null ) && (graphConfigProperty.isMeanWind()))||(graphConfigProperty == null)){
			nsharpNative.nsharpLib.mean_wind( -1, -1, value1, value2, wdir, wspd);
			if( nsharpNative.nsharpLib.qc(wdir.getValue())==1 && nsharpNative.nsharpLib.qc(wspd.getValue())==1) {
				c = WxMath.uvComp(wspd.getValue(),wdir.getValue());
				c= world.map(c);
				if(printEvent== true){
					gc.setLineWidth(1);
					gc.drawRectangle((int) (c.x-radiusUnit), (int)( c.y-radiusUnit), (int)(2*radiusUnit), (int)(2*radiusUnit));
				}
				else {
					PixelExtent pe = new PixelExtent( c.x-radiusUnit, c.x+radiusUnit, c.y-radiusUnit, c.y+radiusUnit );
					target.drawRect(pe, NsharpConstants.color_yellow, markerWidth*2, 1.0f);
					textStr= String.format("%.0f/%.0f MW",wdir.getValue(),wspd.getValue());
					target.drawString(font10, textStr, c.x-radiusUnit, c.y+radiusUnit+5, 0.0,
							TextStyle.NORMAL, NsharpConstants.color_yellow, HorizontalAlignment.LEFT,
							VerticalAlignment.TOP, null);
				}
			}
		}
		//plot 15/85 and/or 30/75 SMV, by default dont plot it
		if((graphConfigProperty != null ) && (graphConfigProperty.isSmv1585() || graphConfigProperty.isSmv3075())){
			nsharpNative.nsharpLib.get_surface(Surfpressure, value1, value2);
			if(nsharpNative.nsharpLib.qc(Surfpressure.getValue()) == 1) {
				nsharpNative.nsharpLib.mean_wind(Surfpressure.getValue(), nsharpNative.nsharpLib.ipres (nsharpNative.nsharpLib.msl (6000.0F)),value1, value2, wdir, wspd);
				if( nsharpNative.nsharpLib.qc(wdir.getValue())==1 && nsharpNative.nsharpLib.qc(wspd.getValue())==1) {
					// ----- Plot 30/75 Storm Motion Vector -----small red circle 
					if(graphConfigProperty.isSmv3075()){
						//System.out.println("Plot 30/75 Storm Motion Vector 2");
						float dir = (wdir.getValue() + 30.0f)%360;
						float spd = wspd.getValue() * 0.75f;
						//System.out.println(spd + " "+ wspd.getValue());
						c = WxMath.uvComp(spd,dir);
						c= world.map(c);
						if(printEvent== true){			
							gc.drawOval((int) (c.x-radiusUnit), (int)( c.y-radiusUnit), (int)(2*radiusUnit), (int)(2*radiusUnit));
							gc.drawLine((int) (c.x - radiusUnit/2), (int)c.y,(int) ( c.x + radiusUnit/2),(int) c.y);
							gc.drawLine((int) (c.x), (int)(c.y - radiusUnit/2),(int) ( c.x ),(int) (c.y + radiusUnit/2));
						}
						else {
							RGB color = NsharpConstants.color_red;
							target.drawCircle(c.x, c.y, 0, radiusUnit,color, markerWidth*2);
							target.drawLine(c.x - radiusUnit/2, c.y, 0.0,c.x + radiusUnit/2, c.y, 0.0, color,
									markerWidth);
							target.drawLine(c.x, c.y- radiusUnit/2, 0.0,c.x, c.y + radiusUnit/2, 0.0, color,
									markerWidth);	
						}
					}
					//----- Plot 15/85 Storm Motion Vector ----- small green color circle
					if(graphConfigProperty.isSmv1585()){
						float dir = (wdir.getValue() + 15.0f)%360;
						float spd = wspd.getValue() * 0.85f;
						//System.out.println(spd + " "+ wspd.getValue());
						c = WxMath.uvComp(spd,dir);
						c= world.map(c);
						if(printEvent== true){
							gc.drawOval((int) (c.x-radiusUnit), (int)( c.y-radiusUnit), (int)(2*radiusUnit), (int)(2*radiusUnit));
							gc.drawLine((int) (c.x - radiusUnit/2), (int)c.y,(int) ( c.x + radiusUnit/2),(int) c.y);
							gc.drawLine((int) (c.x), (int)(c.y - radiusUnit/2),(int) ( c.x ),(int) (c.y + radiusUnit/2));					
						}
						else {
							RGB color = NsharpConstants.color_green;
							target.drawCircle(c.x, c.y, 0, radiusUnit,color, markerWidth*2);
							target.drawLine(c.x - radiusUnit/2, c.y, 0.0,c.x + radiusUnit/2, c.y, 0.0, color,
									markerWidth);
							target.drawLine(c.x, c.y- radiusUnit/2, 0.0,c.x, c.y + radiusUnit/2, 0.0, color,
									markerWidth);
						}
					}
				}
			}
		}
		//plot Corfidi Vectors, color_stellblue small circles, by default Not plot it
		if((graphConfigProperty != null ) && graphConfigProperty.isCorfidiV()){
			//Upwind-Propagating MCS motion vector
			FloatByReference upwdir= new FloatByReference(-999);
			FloatByReference upwspd= new FloatByReference(-999);
			//Downwind-Propagating MCS motion vector
			FloatByReference dpwdir= new FloatByReference(-999);
			FloatByReference dpwspd= new FloatByReference(-999);
			FloatByReference value5= new FloatByReference(-999);
			FloatByReference value6= new FloatByReference(-999);

			nsharpNative.nsharpLib.corfidi_MCS_motion(value1, value2, dpwdir, dpwspd, value5, value6, upwdir, upwspd);
			c = WxMath.uvComp(dpwspd.getValue(),dpwdir.getValue());
			c= world.map(c);
			RGB color = NsharpConstants.color_lightblue;
			target.drawCircle(c.x, c.y, 0, radiusUnit/2,color, markerWidth);
			//target.drawLine(c.x - radiusUnit/2, c.y, 0.0,c.x + radiusUnit/2, c.y, 0.0, color,
			//		markerWidth);
			//target.drawLine(c.x, c.y- radiusUnit/2, 0.0,c.x, c.y + radiusUnit/2, 0.0, color,
			//		markerWidth);
			textStr = String.format("DP= %.0f/%.0f",dpwdir.getValue(), dpwspd.getValue());
			target.drawString(font10, textStr, c.x, c.y, 0.0,
					TextStyle.NORMAL, color, HorizontalAlignment.LEFT,
					VerticalAlignment.TOP, null);

			c = WxMath.uvComp(upwspd.getValue(),upwdir.getValue());
			c= world.map(c);			
			target.drawCircle(c.x, c.y, 0, radiusUnit/2,color, markerWidth);
			//target.drawLine(c.x - radiusUnit/2, c.y, 0.0,c.x + radiusUnit/2, c.y, 0.0, color,
			//		markerWidth);
			//target.drawLine(c.x, c.y- radiusUnit/2, 0.0,c.x, c.y + radiusUnit/2, 0.0, color,
			//		markerWidth);
			textStr = String.format("UP= %.0f/%.0f",upwdir.getValue(), upwspd.getValue());
			target.drawString(font10, textStr, c.x, c.y, 0.0,
					TextStyle.NORMAL, color, HorizontalAlignment.LEFT,
					VerticalAlignment.TOP, null);
		}
		//plot Bunkers Vector,by default plot them
		if(((graphConfigProperty != null ) && graphConfigProperty.isSmvBunkersR())||(graphConfigProperty == null)){
			FloatByReference bwdir= new FloatByReference(-999);
			FloatByReference bwspd= new FloatByReference(-999);
			nsharpNative.nsharpLib.bunkers_storm_motion(value1, value2, bwdir, bwspd);
			//System.out.println("bunkers_storm_motion hodo windspd="+  bwspd.getValue()+ " dir="+bwdir.getValue());
			c = WxMath.uvComp(bwspd.getValue(),bwdir.getValue());
			c= world.map(c);
			RGB color = NsharpConstants.color_firebrick;
			target.drawCircle(c.x, c.y, 0, radiusUnit,color, markerWidth);
			target.drawLine(c.x - radiusUnit, c.y, 0.0,c.x + radiusUnit, c.y, 0.0, color,
					markerWidth);
			target.drawLine(c.x, c.y- radiusUnit, 0.0,c.x, c.y + radiusUnit, 0.0, color,
					markerWidth);
			textStr = String.format("%.0f/%.0f RM",bwdir.getValue(), bwspd.getValue());
			target.drawString(font10, textStr, c.x, c.y+10*zoomLevel*yRatio, 0.0,
					TextStyle.NORMAL, color, HorizontalAlignment.RIGHT,
					VerticalAlignment.TOP, null);
		}
		if(((graphConfigProperty != null ) && graphConfigProperty.isSmvBunkersL())||(graphConfigProperty == null)){
			FloatByReference bwdir= new FloatByReference(-999);
			FloatByReference bwspd= new FloatByReference(-999);
			nsharpNative.nsharpLib.bunkers_left_motion(value1, value2, bwdir, bwspd);
			c = WxMath.uvComp(bwspd.getValue(),bwdir.getValue());
			c= world.map(c);
			RGB color = NsharpConstants.color_skyblue;
			target.drawCircle(c.x, c.y, 0, radiusUnit,color, markerWidth);
			target.drawLine(c.x - radiusUnit, c.y, 0.0,c.x + radiusUnit, c.y, 0.0, color,
					markerWidth);
			target.drawLine(c.x, c.y- radiusUnit, 0.0,c.x, c.y + radiusUnit, 0.0, color,
					markerWidth);
			textStr = String.format("%.0f/%.0f LM",bwdir.getValue(), bwspd.getValue());
			target.drawString(font10, textStr, c.x, c.y-10*zoomLevel*yRatio, 0.0,
					TextStyle.NORMAL, color, HorizontalAlignment.LEFT,
					VerticalAlignment.BOTTOM, null);
		}

		//plot current storm motion vector (mouse click) marker				
		//Coordinate hodoStmCenter = rscHandler.getHodoStmCenter();
		//System.out.println("rscHandler hodo windspd="+ rscHandler.getSmWindSpd()+ " dir="+rscHandler.getSmWindDir());
		Coordinate hodoStmCenter = WxMath.uvComp(rscHandler.getSmWindSpd(),rscHandler.getSmWindDir());
		hodoStmCenter= world.map(hodoStmCenter);
		target.drawCircle(hodoStmCenter.x, hodoStmCenter.y, 0, radiusUnit,
				NsharpConstants.color_white, markerWidth);
		target.drawLine(hodoStmCenter.x - radiusUnit, hodoStmCenter.y, 0.0,hodoStmCenter.x + radiusUnit, hodoStmCenter.y, 0.0, NsharpConstants.color_white,
				markerWidth);
		target.drawLine(hodoStmCenter.x, hodoStmCenter.y- radiusUnit, 0.0,hodoStmCenter.x, hodoStmCenter.y + radiusUnit, 0.0, NsharpConstants.color_white,
				markerWidth);
		textStr = String.format("%.0f/%.0f",rscHandler.getSmWindDir(), rscHandler.getSmWindSpd());
		target.drawString(font10, textStr, hodoStmCenter.x,hodoStmCenter.y+radiusUnit*2, 0.0,
				TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.CENTER,
				VerticalAlignment.TOP, null);

		
		//draw lines from storm motion to top and bottom of effective layer 
		FloatByReference topPF= new FloatByReference(0);
		FloatByReference botPF= new FloatByReference(0);
		nsharpNative.nsharpLib.get_effectLayertopBotPres(topPF, botPF);
		if(botPF.getValue() >=1 ){
			// interpolate wind direction and speed at top and bottom of effective layer 
			float bot_spd = nsharpNative.nsharpLib.iwspd(botPF.getValue());
			float bot_dir = nsharpNative.nsharpLib.iwdir(botPF.getValue());
			float top_spd = nsharpNative.nsharpLib.iwspd(topPF.getValue());
			float top_dir = nsharpNative.nsharpLib.iwdir(topPF.getValue());
			c = WxMath.uvComp(bot_spd,bot_dir); 
			c= world.map(c);
			target.drawLine(hodoStmCenter.x, hodoStmCenter.y, 0.0,c.x, c.y, 0.0, NsharpConstants.color_skyblue,
					markerWidth);
			c = WxMath.uvComp(top_spd,top_dir); 
			c= world.map(c);
			target.drawLine(hodoStmCenter.x, hodoStmCenter.y, 0.0,c.x, c.y, 0.0, NsharpConstants.color_skyblue,
					markerWidth);
		}
	}

	@SuppressWarnings("deprecation")
	private void drawHodoDynamicData(IGraphicsTarget target, double zoomLevel) throws VizException {
		// draw running temp, theta, height etc data at window palette bottom
		double dispX, xmin;
		double dispY, ymin, ymax;
		//display wind direction, speed in m/s and knots
		//Line 1 - wind direction, speed
		IExtent ext = getDescriptor().getRenderableDisplay().getExtent();
		xmin = ext.getMinX();  //Extent's viewable envelope min x and y
		ymin = ext.getMinY();
		dispX = xmin + 20 * zoomLevel * xRatio;
		dispY = ymin + 40 * zoomLevel * yRatio;
		target.drawString(fixedFont, sWindDirection+ "  "+ sWindSpeed, dispX, dispY, 0.0,
				TextStyle.NORMAL, NsharpConstants.color_cyan, HorizontalAlignment.LEFT,
				VerticalAlignment.BOTTOM, null);
		//plot critical angle
		float ca = nsharpNative.nsharpLib.cave_criticalAngel();
		if(ca != -9999){
			ymax = ext.getMaxY();
			dispY = ymax - 20 * zoomLevel;
			String textStr = String.format("Critical Angle = %.0f",ca);
			target.drawString(fixedFont, textStr, dispX,dispY, 0.0,
					TextStyle.NORMAL, NsharpConstants.color_cyan, HorizontalAlignment.LEFT,
					VerticalAlignment.BOTTOM, null);
		}
	}
    //To be used later...
	@SuppressWarnings({ "unused", "deprecation" })
	private void drawHodoWindMotionBox(IGraphicsTarget target, Rectangle rect) throws VizException {
    	target.drawShadedRect(new PixelExtent(rect.x, rect.x+rect.width, rect.y, rect.y+rect.height), NsharpConstants.color_black, 1.0, null);
    	target.drawWireframeShape(hodoWindMotionBoxShape, NsharpConstants.color_cyan, commonLinewidth,commonLineStyle,font10);
    	RGB colorN, colorS, colorM;
    	switch(currentHodoWindMode){
    	case HODO_NORMAL:
    		colorN = NsharpConstants.color_white;
    		colorS = NsharpConstants.color_cyan;
    		colorM = NsharpConstants.color_cyan;
    		break;
    	case HODO_STORMRELATIVE:
    		colorN = NsharpConstants.color_cyan;
    		colorS = NsharpConstants.color_white;
    		colorM = NsharpConstants.color_cyan;
    		break;
    	case HODO_BNDRY:
    		colorN = NsharpConstants.color_cyan;
    		colorS = NsharpConstants.color_cyan;
    		colorM = NsharpConstants.color_cyan;
    		break;
    	case HODO_MEANWIND:
    	default:
    		colorN = NsharpConstants.color_cyan;
    		colorS = NsharpConstants.color_cyan;
    		colorM = NsharpConstants.color_white;
    		break;
    	}
    	double xOri = NsharpConstants.WIND_MOTION_REC_X_ORIG;
    	double yOri = NsharpConstants.WIND_MOTION_REC_Y_ORIG;
    	target.drawString(font10, "NORMAL   ", xOri+ NsharpConstants.WIND_MOTION_REC_WIDTH/2,
    			yOri+NsharpConstants.WIND_MOTION_REC_HEIGHT/3, 0.0,
    			IGraphicsTarget.TextStyle.NORMAL,
    			colorN,
    			HorizontalAlignment.CENTER,  
    			VerticalAlignment.BOTTOM, null);
    	target.drawString(font10, "STORMREL ", xOri+ NsharpConstants.WIND_MOTION_REC_WIDTH/2,
    			yOri+ 2* NsharpConstants.WIND_MOTION_REC_HEIGHT/3, 0.0,
    			IGraphicsTarget.TextStyle.NORMAL,
    			colorS,
    			HorizontalAlignment.CENTER,  
    			VerticalAlignment.BOTTOM, null);
    	target.drawString(font10, "MEAN WIND", xOri+ NsharpConstants.WIND_MOTION_REC_WIDTH/2,
    			yOri+ NsharpConstants.WIND_MOTION_REC_HEIGHT, 0.0,
    			IGraphicsTarget.TextStyle.NORMAL,
    			colorM,
    			HorizontalAlignment.CENTER,  
    			VerticalAlignment.BOTTOM, null);
    }

	@Override
	protected void paintInternal(IGraphicsTarget target,
			PaintProperties paintProps) throws VizException {
		super.paintInternal(target, paintProps);
		if(rscHandler==null)
			return;
		
		timeLineStateList = rscHandler.getTimeLineStateList();	//System.out.println("NsharpHodoPaneResource "+ descriptor.getPaneNumber());
		stnStateList = rscHandler.getStnStateList();
		hodoBackground.paintInternal(target, paintProps);
		if((soundingLys != null) && (soundingLys.size()>= 4))
		{
			this.font10.setSmoothing(false);
			this.font10.setScaleFont(false);
			this.font9.setSmoothing(false);
			this.font9.setScaleFont(false);
			this.font12.setSmoothing(false);
			this.font12.setScaleFont(false);
			fixedFont.setSmoothing(false);
			fixedFont.setScaleFont(false);
			//plot HODO
			PixelExtent extent = new PixelExtent(hodoBackground.getRectangle());
			target.setupClippingPlane(extent);
			if(((graphConfigProperty != null )&& graphConfigProperty.isHodo())|| (graphConfigProperty == null)){
				for(NsharpShapeAndLineProperty shapeNColor: hodoWindRscShapeList){
					target.drawWireframeShape(shapeNColor.getShape(), shapeNColor.getLp().getLineColor(), commonLinewidth*2,commonLineStyle,font10);
				}
			}
			boolean compareStnIsOn = rscHandler.isCompareStnIsOn();
			boolean editGraphOn= rscHandler.isEditGraphOn();
			if(editGraphOn && !compareStnIsOn)
				plotHodoEditPoints( target,NsharpConstants.color_white);
			if(!compareStnIsOn){
				plotNsharpHodoVectors(target, currentZoomLevel,  null, false);
			} 
			target.clearClippingPlane();
			if(cursorInHodo){
				
				drawHodoDynamicData(target, currentZoomLevel);
			}
			

		}
		
	}

	@Override
	protected void initInternal(IGraphicsTarget target) throws VizException {
		super.initInternal(target);
		currentCanvasBoundWidth = NsharpConstants.HODO_PANE_REC_WIDTH;
		currentCanvasBoundHeight = NsharpConstants.HODO_PANE_REC_HEIGHT;
		myDefaultCanvasWidth = NsharpConstants.HODO_PANE_REC_WIDTH;
		myDefaultCanvasHeight = NsharpConstants.HODO_PANE_REC_HEIGHT;	
		hodoBackground.initInternal(target);
		createRscHodoWindShapeAll();
		IFont.Style[] style = {IFont.Style.BOLD};
		fixedFont = target.initializeFont("Monospace", 12, style);
		 
	}
	@Override
	protected void disposeInternal() {
		if(hodoWindRscShapeList.size()>0){
			for(NsharpShapeAndLineProperty shapeColor: hodoWindRscShapeList){
				shapeColor.getShape().dispose();
			}
			hodoWindRscShapeList.clear();
			hodoWindRscShapeList=null;
		}
		if(hodoBackground != null)
		{
			hodoBackground.disposeInternal();
			hodoBackground = null;
		}
		if(hodoWindMotionBoxShape!=null)
			hodoWindMotionBoxShape.dispose();
		super.disposeInternal();
	}
	public String updateDynamicData(Coordinate c) throws VizException {

		try {   
			//System.out.println(" updateDynamicData entered!!!!!C.x="+c.x + " c.y="+c.y);
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

	public void setCursorInHodo(boolean cursorInHodo) {
		this.cursorInHodo = cursorInHodo;
		
	}
	public NsharpHodoPaneBackground getHodoBackground() {
		return hodoBackground;
	}
	//public void setHodoHouseC(Coordinate hodoHouseC) {
	//	this.hodoHouseC = hodoHouseC;
		
	//}
	@Override
	public void handleResize() {
		
		//super.handleResize();
		this.resize=false;
		IExtent ext = getDescriptor().getRenderableDisplay().getExtent();
		ext.reset();
		//System.out.println("hodoPane: handleResize");
   	 	float prevHeight = hodoHeight;
		float prevWidth = hodoWidth;
		//hodoXOrig = (int) (ext.getMinX());
		//hodoYOrig = (int) ext.getMinY();
		hodoWidth = (int) (ext.getWidth());
		hodoHeight = (int) ext.getHeight();
		//hodoXEnd = hodoXOrig+ hodoWidth;
		//hodoYEnd = hodoYOrig+ hodoHeight;
		xRatio = xRatio* hodoWidth/prevWidth;
		yRatio = yRatio* hodoHeight/prevHeight;
		hodoBackground.handleResize(ext);
		world = hodoBackground.computeWorld();
		createRscHodoWindShapeAll();
		
	}
	@Override
	public void setGraphConfigProperty(NsharpGraphProperty graphConfigProperty) {
		super.setGraphConfigProperty(graphConfigProperty);
		hodoBackground.setPaneConfigurationName(paneConfigurationName);
	}
}
