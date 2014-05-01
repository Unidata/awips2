/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.background.NsharpSkewTPaneBackground
 * 
 * This java class performs the NSHARP NsharpSkewTPaneBackground functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 05/02/2012	229			Chin Chen	Initial coding for multiple display panes implementation
 * 									    
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp.background;

import static java.lang.Math.pow;
import static java.lang.Math.signum;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConfigManager;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.NsharpGraphProperty;
import gov.noaa.nws.ncep.ui.nsharp.NsharpWxMath;
import gov.noaa.nws.ncep.ui.nsharp.display.NsharpSkewTPaneDescriptor;

import java.awt.geom.Line2D;
import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.common.sounding.util.Equations;
import com.raytheon.uf.common.sounding.util.UAPoint;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.graphing.WGraphics;
import com.vividsolutions.jts.geom.Coordinate;

public class NsharpSkewTPaneBackground extends NsharpGenericPaneBackground {
	//private NsharpSkewTPaneDescriptor desc;
	private List<Integer> pressureMainList;
	private List<Float> mixRatioMainList;
	private IWireframeShape mixRatioShape;
	private IWireframeShape dryAdiabatsShape;
	private IWireframeShape moistAdiabatsShape;
	private IWireframeShape temperatureLineShape;
	private IWireframeShape presslinesNumbersShape;
	private IWireframeShape tempNumbersShape;
	private double currentZoomLevel=1;
	private int skewtWidth=NsharpConstants.SKEWT_WIDTH;
	private float xRatio=1;
	private NsharpGraphProperty graphConfigProperty;
    
	private Float[]mainMixingRatios = 
            { .5f, 1f, 2f, 5f, 10f, 20f};
	private float[]mixingRatios = 
		{ .5f, 1,1.5f, 2,3,4, 5,6, 7,9,10,12.5f, 15, 20,25,30};

	private static List<List<UAPoint>> saturatedPoints = Equations
    .getSaturatedAdiabats(1000, 100, 20, -60, 60, 5);

	private static List<List<UAPoint>> dryPoints = getDryAdiabats(
    1000, 100, 10, -40, 273, 20);
    
	private static final double Rd = 0.2870586d; 
	
	private int tempOffset = 0;
    public NsharpSkewTPaneBackground(NsharpSkewTPaneDescriptor desc) {
        super();

        this.rectangle = new Rectangle(NsharpConstants.SKEWT_X_ORIG, NsharpConstants.SKEWT_Y_ORIG,
        		NsharpConstants.SKEWT_WIDTH, NsharpConstants.SKEWT_HEIGHT);
        //this.rectangle = new Rectangle(-700, 250, 690, 690);
        //this.rectangle = new Rectangle(10,10, 690,690);
        pe = new PixelExtent(this.rectangle);
        world = new WGraphics(this.rectangle);

        world.setWorldCoordinates(NsharpConstants.left, NsharpConstants.top,
                NsharpConstants.right, NsharpConstants.bottom);
        //System.out.println("NsharpSkewTPaneBackground created");
        this.desc = desc;
       
        NsharpConfigManager configMgr = NsharpConfigManager.getInstance();
        graphConfigProperty = configMgr.retrieveNsharpConfigStoreFromFs().getGraphProperty();
		pressureMainList = Arrays.asList(NsharpConstants.PRESSURE_MAIN_LEVELS);
		mixRatioMainList = Arrays.asList(mainMixingRatios);
    }

    
    
    private static double poisson(double startPressure, double stopPressure,
            double temperature) {
        return temperature * pow((startPressure / stopPressure), Rd);
    }
    
    private static List<List<UAPoint>> getDryAdiabats(double startPressure,
            double stopPressure, double increment, double startTemp,
            double endTemp, double tempDist) {
        List<List<UAPoint>> dryAdiabats = new ArrayList<List<UAPoint>>();

        for (double t = startTemp; t < 100; t += tempDist) {
            dryAdiabats.add(dryAdiabats(1000, 100, /*20*/increment, t + 273.15));
        }
        return dryAdiabats;
    }

    /**
     * 
     * @param startPressure
     * @param stopPressure
     * @param increment
     * @param adiabat
     * @return
     */
    private static List<UAPoint> dryAdiabats(double startPressure,
            double stopPressure, double increment, double adiabat) {
        ArrayList<UAPoint> adiabats = null;
        if (startPressure != stopPressure) {
            if (increment > 0) {
                adiabats = new ArrayList<UAPoint>();

                double delta = signum(stopPressure - startPressure) * increment;

                double basePressure = startPressure;
                // int i = 0;
                for (; startPressure >= stopPressure; startPressure += delta) {
                    UAPoint point = new UAPoint();
                    point.pressure = startPressure;
                    point.temperature = poisson(startPressure, basePressure,
                            adiabat);
                    adiabats.add(point);
                }
            }
        }
        return adiabats;
    }

    private void printDryAdiabats(WGraphics world, GC gc) throws VizException {
    	for (Iterator<List<UAPoint>> iterator = dryPoints
    			.iterator(); iterator.hasNext();) {
    		List<UAPoint> points = iterator.next();
    		UAPoint firstPoint = points.get(0);
    		Coordinate startCoor = NsharpWxMath.getSkewTXY(firstPoint.pressure,
    				NsharpConstants.kelvinToCelsius.convert(firstPoint.temperature));
    		//System.out.println("List size ="+points.size());
    		//System.out.println("First pt pre=" + firstPoint.pressure+ " temp="+ firstPoint.temperature);
    		for (Iterator<UAPoint> iter = points.iterator(); iter.hasNext();) {
    			UAPoint p = iter.next();
    			//System.out.println("pt pre=" + p.pressure+ " temp="+ p.temperature);
    			Coordinate endCoor = NsharpWxMath.getSkewTXY(p.pressure,
    					NsharpConstants.kelvinToCelsius.convert(p.temperature));

    			gc.drawLine((int)world.mapX(startCoor.x), (int)world.mapY(
    					startCoor.y),  (int)world.mapX(endCoor.x),
    					(int)world.mapY(endCoor.y)); 

    			startCoor = endCoor;
    		}
    	}
    }

    /**
     * Print the temperature lines.
     * 
     * @throws VizException
     */
    private void printTempLines(WGraphics world, GC gc) throws VizException {
    	for (int i = 70; i > -200; i -= 10) {
    		Coordinate coorStart = NsharpWxMath.getSkewTXY(1050, i);
    		Coordinate coorEnd = NsharpWxMath.getSkewTXY(100, i);
    		gc.drawLine((int)world.mapX(coorStart.x), (int)world.mapY(
    				coorStart.y), (int)world.mapX(coorEnd.x), (int)world
    				.mapY(coorEnd.y));
    	}
    }


    

    /**
     * Returns the point that two lines instersect, or null if they do not.
     * 
     * @param l1
     * @param l2
     * @return
     */
    private Point2D.Double getLineIntersection(Line2D.Double l1,
            Line2D.Double l2) {
        if (!l1.intersectsLine(l2)) {
            return null;
        }

        Point2D.Double intersection = new Point2D.Double();
        double x1 = l1.getX1(), y1 = l1.getY1(), x2 = l1.getX2(), y2 = l1
                .getY2(), x3 = l2.getX1(), y3 = l2.getY1(), x4 = l2.getX2(), y4 = l2
                .getY2();

        intersection.x = det(det(x1, y1, x2, y2), x1 - x2, det(x3, y3, x4, y4),
                x3 - x4)
                / det(x1 - x2, y1 - y2, x3 - x4, y3 - y4);
        intersection.y = det(det(x1, y1, x2, y2), y1 - y2, det(x3, y3, x4, y4),
                y3 - y4)
                / det(x1 - x2, y1 - y2, x3 - x4, y3 - y4);

        return intersection;
    }

    private  double det(double a, double b, double c, double d) {
        return a * d - b * c;
    }

    
    @Override
    public void disposeInternal(){
    	super.disposeInternal();
    	if(mixRatioShape!=null)
    		mixRatioShape.dispose();
    	if(dryAdiabatsShape!=null)
    		dryAdiabatsShape.dispose();
    	if(moistAdiabatsShape!= null)
    		moistAdiabatsShape.dispose();
    	if(temperatureLineShape!=null)
    		temperatureLineShape.dispose();
    	if(presslinesNumbersShape!=null)
    		presslinesNumbersShape.dispose();
    	if(tempNumbersShape!=null)
    		tempNumbersShape.dispose();
    }
    @Override
    public void initInternal(IGraphicsTarget target){
    	super.initInternal(target);
    	//createTempLineShape();
    	createMoistAdiabatsShape();
    	createDryAdiabatsShape();
    	createMixingRatioShape();
        //createPressureLineNumberShape();
        createTempNumberAndLineShape();
    }
    @SuppressWarnings("unused")
	private void createPressureLineNumberShape(){
    	//pressureLineNumberShape
    	if(target==null)
 			return;
        String s = "";
       
        presslinesNumbersShape = target.createWireframeShape(false,desc );
        presslinesNumbersShape.allocate(150);
       // System.out.println("NsharpConstants.left="+NsharpConstants.left+"NsharpConstants.right"+NsharpConstants.right+" top="+NsharpConstants.top + " bot="+ NsharpConstants.bottom);
       // System.out.println("MAIN*******************");
        /*  Chin::::Note:::
         * move this out to draw them in paintInternal() to fix a bug that will not draw pressure line above 800 after resizing screen.
         * Does not know why this happened. All pressure lines below 800 are still drew but not for those above 800. 
         * Mark lines also have same problem, but do not move to paintInternal as they are not as important as pressure lines.
        for (int i = 0; i < NsharpConstants.PRESSURE_MAIN_LEVELS.length; i++) {
        	//we only care about pressure for this case, temp is no important  when calling getSkewTXY
        	Coordinate coor = NsharpWxMath.getSkewTXY(NsharpConstants.PRESSURE_MAIN_LEVELS[i],0);

        	double [][] lines = {{world.mapX(NsharpConstants.left), world.mapY(coor.y)},{world.mapX(NsharpConstants.right),
        		world.mapY(coor.y)}};
        	presslinesNumbersShape.addLineSegment(lines);
        	//System.out.println("coor.x="+coor.x+"coor.y="+coor.y);
        	//System.out.println("x1="+world.mapX(NsharpConstants.left)+"y1=" +world.mapY(coor.y)+"x2="+world.mapX(NsharpConstants.right)+"y2="+
            //		world.mapY(coor.y));
        }*/
        //System.out.println("MARK*******************");
        for (int i = 0; i < NsharpConstants.PRESSURE_MARK_LEVELS.length; i++) {
        	//we only care about pressure for this case, temp is no important  when calling getSkewTXY
        	Coordinate coor = NsharpWxMath.getSkewTXY(NsharpConstants.PRESSURE_MARK_LEVELS[i],0);

        	double [][] lines = {{world.mapX(NsharpConstants.left), world.mapY(coor.y)},{ world.mapX(NsharpConstants.left)+15* currentZoomLevel*xRatio,
        		world.mapY(coor.y)}};
        	presslinesNumbersShape.addLineSegment(lines);
        	//System.out.println("lines="+ lines[0] + "; "+ lines[1]);
        	//System.out.println("x1="+world.mapX(NsharpConstants.left)+"y1=" +world.mapY(coor.y)+"x2="+world.mapX(NsharpConstants.right)+"y2="+
            //		world.mapY(coor.y));
        }
        IExtent ext = desc.getRenderableDisplay().getExtent();
        double xmin = ext.getMinX();  //Extent's viewable envelope min x and y
        double xDefault = world.mapX(NsharpConstants.left);
        if(xmin <xDefault)
        	xmin = xDefault;
        double dispX = xmin + 25 * currentZoomLevel*xRatio;
       // System.out.println("NUMBER*******************");
        
        for (int i = 0; i < NsharpConstants.PRESSURE_NUMBERING_LEVELS.length; i++) {
        	s = NsharpConstants.pressFormat.format(NsharpConstants.PRESSURE_NUMBERING_LEVELS[i]);
        	//we only care about pressure for this case, temp is no important  when calling getSkewTXY
        	Coordinate coor = NsharpWxMath.getSkewTXY(NsharpConstants.PRESSURE_NUMBERING_LEVELS[i],0);
        	double [] lblXy = {dispX,world.mapY(coor.y)};
        	presslinesNumbersShape.addLabel(s, lblXy);
        	//System.out.println("coor.x="+coor.x+"coor.y="+coor.y);
        	//System.out.println("x1="+world.mapX(NsharpConstants.left)+"y1=" +world.mapY(coor.y)+"x2="+world.mapX(NsharpConstants.right)+"y2="+
            //		world.mapY(coor.y));

        }
        presslinesNumbersShape.compile();
    }
    // Chin: to handle dynamically moving pressure lines and its number within viewable zone when zooming, I could not use wireframeShape successfully
	// It will chop off lower part. Therefore use this draw function.
    @SuppressWarnings("deprecation")
	private void drawPressureLineNumber(IGraphicsTarget target){
    	//pressureLineNumberShape
    	if(target==null)
 			return;
        String s = "";
        
       // System.out.println("NsharpConstants.left="+NsharpConstants.left+"NsharpConstants.right"+NsharpConstants.right+" top="+NsharpConstants.top + " bot="+ NsharpConstants.bottom);
       // System.out.println("MAIN*******************");
        /*for (int i = 0; i < NsharpConstants.PRESSURE_MAIN_LEVELS.length; i++) {
        	//we only care about pressure for this case, temp is no important  when calling getSkewTXY
        	Coordinate coor = NsharpWxMath.getSkewTXY(NsharpConstants.PRESSURE_MAIN_LEVELS[i],0);
        	try {
				target.drawLine(world.mapX(NsharpConstants.left), world.mapY(coor.y), 0.0, world.mapX(NsharpConstants.right),
						world.mapY(coor.y), 0.0,
						NsharpConstants.pressureColor, 1);
			} catch (VizException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
        }*/
        //System.out.println("MARK*******************");
        double xend ;
        IExtent ext = desc.getRenderableDisplay().getExtent();
        double xmin = ext.getMinX();  //Extent's viewable envelope min x and y
        double xDefault = world.mapX(NsharpConstants.left);
        if(xmin <xDefault)
        	xmin = xDefault;
        double dispX = xmin + 25 * currentZoomLevel*xRatio;
        //System.out.println("zoom="+currentZoomLevel);
        // draw pressure line, pressure mark and pressure number label all at once
        // Progressively change pressure line density when zoom in/out
        for (int i = 0; i < NsharpConstants.PRESSURE_MARK_LEVELS.length; i++) {
        	//we only care about pressure for this case, temp is no important  when calling getSkewTXY
        	Coordinate coor = NsharpWxMath.getSkewTXY(NsharpConstants.PRESSURE_MARK_LEVELS[i],0);
        	try {       		
        		int mod = NsharpConstants.PRESSURE_MARK_LEVELS[i] % 100;
        		if(pressureMainList.contains( NsharpConstants.PRESSURE_MARK_LEVELS[i]) || 
        				(currentZoomLevel <=0.7 && mod == 0 ) || (currentZoomLevel <=0.4)){
        			// case 1: pressure main level line defined in NsharpConstants.PRESSURE_MAIN_LEVELS
        			// case 2: zoom factor <= 0.7, and pressure level at 100th
        			// case 3: zoom factor < 0.4, all pressure lines
        			//draw pressure line all the way from left to right on skewt pane
        			xend = ext.getMaxX();  //world.mapX(NsharpConstants.right);
        			// also draw pressure number label
        			s = NsharpConstants.pressFormat.format(NsharpConstants.PRESSURE_MARK_LEVELS[i]);
                	target.drawString(smallFont,s, dispX,world.mapY(coor.y), 0.0, TextStyle.NORMAL,
    						NsharpConstants.pressureColor, HorizontalAlignment.LEFT,
    						VerticalAlignment.MIDDLE, null);
        		}
        		else{
        			// only mark pressure line to a small lengthprivate NsharpGraphProperty graphConfigProperty;
        			xend = xmin+15* currentZoomLevel*xRatio;
        		}
				target.drawLine(xmin, world.mapY(coor.y), 0.0, xend,
						world.mapY(coor.y), 0.0,
						NsharpConstants.pressureColor, 1);
				//System.out.println("lines ="+ NsharpConstants.PRESSURE_MARK_LEVELS[i] + "; "+ world.mapY(coor.y));
			} catch (VizException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
        }
        
       // System.out.println("NUMBER*******************");
        /*
        for (int i = 0; i < NsharpConstants.PRESSURE_NUMBERING_LEVELS.length; i++) {
        	s = NsharpConstants.pressFormat.format(NsharpConstants.PRESSURE_NUMBERING_LEVELS[i]);
        	//we only care about pressure for this case, temp is no important  when calling getSkewTXY
        	Coordinate coor = NsharpWxMath.getSkewTXY(NsharpConstants.PRESSURE_NUMBERING_LEVELS[i],0);
        	try {
				target.drawString(smallFont,s, dispX,world.mapY(coor.y), 0.0, TextStyle.NORMAL,
						NsharpConstants.pressureColor, HorizontalAlignment.LEFT,
						VerticalAlignment.MIDDLE, null);
			} catch (VizException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
        } */
    }
    private void createDryAdiabatsShape()
    {
    	if(target==null)
			return;
    	//DryAdiabats shape	
    	dryAdiabatsShape = target.createWireframeShape(false,desc );
    	dryAdiabatsShape.allocate(1500);
    	for (Iterator<List<UAPoint>> iterator = dryPoints
        		.iterator(); iterator.hasNext();) {
        	List<UAPoint> points = iterator.next();
        	UAPoint firstPoint = points.get(0);
        	Coordinate startCoor = NsharpWxMath.getSkewTXY(firstPoint.pressure,
        			NsharpConstants.kelvinToCelsius.convert(firstPoint.temperature));
        	for (Iterator<UAPoint> iter = points.iterator(); iter.hasNext();) {
        		UAPoint p = iter.next();
        		Coordinate endCoor = NsharpWxMath.getSkewTXY(p.pressure,
        				NsharpConstants.kelvinToCelsius.convert(p.temperature));
        		double [][] lines = {{world.mapX(startCoor.x), world.mapY(
        				startCoor.y)},{world.mapX(endCoor.x),
        					world.mapY(endCoor.y)}};
        		dryAdiabatsShape.addLineSegment(lines);


        		startCoor = endCoor;
        	}
        }
        dryAdiabatsShape.compile();        	
    }
    private void createMoistAdiabatsShape(){
    	if(target==null)
 			return;
    	//moist Adiabats shape
    	moistAdiabatsShape = target.createWireframeShape(false,desc );
    	moistAdiabatsShape.allocate(2500);
    	for (Iterator<List<UAPoint>> iterator = saturatedPoints
                .iterator(); iterator.hasNext();) {
            List<UAPoint> points = iterator.next();
            UAPoint firstPoint = points.get(0);
            Coordinate coor1 = NsharpWxMath.getSkewTXY(firstPoint.pressure,
            		NsharpConstants.kelvinToCelsius.convert(firstPoint.temperature));
            for (Iterator<UAPoint> iter = points.iterator(); iter.hasNext();) {
                UAPoint p = iter.next();
                Coordinate coor2 = NsharpWxMath.getSkewTXY(p.pressure,
                		NsharpConstants.kelvinToCelsius.convert(p.temperature));
                double [][] lines = {{world.mapX(coor1.x), world.mapY(coor1.y)},{world.mapX(coor2.x), world.mapY(coor2.y)}};
                moistAdiabatsShape.addLineSegment(lines);
                coor1 = coor2;
            }
        }
        moistAdiabatsShape.compile();
    }
    private void createMixingRatioShape()
    {
    	if(target==null)
 			return;
    	
    	//mixing ratio shape
    	// get the location of the 850 pressure line...
    	mixRatioShape = target.createWireframeShape(false,desc );
    	mixRatioShape.allocate(12);
    	
    	IExtent ext = desc.getRenderableDisplay().getExtent();
        double xmin = ext.getMinX();  //Extent's viewable envelope min x and y
        double ymax = ext.getMaxY();
        double dispY = ymax - 30 * currentZoomLevel;
        //We are getting Y (pressure) level for plotting mix ratio number, therefore dispX here is not important for the
        // reverseSkewTXY() input.
        Coordinate c = NsharpWxMath.reverseSkewTXY(world.unMap(xmin,dispY));
        double dispPressure = c.y-10* currentZoomLevel;
       if((NsharpConstants.MAX_PRESSURE-dispPressure) < 30) 
        	dispPressure = NsharpConstants.MAX_PRESSURE-30* currentZoomLevel;
       if(dispPressure < 405)
    	   dispPressure = 405;
       if(dispPressure > 1000)
    	   dispPressure = 1000;
      // System.out.println("dispP="+dispPressure);
    	Coordinate coorStart = NsharpWxMath.getSkewTXY(dispPressure,-50);//(850, -50);
        Coordinate coorEnd = NsharpWxMath.getSkewTXY(dispPressure,50);//((850, 50);

        double startX = world.mapX(coorStart.x);
        double startY = world.mapY(coorStart.y);

        double endX = world.mapX(coorEnd.x);
        double endY = world.mapY(coorEnd.y);

        Line2D.Double ratioLabelLine = new Line2D.Double(startX, startY, endX, endY);
        Line2D.Double line2 = new Line2D.Double();
        UAPoint p1 = new UAPoint();
        p1.pressure = 1000;
        UAPoint p2 = new UAPoint();
        p2.pressure = 400;
        String num="";
        for (float ratio : mixingRatios) {
        	if(mixRatioMainList.contains( ratio) || 
        			(currentZoomLevel <=0.4)){       	
        		p1.temperature = Equations
        		.invMixingRatio(p1.pressure, ratio / 1000);
        		p2.temperature = Equations
        		.invMixingRatio(p2.pressure, ratio / 1000);
        		Coordinate coor1 = NsharpWxMath.getSkewTXY(p1.pressure,
        				p1.temperature - 273.15);
        		Coordinate coor2 = NsharpWxMath.getSkewTXY(p2.pressure,
        				p2.temperature - 273.15);
        		double [][] lines = {{world.mapX(coor1.x), world.mapY(coor1.y)},{world.mapX(coor2.x), world.mapY(coor2.y)}};
        		mixRatioShape.addLineSegment(lines);

        		line2.setLine(world.mapX(coor1.x), world.mapY(coor1.y),
        				world.mapX(coor2.x), world.mapY(coor2.y));
        		num = num + ratio;
        		Point2D.Double point = null;
        		if ((point = getLineIntersection(ratioLabelLine, line2)) != null) {
        			double [] lblXy = {point.x, point.y};
        			mixRatioShape.addLabel(num, lblXy);
        		}
        		num = "";
        	}
        }
        mixRatioShape.compile();
        

    }
    private void createTempNumberAndLineShape(){
    	if(target==null)
 			return;
    	if(graphConfigProperty!=null){
    		tempOffset = graphConfigProperty.getTempOffset();
    		NsharpWxMath.setTempOffset(tempOffset);
    	}
    	//System.out.println("Skew temp offset="+tempOffset);
    	if(tempNumbersShape!= null)
    		tempNumbersShape.dispose();
    	tempNumbersShape = target.createWireframeShape(false,desc );
        tempNumbersShape.allocate(2);
        //Chin: to fix the problem that wire frame shape need to have at least one 
    	//line. add a virtual line. 
    	double [][] lines = {{0,0},{0,
			0}};
        tempNumbersShape.addLineSegment(lines);
        
        IExtent ext = desc.getRenderableDisplay().getExtent();
        double xmin = ext.getMinX();  //Extent's viewable envelope min x and y
        double xmax = ext.getMaxX();  //Extent's viewable envelope min x and y
        double ymax = ext.getMaxY();
        double ymin = ext.getMinY();
        //System.out.println(" xmin="+xmin+ " xmax="+ xmax+ " ymax="+ymax+" ymin="+ymin);
        double dispX = xmin + 20 * currentZoomLevel;
        double dispY = ymax - 20 * currentZoomLevel;
        //We are getting Y (pressure) level for plotting Temp number, therefore dispX here is not important for the
        // reverseSkewTXY() input.
        Coordinate c = NsharpWxMath.reverseSkewTXY(world.unMap(xmin,dispY));
        double dispPressure = c.y-10* currentZoomLevel;
        double lowTempMin = c.x;
        lowTempMin = (int)((lowTempMin-10)/10) * 10;
        c = NsharpWxMath.reverseSkewTXY(world.unMap(xmax,dispY));
        double lowTempMax = c.x;
        lowTempMax = (int)((lowTempMax+10)/10) * 10;
        if((NsharpConstants.MAX_PRESSURE-dispPressure) < 30) 
        	dispPressure = NsharpConstants.MAX_PRESSURE-30* currentZoomLevel;
        // bottom temp number 
        int tempGap =10;
        if(currentZoomLevel <=0.1)
        	tempGap= 1;
        else if(currentZoomLevel <=0.2)
        	tempGap= 2;
        else if(currentZoomLevel <=0.3)
        	tempGap= 3;
        else if(currentZoomLevel <=0.4)
        	tempGap= 4;
        else if(currentZoomLevel <=0.5)
        	tempGap= 5;
        else if(currentZoomLevel <=0.75)
        	tempGap= 8;
        //TT605593 assume temp range is from -100 to 100 at ground level
        for (int i = 100; i >= -100; i -= tempGap) {
        // for (int i = /*70*/(int)lowTempMax+tempOffset; i >= /*-70+*/(int)lowTempMin+tempOffset; i -= tempGap) {
            Coordinate coorS = NsharpWxMath.getSkewTXY(dispPressure, i);
            double startX1 = world.mapX(coorS.x);
            double startY1 = world.mapY(coorS.y);
            double [] lblXy = {startX1,startY1};
            tempNumbersShape.addLabel(Integer.toString(i), lblXy);
            
        }
		dispY = ymin + 10 * currentZoomLevel;
		c = NsharpWxMath.reverseSkewTXY(world.unMap(xmin,dispY));
        dispPressure = c.y + 5 * currentZoomLevel;
        double topTempMin = c.x;
        topTempMin = (int)((topTempMin-10)/10) * 10;
        c = NsharpWxMath.reverseSkewTXY(world.unMap(xmax,dispY));
        double topTempMax = c.x;
        topTempMax = (int)((topTempMax+10)/10) * 10;
        //System.out.println("zoomL="+currentZoomLevel + " gap="+tempGap + " topTempMin="+topTempMin+ " topTempMax="+topTempMax+ " tempOffset="+tempOffset);
        
        if((dispPressure -NsharpConstants.MIN_PRESSURE) <10)
        	dispPressure = NsharpConstants.MIN_PRESSURE+5*currentZoomLevel;

        // top temp number.TT605593
        //Chin: without zooming, highest level temp is assume around 70 degree lower than ground
        // Also, to make sure top temp number is in sync with lower temp number, when consider tempGap
        int topTMax = 100- (( 70 / tempGap ) +1) * tempGap; 
        for (int i = topTMax; i >= topTMax - 200; i -= tempGap) {
        //for (int i = (int)topTempMax+tempOffset; i >= (int)topTempMin+tempOffset; i -= tempGap) {
            Coordinate coorEnd1 = NsharpWxMath.getSkewTXY(dispPressure, i);
           double endX1 = world.mapX(coorEnd1.x);
            double endY1 = world.mapY(coorEnd1.y);

            double [] lblXy = {endX1,endY1};
            tempNumbersShape.addLabel(Integer.toString(i), lblXy);
        }
        tempNumbersShape.compile();
        
        
        //temp line shape
    	temperatureLineShape = target.createWireframeShape(false,desc );
    	temperatureLineShape.allocate(((int)lowTempMax -(int)topTempMin)/tempGap * 2 + 2);
    	
    	//System.out.println("xmax="+xmax);
    	for (int i = (int)lowTempMax; i >= topTempMin; i -= tempGap) {
    		Coordinate coorStart = NsharpWxMath.getSkewTXY(1050, i);
    		Coordinate coorEnd = NsharpWxMath.getSkewTXY(100, i);
    		dispX=world.mapX(coorEnd.x);
    		dispY=world.mapY(coorEnd.y);
    		if(dispX>xmax){
    			// when temp line's top point's x coordinate is greater than view xman, the line will not be plotted.
    			// Can not find a solution for that now. 
    			// therefore, we have to find this temp line's top point's Y coordinate at xman position.
    			// In other words, find the intersection  point of temp line with skewT rectangle's right side line.
    			dispX = xmax-20; // need to minus some number. Otherwise, it wont be plotted. 
    			//TT605593
    			coorEnd = world.unMap(xmax, 0);
    			double py= NsharpWxMath.getPressureYFromTemp(i, coorEnd.x); 			
    			dispY = world.mapY(py);
    		}
    		double [][] tlines = {{world.mapX(coorStart.x), world.mapY(coorStart.y)},
    				{dispX, dispY}};
    		//System.out.println("temp="+i+" wLx="+world.mapX(coorStart.x)+" sLx="+coorStart.x+ ",wLy=" +world.mapY(coorStart.y)+ " ;wHx="
    			//	+dispX+" sHx="+coorEnd.x+",wHy=" + dispY);
    		temperatureLineShape.addLineSegment(tlines);
    	}
    	temperatureLineShape.compile();
    	//System.out.println("lower min"+ lowTempMin+ " lower nax "+ lowTempMax+ " upper min "+ topTempMin + " topTempMax "+ topTempMax);
    }

    //@SuppressWarnings("deprecation")
	@Override
    public void paintInternal(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
    	float zoomLevel = paintProps.getZoomLevel();
    	if(zoomLevel > 1.0f)
			zoomLevel = 1.0f;
    	if(zoomLevel != currentZoomLevel ){
			currentZoomLevel = zoomLevel;
			handleZooming();
		}
    	//System.out.println("bkg paintInternal zoomL="+currentZoomLevel);
        target.setupClippingPlane(pe);
        this.smallFont.setSmoothing(false);
		this.smallFont.setScaleFont(false);
        if(graphConfigProperty != null ){
        	if(graphConfigProperty.isMixratio() == true)
        		target.drawWireframeShape(mixRatioShape, NsharpConstants.mixingRatioColor, 1, LineStyle.DASHED,smallFont);
        	if(graphConfigProperty.isDryAdiabat() == true)
        		target.drawWireframeShape(dryAdiabatsShape, NsharpConstants.dryAdiabatColor, 1, LineStyle.SOLID,smallFont);
        	if(graphConfigProperty.isMoistAdiabat() == true)
        		target.drawWireframeShape(moistAdiabatsShape, NsharpConstants.moistAdiabatColor, 1, LineStyle.DOTTED,smallFont);
        }
        else{
        	target.drawWireframeShape(dryAdiabatsShape, NsharpConstants.dryAdiabatColor, 1, LineStyle.SOLID,smallFont);
        }
        	
        target.drawWireframeShape(temperatureLineShape, NsharpConstants.temperatureColor, 1, LineStyle.DOTS,smallFont);
        target.drawRect(pe, NsharpConstants.backgroundColor, 1.0f, 1.0f);
       // target.drawWireframeShape(presslinesNumbersShape, NsharpConstants.pressureColor, 1, LineStyle.SOLID,smallFont);
        target.drawWireframeShape(tempNumbersShape, NsharpConstants.color_white, 1, LineStyle.DEFAULT, smallFont);
        drawPressureLineNumber(target);
        target.clearClippingPlane();
        
    }
    //this function is used for printing
    public void paintForPrint( WGraphics world, GC gc){
    	try {
    		gc.setLineStyle(SWT.LINE_DASH);
    		printDryAdiabats(world, gc);
    		gc.setLineStyle(SWT.LINE_DOT);
    		printTempLines(world,gc);
		} catch (VizException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
    }
    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.skewt.ui.AbstractSkewTBackground#computeWorld()
     */
    @Override
    protected WGraphics computeWorld() {
        return world;
    }



	public void setGraphConfigProperty(NsharpGraphProperty graphConfigProperty) {
		this.graphConfigProperty = graphConfigProperty;
		createTempNumberAndLineShape();
	}
	@Override
    public double getViewableMaxPressure(){
    	IExtent ext = desc.getRenderableDisplay().getExtent();
    	double ymax = ext.getMaxY();
    	double xmin = ext.getMinX();  
    	Coordinate c = NsharpWxMath.reverseSkewTXY(world.unMap(xmin,ymax));
    	return c.y;
    }
    @Override
    public double getViewableMinPressure(){
    	IExtent ext = desc.getRenderableDisplay().getExtent();
    	double ymin = ext.getMinY();
    	double xmin = ext.getMinX();  
    	Coordinate c = NsharpWxMath.reverseSkewTXY(world.unMap(xmin,ymin));
    	return c.y;
    }
    @Override
    public double getYPositionRatioByPressure(double pressure){
    	Coordinate coorS = NsharpWxMath.getSkewTXY(pressure, 0);
        double rtP= world.mapY(coorS.y) ;
        coorS = NsharpWxMath.getSkewTXY(getViewableMaxPressure(), 0);
        double maxP= world.mapY(coorS.y) ;
        coorS = NsharpWxMath.getSkewTXY(getViewableMinPressure(), 0);
        double minP= world.mapY(coorS.y) ;
        double ratio = (rtP-minP) / (maxP-minP);
        //System.out.println("ratio="+ratio+ "rtP="+rtP+" maxP="+maxP+" minP="+minP);
        return ratio;
    }
    @Override
    public ViewablePressureContainer getViewablePressureContainer(List<NcSoundingLayer> soundingLys){
    	ViewablePressureContainer vpc = new ViewablePressureContainer();
    	IExtent ext = desc.getRenderableDisplay().getExtent();
    	float ymax = (float)ext.getMaxY();
    	float xmin = (float)ext.getMinX();  
    	float ymin =(float) ext.getMinY();
    	Coordinate cmax = NsharpWxMath.reverseSkewTXY(world.unMap(xmin,ymax));
    	Coordinate cmin = NsharpWxMath.reverseSkewTXY(world.unMap(xmin,ymin));
    	vpc.maxVIewablePressure = (float)cmax.y;
    	vpc.minVIewablePressure = (float)cmin.y;
    	double maxP= world.mapY(NsharpWxMath.getSkewTXY(cmax.y,0).y) ;
        double minP= world.mapY(NsharpWxMath.getSkewTXY(cmin.y,0).y) ;
    	for(NcSoundingLayer ly: soundingLys){
    		float pressure = ly.getPressure();
    		if(pressure >= cmin.y &&pressure <= cmax.y){
    			Coordinate coorS = NsharpWxMath.getSkewTXY(pressure, 0);
    	        double rtP= world.mapY(coorS.y) ;   	        
    	        float ratio = (float) (rtP-minP) /(float) (maxP-minP);
    	        vpc.pyMap.put(pressure, ratio);
    	       //System.out.println("skewT press="+pressure+" ratio="+ratio+ "rtP="+rtP+" maxP="+maxP+" minP="+minP);
    		}
    	}
    	return vpc;
    }
    
    @Override
    public ViewablePressureContainer getViewablePressureLinesContainer(){
		ViewablePressureContainer vpc = new ViewablePressureContainer();
    	IExtent ext = desc.getRenderableDisplay().getExtent();
    	float ymax = (float)ext.getMaxY();
    	float xmin = (float)ext.getMinX();  
    	float ymin =(float) ext.getMinY();
    	Coordinate cmax = NsharpWxMath.reverseSkewTXY(world.unMap(xmin,ymax));
    	Coordinate cmin = NsharpWxMath.reverseSkewTXY(world.unMap(xmin,ymin));
    	vpc.maxVIewablePressure = (float)cmax.y;
    	vpc.minVIewablePressure = (float)cmin.y;
    	double maxP= world.mapY(NsharpWxMath.getSkewTXY(cmax.y,0).y) ;
        double minP= world.mapY(NsharpWxMath.getSkewTXY(cmin.y,0).y) ;
        for (int i = 0; i < NsharpConstants.PRESSURE_NUMBERING_LEVELS.length; i++) {
    		float pressure = (float)NsharpConstants.PRESSURE_NUMBERING_LEVELS[i];
    		if(pressure >= cmin.y &&pressure <= cmax.y){
    			Coordinate coorS = NsharpWxMath.getSkewTXY(pressure, 0);
    	        double rtP= world.mapY(coorS.y) ;   	        
    	        float ratio = (float) (rtP-minP) /(float) (maxP-minP);
    	        vpc.pyMap.put(pressure, ratio);
    	       //System.out.println("skewT press="+pressure+" ratio="+ratio+ "rtP="+rtP+" maxP="+maxP+" minP="+minP);
    		}
    	}
    	return vpc;
    }
    /*
     * Called from handleResize() in skewTPaneResource only
     */
    public void handleResize(IExtent ext){
    	//IExtent ext = desc.getRenderableDisplay().getExtent();
    	//ext.reset();
    	if(mixRatioShape!=null)
    		mixRatioShape.dispose();
    	if(dryAdiabatsShape!=null)
    		dryAdiabatsShape.dispose();
    	if(moistAdiabatsShape!= null)
    		moistAdiabatsShape.dispose();
    	if(temperatureLineShape!=null)
    		temperatureLineShape.dispose();
    	if(presslinesNumbersShape!=null)
    		presslinesNumbersShape.dispose();
    	if(tempNumbersShape!=null)
    		tempNumbersShape.dispose();
    	this.rectangle = new Rectangle((int)ext.getMinX(), (int) ext.getMinY(),
    			(int) ext.getWidth(), (int) ext.getHeight());
    	pe = new PixelExtent(this.rectangle);
    	//desc.setNewPe(pe);
    	world = new WGraphics(this.rectangle);

    	world.setWorldCoordinates(NsharpConstants.left, NsharpConstants.top,
    			NsharpConstants.right, NsharpConstants.bottom);
    	float prevWidth = skewtWidth;
    	skewtWidth = (int) (ext.getWidth());
    	xRatio = xRatio* skewtWidth/prevWidth;

    	createMoistAdiabatsShape();
    	createDryAdiabatsShape();
    	createMixingRatioShape();
    	//createPressureLineNumberShape();
    	createTempNumberAndLineShape();
    }
    
    public void handleZooming(){
    	if(presslinesNumbersShape!=null)
    		presslinesNumbersShape.dispose();
    	if(mixRatioShape!=null)
    		mixRatioShape.dispose();
    	if(tempNumbersShape!=null)
    		tempNumbersShape.dispose();
    	createTempNumberAndLineShape();
    	createMixingRatioShape();
    	//createPressureLineNumberShape();
    }
    
    public double getWindBarbXPosition(){
		IExtent ext = desc.getRenderableDisplay().getExtent();
        double xmax = ext.getMaxX();  //Extent's viewable envelope min x and y
        double ymax = ext.getMaxY();
        double pX= world.mapX( NsharpConstants.right);
        if(pX < xmax)
        	xmax = pX;
        double windBarbSizfactor = graphConfigProperty.getWindBarbSize()/1.6f;
        if(windBarbSizfactor < 1)
        	windBarbSizfactor=1;
        double dispX = xmax - 40 * currentZoomLevel * xRatio * windBarbSizfactor;
        Coordinate cumap = world.unMap(dispX,ymax);
        
        return cumap.x;
	}
}
