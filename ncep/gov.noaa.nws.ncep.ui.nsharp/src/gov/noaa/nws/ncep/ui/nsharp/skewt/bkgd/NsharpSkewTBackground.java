/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.skewt.bkgd.NsharpSkewTBackground
 * 
 * This java class performs the NSHARP NsharpSkewTBackground functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 03/23/2010	229			Chin Chen	Initial coding
 * 									    Reused some software from com.raytheon.viz.skewt
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp.skewt.bkgd;

import static java.lang.Math.pow;
import static java.lang.Math.signum;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConfigManager;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.NsharpGraphProperty;
import gov.noaa.nws.ncep.ui.nsharp.NsharpWxMath;
import gov.noaa.nws.ncep.ui.nsharp.skewt.NsharpSkewTDescriptor;

import java.awt.geom.Line2D;
import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.edex.util.Equations;
import com.raytheon.edex.util.UAPoint;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.graphing.WGraphics;
import com.vividsolutions.jts.geom.Coordinate;

public class NsharpSkewTBackground extends NsharpAbstractBackground implements
        IRenderable {
	private NsharpSkewTDescriptor desc;
	private IWireframeShape mixRatioShape;
	private IWireframeShape dryAdiabatsShape;
	private IWireframeShape moistAdiabatsShape;
	private IWireframeShape temperatureLineShape;
	private IWireframeShape presslinesNumbersShape;
	private IWireframeShape tempNumbersShape;
	private float[]mixingRatios = 
            { .5f, 1, 2, 5, 10, 20/*, 50 */};

	private static List<List<UAPoint>> saturatedPoints = Equations
    .getSaturatedAdiabats(1000, 100, 20, -60, 60, 5);

	private static List<List<UAPoint>> dryPoints = getDryAdiabats(
    1000, 100, 10, -40, 273, 20);
    
	private static final double Rd = 0.2870586d; 
	private PixelExtent pe;
	private NsharpGraphProperty graphConfigProperty;
	private int tempOffset = 0;
    public NsharpSkewTBackground(NsharpSkewTDescriptor desc) {
        super();

        this.rectangle = new Rectangle(NsharpConstants.SKEWT_REC_X_ORIG, NsharpConstants.SKEWT_REC_Y_ORIG,
        		NsharpConstants.SKEWT_REC_WIDTH, NsharpConstants.SKEWT_REC_HEIGHT);
        //this.rectangle = new Rectangle(-700, 250, 690, 690);
        //this.rectangle = new Rectangle(10,10, 690,690);
        pe = new PixelExtent(this.rectangle);
        world = new WGraphics(this.rectangle);

        world.setWorldCoordinates(NsharpConstants.left, NsharpConstants.top,
                NsharpConstants.right, NsharpConstants.bottom);
        //System.out.println("NsharpSkewTBackground created");
        this.desc = desc;
        NsharpConfigManager configMgr = NsharpConfigManager.getInstance();
		graphConfigProperty = configMgr.retrieveNsharpConfigStoreFromFs().getGraphProperty();
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
    	
    	//temperature line shape
    	temperatureLineShape = target.createWireframeShape(false,desc );
    	temperatureLineShape.allocate(54);
    	for (int i = 70; i > -200; i -= 10) {
    		Coordinate coorStart = NsharpWxMath.getSkewTXY(1050, i);
    		Coordinate coorEnd = NsharpWxMath.getSkewTXY(100, i);
    		double [][] lines = {{world.mapX(coorStart.x), world.mapY(coorStart.y)},
    				{world.mapX(coorEnd.x), world.mapY(coorEnd.y)}};
    		temperatureLineShape.addLineSegment(lines);
    	}
    	temperatureLineShape.compile();
        	
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
        
    	//mixing ratio shape
    	// get the location of the 850 pressure line...
    	mixRatioShape = target.createWireframeShape(false,desc );
    	mixRatioShape.allocate(12);
    	Coordinate coorStart = NsharpWxMath.getSkewTXY(850, -50);
        Coordinate coorEnd = NsharpWxMath.getSkewTXY(850, 50);

        double startX = world.mapX(coorStart.x);
        double startY = world.mapY(coorStart.y);

        double endX = world.mapX(coorEnd.x);
        double endY = world.mapY(coorEnd.y);

        Line2D.Double line = new Line2D.Double(startX, startY, endX, endY);
        Line2D.Double line2 = new Line2D.Double();
        UAPoint p1 = new UAPoint();
        p1.pressure = 1000;
        UAPoint p2 = new UAPoint();
        p2.pressure = 400;
        String num="";
        for (float ratio : mixingRatios) {
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
            if ((point = getLineIntersection(line, line2)) != null) {
                double [] lblXy = {point.x, point.y};
                mixRatioShape.addLabel(num, lblXy);
            }
            num = "";
        }
        mixRatioShape.compile();
        
        //pressureLineNumberShape
        String s = "";
        presslinesNumbersShape = target.createWireframeShape(false,desc );
        presslinesNumbersShape.allocate(150);
        //System.out.println("NsharpConstants.left="+NsharpConstants.left+"NsharpConstants.right"+NsharpConstants.right+" top="+NsharpConstants.top + " bot="+ NsharpConstants.bottom);
        for (int i = 0; i < NsharpConstants.PRESSURE_MAIN_LEVELS.length; i++) {
        	//we only care about pressure for this case, temp is no important  when calling getSkewTXY
        	Coordinate coor = NsharpWxMath.getSkewTXY(NsharpConstants.PRESSURE_MAIN_LEVELS[i],0);

        	double [][] lines = {{world.mapX(NsharpConstants.left), world.mapY(coor.y)},{world.mapX(NsharpConstants.right),
        		world.mapY(coor.y)}};
        	presslinesNumbersShape.addLineSegment(lines);
        	//System.out.println("coor.x="+coor.x+"coor.y="+coor.y);
        	//System.out.println("x1="+world.mapX(NsharpConstants.left)+"y1=" +world.mapY(coor.y)+"x2="+world.mapX(NsharpConstants.right)+"y2="+
            //		world.mapY(coor.y));
        }
        for (int i = 0; i < NsharpConstants.PRESSURE_MARK_LEVELS.length; i++) {
        	//we only care about pressure for this case, temp is no important  when calling getSkewTXY
        	Coordinate coor = NsharpWxMath.getSkewTXY(NsharpConstants.PRESSURE_MARK_LEVELS[i],0);

        	double [][] lines = {{world.mapX(NsharpConstants.left), world.mapY(coor.y)},{ world.mapX(NsharpConstants.left)+10,
        		world.mapY(coor.y)}};
        	presslinesNumbersShape.addLineSegment(lines);
        }
        for (int i = 0; i < NsharpConstants.PRESSURE_NUMBERING_LEVELS.length; i++) {
        	s = NsharpConstants.pressFormat.format(NsharpConstants.PRESSURE_NUMBERING_LEVELS[i]);
        	//we only care about pressure for this case, temp is no important  when calling getSkewTXY
        	Coordinate coor = NsharpWxMath.getSkewTXY(NsharpConstants.PRESSURE_NUMBERING_LEVELS[i],0);

        	double [] lblXy = {world.mapX(NsharpConstants.left)-30,world.mapY(coor.y)+5};
        	presslinesNumbersShape.addLabel(s, lblXy);

        }
        presslinesNumbersShape.compile();
            
        createTempNumberShape();
    }
    private void createTempNumberShape(){
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
        //int lowTemp = 
		for (int i = 50+tempOffset; i > -40+tempOffset; i -= 10) {
            Coordinate coorS = NsharpWxMath.getSkewTXY(1050, i);
            double startX1 = world.mapX(coorS.x);
            double startY1 = world.mapY(coorS.y);
            double [] lblXy = {startX1,startY1+20};
            tempNumbersShape.addLabel(Integer.toString(i), lblXy);
        }
        for (int i = -30+tempOffset; i > -110+tempOffset; i -= 10) {
            Coordinate coorEnd1 = NsharpWxMath.getSkewTXY(100, i);

            //System.out.println("X = "+ startX + " Y = "+ startY);
            double endX1 = world.mapX(coorEnd1.x);
            double endY1 = world.mapY(coorEnd1.y);

            double [] lblXy = {endX1,endY1-10};
            tempNumbersShape.addLabel(Integer.toString(i), lblXy);
        }
        tempNumbersShape.compile();
    }

    @Override
    public void paintInternal(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {

        target.setupClippingPlane(pe);
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
        target.clearClippingPlane();
        target.drawWireframeShape(presslinesNumbersShape, NsharpConstants.pressureColor, 1, LineStyle.SOLID,smallFont);
        target.drawWireframeShape(tempNumbersShape, NsharpConstants.pressureColor, 1, LineStyle.DEFAULT, smallFont);
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
		createTempNumberShape();
	}

    
}
