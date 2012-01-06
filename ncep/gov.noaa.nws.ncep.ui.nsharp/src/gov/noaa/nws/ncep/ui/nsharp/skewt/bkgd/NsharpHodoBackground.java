/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.skewt.bkgd.NsharpHodoBackground
 * 
 * This java class performs the NSHARP NsharpHodoBackground functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 03/23/2010	229			Chin Chen	Initial coding
 * 										Reused some software from com.raytheon.viz.skewt.ui
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp.skewt.bkgd;

import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.skewt.NsharpSkewTDescriptor;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.common.sounding.WxMath;
import com.raytheon.viz.core.graphing.WGraphics;
import com.vividsolutions.jts.geom.Coordinate;

public class NsharpHodoBackground extends NsharpAbstractBackground {
	private PixelExtent pixExt;
	private IWireframeShape hodoShape=null;
    private NsharpSkewTDescriptor desc;
    /**
     * Public constructor
     * 
     * @param target
     * @param world
     * @param paintProps
     * @throws VizException
     */
    public NsharpHodoBackground(NsharpSkewTDescriptor desc) {
        super();
        this.rectangle = new Rectangle(NsharpConstants.HODO_REC_X_ORIG, NsharpConstants.HODO_REC_Y_ORIG,
        		NsharpConstants.HODO_REC_WIDTH, NsharpConstants.HODO_REC_HEIGHT);
        pixExt = new PixelExtent(this.rectangle);
        world = new WGraphics(this.rectangle);
        world.setWorldCoordinates(-70,70,70,-70);//(-50, 90, 90, -50);
        
        //System.out.println("NsharpHodoBackground constructed");
        this.desc = desc;
    }
    @Override
    public void disposeInternal(){
    	super.disposeInternal();
    	if(hodoShape!=null)
    		hodoShape.dispose();
    }
    @Override
    public void initInternal(IGraphicsTarget target){
    	super.initInternal(target);
    	//Create wireframe shape here
    	hodoShape = this.target.createWireframeShape(false,desc );
    	if(hodoShape==null)
    		return;
    	hodoShape.allocate(7200);
    	Coordinate c = new Coordinate(world.mapX(0), world.mapY(0));
        // rings are offset to left of center of hodograph.
        // in D2-D this is offset so the 45 knots ring touches the
        // left side of the display and the 90 knots touches the right side of
        // the display. We adjust our display to mimic this.
        // draw the spokes.
        //System.out.println("paintInternal Begin");
        for (double angle = 0; angle < 2 * Math.PI; angle += Math.PI / 2) {
            double x = 200 * Math.cos(angle);
            double y = 200 * Math.sin(angle);
            /*target.drawLine(c.x, c.y, 0.0, world.mapX(x), world.mapY(
                    y), 0.0, NsharpConstants.degreeSpokeColor, 1,
                    LineStyle.DOTTED);*/
            double [][] lines = {{c.x, c.y},{world.mapX(x), world.mapY(
                    y)}};
            hodoShape.addLineSegment(lines);
    		//System.out.println("c.x="+ c.x + "C.Y="+c.y + " x= " + x + " y= " + y +" world.mapX(x)="+ getWorld().mapX(x));

        }
        // label the spokes for direction
        /*double [] lblXy = {c.x,this.rectangle.y+25};
        hodoShape.addLabel("180", lblXy);
        double [] lblXy1= { c.x,this.rectangle.y + this.rectangle.height};
        hodoShape.addLabel("360", lblXy1);
        c = WxMath.uvComp(67, 90);
        double [] lblXy2= { getWorld().mapX(c.x), getWorld().mapY(c.y)};
        hodoShape.addLabel("90", lblXy2);
        c = WxMath.uvComp(65, 270);
        double [] lblXy3= { getWorld().mapX(c.x), getWorld().mapY(c.y)};
        hodoShape.addLabel("270", lblXy3);*/
     // draw circles
        for (int spd = 10; spd <= 100; spd += 10) {

            Coordinate c0, c1;
            c0 = WxMath.uvComp(spd, 0);
            for (int dir = 1; dir <= 360; dir += 1) {
                c1 = WxMath.uvComp(spd, dir);
                double [][] lines = {{getWorld().mapX(c0.x), getWorld().mapY(c0.y)},{getWorld().mapX(c1.x), getWorld().mapY(c1.y)}};
                hodoShape.addLineSegment(lines);
                
                c0 = c1;
            }
          //plot speed mark at 270 degree
            Coordinate uv = WxMath.uvComp(spd, 270); 
            if (spd != 0) {
                                
                double [] lblXys= { getWorld().mapX(uv.x),
                        getWorld().mapY(uv.y)};
                hodoShape.addLabel("" + spd, lblXys);
            }
          //plot speed mark at 90 degree
            uv = WxMath.uvComp(spd, 90); 
            if (spd != 0) {
                                
                double [] lblXys= { getWorld().mapX(uv.x),
                        getWorld().mapY(uv.y)};
                hodoShape.addLabel("" + spd, lblXys);
            }
            //plot speed mark at 180 degree
            uv = WxMath.uvComp(spd, 180); 
            if (spd != 0) {
                                
                double [] lblXys= { getWorld().mapX(uv.x),
                        getWorld().mapY(uv.y)};
                hodoShape.addLabel("" + spd, lblXys);
            }
            //plot speed mark at 360 degree
            uv = WxMath.uvComp(spd, 360); 
            if (spd != 0) {
                                
                double [] lblXys= { getWorld().mapX(uv.x),
                        getWorld().mapY(uv.y)};
                hodoShape.addLabel("" + spd, lblXys);
            }
        }
        hodoShape.compile();
    }
    
    @Override
    public void paintInternal(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
    	target.setupClippingPlane(pixExt);
        target.drawWireframeShape(hodoShape, NsharpConstants.backgroundColor, 1, LineStyle.SOLID,smallFont);

        target.drawRect(pixExt, NsharpConstants.backgroundColor, 1.0f, 1.0f);

        target.clearClippingPlane();
		
    }
    //this function is used for printing
    public void paintForPrint( WGraphics world, GC gc){
    	Coordinate c = new Coordinate(world.mapX(0), world.mapY(0));
    	// label the spokes
    	gc.drawString( "180" + NsharpConstants.DEGREE_SYMBOL,(int) c.x,
    			(int) world.mapY(world.getWorldYmin())+5, false);
    	
    	gc.drawString( "360" + NsharpConstants.DEGREE_SYMBOL,(int) c.x,
    			(int) world.mapY(world.getWorldYmax())+1, false);
    	
    	gc.drawString( "270" + NsharpConstants.DEGREE_SYMBOL,(int) world.mapX(world.getWorldXmax())-20,
    			(int) c.y,false);
    	
    	gc.drawString( "90" + NsharpConstants.DEGREE_SYMBOL,(int) world.mapX(world.getWorldXmin())+5,
    			(int) c.y,false);
   
    	
    	gc.setClipping((int)world.getViewXmin(), (int)world.getViewYmin(),
    			(int)world.getViewXmax()-(int)world.getViewXmin(), (int)world.getViewYmax()-(int)world.getViewYmin());
    	gc.setLineWidth(1);
    	//System.out.println(" line width = "+gc.getLineWidth());
    	// draw the spokes.
    	for (double angle = 0; angle < 2 * Math.PI; angle += Math.PI / 2) {
    		double x = 200 * Math.cos(angle);
    		double y = 200 * Math.sin(angle);
    		gc.drawLine((int)c.x, (int)c.y, (int)world.mapX(x), (int)world.mapY(y));
    		//System.out.println("c.x="+ c.x + "C.Y="+c.y + " x= " + x + " y= " + y +" world.mapX(x)="+ world.mapX(x));

    	}
    	
   	// draw circles
    	gc.setLineStyle(SWT.LINE_DOT);
    	for (int spd = 10; spd <= 100; spd += 10) {
    		//Coordinate c1;
    		//c1 = WxMath.uvComp(spd, 0);
    		//System.out.println("c1.x="+ c1.x + "C1.Y="+c1.y +" world.mapX(c1.x)="+ world.mapX(c1.x)+" world.mapY(c1.y)="+ world.mapY(c1.y));
    		int dist = (int)(world.mapX(spd)-c.x);
    		gc.drawOval((int)c.x-dist, (int)c.y-dist, 2* dist, 2* dist);
    		
    		if (spd%30 == 0) {
    			Coordinate uv = WxMath.uvComp(spd, 240);
        		
    			gc.drawString("" + spd, (int)world.mapX(uv.x),
    					(int)world.mapY(uv.y), false);
    		}
    	}


 
}

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.skewt.ui.AbstractSkewTBackground#computeWorld()
     */
    @Override
    protected WGraphics computeWorld() {

        
        //set coordinate from -50 to 90 knots on both x and y 
        
        return world;
    }

}
