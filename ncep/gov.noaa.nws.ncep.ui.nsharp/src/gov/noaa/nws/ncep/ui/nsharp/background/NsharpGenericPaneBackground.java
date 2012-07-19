/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.background.NsharpGenericPaneBackground
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

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.display.NsharpAbstractPaneDescriptor;

import java.util.HashMap;
import java.util.List;

import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.graphing.WGraphics;
import com.vividsolutions.jts.geom.Coordinate;

public class NsharpGenericPaneBackground implements IRenderable {
    protected Rectangle rectangle;
    protected NsharpAbstractPaneDescriptor desc;
    protected WGraphics world;
    protected IGraphicsTarget target;
    protected IFont smallFont=null;
    protected PixelExtent pe;
    //private double currentZoomLevel=1;
    //private int currentFontSize= 10;
	public NsharpGenericPaneBackground() {
		super();
		// TODO Auto-generated constructor stub
	}
	public NsharpGenericPaneBackground(Rectangle rect) {
		super();
		this.rectangle = rect;
		world = new WGraphics(this.rectangle);

        world.setWorldCoordinates(rectangle.x,rectangle.y,
        		rectangle.x + rectangle.width, rectangle.y+rectangle.height);
	}
	public double toLogScale (double j) {
		  return(Math.log(j)/Math.log(1000)*1000);
	}
	public double reverseLogScale (double j) {
		  return(Math.exp(Math.log(1000)*j/1000));
	}
	public PixelExtent getPe() {
		return pe;
	}
	public void setCurrentFont(float currentFontSize) {
		//this.currentFontSize = currentFontSize;
		if(smallFont!=null){
			smallFont.dispose();
		}
		smallFont = target.initializeFont(
                target.getDefaultFont().getFontName(), currentFontSize, null);
	}


	public void magnifyFont(double currentZoomLevel) {
		float magFactor = 1.0f / (float)currentZoomLevel;
		if(smallFont!= null)
			smallFont.setMagnification(magFactor);
	}


	/**
     * Find a point on the background
     * 
     * @param c
     * @return
     */
    public boolean contains(Coordinate c) {
        return this.rectangle.contains((int) c.x, (int) c.y);
    }

     
	public Rectangle getRectangle() {
        return this.rectangle;
    }

    public WGraphics getWorld() {
         return this.world;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.IRenderable#paint(com.raytheon.uf.
     * viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
    	/*double zoomLevel = paintProps.getZoomLevel();
    	if(zoomLevel != currentZoomLevel){
    		float magFactor = 1.0f / (float)zoomLevel;
    		if(smallFont!= null)
    			smallFont.setMagnification(magFactor);
    	}*/
    	paintInternal(target, paintProps);

       
    }
    public synchronized void initInternal(IGraphicsTarget target){
    	this.target = target;
    	smallFont = target.initializeFont(
                target.getDefaultFont().getFontName(), 10, null);
    }
    protected void paintInternal(IGraphicsTarget target,
			PaintProperties paintProps) throws VizException {
		PixelExtent pixExt = new PixelExtent(this.rectangle);
		target.setupClippingPlane(pixExt);

		target.drawRect(pixExt, NsharpConstants.backgroundColor, 1.0f, 1.0f);

		target.clearClippingPlane();

	}
    public void disposeInternal(){
    	if(smallFont != null)
    		smallFont.dispose();
    }
    

    protected  WGraphics computeWorld(){
    	return world;
    }
    public class ViewablePressureContainer{
    	float maxVIewablePressure;
    	float minVIewablePressure;
     	//Map: key :: double pressure;
		//     value::double  yPositionRatio; // between 0 to 1,  0=at ymin and  1= at ymax
    	HashMap<Float, Float>pyMap = new HashMap<Float, Float>();
		public float getMaxVIewablePressure() {
			return maxVIewablePressure;
		}
		public float getMinVIewablePressure() {
			return minVIewablePressure;
		}
		public HashMap<Float, Float> getPyMap() {
			return pyMap;
		}
    	
    }
	
   public double getViewableMaxPressure() {
		return 0;
	}
	public double getViewableMinPressure() {
		return 0;
	}
	public double getYPositionRatioByPressure(double pressure) {
		return 0;
	}
	public ViewablePressureContainer getViewablePressureContainer(
			List<NcSoundingLayer> soundingLys) {
		return null;
	}
	public ViewablePressureContainer getViewablePressureLinesContainer(){
		return null;
    }
	public void handleResize(Rectangle rectangle){
    	if(target==null)
 			return;
    	this.rectangle = rectangle;
         pe = new PixelExtent(this.rectangle);
         //desc.setNewPe(pe);
         world = new WGraphics(this.rectangle);
         world.setWorldCoordinates(rectangle.x,rectangle.y,
         		rectangle.x + rectangle.width, rectangle.y+rectangle.height);
	}
	
}
