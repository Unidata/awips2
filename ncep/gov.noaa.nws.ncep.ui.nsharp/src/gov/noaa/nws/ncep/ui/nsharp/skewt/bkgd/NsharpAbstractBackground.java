/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.skewt.bkgd.NsharpAbstractSkewTBackground
 * 
 * This java class performs the NSHARP NsharpAbstractSkewTBackground functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 03/23/2010	229			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp.skewt.bkgd;

import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;

import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.graphing.WGraphics;
import com.vividsolutions.jts.geom.Coordinate;

public abstract class NsharpAbstractBackground implements IRenderable {
    protected Rectangle rectangle;

    protected WGraphics world;
    protected IGraphicsTarget target;
    protected IFont smallFont=null;
    //private double currentZoomLevel=1;
    //private int currentFontSize= 10;
    
    
	public void setCurrentFont(float currentFontSize) {
		//this.currentFontSize = currentFontSize;
		if(smallFont!=null){
			smallFont.dispose();
		}
		if(target==null)
			return;
		smallFont = target.initializeFont(
                target.getDefaultFont().getFontName(), currentFontSize, null);
	}


	public void magnifyFont(double currentZoomLevel) {
		//this.currentZoomLevel = currentZoomLevel;

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

    public NsharpAbstractBackground() {
		super();
		
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
    protected void disposeInternal(){
    	if(smallFont != null)
    		smallFont.dispose();
    }
    

    protected abstract WGraphics computeWorld();
}
