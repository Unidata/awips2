package gov.noaa.nws.ncep.viz.tools.aodt;


import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * AODT Resource
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 08/24/09		150			M. Li		Created
 * 
 * </pre>
 * 
 * @version 1
 */
public class AODTResource extends AbstractVizResource<AODTResourceData,MapDescriptor> {

    private RGB color = new RGB(255, 255, 0);
    
    private Coordinate seldLoc = null;
    
    public AODTResource(AODTResourceData resourceData,
			LoadProperties loadProperties) {
    	super(resourceData, loadProperties);
    }

    public void setSelectedLoc( Coordinate c1 ) {
    	seldLoc = c1;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#getName()
     */
    public String getName() {
        return new String("");
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.core.rsc.IVizResource#init(com.raytheon.viz.core.
     * IGraphicsTarget)
     */
    public void initInternal(IGraphicsTarget target) throws VizException {
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.IVizResource#isApplicable(com.raytheon.viz.
     * core.PixelExtent)
     */
    public boolean isApplicable(PixelExtent anExtent) {
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#paint(com.raytheon.viz.core.
     * IGraphicsTarget, com.raytheon.viz.core.PixelExtent, double, float)
     */
    public void paintInternal(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
    	if( seldLoc != null ) {
    		float zoomLevel = paintProps.getZoomLevel();
    		int d = (int)(200 * zoomLevel + 1.0);

    		// Draw end point cross
    		double[] p1 = descriptor.worldToPixel(new double[] { seldLoc.x, seldLoc.y });
    		if ( p1 == null) return;
    		target.drawLine( p1[0]+d, p1[1],   0.0, p1[0]-d, p1[1], 0.0,  color, 1.0f );
    		target.drawLine( p1[0],   p1[1]+d, 0.0, p1[0],   p1[1]-d, 0.0, color, 1.0f );
    	}
    }

   
     /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#dispose()
     */
    public void disposeInternal() {
    }

   

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.capabilities.IColorableResource#getColor()
     */
    public RGB getColor() {
        return color;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.capabilities.IColorableResource#setColor(org
     * .eclipse.swt.graphics.RGB)
     */
    public void setColor(RGB c) {
        color = c;
    }

  
}
