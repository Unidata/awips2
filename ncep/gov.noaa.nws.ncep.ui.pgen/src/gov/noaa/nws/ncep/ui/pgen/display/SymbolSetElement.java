/*
 * SymbolSetElement
 * 
 * Date created: 20 APRIL 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.display;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.gl.images.GLImage;
import com.vividsolutions.jts.geom.Coordinate;
/**
 * Contains a raster image and information needed to readily display that image on a graphics target
 * at one or more locations.
 * <P>
 * Objects of this class are typically created from Symbol or SymbolLocationSet elements using the 
 * DisplayElementFactory class.
 * @author sgilbert
 *
 */
public class SymbolSetElement implements IDisplayable {

    /*
     * The raster image to be displayed
     */
    private final IImage raster;

    /*
     * Array of plot locations in pixel coordinates
     */
    private final double[][] locations;

    private final Coordinate ul = new Coordinate(), ur = new Coordinate(),
            lr = new Coordinate(), ll = new Coordinate();

    /**
     * Constructor used to set an image and its associated locations
     * 
     * @param raster
     *            Rater image to display
     * @param paintProps
     *            paint properties for the target
     * @param locations
     *            pixel coordinate locations to display the image
     */
    public SymbolSetElement(IImage raster,
            double[][] locations) {
        this.raster = raster;
        this.locations = locations;
    }

	/**
     * disposes the resources held by the raster image
     * 
     * @see gov.noaa.nws.ncep.ui.pgen.display.IDisplayable#dispose()
     */
    @Override
    public void dispose() {

		raster.dispose();

	}

	/**
     * Plots the image to the specified graphics target at the various locations
     * 
     * @see gov.noaa.nws.ncep.ui.pgen.display.IDisplayable#draw(com.raytheon.viz.core.IGraphicsTarget)
     */
    @Override
    public void draw(IGraphicsTarget target, PaintProperties paintProps) {

		double[] loc = new double[3];

		/*
         * Scale image
         */
        double halfWidth;
        double halfHeight;
        double screenToWorldRatio = paintProps.getCanvasBounds().width
                / paintProps.getView().getExtent().getWidth();
        double scale = 0.5 / screenToWorldRatio;
        if (raster instanceof GLImage) {
            halfWidth = ((GLImage) raster).getImage().getWidth() * scale;
            halfHeight = ((GLImage) raster).getImage().getHeight() * scale;
        } else {
            halfWidth = raster.getWidth() * scale;
            halfHeight = raster.getHeight() * scale;
        }

		/*
         * draw raster image at each location
         */
        for (int j = 0; j < locations.length; j++) {
            loc = locations[j];
            ul.x = loc[0] - halfWidth;
            ul.y = loc[1] - halfHeight;
            ur.x = loc[0] + halfWidth;
            ur.y = loc[1] - halfHeight;
            lr.x = loc[0] + halfWidth;
            lr.y = loc[1] + halfHeight;
            ll.x = loc[0] - halfWidth;
            ll.y = loc[1] + halfHeight;
            PixelCoverage extent = new PixelCoverage(ul, ur, lr, ll);
            /*
             * PixelCoverage extent = new PixelCoverage(new
             * Coordinate(loc[0]-halfWidth,loc[1]-halfHeight), new
             * Coordinate(loc[0]+halfWidth,loc[1]-halfHeight), new
             * Coordinate(loc[0]+halfWidth,loc[1]+halfHeight), new
             * Coordinate(loc[0]-halfWidth,loc[1]+halfHeight));
             */
            try {
                target.drawRaster(raster, extent, paintProps);
            } catch (VizException ve) {
                ve.printStackTrace();
            }
        }
    }

}
