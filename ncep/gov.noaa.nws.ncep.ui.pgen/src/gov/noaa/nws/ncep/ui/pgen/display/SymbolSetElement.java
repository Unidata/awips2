/*
 * SymbolSetElement
 * 
 * Date created: 20 APRIL 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.display;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Contains a raster image and information needed to readily display that image
 * on a graphics target at one or more locations.
 * <P>
 * Objects of this class are typically created from Symbol or SymbolLocationSet
 * elements using the DisplayElementFactory class.
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 
 * 04/22/09       ?        S. Gilbert	Initial coding
 * 08/06/13       ?        J. Wu		Add Matt's (Hazard Services) changes 
 * 										to speed up the performance.
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
    public SymbolSetElement(IImage raster, double[][] locations) {
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
        double screenToWorldRatio = paintProps.getCanvasBounds().width
                / paintProps.getView().getExtent().getWidth();
        double scale = 1 / screenToWorldRatio;

		/*
         * Add image at each location to the list
         */
        List<DrawableImage> images = new ArrayList<DrawableImage>();
        for (int j = 0; j < locations.length; j++) {
            loc = locations[j];
            PixelCoverage extent = new PixelCoverage(new Coordinate(loc[0],
                    loc[1]), raster.getWidth() * scale, raster.getHeight()
                    * scale);
            images.add(new DrawableImage(raster, extent));
            }
        
        //Draw all images.
        try {
            target.drawRasters(paintProps, images.toArray(new DrawableImage[0]));
        } catch (VizException e) {
            e.printStackTrace();
        }
    }

}
