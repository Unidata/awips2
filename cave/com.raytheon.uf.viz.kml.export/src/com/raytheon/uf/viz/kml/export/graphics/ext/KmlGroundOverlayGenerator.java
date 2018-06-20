/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.viz.kml.export.graphics.ext;

import java.awt.image.RenderedImage;
import java.nio.FloatBuffer;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.Envelope2D;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.common.colormap.image.Colormapper;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.geospatial.interpolation.BilinearInterpolation;
import com.raytheon.uf.common.geospatial.interpolation.GridReprojection;
import com.raytheon.uf.common.geospatial.interpolation.Interpolation;
import com.raytheon.uf.common.geospatial.interpolation.NearestNeighborInterpolation;
import com.raytheon.uf.common.geospatial.interpolation.PrecomputedGridReprojection;
import com.raytheon.uf.common.numeric.buffer.FloatBufferWrapper;
import com.raytheon.uf.common.numeric.source.DataSource;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.kml.export.KmlFeatureGenerator;
import com.raytheon.uf.viz.kml.export.io.KmlOutputManager;

import de.micromata.opengis.kml.v_2_2_0.GroundOverlay;
import de.micromata.opengis.kml.v_2_2_0.LatLonBox;

/**
 * Base class for any feature generator that is creating a ground overlay,
 * provides the reproject logic as well as logic for actually making the
 * overlay.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 14, 2012            bsteffen    Initial creation
 * Jul 17, 2013 2185       bsteffen    Cache computed grid reprojections.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public abstract class KmlGroundOverlayGenerator extends KmlFeatureGenerator {

    protected final double alpha;

    protected final DrawableImage[] images;

    public KmlGroundOverlayGenerator(double alpha, DrawableImage[] images) {
        this.images = new DrawableImage[images.length];
        for (int i = 0; i < images.length; i += 1) {
            // Clone the DrawableImage so that if whatever is drawing modifies
            // or disposes of it then when the generator runs it will still draw
            // what was rendered.
            PixelCoverage oldCov = images[i].getCoverage();
            PixelCoverage newCov = new PixelCoverage(oldCov.getUl(),
                    oldCov.getUr(), oldCov.getLr(), oldCov.getLl());
            newCov.setMesh(oldCov.getMesh());
            this.images[i] = new DrawableImage(images[i].getImage(), newCov,
                    images[i].getMode());
        }
        this.alpha = alpha;
    }

    protected void makeOverlay(KmlOutputManager out, GridGeometry2D geometry,
            float[] fdata, ColorMapParameters parameters) {
        GridEnvelope2D range = geometry.getGridRange2D();
        int[] dimensions = { range.width, range.height };
        ColorMapData data = new ColorMapData(FloatBuffer.wrap(fdata),
                dimensions);
        RenderedImage image = Colormapper.colorMap(data, parameters);
        makeOverlay(out, image, geometry.getEnvelope2D());
    }

    protected void makeOverlay(KmlOutputManager out, RenderedImage image,
            Envelope2D envelope) {
        GroundOverlay overlay = new GroundOverlay();
        overlay.setColor(toColorStr(alpha, new RGB(255, 255, 255)));
        overlay.setName("Image");
        overlay.createAndSetIcon().setHref(out.addImage(image));
        LatLonBox box = overlay.createAndSetLatLonBox();
        box.setNorth(envelope.getMaxY());
        box.setEast(envelope.getMaxX());
        box.setSouth(envelope.getMinY());
        box.setWest(envelope.getMinX());
        out.addFeature(overlay);
    }

    protected float[] reproject(GridGeometry2D src, GridGeometry2D dest,
            DataSource data, Boolean interpolated) throws FactoryException,
            TransformException {
        GridReprojection reproj = getReprojection(src, dest);
        Interpolation interp = null;
        if (interpolated) {
            interp = new BilinearInterpolation();
        } else {
            interp = new NearestNeighborInterpolation();
        }
        FloatBufferWrapper dst = new FloatBufferWrapper(dest.getGridRange2D());
        return reproj.reprojectedGrid(interp, data, dst).getArray();
    }

    protected GridReprojection getReprojection(GridGeometry2D src,
            GridGeometry2D dest) throws TransformException {
        return PrecomputedGridReprojection.getReprojection(src, dest);
    }
}
