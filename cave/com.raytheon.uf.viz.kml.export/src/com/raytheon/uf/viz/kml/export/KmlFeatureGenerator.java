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
package com.raytheon.uf.viz.kml.export;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.TransformFactory;
import com.raytheon.uf.viz.kml.export.io.KmlOutputManager;

import de.micromata.opengis.kml.v_2_2_0.Coordinate;

/**
 * Anything that can be drawn on the screen can also be used to create a KML
 * feature, this class provides some basic utility functions as well as an
 * interface for classes that generate KML.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 14, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public abstract class KmlFeatureGenerator {

    protected GridGeometry2D gridGeometry;

    protected MathTransform gridToLatLon;

    protected RGB backgroundColor;

    protected KmlExportOptions options;

    public void setGridGeometry(GeneralGridGeometry gridGeometry) {
        this.gridGeometry = GridGeometry2D.wrap(gridGeometry);
    }

    public void setBackgroundColor(RGB backgroundColor) {
        this.backgroundColor = backgroundColor;
    }

    public void setOptions(KmlExportOptions options) {
        this.options = options;
    }

    public Coordinate transformToLatLon(double gridX, double gridY)
            throws TransformException, FactoryException {
        return transformToLatLon(new double[] { gridX, gridY });
    }

    public Coordinate transformToLatLon(double[] gridPixel)
            throws TransformException, FactoryException {
        if (gridToLatLon == null) {
            gridToLatLon = TransformFactory.gridToLatLon(gridGeometry,
                    PixelInCell.CELL_CENTER);
        }
        double[] out = new double[2];
        gridToLatLon.transform(gridPixel, 0, out, 0, 1);
        return new Coordinate(out[0], out[1]);
    }

    public List<Coordinate> transformToLatLon(List<double[]> gridPixels)
            throws TransformException, FactoryException {
        List<Coordinate> result = new ArrayList<Coordinate>();
        for (double[] gridPixel : gridPixels) {
            result.add(transformToLatLon(gridPixel));
        }
        return result;
    }

    public abstract void addFeature(KmlOutputManager outputManager);

    public static String toColorStr(double alpha, RGB rgb) {
        return String.format("%02x%02x%02x%02x", (int) (alpha * 255), rgb.blue,
                rgb.green, rgb.red);
    }

}
