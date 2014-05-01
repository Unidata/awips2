/*****************************************************************************************
 * COPYRIGHT (c), 2006-2008, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/
package gov.noaa.nws.ncep.viz.rsc.ncgrid.contours;

import java.nio.FloatBuffer;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.ReferencedObject.Type;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.vividsolutions.jts.geom.Coordinate;

import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

/**
 * Display grid point values
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * June, 2010    164        M. Li     	Initial creation
 * October,2011             X. Guo      Display grid point in GRID_CENTER
 * Apr, 2013				B. Yin		Don't plot missing values
 * 
 * </pre>
 * 
 * @author mli
 * @version 1.0
 */

public class GridPointValueDisplay implements IRenderable {

    private final IMapDescriptor descriptor;

    private final GeneralGridGeometry gridGeometryOfGrid;

    private final int[] gridDims;

    private RGB color;

    private FloatBuffer displayValues;

    private IFont font;

    public GridPointValueDisplay(FloatBuffer values, RGB color,
            IMapDescriptor descriptor, ISpatialObject gridLocation) {

        this.displayValues = values;
        this.descriptor = descriptor;
        this.gridGeometryOfGrid = MapUtil.getGridGeometry(gridLocation);
        this.gridDims = new int[] { gridLocation.getNx(), gridLocation.getNy() };
        this.color = color;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.drawables.IRenderable#paint(com.raytheon.viz.core
     * .IGraphicsTarget, com.raytheon.viz.core.drawables.PaintProperties)
     */
    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {

        if (paintProps.isZooming()) {
            return;
        }
        font = target.initializeFont("Monospace", 10,
                new IFont.Style[] { IFont.Style.BOLD });

        IExtent screenExtentInPixels = paintProps.getView().getExtent();

        double ratio = screenExtentInPixels.getWidth()
                / paintProps.getCanvasBounds().width;
        int interval0 = (int) (10 * ratio / (Math.min(2.0, 1.0/*
                                                               * paintProps.
                                                               * getDensity()
                                                               */)));
        // int interv = (int) ((interval0 + 15) / 15.0);
        int interv = (int) Math.log(interval0);
        if (interv < 0)
            interv = 0;
        if (interv > 5)
            interv = 5;
        interval0 = 1;

        for (int i = 0; i < gridDims[0]; i += interv + 1) {

            for (int j = 0; j < gridDims[1]; j += interv + 1) {
            	if (displayValues.get((i + (j * this.gridDims[0])) ) == IDecoderConstantsN.GRID_MISSING) continue;
 	
                ReferencedCoordinate c = new ReferencedCoordinate(
                        new Coordinate(i, j), this.gridGeometryOfGrid,
                        Type.GRID_CENTER);

                /*
                 * check map area
                 */
                try {
                    double[] d = this.descriptor.worldToPixel(new double[] {
                            (double) c.asLatLon().x, (double) c.asLatLon().y });

                    if (d == null || !screenExtentInPixels.contains(d)) {
                        continue;
                    }

                    /*
                     * Plot grid point values
                     */
                    String text = getValueString(i, j);

                    target.drawString(font, text, d[0], d[1], 0.0,
                            TextStyle.NORMAL, color,
                            HorizontalAlignment.CENTER,
                            VerticalAlignment.MIDDLE, 0.0);
                } catch (TransformException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                } catch (FactoryException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }
        }
    }

    private String getValueString(int x, int y) {
        if (x >= this.gridDims[0] || y >= this.gridDims[1] || x < 0 || y < 0) {
            return null;
        }

        int idx = (x + (y * this.gridDims[0]));
        float value = this.displayValues.get(idx);

        if (Float.isNaN(value)) {
            return null;
        }

        return String.valueOf((int) value);
    }

    public void dispose() {

        if (font != null) {
            font.dispose();
        }

    }
}
