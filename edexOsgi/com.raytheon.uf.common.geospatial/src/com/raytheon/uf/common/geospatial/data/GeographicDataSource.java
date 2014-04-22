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
package com.raytheon.uf.common.geospatial.data;

import java.nio.Buffer;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.coverage.grid.GridGeometry;

import com.raytheon.uf.common.geospatial.util.GridGeometryWrapChecker;
import com.raytheon.uf.common.numeric.buffer.BufferWrapper;
import com.raytheon.uf.common.numeric.filter.DataFilter;
import com.raytheon.uf.common.numeric.source.DataSource;
import com.raytheon.uf.common.numeric.source.FilteredDataSource;

/**
 * A data source with a grid geometry. Grids that "wrap" around the world allow
 * indexing outside the normal x range to wrap around the world.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Mar 06, 2014  2791     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GeographicDataSource implements DataSource {

    protected final DataSource wrappedSource;

    protected final GridGeometry2D gridGeometry;

    protected final int nx;

    protected final int wrapX;

    public GeographicDataSource(DataSource wrappedSource, GridGeometry geometry) {
        this.wrappedSource = wrappedSource;
        this.gridGeometry = GridGeometry2D.wrap(geometry);
        this.nx = this.gridGeometry.getGridRange2D().width;
        this.wrapX = GridGeometryWrapChecker
                .checkForWrapping(this.gridGeometry);
    }

    public GeographicDataSource(Buffer buffer, GeneralGridGeometry geometry) {
        this.gridGeometry = GridGeometry2D.wrap(geometry);
        this.nx = this.gridGeometry.getGridRange2D().width;
        this.wrapX = GridGeometryWrapChecker
                .checkForWrapping(this.gridGeometry);
        int ny = this.gridGeometry.getGridRange2D().height;
        this.wrappedSource = BufferWrapper.wrap(buffer, nx, ny);

    }

    public GeographicDataSource(Class<? extends Number> primitiveType,
            GeneralGridGeometry geometry) {
        this.gridGeometry = GridGeometry2D.wrap(geometry);
        this.nx = this.gridGeometry.getGridRange2D().width;
        this.wrapX = GridGeometryWrapChecker
                .checkForWrapping(this.gridGeometry);
        int ny = this.gridGeometry.getGridRange2D().height;
        this.wrappedSource = BufferWrapper.create(primitiveType, nx, ny);
    }

    protected GeographicDataSource(DataSource wrappedSource,
            GridGeometry2D gridGeometry, int nx, int wrapX) {
        this.wrappedSource = wrappedSource;
        this.gridGeometry = gridGeometry;
        this.nx = nx;
        this.wrapX = wrapX;
    }

    protected int fixX(int x) {
        if (wrapX > 0 && (x < 0 || x > nx - 1)) {
            while (x > nx) {
                x -= wrapX;
            }
            while (x < 0) {
                x += wrapX;
            }
        }
        return x;
    }

    @Override
    public double getDataValue(int x, int y) {
        return wrappedSource.getDataValue(fixX(x), y);
    }

    public DataSource getWrappedSource() {
        return wrappedSource;
    }

    public GridGeometry2D getGridGeometry() {
        return gridGeometry;
    }

    /**
     * Create a new GeographicDataSource that is a copy of this source but with
     * additional filters. Since a FilteredDataSource is not geographic this is
     * the prefered method of adding filters while preserving geographic
     * attributes.
     * 
     * @param filters
     * @return
     */
    public GeographicDataSource applyFilters(DataFilter... filters) {
        DataSource newWrappedSource = FilteredDataSource.addFilters(
                wrappedSource, filters);
        return new GeographicDataSource(newWrappedSource, gridGeometry, nx,
                wrapX);
    }

    /**
     * Create a new GeographicDataSource only if the provided source is not
     * already a GeographicDataSource.
     * 
     * @param source
     *            a DataSource
     * @param geometry
     *            The geometry of the source
     * @return Either a new GeographicDataSource or source cast to a
     *         GeographicDataSource.
     */
    public static GeographicDataSource wrap(DataSource source,
            GridGeometry geometry) {
        if (source instanceof GeographicDataSource) {
            return (GeographicDataSource) source;
        } else {
            return new GeographicDataSource(source, geometry);
        }
    }

}
