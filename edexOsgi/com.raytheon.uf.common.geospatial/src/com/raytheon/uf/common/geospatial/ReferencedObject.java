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
package com.raytheon.uf.common.geospatial;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

/**
 * Represents an abstract object that can be transformed from any reference
 * system to another
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 30, 2008            chammack     Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public abstract class ReferencedObject<T> {
    public static enum Type {
        CRS, GRID_CENTER, GRID_CORNER, LAT_LON
    }

    private static enum CoordinateType {
        GRID_CRS_NATIVE, MAP_CRS_NATIVE, GRID, LAT_LON, PIXEL
    };

    protected T internalObject;

    private PixelInCell orientation;

    private CoordinateType type;

    private GeneralGridGeometry gridGeometry;

    /**
     * Create object as a map crs native object
     * 
     * @param obj
     * @param mapGeometry
     * @return
     */
    public ReferencedObject(T obj, GeneralGridGeometry mapGeometry) {
        this.internalObject = obj;
        this.type = CoordinateType.MAP_CRS_NATIVE;
        this.gridGeometry = mapGeometry;
    }

    /**
     * Create object as grid crs native or grid cell native
     * 
     * @param obj
     * @param geometry
     * @param type
     * 
     */
    public ReferencedObject(T obj, GeneralGridGeometry geometry, Type type) {
        this.internalObject = obj;
        if (type == Type.CRS) {
            this.type = CoordinateType.GRID_CRS_NATIVE;
        } else if (type == Type.GRID_CENTER) {
            this.type = CoordinateType.GRID;
            this.orientation = PixelInCell.CELL_CENTER;
        } else if (type == Type.GRID_CORNER) {
            this.type = CoordinateType.GRID;
            this.orientation = PixelInCell.CELL_CORNER;
        } else if (type == Type.LAT_LON) {
            this.type = CoordinateType.LAT_LON;
        } else {
            throw new IllegalArgumentException("Unsupported type: " + type);
        }
        this.gridGeometry = geometry;
    }

    /**
     * Create an object as a lat lon object
     * 
     * @param obj
     */
    public ReferencedObject(T obj) {
        this.internalObject = obj;
        this.type = CoordinateType.LAT_LON;
    }

    /**
     * Create an object as a pixel object
     * 
     * @param mapGeometry
     * @param obj
     */
    public ReferencedObject(GeneralGridGeometry mapGeometry, T obj) {
        this.internalObject = obj;
        this.type = CoordinateType.PIXEL;
        this.orientation = PixelInCell.CELL_CENTER;
        this.gridGeometry = mapGeometry;
    }

    protected abstract T transform(MathTransform mt) throws TransformException;

    public T asLatLon() throws TransformException, FactoryException {
        switch (this.type) {
        case LAT_LON:
            return internalObject;
        case GRID_CRS_NATIVE:
        case MAP_CRS_NATIVE:
            MathTransform mt = CRSCache.getInstance().getTransformToLatLon(
                    this.gridGeometry.getCoordinateReferenceSystem());
            return transform(mt);
        case GRID:
        case PIXEL:
            mt = TransformFactory.gridToLatLon(gridGeometry, this.orientation);
            return transform(mt);
        default:
            return null;
        }

    }

    public T asPixel(GeneralGridGeometry mapGeometry)
            throws TransformException, FactoryException {
        MathTransform mt = null;
        switch (this.type) {
        case LAT_LON:
            mt = TransformFactory.latLonToGrid(mapGeometry,
                    PixelInCell.CELL_CENTER);
            return transform(mt);
        case GRID_CRS_NATIVE:
            mt = TransformFactory.gridCRSToWorldPixel(this.gridGeometry,
                    mapGeometry);
            return transform(mt);
        case MAP_CRS_NATIVE:
            mt = CRSCache.getInstance().getCoordinateSystemToGrid(mapGeometry,
                    PixelInCell.CELL_CENTER);
            return transform(mt);
        case GRID:
            mt = TransformFactory.gridCellToGridCell(this.gridGeometry,
                    this.orientation, mapGeometry, PixelInCell.CELL_CENTER);
            return transform(mt);
        case PIXEL:
            return this.internalObject;
        default:
            return null;
        }

    }

    public T asGridCell(GeneralGridGeometry gridGeometry,
            PixelInCell orientation) throws TransformException,
            FactoryException {

        MathTransform mt = null;
        switch (this.type) {
        case LAT_LON:
            mt = TransformFactory.latLonToGrid(gridGeometry, orientation);
            return transform(mt);
        case GRID_CRS_NATIVE:
        case MAP_CRS_NATIVE:
            mt = TransformFactory.gridCRSToGridCell(this.gridGeometry,
                    gridGeometry, orientation);
            return transform(mt);
        case GRID:
        case PIXEL:
            mt = TransformFactory.gridCellToGridCell(this.gridGeometry,
                    this.orientation, gridGeometry, orientation);
            return transform(mt);
        default:
            return null;
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((gridGeometry == null) ? 0 : gridGeometry.hashCode());
        result = prime * result
                + ((internalObject == null) ? 0 : internalObject.hashCode());
        result = prime * result + ((type == null) ? 0 : type.hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @SuppressWarnings("unchecked")
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        ReferencedObject other = (ReferencedObject) obj;
        if (gridGeometry == null) {
            if (other.gridGeometry != null) {
                return false;
            }
        } else if (!gridGeometry.equals(other.gridGeometry)) {
            return false;
        }
        if (internalObject == null) {
            if (other.internalObject != null) {
                return false;
            }
        } else if (!internalObject.equals(other.internalObject)) {
            return false;
        }
        if (type == null) {
            if (other.type != null) {
                return false;
            }
        } else if (!type.equals(other.type)) {
            return false;
        }
        return true;
    }

    public T getObject() {
        return this.internalObject;
    }

}
