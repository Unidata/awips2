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
package com.raytheon.edex.plugin.gfe.reference;

import com.raytheon.edex.plugin.gfe.reference.ShapeFile.ShapeType;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Apr 17, 2008				randerso	Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class MapID {
    private ShapeType _mapType;

    private String _shapefileName;

    private String _name;

    /**
     * Default constructor for MapID.
     * 
     */
    public MapID() {
        this._mapType = ShapeType.NONE;
    }

    /**
     * Constructor for MapID class taking a map name, a map type, and a
     * 
     * @param name
     * @param mapType
     * @param shapefileName
     */
    public MapID(final String name, final ShapeType mapType,
            final String shapefileName) {
        this._name = name;
        this._mapType = mapType;
        this._shapefileName = shapefileName;
    }

    public String mapTypeAsString() {
        return this._mapType.toString();
    }

    public String filename() {
        return "MAPS_" + this._name;
    }

    /**
     * @return the _mapType
     */
    public ShapeType getMapType() {
        return _mapType;
    }

    /**
     * @return the _shapefileName
     */
    public String getShapefileName() {
        return _shapefileName;
    }

    /**
     * @return the _name
     */
    public String getName() {
        return _name;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        String o = "(" + getName() + "," + mapTypeAsString() + ","
                + getShapefileName() + ")";
        return o;
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
        result = prime * result + (_mapType == null ? 0 : _mapType.hashCode());
        result = prime * result + (_name == null ? 0 : _name.hashCode());
        result = prime * result
                + (_shapefileName == null ? 0 : _shapefileName.hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
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
        final MapID other = (MapID) obj;
        if (_mapType == null) {
            if (other._mapType != null) {
                return false;
            }
        } else if (!_mapType.equals(other._mapType)) {
            return false;
        }
        if (_name == null) {
            if (other._name != null) {
                return false;
            }
        } else if (!_name.equals(other._name)) {
            return false;
        }
        if (_shapefileName == null) {
            if (other._shapefileName != null) {
                return false;
            }
        } else if (!_shapefileName.equals(other._shapefileName)) {
            return false;
        }
        return true;
    }

}
