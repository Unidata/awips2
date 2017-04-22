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

package com.raytheon.uf.viz.points.data;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.points.PointUtilities;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * A Point is a user-defined, named, geospatial location as defined by a
 * latitude and longitude. This also allows the point to be hidden, the point to
 * be movable, allow grouping with other points, and finally the font color and
 * size to use to display the name on a map.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * October-2010            epolster    Initial Creation.
 * July 16, 1012 #875      rferrel     Converted for use with CAVE
 * 
 * 
 * </pre>
 * 
 * @author epolster
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "point")
@DynamicSerialize
public class Point implements IPointNode, Comparable<IPointNode> {
    /**
     * This is used to compare the double values of latitude and longitude.
     */
    private static final double DELTA = 0.000001;

    @XmlElement(name = "name")
    @DynamicSerializeElement
    private String name;

    @XmlElement(name = "longitude")
    @DynamicSerializeElement
    private double longitude;

    @XmlElement(name = "latitude")
    @DynamicSerializeElement
    private double latitude;

    @XmlElement(name = "colorActive")
    @DynamicSerializeElement
    private boolean colorActive;

    @XmlElement(name = "red")
    @DynamicSerializeElement
    private int red;

    @XmlElement(name = "green")
    @DynamicSerializeElement
    private int green;

    @XmlElement(name = "blue")
    @DynamicSerializeElement
    private int blue;

    @XmlElement(name = "hidden")
    @DynamicSerializeElement
    private PointFieldState hidden;

    @XmlElement(name = "movable")
    @DynamicSerializeElement
    private PointFieldState movable;

    @XmlElement(name = "fontSize")
    @DynamicSerializeElement
    private PointSize fontSize = PointSize.DEFAULT;

    private transient String group;

    public Point() {
        this.movable = PointFieldState.TRUE;
        this.hidden = PointFieldState.FALSE;
    }

    /**
     * Copy constructor.
     * 
     * @param point
     */
    public Point(Point point) {
        this.name = point.name;
        this.longitude = point.longitude;
        this.latitude = point.latitude;
        this.colorActive = point.colorActive;
        this.red = point.red;
        this.green = point.green;
        this.blue = point.blue;
        this.fontSize = point.fontSize;
        this.hidden = point.hidden;
        this.movable = point.movable;
        this.group = point.group;
    }

    /**
     * Constructor must take valid lat/lon coordinates!
     * 
     * @param pointName
     * @param lat
     * @param lon
     * @param hidden
     * @param movable
     * @param colorActive
     * @param c
     * @param group
     */
    public Point(String pointName, double lat, double lon,
            PointFieldState hidden, PointFieldState movable,
            boolean colorActive, RGB c, String group) {
        setName(pointName);
        this.fontSize = PointSize.DEFAULT;
        this.colorActive = colorActive;
        this.red = c.red;
        this.green = c.green;
        this.blue = c.blue;
        this.movable = movable;
        this.hidden = hidden;
        this.longitude = lon;
        this.latitude = lat;
        setGroup(group);
    }

    /**
     * Constructor must take valid lat/lon coordinates!
     * 
     * @param pointName
     * @param lat
     * @param lon
     * @param movable
     * @param colorActive
     * @param c
     * @param size
     * @param group
     */
    public Point(String pointName, double lat, double lon,
            PointFieldState movable, boolean colorActive, RGB c,
            PointSize size, String group) {
        setName(pointName);
        this.fontSize = size;
        this.colorActive = colorActive;
        this.red = c.red;
        this.green = c.green;
        this.blue = c.blue;
        this.movable = movable;
        this.hidden = PointFieldState.FALSE;
        this.longitude = lon;
        this.latitude = lat;
        setGroup(group);
    }

    /**
     * @return latitude
     */
    public double getLatitude() {
        return latitude;
    }

    /**
     * @param latitude
     */
    public void setLatitude(double latitude) {
        this.latitude = latitude;
    }

    /**
     * @return longitude
     */
    public double getLongitude() {
        return longitude;
    }

    /**
     * @return coordinate
     */
    public Coordinate getCoordinate() {
        return new Coordinate(longitude, latitude);
    }

    /**
     * @param coordinate
     */
    public void setCoordinate(Coordinate coordinate) {
        this.longitude = coordinate.x;
        this.latitude = coordinate.y;
    }

    /**
     * @param longitude
     */
    public void setLongitude(double longitude) {
        this.longitude = longitude;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.points.data.IPointNode#getName()
     */
    @Override
    public String getName() {
        return name;
    }

    /**
     * @param pointName
     */
    public void setName(String pointName) {
        name = PointUtilities.trimAll(pointName);
    }

    /**
     * @param state
     */
    public void setHidden(PointFieldState state) {
        hidden = state;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.points.data.IPointNode#getHidden()
     */
    @Override
    public PointFieldState getHidden() {
        return hidden;
    }

    /**
     * @return
     */
    public boolean isColorActive() {
        return colorActive;
    }

    /**
     * @param colorActive
     */
    public void setColorActive(boolean colorActive) {
        this.colorActive = colorActive;
    }

    /**
     * @return rgb
     */
    public RGB getColor() {
        return new RGB(red, green, blue);
    }

    /**
     * @param c
     */
    public void setColor(RGB c) {
        this.red = c.red;
        this.green = c.green;
        this.blue = c.blue;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    public Object clone() {
        Point point = new Point(this);
        return point;
    }

    /**
     * @return
     */
    public PointSize getFontSize() {
        return fontSize;
    }

    /**
     * @param fs
     */
    public void setFontSize(PointSize fs) {
        fontSize = fs;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.points.data.IPointNode#getMovable()
     */
    @Override
    public PointFieldState getMovable() {
        return movable;
    }

    /**
     * @param notAnchored
     */
    public void setMovable(PointFieldState notAnchored) {
        movable = notAnchored;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.points.data.IPointNode#getGroup()
     */
    @Override
    public String getGroup() {
        return group;
    }

    /**
     * @param group
     */
    public void setGroup(String group) {
        this.group = PointUtilities.trimAll(group).replace(
                PointUtilities.DELIM_CHAR, ' ');
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.points.data.IPointNode#isGroup()
     */
    @Override
    public boolean isGroup() {
        return false;
    }

    public int getRed() {
        return red;
    }

    public void setRed(int red) {
        this.red = red;
    }

    public int getGreen() {
        return green;
    }

    public void setGreen(int green) {
        this.green = green;
    }

    public int getBlue() {
        return blue;
    }

    public void setBlue(int blue) {
        this.blue = blue;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuffer sb = new StringBuffer();
        sb.append("Point: \n");
        sb.append("\t Name: ").append(name).append("\n");
        sb.append("\t longitude, latitude: ").append("{").append(longitude)
                .append(", ").append(latitude).append("}\n");
        sb.append("\t Color Active: ").append(colorActive).append("\n");
        sb.append("\t Color: ").append("(").append(red).append(", ")
                .append(green).append(", ").append(blue).append(")")
                .append("\n");
        sb.append("\t isHidden: ").append(hidden).append("\n");
        sb.append("\t isMovable: ").append(movable).append("\n");
        sb.append("\t fontSize: ").append(fontSize.toString()).append("\n");
        sb.append("\t group: \"").append(group).append("\"\n");
        sb.append("\t isGroup: ").append(isGroup()).append("\n");
        return sb.toString();
    }

    public Coordinate getLocation() {
        return new Coordinate(longitude, latitude);
    }

    public void setLocation(Coordinate location) {
        this.longitude = location.x;
        this.latitude = location.y;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.points.data.IPointNode#compareTo(com.raytheon.uf.
     * viz.points.data.IPointNode)
     */
    @Override
    public int compareTo(IPointNode o) {
        // Put groups at the top of a list
        if (isGroup()) {
            if (!o.isGroup()) {
                return -1;
            }
        } else if (o.isGroup()) {
            return 1;
        }
        return getName().compareToIgnoreCase(o.getName());
    }

    /**
     * Determine if any of the contains of point is different.
     * 
     * @param point
     * @return state true when contains differ otherwise false
     */
    public boolean differentContent(Point point) {
        boolean state = false;
        if (!name.equals(point.name)) {
            state = true;
        } else if (Math.abs(latitude - point.latitude) > DELTA) {
            state = true;
        } else if (Math.abs(longitude - point.longitude) > DELTA) {
            state = true;
        } else if (colorActive != point.colorActive) {
            state = true;
        } else if (red != point.red) {
            state = true;
        } else if (green != point.green) {
            state = true;
        } else if (blue != point.blue) {
            state = true;
        } else if (hidden != point.hidden) {
            state = true;
        } else if (movable != point.movable) {
            state = true;
        } else if (fontSize != point.fontSize) {
            state = true;
        } else if (!group.equals(point.group)) {
            state = true;
        }
        return state;
    }
}
