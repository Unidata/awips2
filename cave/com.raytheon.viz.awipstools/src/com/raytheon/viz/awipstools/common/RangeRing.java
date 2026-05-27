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
package com.raytheon.viz.awipstools.common;

import java.util.Arrays;

import org.locationtech.jts.geom.Coordinate;

/**
 * A class which represents a RangeRing(although really it can also have
 * multiple rings around a single center, so its more of a target shaped object
 * then a ring).
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10-21-09     #1711      bsteffen    Initial Comments, changed to accomodate both Fixed and Movable Rings
 * 08-13-14     #3467      mapeters    Modified equals() method to prevent ArrayIndexOutOfBoundsException.
 * 09-08-16     #5871      njensen     Added hashCode() and formatted
 * 
 * </pre>
 * 
 * @author unknown
 */
public class RangeRing {

    public enum RangeRingType {
        FIXED, MOVABLE
    }

    private RangeRingType type;

    private boolean visible = false;

    private String id = "";

    private Coordinate centerCoordinate;

    private int[] radii;

    private String label = "";

    public RangeRing(String id, Coordinate centerCoordinate, int radius,
            String label) {
        this(id, centerCoordinate, radius, label, false);
    }

    public RangeRing(String id, Coordinate centerCoordinate, int radius,
            String label, boolean visible) {
        this.type = RangeRingType.MOVABLE;
        this.id = id;
        this.centerCoordinate = centerCoordinate;
        this.radii = new int[] { radius };
        this.label = label;
        this.visible = visible;
    }

    public RangeRing(String id, Coordinate centerCoordinate, int[] radii,
            String label) {
        this(id, centerCoordinate, radii, label, false);

    }

    public RangeRing(String id, Coordinate centerCoordinate, int[] radii,
            String label, boolean visible) {
        this.type = RangeRingType.FIXED;
        this.id = id;
        this.centerCoordinate = centerCoordinate;
        this.radii = radii;
        this.label = label;
        this.visible = visible;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public Coordinate getCenterCoordinate() {
        return centerCoordinate;
    }

    public void setCenterCoordinate(Coordinate centerCoordinate) {
        this.centerCoordinate = centerCoordinate;
    }

    public int getRadius() {
        return radii[0];
    }

    public void setRadius(int radius) {
        this.radii[0] = radius;
    }

    public int[] getRadii() {
        return radii;
    }

    public void setRadii(int[] radii) {
        this.radii = radii;
    }

    public String getLabel() {
        return label;
    }

    public void setLabel(String label) {
        this.label = label;
    }

    public boolean isVisible() {
        return visible;
    }

    public void setVisible(boolean visible) {
        this.visible = visible;
    }

    public RangeRingType getType() {
        return type;
    }

    public void setType(RangeRingType type) {
        this.type = type;
    }

    @Override
    public RangeRing clone() {
        RangeRing newRing = new RangeRing(this.id,
                new Coordinate(this.centerCoordinate),
                Arrays.copyOf(this.radii, this.radii.length), this.label,
                this.visible);
        newRing.setType(this.type);
        return newRing;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((centerCoordinate == null) ? 0
                : centerCoordinate.hashCode());
        result = prime * result + ((id == null) ? 0 : id.hashCode());
        result = prime * result + ((label == null) ? 0 : label.hashCode());
        result = prime * result + Arrays.hashCode(radii);
        result = prime * result + ((type == null) ? 0 : type.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        RangeRing other = (RangeRing) obj;
        if (centerCoordinate == null) {
            if (other.centerCoordinate != null)
                return false;
        } else if (!centerCoordinate.equals(other.centerCoordinate))
            return false;
        if (id == null) {
            if (other.id != null)
                return false;
        } else if (!id.equals(other.id))
            return false;
        if (label == null) {
            if (other.label != null)
                return false;
        } else if (!label.equals(other.label))
            return false;
        if (!Arrays.equals(radii, other.radii))
            return false;
        if (type != other.type)
            return false;
        return true;
    }

}
