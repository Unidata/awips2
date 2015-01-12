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
package com.raytheon.viz.mpe.ui.rsc;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 22, 2009            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public abstract class HydroPointResourceData<T extends HydroPointResource<?>>
        extends AbstractResourceData {
    @XmlElement
    protected Coordinate location;

    public enum Style {
        STAR, RECTANGLE, CIRCLE
    }

    @XmlElement
    protected Style style;

    @XmlElement
    protected float lineWidth = 1.0f;

    @XmlElement
    protected String name;

    private RGB color = new RGB(0, 0, 0);

    private int red;

    private int green;

    private int blue;

    public HydroPointResourceData() {

    }

    public HydroPointResourceData(String name, RGB color, Coordinate location,
            Style style) {
        this.name = name;
        if (color != null) {
            this.color = color;
            this.red = color.red;
            this.green = color.green;
            this.blue = color.blue;
        }
        this.location = location;
        this.style = style;
    }

    public Style getStyle() {
        return style;
    }

    public void setLineWidth(float lineWidth) {
        this.lineWidth = lineWidth;
    }

    /**
     * @return the location
     */
    public Coordinate getLocation() {
        return location;
    }

    /**
     * @param location
     *            the location to set
     */
    public void setLocation(Coordinate location) {
        this.location = location;
    }

    @Override
    public void update(Object updateData) {
        // TODO Auto-generated method stub
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public RGB getColor() {
        return color;
    }

    public float getLineWidth() {
        return lineWidth;
    }

    public void setStyle(Style style) {
        this.style = style;
    }

    @XmlElement
    public int getRed() {
        return red;
    }

    public void setRed(int red) {
        this.red = red;
        this.color.red = red;
    }

    @XmlElement
    public int getGreen() {
        return green;
    }

    public void setGreen(int green) {
        this.green = green;
        this.color.green = green;
    }

    @XmlElement
    public int getBlue() {
        return blue;
    }

    public void setBlue(int blue) {
        this.blue = blue;
        this.color.blue = blue;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + blue;
        result = prime * result + ((color == null) ? 0 : color.hashCode());
        result = prime * result + green;
        result = prime * result + Float.floatToIntBits(lineWidth);
        result = prime * result
                + ((location == null) ? 0 : location.hashCode());
        result = prime * result + ((name == null) ? 0 : name.hashCode());
        result = prime * result + red;
        result = prime * result + ((style == null) ? 0 : style.hashCode());
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
        HydroPointResourceData<?> other = (HydroPointResourceData<?>) obj;
        if (blue != other.blue)
            return false;
        if (color == null) {
            if (other.color != null)
                return false;
        } else if (!color.equals(other.color))
            return false;
        if (green != other.green)
            return false;
        if (Float.floatToIntBits(lineWidth) != Float
                .floatToIntBits(other.lineWidth))
            return false;
        if (location == null) {
            if (other.location != null)
                return false;
        } else if (!location.equals(other.location))
            return false;
        if (name == null) {
            if (other.name != null)
                return false;
        } else if (!name.equals(other.name))
            return false;
        if (red != other.red)
            return false;
        if (style != other.style)
            return false;
        return true;
    }

}
