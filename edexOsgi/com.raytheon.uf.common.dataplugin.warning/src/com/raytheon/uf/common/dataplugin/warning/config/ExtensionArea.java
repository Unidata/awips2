package com.raytheon.uf.common.dataplugin.warning.config;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

/**
 * Describes how polygon is allowed to extend into a site's marine areas
 * (for land-based warnings) or onto land (for marine-based warnings).
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer     Description
 * ------------ ---------- ------------ --------------------------
 * 12/21/2015   DCS 17942  D. Friedman  Initial revision
 * 03/22/2016   DCS 18719  D. Friedman  Add dynamicArea option
 * </pre>
 *
 */
@XmlAccessorType(XmlAccessType.NONE)
public class ExtensionArea implements Cloneable {
    private double distance = Double.NaN;
    private double simplificationTolerance = Double.NaN;
    private boolean dynamicArea = false;

    @XmlAttribute
    public double getDistance() {
        return distance;
    }

    public void setDistance(double distance) {
        this.distance = distance;
    }

    @XmlAttribute(name="simplification")
    public double getSimplificationTolerance() {
        return simplificationTolerance;
    }

    public void setSimplificationTolerance(double simplificationTolerance) {
        this.simplificationTolerance = simplificationTolerance;
    }

    @XmlAttribute
    public boolean isDynamicArea() {
        return dynamicArea;
    }

    public void setDynamicArea(boolean dynamicArea) {
        this.dynamicArea = dynamicArea;
    }

    public ExtensionArea clone() {
        try {
            return (ExtensionArea) super.clone();
        } catch (CloneNotSupportedException e) {
            throw new RuntimeException(e);
        }
    }
}
