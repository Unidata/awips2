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
package com.raytheon.uf.edex.plugin.pointset.netcdf.description;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlEnumValue;

import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;

/**
 * Describes the type of triangulation that should be performed. If we need any
 * additional parameters to get an accurate triangulation then new attributes
 * will be added here.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Aug 24, 2015  4709     bsteffen  Initial creation
 * May 18, 2016  5452     bsteffen  Enable configurable alpha shaping
 *
 * </pre>
 *
 * @author bsteffen
 */
@XmlAccessorType(XmlAccessType.NONE)
public class TriangulationDescription {

    public static final String TRIANGULATION_KEY = "__triangulation";

    @XmlAttribute
    private TriangulationType type;

    /**
     * The maximum circumradius of a triangle to include in the triangulation.
     * This value is used in the DelauneyTriangulator to create an AlphaShape.
     * The DelauneyTriangulator class refers to this value as alpha, but most
     * references describe the max radius as 1/α so to avoid any confusion over
     * what the value means call it the max radius.
     *
     * The units for this value is projected meters. Since triangulation is done
     * using a 2D map projection the meters do not align perfectly to real world
     * meters, but as long as the data covers a small area it should be very
     * close.
     */
    @XmlAttribute
    private Double maxRadius;

    public TriangulationType getType() {
        return type;
    }

    public void setType(TriangulationType type) {
        this.type = type;
    }

    public Double getMaxRadius() {
        return maxRadius;
    }

    public void setMaxRadius(Double maxRadius) {
        this.maxRadius = maxRadius;
    }

    public void validate() throws InvalidDescriptionException {
        if (type == TriangulationType.GRID && maxRadius != null) {
            throw new InvalidDescriptionException(
                    "maxRadius is only valid for delauney triangulation.");
        }
    }

    public static enum TriangulationType {
        @XmlEnumValue(value = "grid")
        GRID,

        @XmlEnumValue(value = "delauney")
        DELAUNEY;
    }
}
