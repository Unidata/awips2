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
package com.raytheon.viz.core.graphing;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.geotools.referencing.crs.DefaultEngineeringCRS;

import com.raytheon.uf.common.geospatial.adapter.CoordAdapter;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.AbstractDescriptor;
import org.locationtech.jts.geom.Coordinate;

/**
 * Generic descriptor that can be used by graphing displays
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Nov 5, 2007            njensen     Initial creation.
 * Oct 22, 2009  3348     bsteffen    Moved getter/setters for numberOfFrames
 *                                    down to AbstractDescriptor
 * Oct 24, 2013  2491     bsteffen    Remove ISerializableObject
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class GraphDescriptor extends AbstractDescriptor {

    protected double zoomLevel = 1.0f;

    @XmlElement
    @XmlJavaTypeAdapter(value = CoordAdapter.class)
    protected Coordinate location;

    protected int verticalFrameCount = 0;

    public GraphDescriptor() {
        this(new PixelExtent(0, 1000, 0, 1000));
    }

    public GraphDescriptor(PixelExtent anExtent) {
        super(createGridGeometry(anExtent, DefaultEngineeringCRS.CARTESIAN_2D));
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

}
