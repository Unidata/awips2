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
package com.raytheon.uf.viz.collaboration.ui.telestrator;

import java.util.List;

import org.eclipse.swt.graphics.Color;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * A line for the telestrator
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 26, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class TelestratorLine extends TelestratorObject {
    // the color of the shape to be drawn
    private Color color;

    // the coordinates to be converted into a wireframe shape
    private List<Coordinate> coordinates;

    /**
     * @param color
     *            the color to set
     */
    public void setColor(Color color) {
        this.color = color;
    }

    /**
     * @return the color
     */
    public Color getColor() {
        return color;
    }

    /**
     * @param coordinates
     *            the coordinates to set
     */
    public void setCoordinates(List<Coordinate> coordinates) {
        this.coordinates = coordinates;
    }

    /**
     * @return the coordinates
     */
    public List<Coordinate> getCoordinates() {
        return coordinates;
    }
}
