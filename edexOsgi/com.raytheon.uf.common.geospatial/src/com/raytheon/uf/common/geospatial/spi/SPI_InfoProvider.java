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
package com.raytheon.uf.common.geospatial.spi;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 6, 2009            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public interface SPI_InfoProvider {

    /**
     * Return the nearest SPIEntry to the given point and that is also within
     * maxDist kilometers to the point.
     * @param latlon Latitude/Longitude of the target point.
     * @param maxDist Maximum distance from the target point.
     * @return The nearest SPIEntry to the given point. Returns null if no such
     * point exists.
     */
    public SPIEntry nearest(Coordinate latlon, float maxDist);

    /**
     * Return the nearest SPIEntry to the given point. 
     * @return The nearest SPIEntry to the given point.
     */
    public SPIEntry nearest(Coordinate latlon);

    /**
     * 
     * @param id
     * @return
     */
    public SPIEntry getEntryById(String id);
    
    /**
     * 
     * @param name
     * @return
     */
    public SPIEntry getEntryByName(String name);
    
    /**
     * Has the backing storage been loaded properly?
     * @return Has the backing storage been loaded properly!
     */
    public boolean isLoaded();
}
