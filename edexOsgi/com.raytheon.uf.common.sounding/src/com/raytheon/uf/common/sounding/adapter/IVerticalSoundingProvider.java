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
package com.raytheon.uf.common.sounding.adapter;

import com.raytheon.uf.common.sounding.VerticalSounding;
import com.raytheon.uf.common.time.DataTime;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Interface for an object that provides soundings for a time and location
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 19, 2013       2190 mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public interface IVerticalSoundingProvider {

    /**
     * The available times the provider has soundings for
     * 
     * @return
     */
    public DataTime[] getSoundingTimes();

    /**
     * Given a time and location, the provider will attempt to create a
     * {@link VerticalSounding}
     * 
     * @param time
     * @param location
     * @return
     */
    public VerticalSounding getSounding(DataTime time, Coordinate location);

    /**
     * The name of the sounding provider's data source
     * 
     * @return
     */
    public String getSoundingSource();
}
