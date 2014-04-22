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
package com.raytheon.uf.common.dataaccess;

import java.util.Set;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.time.DataTime;

/**
 * An IData represents the bare minimum of any piece of data, namely that it can
 * have attributes, a time, and a level.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 10, 2012            njensen     Initial creation
 * Jun 03, 2013  #2023     dgilling    Add getAttributes().
 * Jan 21, 2014  2667      bclement    attribute method comments
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public interface IData {

    /**
     * Gets an attribute of the data based on the key. Attributes are metadata
     * providing additional information on the dataset.
     * 
     * @param key
     * @return the attribute
     */
    public Object getAttribute(String key);

    /**
     * Gets the list of attributes associated with this data. Attributes are
     * metadata providing additional information on the dataset.
     * 
     * @return the attributes
     */
    public Set<String> getAttributes();

    /**
     * Gets the DataTime of the data. May be null if the data is time agnostic.
     * 
     * @return the time of the data, possibly null for some data types
     */
    public DataTime getDataTime();

    /**
     * Gets the level of the data. May be null depending on the datatype.
     * 
     * @return the level of the data, possibly null for some data types
     */
    public Level getLevel();

    /**
     * Gets the location name associated with this instance of IData. May be
     * null.
     * 
     * @return the location name or null
     */
    public String getLocationName();

}
