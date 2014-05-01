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
package com.raytheon.uf.common.datadelivery.service.subscription;

import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.DataType;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.localization.exception.LocalizationException;

/**
 * Reads and writes overlap config files for code in common.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 09, 2013  2000      djohnson     Initial creation
 * Oct  1, 2013  1797      dhladky      More Generics
 * Oct 21, 2013  2292      mpduff       Changed service to read/write config files.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 * @param <T>
 * @param <C>
 */

public interface ISubscriptionOverlapService<T extends Time, C extends Coverage> {
    /**
     * Writes a new configuration file.
     * 
     * @param config
     *            the configuration
     * @throws LocalizationException
     *             on error saving the configuration
     */
    void writeConfig(SubscriptionOverlapConfig config)
            throws LocalizationException;

    /**
     * Get the overlap config file for this data type.
     * 
     * @param type
     *            The data type
     * @return the config file for the data type
     */
    SubscriptionOverlapConfig getConfigFile(DataType type);
}