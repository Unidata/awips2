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
package com.raytheon.uf.edex.datadelivery.harvester.purge;

import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.edex.datadelivery.harvester.config.HarvesterConfig;

/**
 * Defines a purge for a {@link DataSetMetaData} type. Intentionally
 * package-level as it's an internal implementation detail that should only
 * reside and be accessible within this package.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 4, 2012  1102      djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

interface IServiceDataSetMetaDataPurge<T extends DataSetMetaData> {

    /**
     * Perform the purge of a {@link DataSetMetaData} instance if required.
     * 
     * @param metaData
     *            the metaData
     * @param harvesterConfig TODO
     * @return
     */
    boolean isTimeToPurge(T metaData, HarvesterConfig harvesterConfig);
}
