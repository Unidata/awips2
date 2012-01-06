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
package com.raytheon.viz.warngen.config;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.warning.config.PointSourceConfiguration;

/**
 * Creates data adaptors for PointSource and Pathcast data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 26, 2011            bgonzale     Initial creation
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

public class DataAdaptorFactory {

    private static Map<Class<?>, IPointSourceDataAdaptor> adapterMap = new HashMap<Class<?>, IPointSourceDataAdaptor>();
    static {
        // Only data source config so far
        adapterMap.put(PointSourceConfiguration.class,
                new DbPointSourceDataAdaptor());
    }

    public static IPointSourceDataAdaptor createPointSource(
            PointSourceConfiguration pointConfig) {
        return adapterMap.get(pointConfig.getClass());
    }

}
