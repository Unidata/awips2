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
package com.raytheon.uf.viz.monitor;

import com.raytheon.uf.common.monitor.data.ObConst;

/**
 * The ObReportGenFactory class is a Simple Factory Pattern that creates an
 * observation report generator entity that satisfies the IObReportable
 * interface.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 9, 2009  2047       grichard    Initial creation.
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public class ObReportGenFactory {

    /**
     * createObReportableEntity is a static method in a simple factory pattern
     * that news-up the observation-reportable object requested.
     * 
     * @param pluginName
     * @return the observation-reportable object
     */
    public static IObReportable createObReportableEntity(String pluginName) {
        IObReportable entity = null;
        if (pluginName.equalsIgnoreCase(ObConst.METAR_PLUGIN_NAME)) {
            entity = new GenerateMetarObReport();
        } else if (pluginName.equalsIgnoreCase(ObConst.MARINE_PLUGIN_NAME)) {
            entity = new GenerateMarineObReport();
        } else if (pluginName.equalsIgnoreCase(ObConst.MESO_PLUGIN_NAME)) {
            entity = new GenerateMesoObReport();
        }
        return entity;
    }

}
