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

package com.raytheon.edex.plugin.gfe.cache.d2dparms;

import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.server.notify.GfeNotification;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.GridUpdateNotification;

/**
 * Extracts a ParmID from a GribRecord. This class is used as part of the ESB
 * route which updates the D2DParmIdCache as products arrive.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 1/10/09      1674       bphillip    Initial creation
 * 10/06/09     3172       njensen     Based on grib notification
 * 01/18/13     #1504      randerso    Changed to send full GridUpdateNotification 
 *                                     to D2DParmIdCache
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class D2DParmIdFilter {

    /**
     * Extracts ParmIDs and insert times from a list of messages in the
     * container.
     * 
     * @param container
     *            the container of messages
     */
    public void updateParmIdCache(List<? extends GfeNotification> notifications) {
        for (GfeNotification notify : notifications) {
            if (notify instanceof GridUpdateNotification) {
                D2DParmIdCache.getInstance().processGridUpdateNotification(
                        (GridUpdateNotification) notify);
            }
        }
    }
}
