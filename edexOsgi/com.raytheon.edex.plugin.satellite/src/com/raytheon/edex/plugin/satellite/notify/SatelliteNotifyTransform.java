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
package com.raytheon.edex.plugin.satellite.notify;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 30, 2011            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class SatelliteNotifyTransform {

    /**
     * Translates the satellite records into messages that have specific
     * readable data about what was ingested.
     * 
     * @param gribs
     * @return
     */
    public static SatelliteNotifyContainer transformToMessages(
            PluginDataObject[] satRecs) {
        SatelliteNotifyContainer container = new SatelliteNotifyContainer();
        List<SatelliteNotifyMessage> msgList = new ArrayList<SatelliteNotifyMessage>();
        for (PluginDataObject pdo : satRecs) {
            SatelliteRecord satRec = (SatelliteRecord) pdo;
            SatelliteNotifyMessage msg = new SatelliteNotifyMessage(satRec);
            msgList.add(msg);
        }

        container.setMessages(msgList);
        return container;
    }
}
