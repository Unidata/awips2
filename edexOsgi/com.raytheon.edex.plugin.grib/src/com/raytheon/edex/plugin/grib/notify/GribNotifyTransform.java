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
package com.raytheon.edex.plugin.grib.notify;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grib.GribModel;
import com.raytheon.uf.common.dataplugin.grib.GribRecord;

/**
 * Translates a GribRecord into a GribNotifyMessage
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 5, 2009            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class GribNotifyTransform {

    /**
     * Translates the grib records into messages that have specific readable
     * data about what was ingested.
     * 
     * @param gribs
     * @return
     */
    public static GribNotifyContainer transformToMessages(
            PluginDataObject[] gribs) {
        GribNotifyContainer container = new GribNotifyContainer();
        List<GribNotifyMessage> msgList = new ArrayList<GribNotifyMessage>();
        for (PluginDataObject pdo : gribs) {
            GribRecord grib = (GribRecord) pdo;
            GribModel modelInfo = grib.getModelInfo();
            // String level = GridTranslator.getShortLevelName(
            // modelInfo.getLevelName(), modelInfo.getLevelOneValue(),
            // modelInfo.getLevelTwoValue());

            GribNotifyMessage msg = new GribNotifyMessage();
            msg.setInsertTime(grib.getInsertTime().getTime());
            msg.setDataTime(grib.getDataTime());
            msg.setModel(modelInfo.getModelName());
            msg.setLevelName(modelInfo.getLevelName());
            msg.setLevelOne(modelInfo.getLevelOneValue());
            msg.setLevelTwo(modelInfo.getLevelTwoValue());
            msg.setParamAbbreviation(grib.getModelInfo()
                    .getParameterAbbreviation());
            msg.setDataURI(grib.getDataURI());
            msgList.add(msg);
        }
        container.setMessages(msgList.toArray(new GribNotifyMessage[msgList
                .size()]));
        return container;
    }
}
