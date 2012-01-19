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
package gov.noaa.nws.ncep.edex.plugin.ncgrib.notify;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.PluginDataObject;

import gov.noaa.nws.ncep.edex.util.ncgrib.NcgribModelLookup;
import gov.noaa.nws.ncep.edex.util.ncgrib.NcgridTranslator;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.NcgribModel;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.NcgribRecord;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.util.NcgridModel;
/**
 * Translates a NcgribRecord into a NcgribNotifyMessage
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

public class NcgribNotifyTransform {

    /**
     * Translates the grib records into messages that have specific readable
     * data about what was ingested.
     * 
     * @param gribs
     * @return
     */
    public static NcgribNotifyContainer transformToMessages(
            PluginDataObject[] gribs) {
        NcgribNotifyContainer container = new NcgribNotifyContainer();
        List<NcgribNotifyMessage> msgList = new ArrayList<NcgribNotifyMessage>();
        for (PluginDataObject pdo : gribs) {
            NcgribRecord grib = (NcgribRecord) pdo;
            NcgribModel modelInfo = grib.getModelInfo();
            String level = NcgridTranslator.getInstance().getShortLevelName(
                    modelInfo.getLevelName(), modelInfo.getLevelOneValue(),
                    modelInfo.getLevelTwoValue());

            // currently the consumers of this data (GFE) don't care about it
            // if we can't identify a model name and level
            if (level != null) {
                NcgribNotifyMessage msg = new NcgribNotifyMessage();
                msg.setInsertTime(grib.getInsertTime().getTime());
                msg.setModelTime(grib.getDataTime().getRefTime());
                msg.setModel(modelInfo.getModelName());
                msg.setLevelAbbreviation(level);
                msg.setParamAbbreviation(grib.getModelInfo()
                        .getParameterAbbreviation());
                msgList.add(msg);
            }
        }
        container.setMessages(msgList.toArray(new NcgribNotifyMessage[msgList
                .size()]));
        return container;
    }
}
