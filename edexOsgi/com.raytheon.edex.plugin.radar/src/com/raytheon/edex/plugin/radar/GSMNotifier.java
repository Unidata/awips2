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
package com.raytheon.edex.plugin.radar;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.edex.plugin.radar.dao.RadarDao;
import com.raytheon.edex.plugin.radar.level3.Level3BaseRadar;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.level3.GSMBlock.GSMMessage;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants;
import com.raytheon.uf.common.dataplugin.radar.util.RadarUtil;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.core.EDEXUtil;

/**
 * Send AlertViz notifications for Radar General Status Messages
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Mar 22, 2018  6711     randerso  Initial creation
 *
 * </pre>
 *
 * @author randerso
 */

public class GSMNotifier {
    private static final String EDEX = "EDEX";

    private static final String RADAR = "RADAR";

    private Logger logger = LoggerFactory.getLogger(getClass());

    private RadarDao dao;

    /**
     * Constructor
     *
     * @throws PluginException
     */
    public GSMNotifier() throws PluginException {
        this.dao = new RadarDao();
    }

    /**
     * @param newRecords
     */
    public void handleGeneralStatusMessage(List<RadarRecord> newRecords) {
        // for each new record, (should only be one)
        for (RadarRecord newRecord : newRecords) {
            String icao = newRecord.getIcao();

            // get the previous GSM message from this icao, if any
            GSMMessage prevMessage = null;
            try {
                // Retrieve the two latest GSM records for this icao
                Map<String, Object> paramMap = new HashMap<>(2, 1.0f);
                paramMap.put("icao", icao);
                paramMap.put("productCode",
                        Level3BaseRadar.GENERAL_STATUS_MESSAGE);
                List<?> results = dao.findByNamedQueryAndNamedParams(
                        RadarRecord.GET_LATEST_RADAR_RECORDS, paramMap, 2);

                // latest should be this new record, if not we continue
                if (!results.isEmpty()) {
                    if (!((RadarRecord) results.get(0)).getDataTime()
                            .getRefTime()
                            .equals(newRecord.getDataTime().getRefTime())) {
                        continue;
                    }
                }

                // if 2 records returned then get the previous record
                if (results.size() >= 2) {
                    RadarRecord prevRecord = ((RadarRecord) results.get(1));

                    // if previous message is newer than the new record,
                    // continue
                    if (prevRecord.getDataTime().getRefTime()
                            .after(newRecord.getDataTime().getRefTime())) {
                        continue;
                    }

                    // get the previous GSM message
                    dao.populateData(prevRecord);
                    prevMessage = prevRecord.getGsmMessage();
                }
            } catch (Exception e) {
                logger.error("Unable to retrieve previous GSM message", e);
                // we will treat the new GSM message as the first received from
                // this icao
            }

            // get the new GSM message and format the details
            GSMMessage newMessage = newRecord.getGsmMessage();
            String details = newMessage.toString();

            // flag to keep track of message being sent
            boolean messageSent = false;

            // if product availability has changed
            if (prevMessage == null || prevMessage
                    .getProductAvail() != newMessage.getProductAvail()) {
                EDEXUtil.sendMessageAlertViz(Priority.SIGNIFICANT,
                        RadarConstants.PLUGIN_ID, EDEX, RADAR,
                        newRecord.getIcao() + ": "
                                + RadarUtil.formatBits(
                                        (short) newMessage.getProductAvail(),
                                        RadarConstants.productAvailStr),
                        details, null);
                messageSent = true;
            }

            // if VCP has changed
            if (prevMessage == null
                    || prevMessage.getVolumeCoveragePattern() != newMessage
                            .getVolumeCoveragePattern()) {
                EDEXUtil.sendMessageAlertViz(Priority.SIGNIFICANT,
                        RadarConstants.PLUGIN_ID, EDEX, RADAR,
                        newRecord.getIcao() + ": Changed to VCP "
                                + newMessage.getVolumeCoveragePattern(),
                        details, null);
                messageSent = true;
            }

            // if narrow band load shedding is active
            short rpgNarrowBandStatus = (short) newMessage
                    .getRpgNarrowbandStatus();
            // Only alert if bit 14 is 1, (LSB=15)
            if ((rpgNarrowBandStatus & 0x0002) != 0) {
                EDEXUtil.sendMessageAlertViz(Priority.SIGNIFICANT,
                        RadarConstants.PLUGIN_ID, EDEX, RADAR,
                        newRecord.getIcao() + ": "
                                + RadarConstants.rpgNarrowbandStatus.get(1),
                        details, null);
                messageSent = true;
            }

            // if no message has been sent yet
            if (!messageSent) {
                EDEXUtil.sendMessageAlertViz(Priority.INFO,
                        RadarConstants.PLUGIN_ID, EDEX, RADAR,
                        newRecord.getIcao()
                                + ": General Status Message received",
                        details, null);
            }
        }
    }
}
