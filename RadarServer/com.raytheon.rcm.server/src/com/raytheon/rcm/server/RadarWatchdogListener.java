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
package com.raytheon.rcm.server;

import java.util.HashMap;

import com.raytheon.rcm.config.Configuration;
import com.raytheon.rcm.event.RadarEvent;
import com.raytheon.rcm.event.RadarEventAdapter;
import com.raytheon.rcm.message.GSM;
import com.raytheon.rcm.message.GraphicProduct;
import com.raytheon.rcm.message.Message;
import com.raytheon.rcm.message.GraphicProduct.PDB;
import com.raytheon.rcm.products.ProductInfo;
import com.raytheon.rcm.products.RadarProduct;
import com.raytheon.rcm.server.RadarWatchdog;

/**
 * 
 * This class listens to messages sent to the RadarServer, that the watchdog has
 * an interest in.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#    Engineer    Description
 * ------------- ---------- ----------- --------------------------
 * May 12, 2014  DR 16319   dhuffman    Initial creation.
 * 
 * </pre>
 * 
 * @author dhuffman
 * @version 1.0
 */
public class RadarWatchdogListener extends RadarEventAdapter {

    protected static RadarWatchdog radarWatchdog;

    private static HashMap<String, String> mnemonicMap = new HashMap<String, String>();
    {
        mnemonicMap.put("HZ", "Z");
        mnemonicMap.put("HV", "V");
        mnemonicMap.put("HSW", "SW");
    }

    public RadarWatchdogListener(Configuration configuration) {
        radarWatchdog = new RadarWatchdog(configuration);
        radarWatchdog.start();
    }

    @Override
    public void handleRadarEvent(RadarEvent event) {
        if (event.getType() == RadarEvent.MESSAGE_RECEIVED) {
            byte[] msg = event.getMessageData();
            int messageCode = Message.messageCodeOf(msg);
            if (messageCode == Message.GSM) {
                GSM gsm = null;
                try {
                    gsm = GSM.decode(msg);
                } catch (Exception e) {
                    // This message error will be reported by EventLogger.
                    return;
                }

                if (gsm != null) {
                    RadarWatchdog.GsmItem gi = new RadarWatchdog.GsmItem();
                    gi.radarID = event.getRadarID();
                    gi.vcp = gsm.vcp;
                    gi.time = gsm.time.getTimeInMillis();

                    RadarWatchdog.GsmItem oldgi = radarWatchdog
                            .getGSMItem(gi.radarID);
                    if (oldgi == null
                            || (oldgi != null && oldgi.time <= gi.time)) {
                        radarWatchdog.putGSMItem(gi);
                    }
                }

            } else if (16 <= messageCode) {
                int mcode = 0;
                PDB pdb = null;
                RadarWatchdog.RadarItem ri = new RadarWatchdog.RadarItem();
                mcode = Message.messageCodeOf(msg);
                ri.messageTime = (Message.decodeHeader(msg).time)
                        .getTimeInMillis();

                RadarProduct rp = ProductInfo.getInstance().getPoductForCode(
                        mcode);
                if (rp == null)
                    return;

                ri.mnemonic = rp.mnemonic;
                String newMnemonic = mnemonicMap.get(ri.mnemonic);
                if (newMnemonic != null)
                    ri.mnemonic = newMnemonic;

                try {
                    pdb = GraphicProduct.pdbOfMessage(msg);
                } catch (Exception e) {
                    // This message error will be reported by EventLogger.
                    return;
                }

                if (pdb != null) {
                    ri.radarID = event.getRadarID();
                    ri.time = System.currentTimeMillis();

                    RadarWatchdog.RadarItem oldri = radarWatchdog.getRadarItem(
                            ri.mnemonic, ri.radarID);

                    if (oldri == null
                            || (oldri != null && oldri.messageTime <= ri.messageTime)) {
                        radarWatchdog.putRadarItem(ri);
                        radarWatchdog.notifyWatchdog();
                    }
                }
            }
        }
    }

}
