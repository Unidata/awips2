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
package com.raytheon.edex.textdb.alarms;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.util.Util;
import com.raytheon.uf.common.dataplugin.text.alarms.AlarmAlertProduct;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;

/**
 * Filters based on afos pils and sends certain text products to alarm alert
 * functionality in CAVE
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 29, 2009            mnash     Initial creation
 * 15Feb2010    4426       MW Fegan    Added over-ride of sendProductAlarmAlert(...)
 * 08Jul2010    2187       cjeanbap    Added Operational mode functionality.
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class AlarmAlertUtil {

    private static Log logger = LogFactory.getLog(Util.class);

    private static final String alarmEndpoint = "alarmAlertNotify";

    public AlarmAlertUtil() {

    }

    /**
     * parse the file that contains the proximity product
     * 
     * @param line
     * @return
     */
    public static AlarmAlertProduct parsePAProduct(String line) {
        AlarmAlertProduct aap = new AlarmAlertProduct();
        String[] vars = line.split(" ");
        aap.setProductId(vars[0]);
        aap.setAlarmType(vars[1]);
        aap.setActionCmd(vars[2]);
        if (vars.length > 3 && vars[3].contains("AOR")) {
            if (vars[3].split("+").length > 1) {
                aap.setAorDistance(vars[3].split("+")[1]);
            }
            aap.setAor(true);
        } else if (vars.length > 3 && vars[3].contains("UGC")) {
            aap.setUgcList(vars[3]);
        }
        return aap;
    }

    /**
     * parse the file that contains the regular alarm product
     * 
     * @param line
     * @return
     */
    public static AlarmAlertProduct parseAAProduct(String line) {
        AlarmAlertProduct aap = new AlarmAlertProduct();
        String[] vars = line.split(" ");
        aap.setProductId(vars[0]);
        aap.setAlarm(("1".equals(vars[1])) ? true : false);
        if (aap.isAlarm()) {
            aap.setAlarmType("Alarm");
        }
        if (vars.length > 2) {
            aap.setSearchString(vars[2]);
        }
        return aap;
    }

    /**
     * Sends an asynch message to the alarm end point. This version accepts a
     * string containing the date.
     * 
     * @param afosPIL
     *            the AFOS PIL for the message
     * @param date
     *            the date of the message
     */
    public static void sendProductAlarmAlert(String afosPIL, String date,
            boolean operationalMode) {
        DateFormat formatter = null;
        Date d = null;
        formatter = new SimpleDateFormat("EEE MMM dd HH:mm:ss zzz yyy");
        try {
            d = formatter.parse(date);
        } catch (ParseException e) {
            logger.error("Could not parse date, setting to now", e);
            d = new Date();
        }
        sendProductAlarmAlert(afosPIL, d, operationalMode);
    }

    /**
     * Sends an asynch message to the alarm end point. This version requires
     * that the caller provide the message time stamp as a Data object.
     * 
     * @param afosPIL
     *            the AFOS PIL for the message
     * @param date
     *            the date of the message
     */
    public static void sendProductAlarmAlert(String afosPIL, Date date,
            boolean operationalMode) {
        AlarmAlertProduct aap = new AlarmAlertProduct();
        aap.setProductId(afosPIL);
        aap.setDateReceived(date);
        aap.setOperationalMode(operationalMode);
        try {
            EDEXUtil.getMessageProducer().sendAsync(alarmEndpoint, aap);
        } catch (EdexException e) {
            logger.error("Could not send message to alarm/alert", e);
        }
    }
}
