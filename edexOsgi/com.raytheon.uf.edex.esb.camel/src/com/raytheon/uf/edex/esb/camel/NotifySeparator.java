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
package com.raytheon.uf.edex.esb.camel;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import org.apache.camel.Body;
import org.apache.camel.Header;
import org.apache.camel.Message;
import org.apache.camel.impl.DefaultMessage;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 3, 2009            brockwoo     Initial creation
 * 
 * </pre>
 * 
 * @author brockwoo
 * @version 1.0
 */

public class NotifySeparator {

    private static Pattern notificationSplit = Pattern.compile("\\|\\|");

    public static List<Message> separate(
            @Header(value = "header") String header,
            @Header(value = "enqueueTime") Long queuetime, @Body String body) {
        String[] headers = notificationSplit.split(header);
        String[] locations = notificationSplit.split(body);
        List<Message> answer = new ArrayList<Message>();
        for (int i = 0; i < locations.length; i++) {
            DefaultMessage message = new DefaultMessage();
            if (locations.length == headers.length) {
                message.setHeader("header", headers[i]);
            } else {
                message.setHeader("header", locations[i]);
            }
            message.setHeader("enqueueTime", queuetime);
            message.setBody(locations[i]);
            answer.add(message);
        }
        return answer;
    }
}
