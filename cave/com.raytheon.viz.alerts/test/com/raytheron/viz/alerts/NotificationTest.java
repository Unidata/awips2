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
package com.raytheron.viz.alerts;

import javax.jms.Connection;
import javax.jms.ConnectionFactory;
import javax.jms.MessageProducer;
import javax.jms.Session;
import javax.jms.TextMessage;
import javax.jms.Topic;

import com.raytheon.uf.viz.core.comm.JMSConnection;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * May 12, 2008				randerso	Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class NotificationTest {

    private static final String ALERT_TOPIC = "edex.alerts";

    public static void main(String[] args) {
        String topic = ALERT_TOPIC;
        String text = "Hello!";

        if (args.length == 1) {
            text = args[0];
        } else if (args.length >= 2) {
            topic = args[0];
            text = args[1];
        }

        Connection connection = null;
        try {
            JMSConnection jms = new JMSConnection("tcp://localhost:61616");
            ConnectionFactory connectionFactory = jms.getFactory();

            connection = connectionFactory.createConnection();
            Session session = connection.createSession(false,
                    Session.AUTO_ACKNOWLEDGE);

            Topic t = session.createTopic(topic);

            connection.start();

            MessageProducer producer = session.createProducer(t);

            TextMessage message = session.createTextMessage(text);
            System.out.println("\nSending: \"" + message.getText() + "\" to "
                    + topic);

            producer.send(message);
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            if (connection != null) {
                try {
                    connection.close();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }

            // ALWAYS close your connection in a finally block to avoid
            // leaks.
            // Closing connection also takes care of closing its related
            // objects e.g. sessions.
        }
    }

}
