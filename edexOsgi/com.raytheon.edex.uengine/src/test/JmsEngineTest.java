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

package test;

import java.io.IOException;

import javax.jms.Connection;
import javax.jms.DeliveryMode;
import javax.jms.JMSException;
import javax.jms.MessageProducer;
import javax.jms.Session;
import javax.jms.TextMessage;

/**
 * @author fpg
 */
public class JmsEngineTest extends ToolSupport {

    protected int messageCount = 10;

    protected long sleepTime = 0L;

    protected boolean verbose = true;

    protected int messageSize = 255;

    private long timeToLive;

    private String scriptFileName;

    public static void main(String[] args) {
        runTest(args, new JmsEngineTest());
    }

    protected static void runTest(String[] args, JmsEngineTest tool) {
        tool.clientID = null;
        if (args.length > 0) {
            tool.url = args[0];
        }
        if (args.length > 1) {
            tool.topic = args[1].equalsIgnoreCase("true");
        }
        if (args.length > 2) {
            tool.subject = args[2];
        }
        if (args.length > 3) {
            tool.durable = args[3].equalsIgnoreCase("true");
        }
        if (args.length > 4) {
            tool.messageCount = Integer.parseInt(args[4]);
        }
        if (args.length > 5) {
            tool.messageSize = Integer.parseInt(args[5]);
        }
        if (args.length > 6) {
            if (!"null".equals(args[6])) {
                tool.clientID = args[6];
            }
        }
        if (args.length > 7) {
            tool.timeToLive = Long.parseLong(args[7]);
        }
        if (args.length > 8) {
            tool.sleepTime = Long.parseLong(args[8]);
        }
        if (args.length > 9) {
            tool.transacted = "true".equals(args[9]);
        }
        if (args.length > 10) {
            tool.scriptFileName = args[10];
        }
        tool.run();
    }

    private void run() {
        System.out.println("Running JMS message send with script: "
                + scriptFileName);
        try {
            System.out.println("Connecting to URL: " + url);
            System.out.println("Publishing a Message with size " + messageSize
                    + " to " + (topic ? "topic" : "queue") + ": " + subject);
            System.out.println("Using " + (durable ? "durable" : "non-durable")
                    + " publishing");
            System.out.println("Sleeping between publish " + sleepTime + " ms");

            if (timeToLive != 0) {
                System.out.println("Messages time to live " + timeToLive
                        + " ms");
            }
            Connection connection = createConnection();
            Session session = createSession(connection);
            MessageProducer producer = createProducer(session);

            TextMessage message = session
                    .createTextMessage(getInputDataString(scriptFileName));

            producer.send(message);

            if (transacted) {
                session.commit();
            }

            System.out.println("Done.");
            close(connection, session);
        } catch (Exception e) {
            System.out.println("Caught: " + e);
            e.printStackTrace();
        }
    }

    protected MessageProducer createProducer(Session session)
            throws JMSException {
        MessageProducer producer = session.createProducer(destination);
        if (durable) {
            producer.setDeliveryMode(DeliveryMode.PERSISTENT);
        } else {
            producer.setDeliveryMode(DeliveryMode.NON_PERSISTENT);
        }
        if (timeToLive != 0)
            producer.setTimeToLive(timeToLive);

        return producer;
    }

    /*
     * Reads the specified file into memory, and returns a StringReader object
     * which reads from that in-memory buffer. <p> This method exists just to
     * demonstrate that the input to the digester doesn't need to be from a
     * file; for example, xml could be read from a database or generated
     * dynamically; any old buffer in memory can be processed by the digester.
     * <p> Clearly, if the data is always coming from a file, then calling the
     * Digester.parse method that takes a File object would be more sensible
     * (see AddressBook example).
     */
    // private static java.io.Reader getInputData(String filename) {
    // return new java.io.StringReader( getInputDataString(filename) );
    // }
    private static String getInputDataString(String filename) {
        java.io.File srcfile = new java.io.File(filename);

        java.io.ByteArrayOutputStream baos = new java.io.ByteArrayOutputStream(
                1000);
        byte[] buf = new byte[100];
        java.io.FileInputStream fis = null;
        try {
            fis = new java.io.FileInputStream(srcfile);
            for (;;) {
                int nread = fis.read(buf);
                if (nread == -1) {
                    break;
                }
                baos.write(buf, 0, nread);
            }
        } catch (java.io.IOException ioe) {
            ioe.printStackTrace();
        } finally {
            try {
                fis.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }

        return baos.toString();
    }
}
