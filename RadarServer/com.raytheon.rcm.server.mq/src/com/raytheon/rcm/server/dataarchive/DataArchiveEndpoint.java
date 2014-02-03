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
package com.raytheon.rcm.server.dataarchive;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.BufferUnderflowException;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Scanner;
import java.util.regex.Pattern;

import javax.jms.JMSException;
import javax.jms.MessageProducer;
import javax.jms.Queue;
import javax.jms.QueueConnection;
import javax.jms.QueueSession;
import javax.jms.Session;
import javax.jms.TextMessage;

import org.apache.qpid.client.AMQConnectionFactory;
import org.itadaki.bzip2.BZip2InputStream;

import com.raytheon.rcm.config.EndpointConfig;
import com.raytheon.rcm.config.awips1.Awips1ProdDistInfoBuilder;
import com.raytheon.rcm.event.ConfigEvent;
import com.raytheon.rcm.event.ConfigEvent.Category;
import com.raytheon.rcm.event.RadarEvent;
import com.raytheon.rcm.event.RadarEventAdapter;
import com.raytheon.rcm.message.GraphicProduct;
import com.raytheon.rcm.message.GraphicProduct.PDB;
import com.raytheon.rcm.message.Message;
import com.raytheon.rcm.message.MessageFormatException;
import com.raytheon.rcm.products.ProductInfo;
import com.raytheon.rcm.products.RadarProduct;
import com.raytheon.rcm.products.RadarProduct.Param;
import com.raytheon.rcm.server.Log;
import com.raytheon.rcm.server.RadarServer;

/**
 * Stores radar messages in a directory structure and notifies EDEX via JMS.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ...
 * 2014-02-03   DR 14762   D. Friedman Refactor config events.
 * </pre>
 *
 */
public class DataArchiveEndpoint extends RadarEventAdapter {
    private RadarServer radarServer;

    private File dataArchivePath;

    private boolean prefixPathWithRadar;

    private Sender sender = new Sender();

    private HashSet<Integer> noDecompressList = new HashSet<Integer>();

    private static final String MESSAGE_HEADER_HEADER = "header";

    private static final String MESSAGE_HEADER_ENQUEUE_TIME = "enqueueTime";

    /**
     * Specifies the "header" field in the notification message sent to EDEX.
     * The first argument is the radar name. The second argument is the product
     * number.
     */
    private static final String HEADER_VALUE_FORMAT = "RadarServer.%s.%s.";

    private static final int DEFAULT_MAX_BACKLOG_PER_RADAR = 3000;

    private static class Item {
        public String path;

        public String header;

        public Item(String path, String header) {
            this.path = path;
            this.header = header;
        }
    }

    private class Sender {
        private ArrayList<Item> backlog = new ArrayList<Item>();

        private QueueConnection queueConnection;

        private QueueSession queueSession;

        private MessageProducer messageProducer;

        // TODO: A time limit would be better
        private int maxBacklogSize = DEFAULT_MAX_BACKLOG_PER_RADAR;

        public void start() {
            tryConnect();
        }

        private synchronized boolean tryConnect() {
            if (queueConnection == null) {
                Log.event("Trying to connect to qpid");
                EndpointConfig endpointConfig = radarServer.getConfiguration()
                        .getEndpointConfig();

                if (!isJMSConfigurationValid(endpointConfig))
                    return false;

                String url = endpointConfig.getConnectionURL();

                QueueConnection conn = null;
                try {
                    AMQConnectionFactory connFac = new AMQConnectionFactory(url);
                    conn = connFac.createQueueConnection();
                    QueueSession session = conn.createQueueSession(false,
                            Session.AUTO_ACKNOWLEDGE);
                    Queue queue = session
                            .createQueue(endpointConfig.getTopic());
                    MessageProducer producer = session.createProducer(queue);
                    conn.start(); // Not receiving any messages so this is not
                    // strictly necessary?
                    this.queueConnection = conn;
                    this.queueSession = session;
                    this.messageProducer = producer;
                } catch (Throwable e) {
                    Log.errorf("Failed to connect to JMS queue %s (%s): %s",
                            url, endpointConfig.getTopic(), e);
                    if (conn != null) {
                        try {
                            conn.close();
                        } catch (JMSException e2) {
                            // ignore
                        }
                        conn = null;
                    }
                }
            }
            return queueConnection != null;
        }

        public synchronized void send(Item item) {
            if (send2(item)) {
                if (backlog.size() > 0) {
                    int i;
                    for (i = 0; i < backlog.size(); ++i) {
                        if (!send2(backlog.get(i)))
                            break;
                    }
                    backlog.subList(0, i).clear();
                }
            } else {
                backlog.add(item);
                if (backlog.size() > maxBacklogSize * 3 / 2)
                    backlog.subList(0, backlog.size() - maxBacklogSize).clear();
            }
        }

        private boolean send2(Item item) {
            if (tryConnect()) {
                try {
                    TextMessage msg = queueSession.createTextMessage();
                    msg.setStringProperty(MESSAGE_HEADER_HEADER, item.header);
                    msg.setLongProperty(MESSAGE_HEADER_ENQUEUE_TIME,
                            System.currentTimeMillis());
                    msg.setText(item.path);
                    messageProducer.send(msg);
                    return true;
                } catch (JMSException e) {
                    Log.errorf("Failed to send notification: %s", e);
                    disconnect();
                }
            }
            return false;
        }

        public synchronized void disconnect() {
            Log.event("Disconnecting from qpid...");
            if (queueConnection != null) {
                QueueConnection connToClose = queueConnection;
                queueConnection = null;
                try {
                    connToClose.close();
                } catch (JMSException e) {
                    Log.warnf("Failed to close the connection : %s", e);
                    // ignore
                } catch (Throwable t) {
                    Log.errorf("Failed to close the connection : %s", t);
                }
            }
        }

        public void setMaxBacklogSize(int maxBacklogSize) {
            this.maxBacklogSize = maxBacklogSize;
        }
    }

    public DataArchiveEndpoint(RadarServer radarServer) {
        this.radarServer = radarServer;

        Log.eventf("Compressed products will %sbe decompressed", radarServer
                .getConfiguration().isDecompressProducts() ? "" : "not ");

        updateConfig();
        sender.start();
    }

    private static boolean isJMSConfigurationValid(EndpointConfig config) {
        return config != null && config.getConnectionURL() != null
                && config.getConnectionURL().length() > 0
                && config.getTopic() != null && config.getTopic().length() > 0;
    }

    private void updateConfig() {

        EndpointConfig config = radarServer.getConfiguration()
                .getEndpointConfig();
        File newPath = null;

        prefixPathWithRadar = false;

        if (config != null) {
            int backlogLimit = config.getBacklogLimitPerRadar() != null ? config
                    .getBacklogLimitPerRadar() : DEFAULT_MAX_BACKLOG_PER_RADAR;
            sender.setMaxBacklogSize(backlogLimit
                    * Math.max(1, radarServer.getConfiguration()
                            .getConfiguredRadarList().size()));

            String pathName = config.getArchiveRoot();
            if (pathName != null && pathName.length() > 0) {
                newPath = new File(pathName);
                if (!newPath.exists() || !newPath.isDirectory()
                        || !newPath.canWrite()) {
                    Log.errorf(
                            "Data archive root '%s' does not exist, is not a directory, or is not writable",
                            newPath);
                    newPath = null;
                }
            }

            if (config.getPrefixPathWithRadar() != null)
                prefixPathWithRadar = config.getPrefixPathWithRadar();

            if (!isJMSConfigurationValid(config))
                Log.errorf("EDEX JMS notification is not configured correctly");
            sender.disconnect();

        } else {
            Log.errorf("Data archive endpoint configuration was not defined.");
        }

        dataArchivePath = newPath;

        loadNoDecompressList();
    }

    private void loadNoDecompressList() {
        try {
            HashSet<Integer> newList = new HashSet<Integer>();
            InputStream ins = getClass().getResourceAsStream(
                    "no_decompress.txt");
            if (ins != null) {
                try {
                    Scanner s = new Scanner(ins);
                    while (s.hasNext()) {
                        if (s.hasNextInt()) {
                            int productID = s.nextInt();
                            newList.add(productID);
                        } else {
                            s.next();
                        }
                    }
                } finally {
                    ins.close();
                }
            }
            noDecompressList = newList;
        } catch (Exception e) {
            Log.errorf("Error loading no-decompress list: %s", e);
        }
    }

    private boolean checkConfig() {
        if (dataArchivePath == null)
            updateConfig();
        return dataArchivePath != null;
    }

    @Override
    public void handleRadarEvent(RadarEvent event) {
        if (event.getType() == RadarEvent.MESSAGE_RECEIVED) {

            if (!checkConfig())
                return;

            byte[] msg = event.getMessageData();

            msg = maybeDecompressProduct(msg);

            String pathName = getArchivePathForMessage(event.getRadarID(), msg);

            // pathName may be null. validateArchivePath handles this.
            File outPath = validateArchivePath(pathName);
            if (outPath == null) {
                Log.errorf("Rejecting data archive path name '%s'", pathName);
                pathName = getFallbackArchivePathForMessage(event.getRadarID(),
                        msg);
                outPath = validateArchivePath(pathName);
                if (outPath == null)
                    return;
            }

            if (!preparePath(outPath))
                return;

            String header = String.format(HEADER_VALUE_FORMAT,
                    event.getRadarID(), Message.messageCodeOf(msg));

            File parentDirectory = outPath.getParentFile();
            String baseName = String.format("%s.%s.%s", event.getRadarID(),
                    Message.messageCodeOf(msg), outPath.getName());
            String name = baseName;
            int seq = 0;
            FileOutputStream fo;

            try {
                do {
                    outPath = new File(parentDirectory, name);
                    if (outPath.createNewFile())
                        break;
                    name = String.format("%s.%s", baseName, ++seq);
                } while (true);

                fo = new FileOutputStream(outPath);
            } catch (IOException e) {
                Log.errorf("Failed to create or open '%s': %s", outPath, e);
                return;
            }

            try {
                try {
                    fo.write(msg);
                    fo.close();
                    fo = null;
                } catch (IOException e) {
                    Log.errorf("Failed to write to '%s': %s", outPath, e);
                }
                Log.eventf("Stored message in %s", outPath);
                sender.send(new Item(outPath.getAbsolutePath(), header));
            } finally {
                if (fo != null) {
                    try {
                        fo.close();
                    } catch (IOException e) {
                        // nothing
                    }
                }
            }
        }
    }

    private boolean preparePath(File outPath) {
        File parentDir = outPath.getParentFile();

        if (!parentDir.exists()) {
            ArrayList<File> toChmod = new ArrayList<File>();
            File dir = parentDir;

            while (!dir.exists() && !dir.equals(dataArchivePath)) {
                toChmod.add(dir);
                dir = dir.getParentFile();
            }

            if (!parentDir.mkdirs()) {
                Log.errorf("Could not create directory '%s'", parentDir);
                return false;
            }

            Process p = null;
            try {
                ArrayList<String> cmd = new ArrayList<String>(
                        2 + toChmod.size());
                cmd.add("chmod");
                cmd.add("g+w");
                for (File f : toChmod)
                    cmd.add(f.getAbsolutePath());
                p = Runtime.getRuntime().exec(
                        cmd.toArray(new String[cmd.size()]));
                p.waitFor();
            } catch (InterruptedException e) {
                // ignore
            } catch (IOException e) {
                // ignore
            } finally {
                if (p != null) {
                    p.destroy();
                }
            }
        }

        if (parentDir.isDirectory())
            return true;
        else {
            Log.errorf("Data archive path '%s' is not a directory", parentDir);
            return false;
        }
    }

    private String getFallbackArchivePathForMessage(String radarID, byte[] msg) {
        String codeString;

        try {
            codeString = Integer.toString(Message.messageCodeOf(msg));
        } catch (Exception e) {
            codeString = "unknown";
            // Probably should return null. How could it be a valid product?
        }

        return String.format("%s%s/other/%s.%s", prefixPathWithRadar ? "radar/"
                : "", radarID != null ? radarID : "unknown", codeString, System
                .currentTimeMillis());
    }

    private File validateArchivePath(String pathName) {
        File result = null;

        if (pathName != null) {
            String[] inPieces = pathName.split(Pattern.quote("/"));
            StringBuilder outPath = new StringBuilder(64);

            // Reject if last component (the file name) is empty.
            if (inPieces.length < 1
                    || inPieces[inPieces.length - 1].length() < 1)
                return null;

            for (String s : inPieces) {
                if (s.length() < 1)
                    continue;

                if (s.equals(".."))
                    return null;

                // outPieces.add(s);
                if (outPath.length() > 0)
                    outPath.append(File.separator);
                outPath.append(s);
            }

            result = new File(dataArchivePath, outPath.toString());
        }

        return result;
    }

    /*
     * Products that are stored in a "layer0" subdirectory even though the
     * product has no layer parameter.
     */
    private HashSet<Integer> layerZeroProducts = new HashSet<Integer>(
            Arrays.asList(80, // STP
                    138, // STP
                    78, // OHP
                    79, // THP
                    31, // USP
                    144, // OSW
                    145, // OSD
                    146, // SSW
                    147, // SSD
                    150, // USW
                    151, // USD
                    32, // DHR
                    33, // HSR
                    137, // ULR
                    57, // VIL,
                    134, // DVL,
                    41, // ET
                    135, // EET
                    35, 36, 37, 38, // CZ
                    81, // DPA
                    82, // SPD
                    50, 85, // RCS
                    51, 86, // VCS
                    177, // HHC
                    176, // DPR,
                    169, // OHA
                    171, // STA
                    172, // STA/DSA
                    174, // DOD
                    175, // DSD
                    0));

    private HashMap<String, String> mnemonicMap = new HashMap<String, String>();
    {
        mnemonicMap.put("HZ", "Z");
        mnemonicMap.put("HV", "V");
        mnemonicMap.put("HSW", "SW");
    }

    private String getArchivePathForMessage(String radarID, byte[] msg) {
        try {
            int code = Message.messageCodeOf(msg);

            String radarCompon = radarID;
            String mneCompon = "";
            String elevCompon = "";
            String layerCompon = "";
            String resCompon = "";
            String azResCompon = "";
            String levelsCompon = "";
            Calendar timeToUse;

            RadarProduct rp = ProductInfo.getInstance().getPoductForCode(code);
            if (rp == null) {
                Log.errorf("Unknown message code %d", code);
                return null;
            }
            mneCompon = rp.mnemonic;

            String newMnemonic = mnemonicMap.get(mneCompon);
            if (newMnemonic != null)
                mneCompon = newMnemonic;

            if (code > 16) {
                PDB pdb = GraphicProduct.pdbOfMessage(msg);
                // Elevation and layer are mutually exclusive
                if (rp.params.contains(Param.ELEVATION)) {
                    int elev = pdb.getElevationAngle();
                    int bucketElev = Awips1ProdDistInfoBuilder
                            .getAngleGroup(elev);
                    String elevStr = String.format("%.1f", bucketElev / 10.0);
                    elevCompon = "elev" + elevStr.replace('.', '_');
                } else {
                    Integer layer = null;
                    if (rp.layer != null)
                        layer = rp.layer;
                    else if (layerZeroProducts.contains(code))
                        layer = 0;
                    else if (rp.params.contains(Param.CFC_BITMAP))
                        layer = pdb.getElevationSegmentNumber();
                    else if (code == 173) {
                        /*
                         * From RadarStorageController.C:730. Uses whole hours
                         * instead of ranges and is limited to 6 hours. Not sure
                         * why...
                         */

                        layer = 0; // Default to zero

                        int timeSpan = pdb.getTimeSpan();
                        if ((timeSpan % 60) == 0) {
                            int hours = timeSpan / 60;
                            if (hours >= 1 && hours <= 6)
                                layer = hours;
                        }
                    }

                    if (layer != null)
                        layerCompon = String.format("layer%d", layer);
                }

                /*
                 * Only add resolution and level components if there was an
                 * elevation or layer component.
                 */
                if (elevCompon.length() > 0 || layerCompon.length() > 0) {
                    if (rp.resolution != null && rp.resolution != 0) {
                        float res = rp.resolution;

                        if ((code >= 144 && code <= 147) || code == 150
                                || code == 151) {
                            /*
                             * AWIPS 1 mistakenly (?) puts these in a
                             * subdirectory named "res2".
                             */
                            res = 2;
                        }

                        int resInt = (int) res;
                        String resStr;

                        if (res - resInt == 0) {
                            resStr = Integer.toString(resInt);
                        } else {
                            resStr = String.format("%.2f", res).replace('.',
                                    '_');
                        }
                        resCompon = "res" + resStr;
                    }

                    if (rp.azimuthalResolution != null
                            && rp.azimuthalResolution != 0) {
                        String resStr = String.format("az%.1f",
                                rp.azimuthalResolution);
                        azResCompon = resStr.replace('.', '_');
                    }

                    if (rp.levels != null && rp.levels != 0)
                        levelsCompon = String.format("level%d", rp.levels);
                }

                timeToUse = pdb.volumeScanTime;
            } else {
                timeToUse = Message.decodeHeader(msg).time;
            }

            String fileName = String.format("%1$tY%1$tm%1$td_%1$tH%1$tM",
                    timeToUse);

            StringBuilder sb = new StringBuilder(64);
            if (prefixPathWithRadar)
                sb.append("radar/");
            sb.append(radarCompon).append('/').append(mneCompon).append('/')
                    .append(elevCompon).append('/').append(layerCompon)
                    .append('/').append(resCompon).append('/')
                    .append(azResCompon).append('/').append(levelsCompon)
                    .append('/').append(fileName);

            return sb.toString();
        } catch (Exception e) {
            Log.errorf("Error while determining archive path for message: %s",
                    e);
        }
        return null;
    }

    @Override
    public void handleConfigEvent(ConfigEvent event) {
        if (event.getCategory() == Category.GLOBAL_CONFIG)
            updateConfig();
    }

    private byte[] maybeDecompressProduct(byte[] msg) {
        /*
         * The data archive supports applications that assume products will be
         * uncompressed so do this unconditionally.
         */

        try {

            int code = Message.messageCodeOf(msg);
            if (code > 16 && !noDecompressList.contains(code)) {
                PDB pdb = GraphicProduct.pdbOfMessage(msg);
                if (pdb.isBzip2Compressed()) {
                    int uncompressedSize = pdb.getUncompressedSize();
                    byte[] uncompressed;
                    try {
                        InputStream ins = new ByteArrayInputStream(msg, 120,
                                msg.length - 120);
                        ins = new BZip2InputStream(ins, false);
                        // ByteArrayOutputStream outs = new
                        // ByteArrayOutputStream(uncompressedSize);
                        uncompressed = new byte[uncompressedSize];
                        ins.read(uncompressed);
                    } catch (IOException e) {
                        Log.errorf("Error decompressing product: %s", e);
                        return msg;
                    }
                    byte[] result = new byte[120 + uncompressed.length];
                    System.arraycopy(msg, 0, result, 0, 120);
                    System.arraycopy(uncompressed, 0, result, 120,
                            uncompressed.length);

                    ByteBuffer buf = ByteBuffer.wrap(result);
                    buf.putInt(8, result.length); // Modify size of entire
                    // product
                    buf.putShort(100, (short) 0); // Mark as uncompressed

                    msg = result;
                }
            }

        } catch (MessageFormatException e) {
            /*
             * Probably because this a product without a PDB. This is not an
             * error.
             */
        } catch (BufferUnderflowException e) {
            // See above
        }

        return msg;
    }

}
