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

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.BufferUnderflowException;
import java.nio.ByteBuffer;

import org.itadaki.bzip2.BZip2InputStream;

import com.raytheon.rcm.event.ConfigEvent;
import com.raytheon.rcm.event.ConfigEvent.Category;
import com.raytheon.rcm.event.RadarEvent;
import com.raytheon.rcm.event.RadarEventAdapter;
import com.raytheon.rcm.message.GraphicProduct;
import com.raytheon.rcm.message.GraphicProduct.PDB;
import com.raytheon.rcm.message.Message;
import com.raytheon.rcm.message.MessageFormatException;

/**
 * <p>A radar server component that delivers radar products to an EDEX file
 * endpoint.
 *
 * <p>This class is obsoleted by DataArchiveEndpoint.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ...
 * 2014-02-03   DR 14762   D. Friedman Refactor config events.
 * </pre>
*/
public class Awips2Endpoint extends RadarEventAdapter {

    RadarServer radarServer;

    File radarEndpointPath;

    String radarEndpointPathString;

    public Awips2Endpoint(RadarServer radarServer) {
        this.radarServer = radarServer;

        Log.eventf("Compressed products will %sbe decompressed", radarServer
                .getConfiguration().isDecompressProducts() ? "" : "not ");

        updateConfig();
    }

    private void updateConfig() {
        String pathName = radarServer.getConfiguration().getEdexEndpoint();
        File newPath = null;

        if (pathName != null && pathName.length() > 0) {
            newPath = new File(pathName);
            if (!newPath.exists() || !newPath.isDirectory()) {
                Log
                        .errorf(
                                "EDEX endpoint '%s' does not exist or is not a directory",
                                newPath);
                newPath = null;
            } else if (!newPath.canWrite()) {
                Log.errorf("EDEX endpoint '%s' is not writable", newPath);
                newPath = null;
            }
        } else {
            Log.errorf("EDEX endpoint not defined.");
        }
        radarEndpointPath = newPath;
    }

    @Override
    public void handleRadarEvent(RadarEvent event) {
        if (event.getType() == RadarEvent.MESSAGE_RECEIVED) {

            if (radarEndpointPath == null) {
                updateConfig();
                return;
            }

            byte[] msg = event.getMessageData();
            int code = Message.messageCodeOf(msg);

            // Send everything to EDEX except for the product list.
            if (code == Message.PRODUCT_LIST)
                return;

            msg = maybeDecompressProduct(msg);

            // where do I put the temp files?
            String name;
            try {
                name = String.format("%s.%s.%s", event.getRadarID(), code,
                        System.currentTimeMillis());
            } catch (Exception e) {
                name = Long.toString(System.currentTimeMillis());
            }
            String baseName = name;
            File outPath = null;
            int seq = 0;
            FileOutputStream fo;

            try {
                do {
                    outPath = new File(radarEndpointPath, name);
                    // TODO: test IOException if fails to create
                    if (outPath.createNewFile())
                        break;
                    name = String.format("%s.%s", baseName, ++seq);
                } while (true);

                fo = new FileOutputStream(outPath);
            } catch (IOException e) {
                Log.errorf("Failed to create or open '%s': %s", outPath, e);
                outPath.mkdir();
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
            } finally {
                if (fo != null) {
                    try {
                        fo.close();
                    } catch (IOException e) {
                        // nothing
                    }
                }
            }

            // TODO: tmpPath.renameTo(dest)..
        }
    }

    @Override
    public void handleConfigEvent(ConfigEvent event) {
        if (event.getCategory() == Category.GLOBAL_CONFIG)
            updateConfig();
    }

    private byte[] maybeDecompressProduct(byte[] msg) {
        if (!radarServer.getConfiguration().isDecompressProducts())
            return msg;

        try {

            int code = Message.messageCodeOf(msg);
            if (code > 16) {
                PDB pdb = GraphicProduct.pdbOfMessage(msg);
                if (pdb.isBzip2Compressed()) {
                    Log.event("decompressing product");
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
