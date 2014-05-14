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
package com.raytheon.uf.edex.bufrtools;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.decodertools.bufr.BUFRDataDocument;
import com.raytheon.uf.edex.decodertools.bufr.BUFRDocument;
import com.raytheon.uf.edex.decodertools.bufr.BUFRFile;
import com.raytheon.uf.edex.decodertools.bufr.descriptors.IDescriptorFactoryDelegate;
import com.raytheon.uf.edex.decodertools.bufr.descriptors.IDescriptorFactorySelector;
import com.raytheon.uf.edex.decodertools.bufr.packets.BUFRSublistPacket;
import com.raytheon.uf.edex.decodertools.bufr.packets.IBUFRDataPacket;

/**
 * Abstract base class for implementing BUFR data decoders using subclasses of
 * BUFRPointDataAdapter. In use the public decode method is exposed via the bean
 * instance. The abstract decodeData method is implemented by the specific
 * subclass to actually perform the data decode.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 19, 2009       2519 jkorman     Initial creation
 * May 14, 2014 2536       bclement    moved WMO Header to common
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public abstract class AbstractBUFRDecoder implements IDescriptorFactorySelector {

    /** The logger */
    protected Log logger = LogFactory.getLog(getClass());

    protected String pluginName;

    // If failSafe is set to true, this plugin sinks the data but
    // does no processing!
    private boolean failSafe = false;

    private IDescriptorFactoryDelegate delegate;

    /**
     * 
     * @param pluginName
     */
    protected AbstractBUFRDecoder(String pluginName) {
        this.pluginName = pluginName;
        createDAO(false);
    }

    /**
     * 
     * @param data
     * @param headers
     * @return
     */
    public PluginDataObject[] decode(byte[] data, Headers headers) {

        String traceId = null;
        WMOHeader wmoHeader;

        PluginDataObject[] decodedData = null;

        if (headers != null) {
            traceId = (String) headers.get("traceId");
        }
        if (isFailSafe()) {
            return new PluginDataObject[0];
        }

        logger.debug(traceId + " - Decoding data");
        try {
            if (data != null && data.length > 0) {
                String fileName = (String) headers
                        .get(WMOHeader.INGEST_FILE_NAME);
                wmoHeader = new WMOHeader(data, fileName);
                if ((wmoHeader != null) && (wmoHeader.isValid())) {

                    int start = wmoHeader.getMessageDataStart();
                    int len = data.length - start;

                    byte[] messageData = new byte[len];
                    System.arraycopy(data, start, messageData, 0, len);

                    IDescriptorFactoryDelegate delegate = getFactoryDelegate();

                    List<BUFRDataDocument> document = decodeBUFRData(
                            messageData, delegate);

                    List<PluginDataObject> obsList = null;
                    try {
                        logger.debug(traceId + " - decodeData() < ");
                        obsList = decodeData(document, traceId, wmoHeader);
                        logger.debug(traceId + " - decodeData() > "
                                + ((obsList != null) ? obsList.size() : 0));
                    } catch (Exception e) {
                        logger.error(traceId + "-Error in decode", e);
                    } finally {
                        if ((obsList != null) && (obsList.size() > 0)) {
                            decodedData = obsList
                                    .toArray(new PluginDataObject[obsList
                                            .size()]);
                            logger.info(String.format("%s - Decoded %d obs",
                                    traceId, decodedData.length));
                        } else {
                            decodedData = null;
                            logger.info(String.format("%s - Decoded no obs",
                                    traceId));
                        }
                    }
                } else {
                    logger.error(traceId + "- Missing or invalid WMOHeader");
                    decodedData = null;
                }
            } else {
                logger.info(traceId + "- No data in file");
                decodedData = null;
            }
        } catch (Exception e) {
            logger.error(traceId + "- Decoder error", e);
        } finally {
            if (decodedData == null) {
                decodedData = new PluginDataObject[0];
            }
        }
        return decodedData;
    }

    /**
     * 
     * @param data
     * @param headers
     * @return
     */
    protected abstract List<PluginDataObject> decodeData(
            List<BUFRDataDocument> document, String traceId, WMOHeader wmoHeader);

    /**
     * 
     * @param recreate
     */
    protected abstract void createDAO(boolean recreate);

    /**
     * 
     * @return
     */
    protected IDescriptorFactoryDelegate getFactoryDelegate() {
        return delegate;
    }

    /**
     * 
     * @return
     */
    protected void setFactoryDelegate(IDescriptorFactoryDelegate fDelegate) {
        delegate = fDelegate;
    }

    /**
     * @return the failSafe
     */
    public boolean isFailSafe() {
        return failSafe;
    }

    /**
     * @param failSafe
     *            the failSafe to set
     */
    public void setFailSafe(boolean failSafe) {
        this.failSafe = failSafe;
    }

    /**
     * @return the pluginName
     */
    public String getPluginName() {
        return pluginName;
    }

    /**
     * Get the selector associated with this separators data. ProfilerSeparator
     * always returns "DEFAULT".
     * 
     * @return Return the string value "DEFAULT".
     * @see com.raytheon.edex.tools.bufr.descriptors.IDescriptorFactorySelector#getSelector()
     */
    @Override
    public String getSelector() {
        return "DEFAULT";
    }

    /**
     * Decode the entire physical file.
     * 
     * @param messageData
     *            The data message to decode.
     */
    @SuppressWarnings("unchecked")
    protected List<BUFRDataDocument> decodeBUFRData(byte[] messageData,
            IDescriptorFactoryDelegate delegate) {
        BUFRFile bFile = new BUFRFile(messageData, delegate);
        List<BUFRDataDocument> reportData = new ArrayList<BUFRDataDocument>();

        for (BUFRDocument doc : bFile) {
            BUFRDataDocument docData = null;
            try {
                docData = doc.execute();

                List<IBUFRDataPacket> data = docData.getList();
                for (IBUFRDataPacket packet : data) {
                    if (packet != null) {
                        if (packet instanceof BUFRSublistPacket) {
                            List<IBUFRDataPacket> p = (List<IBUFRDataPacket>) packet
                                    .getValue();
                            BUFRDataDocument dd = new BUFRDataDocument(p);
                            dd.setEnclosingDocument(docData
                                    .getEnclosingDocument());
                            reportData.add(dd);
                            logger.debug("Number of packet subsets = "
                                    + data.size());
                        }
                    }
                }
            } catch (Exception e) {
                logger.error("Error decoding BUFR ", e);
            }
        } // for
        return reportData;
    }

}
