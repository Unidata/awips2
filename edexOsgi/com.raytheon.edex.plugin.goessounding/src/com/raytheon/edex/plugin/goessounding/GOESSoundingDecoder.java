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
package com.raytheon.edex.plugin.goessounding;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.edex.plugin.goessounding.dao.GOESSoundingDAO;
import com.raytheon.edex.plugin.goessounding.decoder.GOESSoundingDataAdapter;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.goessounding.GOESSounding;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.edex.decodertools.bufr.BUFRDataDocument;
import com.raytheon.uf.edex.decodertools.bufr.BUFRDocument;
import com.raytheon.uf.edex.decodertools.bufr.BUFRFile;
import com.raytheon.uf.edex.decodertools.bufr.descriptors.DefaultDescriptorDelegate;
import com.raytheon.uf.edex.decodertools.bufr.descriptors.IDescriptorFactoryDelegate;
import com.raytheon.uf.edex.decodertools.bufr.descriptors.IDescriptorFactorySelector;
import com.raytheon.uf.edex.decodertools.bufr.packets.BUFRSublistPacket;
import com.raytheon.uf.edex.decodertools.bufr.packets.IBUFRDataPacket;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * Perform decode on BUFR GOES satellite sounding data. Currently this decoder
 * assumes that the valid time is good. This decoder does check for possible
 * duplicate data in the database prior to returning the decoded data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 14, 2008 1077       jkorman     Initial implementation.
 * Nov 25, 2008 1684       chammack    Camel Refactor
 * May 15, 2013 1869       bsteffen    Remove DataURI from goes/poes soundings.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class GOESSoundingDecoder extends AbstractDecoder implements
        IDescriptorFactorySelector {

    // Name of the plugin controlling this decoder.
    public static final String PLUGIN_NAME = "goessounding";

    /** The logger */
    private Log logger = LogFactory.getLog(getClass());

    private PointDataDescription pdd;

    private IDescriptorFactoryDelegate delegate;

    private GOESSoundingDAO dao;

    // If failSafe is set to true, this plugin sinks the data but
    // does no processing!
    private boolean failSafe = false;

    /**
     * Construct a ProfilerDecoder instance.
     */
    public GOESSoundingDecoder() {
        try {
            pdd = PointDataDescription.fromStream(GOESSoundingDecoder.class
                    .getResourceAsStream("/res/pointdata/goes.xml"));

            logger.info("PointDataDescription loaded");

            delegate = new DefaultDescriptorDelegate(this);
        } catch (Exception e) {
            logger.error("PointDataDescription failed", e);
            logger.error("Plugin set to failSafe mode");
            failSafe = true;
        }
        try {
            createDAO(false);
        } catch (Exception e) {

        }

    }

    /**
     * Get one entry from the separator and interpret that data as a single
     * profiler observation.
     * 
     * @return A single decoded observation. May return null if the observation
     *         is a duplicate in the database or is invalid.
     */
    public PluginDataObject[] decode(GoesSoundingInput input, Headers headers)
            throws DecoderException {

        String traceId = "";
        if (headers != null) {
            traceId = (String) headers.get("traceId");
        }

        if (failSafe) {
            return new PluginDataObject[0];
        }

        PluginDataObject[] decodedData = null;

        if (input != null && input.getDocumentData().length > 0) {

            WMOHeader wmoHeader = input.getWmoHeader();

            if ((wmoHeader != null) && (wmoHeader.isValid())) {
                try {
                    byte[] messageData = input.getDocumentData();

                    List<BUFRDataDocument> document = decodeBUFRData(
                            messageData, delegate);
                    Iterator<BUFRDataDocument> iterator = document.iterator();
                    List<GOESSounding> pdoList = new ArrayList<GOESSounding>();

                    Map<File, PointDataContainer> container = new HashMap<File, PointDataContainer>();

                    while (iterator.hasNext()) {

                        GOESSounding soundingData = GOESSoundingDataAdapter
                                .createSoundingData(iterator.next(), wmoHeader,
                                        container, this.pdd, dao);
                        if (soundingData != null) {
                            soundingData.setTraceId(traceId);
                            soundingData.setPluginName(PLUGIN_NAME);
                            PointDataView pdv = soundingData.getPointDataView();
                            pdv.setString("wmoHeader",
                                    soundingData.getWmoHeader());
                            pdoList.add(soundingData);

                        }
                    }

                    if (pdoList.size() > 0) {
                        decodedData = pdoList
                                .toArray(new PluginDataObject[pdoList.size()]);
                    } else {
                        decodedData = new PluginDataObject[0];
                    }
                } catch (Exception ee) {
                    logger.error(traceId + "- Decoder error", ee);
                } finally {
                    if (decodedData == null) {
                        decodedData = new PluginDataObject[0];
                    }
                }
            } else {
                logger.error(traceId + "- Missing or invalid WMOHeader");
                decodedData = new PluginDataObject[0];
            }
        } else {
            logger.info(traceId + "- No data in file");
            decodedData = new PluginDataObject[0];
        }

        return decodedData;
    }

    /**
     * Decode the entire physical file.
     * 
     * @param messageData
     *            The data message to decode.
     */
    @SuppressWarnings("unchecked")
    private List<BUFRDataDocument> decodeBUFRData(byte[] messageData,
            IDescriptorFactoryDelegate delegate) {
        BUFRFile bFile = new BUFRFile(messageData, delegate);
        List<BUFRDataDocument> reportData = new ArrayList<BUFRDataDocument>();

        for (BUFRDocument doc : bFile) {
            BUFRDataDocument docData = doc.execute();

            List<IBUFRDataPacket> data = docData.getList();
            for (IBUFRDataPacket packet : data) {
                if (packet != null) {
                    if (packet instanceof BUFRSublistPacket) {
                        List<IBUFRDataPacket> p = (List<IBUFRDataPacket>) packet
                                .getValue();
                        BUFRDataDocument dd = new BUFRDataDocument(p);
                        dd.setEnclosingDocument(docData.getEnclosingDocument());
                        reportData.add(dd);
                        logger.debug("Number of packet subsets = "
                                + data.size());
                    }
                }
            }
        }

        return reportData;

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
     * 
     * @param recreate
     */
    protected void createDAO(boolean recreate) {
        if (recreate) {
            dao = null;
        }
        try {
            dao = new GOESSoundingDAO(PLUGIN_NAME);
        } catch (Exception e) {
            logger.error("GOESSoundingDAO creation failed", e);
            logger.error("Plugin set to failSafe mode");
            failSafe = true;
        }
    }

}
