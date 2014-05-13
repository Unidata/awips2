/**
 * SgwhSeparator
 *
 * This class sets the BUFR BufrSgwh section 4 data in an array for parsing. 
 *
 ** <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * -------		-------		--------	-----------
 * 08/18/2011               Chin Chen   Initial coding from BufrSgwhSeparator
 * </pre>
 *
 * @author Chin J. Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.edex.plugin.sgwh.decoder;

import gov.noaa.nws.ncep.edex.plugin.sgwh.util.SgwhDescriptorDelegate;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.AbstractRecordSeparator;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.decodertools.bufr.BUFRDataDocument;
import com.raytheon.uf.edex.decodertools.bufr.BUFRDocument;
import com.raytheon.uf.edex.decodertools.bufr.BUFRFile;
import com.raytheon.uf.edex.decodertools.bufr.descriptors.IDescriptorFactorySelector;
import com.raytheon.uf.edex.decodertools.bufr.packets.BUFRSublistPacket;
import com.raytheon.uf.edex.decodertools.bufr.packets.IBUFRDataPacket;

public class SgwhSeparator extends AbstractRecordSeparator implements
        IDescriptorFactorySelector {

    private Log logger = LogFactory.getLog(getClass());

    private WMOHeader wmoHeader = null;

    // Raw message data
    private byte[] messageData = null;

    // List of BUFR documents created from the physical message
    private List<BUFRDataDocument> records = null;

    // Pointer to the current BUFR document to be processed.
    private int currentRecord = -1;

    /*
     * 
     * (non-Javadoc)
     * 
     * @see java.util.Iterator#next()
     */
    public Object next() {
        BUFRDataDocument data = null;
        if (hasNext()) {
            data = records.get(currentRecord);
            currentRecord++;
        }
        return data;
    }

    /**
     * Is there another record available?
     * 
     * @return Is there another record available?
     */
    @Override
    public boolean hasNext() {
        boolean hasMore = (records != null);
        hasMore = hasMore && (records.size() > 0);
        hasMore = hasMore && (currentRecord < records.size());
        return hasMore;
    }

    /**
     * Set the raw BUFR data and invoke the internal message separation process.
     * 
     * @param bufrData
     *            The BufrSgwh BUFR data.
     */
    @Override
    public void setData(byte[] bufrData, Headers headers) {
        currentRecord = -1;
        try {
            if (bufrData != null) {
                wmoHeader = new WMOHeader(bufrData);
                if ((wmoHeader != null) && (wmoHeader.isValid())) {
                    int start = wmoHeader.getMessageDataStart();
                    int len = bufrData.length - start;
                    messageData = new byte[len];
                    System.arraycopy(bufrData, start, messageData, 0, len);
                    decodeBUFRData();
                }
            }
        } finally {
            if ((records != null) && (records.size() > 0)) {
                currentRecord = 0;
            }
        }
    }

    /**
     * Get the WMO header associated with the message data.
     * 
     * @return The WMO header.
     */
    public WMOHeader getWmoHeader() {
        return wmoHeader;
    }

    /**
     * Decode the entire physical file that was used to create this separator.
     */
    @SuppressWarnings("unchecked")
    private void decodeBUFRData() {
        BUFRFile bfile = new BUFRFile(messageData,
                new SgwhDescriptorDelegate(this));
        records = new ArrayList<BUFRDataDocument>();
        int bulletinNumber = 1;

        /*
         * Create an arrayList which stores all the binary records of the
         * BufrSgwh data.
         * 
         * bulletin: a block of BufrSgwh data, including Sections 0 to 5, which
         * starts with WMO header and an indicator key word "BUFR". A BUFR file
         * may contain multiple bulletins. bufrsgwhData: Section 4 BUFRSGWH data
         * (BUFRDataDocument) in one bulletin which may contain multiple
         * BufrSgwh records. record: one BUFRSGWH record which contains multiple
         * parameters, e.g., TMPK, RELH, etc.
         */

        for (BUFRDocument bulletin : bfile) {
            logger.debug("Executing bulletin " + bulletinNumber);
            BUFRDataDocument bufrsgwhData = bulletin.execute();
            List<IBUFRDataPacket> idps = bufrsgwhData.getList();
            logger.debug("Number of records = " + idps.size() + " in bulletin "
                    + bulletinNumber);
            bulletinNumber++;
            for (IBUFRDataPacket record : idps) {
                if (record != null) {
                    if (record instanceof BUFRSublistPacket) {
                        List<IBUFRDataPacket> idps2 = (List<IBUFRDataPacket>) record
                                .getValue();
                        BUFRDataDocument dd = new BUFRDataDocument(idps2);
                        records.add(dd);
                    }
                }
            }
        }
    }

    /**
     * Get the selector value associated with this separator.
     * 
     * @return The selector associated with this separator.
     */
    @Override
    public String getSelector() {
        return null;
    }
}
