/**
 * This class sets the BUFR Ssha section 4 data in an array for parsing. 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * -------		-------		--------	-----------
 * 09/11					Chin J Chen	Initial coding from BufrSshaSeparator
 * </pre>
 * 
 * @author Chin J. Chen
 * @version 1.0
 */

package gov.noaa.nws.ncep.edex.plugin.ssha.decoder;

import gov.noaa.nws.ncep.edex.plugin.ssha.util.SshaDescriptorDelegate;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.AbstractRecordSeparator;
import com.raytheon.uf.edex.decodertools.bufr.BUFRDataDocument;
import com.raytheon.uf.edex.decodertools.bufr.BUFRDocument;
import com.raytheon.uf.edex.decodertools.bufr.BUFRFile;
import com.raytheon.uf.edex.decodertools.bufr.descriptors.IDescriptorFactorySelector;
import com.raytheon.uf.edex.decodertools.bufr.packets.BUFRSublistPacket;
import com.raytheon.uf.edex.decodertools.bufr.packets.IBUFRDataPacket;

public class SshaSeparator extends AbstractRecordSeparator implements
        IDescriptorFactorySelector {

    private Log logger = LogFactory.getLog(getClass());

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
     *            The BufrSsha BUFR data.
     */
    @Override
    public void setData(byte[] bufrData, Headers headers) {
        currentRecord = -1;
        try {
            if (bufrData != null) {
                ;
                int start = 0;
                int len = bufrData.length - start;
                messageData = new byte[len];
                System.arraycopy(bufrData, start, messageData, 0, len);
                decodeBUFRData();
            }
        } finally {
            if ((records != null) && (records.size() > 0)) {
                currentRecord = 0;
            }
        }
    }

    /**
     * Decode the entire physical file that was used to create this separator.
     */
    @SuppressWarnings("unchecked")
    private void decodeBUFRData() {
        BUFRFile bfile = new BUFRFile(messageData,
                new SshaDescriptorDelegate(this));
        records = new ArrayList<BUFRDataDocument>();
        int bulletinNumber = 1;

        /*
         * Create an arrayList which stores all the binary records of the
         * BufrSsha data.
         * 
         * bulletin: a block of BufrSsha data, including Sections 0 to 5, which
         * starts with WMO header and an indicator key word "BUFR". A BUFR file
         * may contain multiple bulletins. bufrsshaData: Section 4 BufrSsha data
         * (BUFRDataDocument) in one bulletin which may contain multiple Bufr
         * records. record: one BufrSsha record which contains multiple
         * parameters, e.g., MSSH, TMBRST, SSHA. etc.
         */
        for (BUFRDocument bulletin : bfile) {
            logger.debug("Executing bulletin " + bulletinNumber);
            BUFRDataDocument bufrsshaData = bulletin.execute();
            List<IBUFRDataPacket> idps = bufrsshaData.getList();
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
        String retValue = "SSHA";
        return retValue;
    }
}