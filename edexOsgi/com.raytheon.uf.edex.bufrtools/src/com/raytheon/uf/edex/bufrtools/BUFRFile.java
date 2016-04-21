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

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.edex.bufrtools.descriptors.IDescriptorFactoryDelegate;
import com.raytheon.uf.edex.decodertools.core.DecoderTools;

/**
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20080204            862 jkorman     Initial development. Factored from
 *                                     BUFRSeparator.
 * 9/16/2014    #3628      mapeters    Moved from uf.edex.decodertools plugin.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class BUFRFile implements Iterable<BUFRDocument> {

    private Log logger = LogFactory.getLog(getClass());

    private List<BUFRDocument> documents;

    private byte[] fileData;

    private IDescriptorFactoryDelegate factoryDelegate;

    /**
     * Create a BUFRFile object using a file path
     * 
     * @param path
     *            A file path to a file containing the potential BUFR
     *            document(s).
     */
    public BUFRFile(String path, IDescriptorFactoryDelegate delegate) {
        factoryDelegate = delegate;
        loadData(path);
        populateDocuments();
    }

    /**
     * Create a BUFRFile object using a byte array data previously read.
     * 
     * @param bufrData
     *            A byte array containing the potential BUFR document(s).
     */
    public BUFRFile(byte[] bufrData, IDescriptorFactoryDelegate delegate) {
        factoryDelegate = delegate;
        if ((bufrData != null) && (bufrData.length > 0)) {
            fileData = new byte[bufrData.length];
            System.arraycopy(bufrData, 0, fileData, 0, fileData.length);
        }
        populateDocuments();
    }

    /**
     * Is this a valid BUFR file. Does it contain at least one BUFRDocument.
     * 
     * @return Is this a valid BUFR file?
     */
    public boolean isValid() {
        return (documents != null);
    }

    /**
     * Get the count of BUFR documents found in this file.
     * 
     * @return The count of BUFR documents. Returns -1 if no BUFRDocuments were
     *         found in the BUFRFile.
     */
    public int getDocumentCount() {
        int count = -1;
        if (isValid()) {
            count = documents.size();
        }
        return count;
    }

    /**
     * Get a specified BUFR document from the BUFRFile.
     * 
     * @param index
     *            The index of the document to get.
     * @return The requested BUFRDocument instance, or null if this is not a
     *         valid BUFRFile.
     */
    public BUFRDocument getDocument(int index) {
        BUFRDocument document = null;
        if (isValid() && (documents.size() > 0)) {
            document = documents.get(index);
        }
        return document;
    }

    /**
     * Returns an Iterator to the internal BUFRDocuments in this BUFRFile. If
     * the BUFRFile is not valid, an empty Iterator instance is returned.
     * 
     * @return An iterator to the BUFRDocument(s) in this BUFRFile.
     */
    @Override
    public Iterator<BUFRDocument> iterator() {
        Iterator<BUFRDocument> iter = null;
        if (isValid()) {
            iter = documents.iterator();
        } else {
            iter = new Iterator<BUFRDocument>() {
                @Override
                public boolean hasNext() {
                    return false;
                }

                @Override
                public BUFRDocument next() {
                    return null;
                }

                @Override
                public void remove() {
                }
            };
        }
        return iter;
    }

    /**
     * 
     */
    private void populateDocuments() {
        if ((fileData != null) && (fileData.length > 0)) {
            List<BUFROffsets> offsets = new ArrayList<BUFROffsets>();
            // Find the start of the first BUFR signature.
            int start = findStart(0);
            while (start >= 0) {
                // we have the starting position. So get the document length.
                int len = findBUFRLength(start);
                // and verify that the document ends where it supposed to!
                if (verifyEnd(start + len)) {
                    BUFROffsets offset = new BUFROffsets(start, start + len);
                    offsets.add(offset);
                }
                // Update the location for the next possible document.
                start = findStart(start + len);
            }
            logger.debug("Creating BUFRDocuments");
            if (offsets.size() > 0) {
                documents = new ArrayList<BUFRDocument>();
                for (BUFROffsets offset : offsets) {
                    ByteBuffer buf = ByteBuffer.wrap(fileData, offset
                            .getStartPos(), offset.getLength());
                    BUFRDocument doc = new BUFRDocument(factoryDelegate
                            .getInstance(), buf);
                    if (doc != null) {
                        documents.add(doc);
                    }
                }
            }
        }
    }

    /**
     * Load the candidate BUFR file data into an internal buffer.
     * 
     * @param path
     *            The path to the file.
     */
    private void loadData(String path) {
        File f = new File(path);
        InputStream is = null;

        try {
            is = new FileInputStream(f);

            fileData = new byte[(int) f.length()];

            is.read(fileData);
        } catch (IOException ioe) {
            ioe.printStackTrace();
            System.out.println("Exception reading input file " + f.getName());
        } finally {
            if (is != null) {
                try {
                    is.close();
                } catch (IOException ioe) {
                    ioe.printStackTrace();
                }
            }
        }
    }

    /**
     * Find the start of a BUFR section starting at some specified location
     * within the data.
     * 
     * @param startAt
     *            The starting position.
     * @return The found location. If not found a value of -1 is returned.
     */
    private int findStart(int startAt) {
        return DecoderTools.indexOf(startAt, BUFRSection0.BUFR_HDR, fileData);
    }

    /**
     * Verify that the end signature is located at the declared end of the BUFR
     * document.
     * 
     * @param startAt
     *            Starting location of the BUFR document.
     * @param endPos
     *            Declared ending location of the BUFR document.
     * @return Is there a valid end marker in the document?
     */
    private boolean verifyEnd(int endPos) {
        // find the section 5 signature.
        int sigPos = endPos - BUFRSection5.BUFR_TRAILER.length;
        int i = DecoderTools.indexOf(sigPos, BUFRSection5.BUFR_TRAILER,
                fileData);

        return (i == sigPos);
    }

    /**
     * Attempt to read the BUFR section 0 length segment.
     * 
     * @param startAt
     *            The starting position of the BUFR signature.
     * @return The length of the BUFR document.
     */
    private int findBUFRLength(int startAt) {
        final int sigLen = BUFRSection0.BUFR_HDR.length;
        final int lenSize = 3;

        int value = 0;
        for (int i = startAt + sigLen; i < startAt + sigLen + lenSize; i++) {
            value <<= 8;
            value |= (fileData[i] & 0xFF);
        }
        return value;
    }
}
