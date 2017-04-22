package com.raytheon.uf.edex.plugin.mpe.dpa;

import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.channels.FileChannel;

import com.raytheon.uf.common.mpe.constants.DPAConstants;
import com.raytheon.uf.edex.plugin.mpe.SequenceFinder;

/**
 * Little Endian conversion routine for DPA product.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 22, 2016 4622       skorolev     Initial creation
 *
 * </pre>
 *
 * @author skorolev
 */
public class ConvertToArchiveFile {

    private static final int WMO_HEADER_END = 30;

    private static final SequenceFinder productDataSeq = new SequenceFinder();

    private static final int DATA_HEADER_END = 146;

    public ConvertToArchiveFile() {
    }

    /**
     * Writes PDA product to archive file.
     * 
     * @param archFilepath
     * @param productBuffer
     * @return
     */
    public boolean writeToArchFile(String archFilepath,
            ByteBuffer productBuffer) {
        boolean retVal = true;

        // skip extra info at the top
        productBuffer.rewind().position(WMO_HEADER_END);
        int archStartPos = productBuffer.position();

        // end of header
        int headerEndPos = WMO_HEADER_END + DATA_HEADER_END;

        // the end of data until finding adaptation data header adaptation data
        // header = "ADAP(..)"
        int dataEndPos = dpaEndDataPos(productBuffer);

        // beginning of remainder of product
        byte[] otherBytes = { 0x41, 0x44, 0x41, 0x50, 0x28 }; // "ADAP("
        productDataSeq.advanceToSequence(productBuffer, otherBytes);
        int otherStartPos = productBuffer.position() - otherBytes.length;

        // header portion - the first 73 16-bit words
        productBuffer.position(archStartPos);
        byte[] headerBytesArray = new byte[dataEndPos - archStartPos];
        productBuffer.get(headerBytesArray, 0, headerBytesArray.length);
        byte[] headerArray = new byte[headerEndPos - archStartPos];
        // inverse one word (1) or two words (2)
        int[] indx = { 1, 1, 2, 2, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1,
                2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 2, 1, 1, 2, 1, 2, 1, 1 };

        int i = 0;
        for (int j : indx) {
            if (1 == j) {
                headerArray[i] = headerBytesArray[i + 1];
                headerArray[i + 1] = headerBytesArray[i];
                i = i + 2;
            } else {
                headerArray[i + 3] = headerBytesArray[i];
                headerArray[i + 2] = headerBytesArray[i + 1];
                headerArray[i + 1] = headerBytesArray[i + 2];
                headerArray[i] = headerBytesArray[i + 3];
                i = i + 4;
            }
        }

        // data portion
        productBuffer.position(headerEndPos);
        byte[] dataBytesArray = new byte[dataEndPos - headerEndPos];
        productBuffer.get(dataBytesArray, 0, dataBytesArray.length);
        byte[] dataArray = new byte[dataEndPos - headerEndPos];
        int d = 0;
        for (int k = 0; k < DPAConstants.NUM_DPA_COLS; k++) {
            int count = 0;
            /* number of bytes in row, 2 x number of runs in row */
            if (d <= dataEndPos - headerEndPos) {
                dataArray[d] = dataBytesArray[d + 1];
                dataArray[d + 1] = dataBytesArray[d];
                Byte b = dataArray[d];
                d = d + 2;
                /* read in run data */
                for (int j = 0; j < (int) (b.intValue()) / 2; j++) {
                    /* Begin run data (MSB = number of grids) */
                    dataArray[d] = dataBytesArray[d];
                    dataArray[d + 1] = dataBytesArray[d + 1];
                    Byte b2 = dataArray[d];
                    count = count + b2.intValue();
                    d = d + 2;
                }
            }
        }

        // remainder portion
        productBuffer.position(otherStartPos);
        byte[] otherBytesArray = new byte[productBuffer.limit()
                - otherStartPos];
        productBuffer.get(otherBytesArray, 0, otherBytesArray.length);

        // collect for output
        ByteBuffer outputBuf = ByteBuffer.allocate(
                headerArray.length + dataArray.length + otherBytesArray.length);
        outputBuf.order(ByteOrder.LITTLE_ENDIAN);
        outputBuf.put(headerArray);
        outputBuf.put(dataArray);
        outputBuf.put(otherBytesArray);
        outputBuf.rewind();

        // write to archive file
        try (FileChannel fch = new FileOutputStream(archFilepath, false)
                .getChannel();) {
            while (outputBuf.hasRemaining()) {
                fch.write(outputBuf);
            }
        } catch (IOException e) {
            retVal = false;
        }

        return retVal;
    }

    /**
     * Find the end of data block
     * 
     * @param productBuffer
     * @return
     */
    private int dpaEndDataPos(ByteBuffer productBuffer) {
        boolean foundDpaMarker = false;
        byte curByte;
        productBuffer.rewind();
        // data block ends after DPA header, symHeaderData and header of data
        // array packet
        for (int i = 0; i < 4; i++) {
            byte lastByte = productBuffer.get();
            while (!foundDpaMarker) {
                // Match consecutive pattern: '0xFFFF'
                curByte = productBuffer.get();
                foundDpaMarker = ((DPAConstants.BEGIN_DPA == curByte)
                        && (DPAConstants.BEGIN_DPA == lastByte));
                lastByte = curByte;
            }
            foundDpaMarker = false;
        }
        return productBuffer.position() - 1;
    }

}
