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
 */
package com.raytheon.uf.edex.textdbsrv;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.ByteArrayOutputStreamPool;
import com.raytheon.uf.common.util.SizeUtil;
import com.raytheon.uf.common.util.stream.LimitingInputStream;
import com.raytheon.uf.common.util.stream.LimitingOutputStream;
import com.raytheon.uf.edex.services.TextDBSrv;
import com.raytheon.uf.edex.services.textdbimpl.CommandExecutor;

/**
 * Thin wrapper around TextDBSrv to handle marshalling/unmarshalling and
 * limiting of the byte stream.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 15, 2014 2928       rjpeter     Initial creation.
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class TextDBSrvWrapper {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(TextDBSrvWrapper.class);

    private static final IUFStatusHandler textDbSrvLogger = UFStatus
            .getNamedHandler("TextDBSrvRequestLogger");

    /**
     * The limit of bytes that we are able to read in without erroring off.
     */
    private long byteLimitInMB;

    /**
     * TextDbSrv implementation to use.
     */
    private TextDBSrv textdbSrv;

    /**
     * Unmarshalls the input stream as xml data and sends to textdbsrv for
     * processing.
     * 
     * @param is
     * @return
     */
    public byte[] executeTextDBMessage(InputStream xmlDataStream) {
        ITimer timer = TimeUtil.getTimer();
        timer.start();

        /*
         * This stream does not need to be closed, Camel will handle closing of
         * data
         */
        LimitingInputStream inputStream = null;
        String sizeString = null;
        Message rval;

        try {
            inputStream = new LimitingInputStream(xmlDataStream, byteLimitInMB
                    * SizeUtil.BYTES_PER_MB);
            Message message = SerializationUtil.jaxbUnmarshalFromInputStream(
                    Message.class, inputStream);
            sizeString = SizeUtil.prettyByteSize(inputStream.getBytesRead());
            textDbSrvLogger.info("Processing xml message of length: "
                    + sizeString);

            rval = textdbSrv.processMessage(message);
        } catch (Throwable e) {
            statusHandler
                    .error("Error occured processing textDbSrv message", e);
            rval = CommandExecutor
                    .createErrorMessage("Error occurred during processing: "
                            + e.getLocalizedMessage());
        } finally {
            if (inputStream != null) {
                try {
                    inputStream.close();
                } catch (IOException e) {
                    // ignore
                }
            }
        }

        LimitingOutputStream outStream = null;
        int tries = 0;
        byte[] bytesOut = null;

        while ((bytesOut == null) && (tries < 2)) {
            try {
                ByteArrayOutputStream baos = ByteArrayOutputStreamPool
                        .getInstance().getStream();
                outStream = new LimitingOutputStream(baos, byteLimitInMB
                        * SizeUtil.BYTES_PER_MB);
                SerializationUtil.jaxbMarshalToStream(rval, outStream);
                bytesOut = baos.toByteArray();
            } catch (Exception e) {
                statusHandler.error("Error occured marshalling response", e);
                tries++;
                rval = CommandExecutor
                        .createErrorMessage("Error occurred during processing: "
                                + e.getLocalizedMessage());
            } finally {
                if (outStream != null) {
                    try {
                        outStream.close();
                    } catch (IOException e) {
                        // ignore
                    }
                }
            }
        }

        timer.stop();

        StringBuilder sb = new StringBuilder(300);
        sb.append("Processed message in ").append(timer.getElapsedTime())
                .append("ms, ");
        sb.append("request was size ").append(sizeString);
        sb.append(", response was size ")
                .append(SizeUtil
                        .prettyByteSize(bytesOut != null ? bytesOut.length : 0));
        textDbSrvLogger.info(sb.toString());

        return bytesOut;
    }

    public long getByteLimitInMB() {
        return byteLimitInMB;
    }

    public void setByteLimitInMB(long byteLimitInMB) {
        this.byteLimitInMB = byteLimitInMB;
    }

    public TextDBSrv getTextdbSrv() {
        return textdbSrv;
    }

    public void setTextdbSrv(TextDBSrv textdbSrv) {
        this.textdbSrv = textdbSrv;
    }
}
