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
package com.raytheon.uf.edex.esb.camel.http;

import java.io.IOException;
import java.io.InputStream;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletResponse;

import org.apache.camel.Exchange;
import org.apache.camel.Message;
import org.apache.camel.component.http.DefaultHttpBinding;
import org.apache.camel.util.IOHelper;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 2, 2011            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class BinaryHttpBinding extends DefaultHttpBinding {
    /**
     * Pure copy of DefaultHttpBinding, with added support for non chunked byte[], 99% of edex sends this way.
     */
    @Override
    protected void doWriteDirectResponse(Message message,
            HttpServletResponse response, Exchange exchange) throws IOException {
        InputStream is = null;
        if (checkChunked(message, exchange)) {
            is = message.getBody(InputStream.class);
        }
        if (is != null) {
            ServletOutputStream os = response.getOutputStream();
            try {
                // copy directly from input stream to output stream
                IOHelper.copy(is, os);
            } finally {
                try {
                    os.close();
                } catch (Exception e) {
                    // ignore, maybe client have disconnected or timed out
                }
                try {
                    is.close();
                } catch (Exception e) {
                    // ignore, maybe client have disconnected or timed out
                }
            }
        } else {
            // not convertible as a stream so try as a String
            Object body = message.getBody();
            if (body instanceof byte[]) {
                // add support for byte[] data
                byte[] data = (byte[]) body;

                // set content length before we write data
                response.setContentLength(data.length);
                ServletOutputStream os = response.getOutputStream();
                try {
                    os.write(data);
                    os.flush();
                } finally {
                    try {
                        os.close();
                    } catch (Exception e) {
                        // ignore, maybe client have disconnected or timed out
                    }
                }
            } else {
                String data = message.getBody(String.class);
                if (data != null) {
                    // set content length before we write data
                    response.setContentLength(data.length());
                    response.getWriter().print(data);
                    response.getWriter().flush();
                }
            }
        }
    }
}
