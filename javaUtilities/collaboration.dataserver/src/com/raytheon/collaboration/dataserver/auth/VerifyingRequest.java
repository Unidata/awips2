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
package com.raytheon.collaboration.dataserver.auth;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.security.Signature;
import java.security.SignatureException;

import javax.servlet.ServletInputStream;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletRequestWrapper;

import org.eclipse.jetty.util.log.Log;
import org.eclipse.jetty.util.log.Logger;

import com.raytheon.collaboration.dataserver.storage.FileManager;

/**
 * HTTP Servlet Request Wrapper for public key signature verification. Allows
 * body of request to be read multiple times.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 26, 2014 2756       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class VerifyingRequest extends HttpServletRequestWrapper {

    private final Logger log = Log.getLogger(this.getClass());

    private byte[] body = null;

    /**
     * @param request
     */
    public VerifyingRequest(HttpServletRequest request) throws IOException {
        super(request);
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.servlet.ServletRequestWrapper#getInputStream()
     */
    @Override
    public ServletInputStream getInputStream() throws IOException {
        if (body == null) {
            ByteArrayOutputStream out = new ByteArrayOutputStream(
                    FileManager.BUFFER_SIZE);
            FileManager.copy(super.getInputStream(), out);
            body = out.toByteArray();
        }
        final ByteArrayInputStream bais = new ByteArrayInputStream(body);
        return new ServletInputStream() {
            @Override
            public int read() throws IOException {
                return bais.read();
            }
        };
    }

    /**
     * @param verification
     * @param signature
     * @return true if signature matches request
     * @throws IOException
     */
    private boolean verifyInternal(final Signature verification,
            byte[] signature) throws IOException {
        final boolean[] errorFlag = { false };
        ServletInputStream in = super.getInputStream();
        ByteArrayOutputStream out = new ByteArrayOutputStream(
                FileManager.BUFFER_SIZE) {

            @Override
            public synchronized void write(byte[] b, int off, int len) {
                super.write(b, off, len);
                try {
                    verification.update(b, off, len);
                } catch (SignatureException e) {
                    log.warn("Problem verifying request", e);
                    errorFlag[0] = true;
                }
            }

            @Override
            public synchronized void write(int b) {
                super.write(b);
                try {
                    verification.update((byte) b);
                } catch (SignatureException e) {
                    log.warn("Problem verifying request", e);
                    errorFlag[0] = true;
                }
            }
        };
        FileManager.copy(in, out);
        this.body = out.toByteArray();
        boolean rval = false;
        try {
            rval = errorFlag[0] ? false : verification.verify(signature);
        } catch (SignatureException e) {
            log.warn("Problem verifying request", e);
            rval = false;
        }
        return rval;
    }

    /**
     * @param verification
     * @param signature
     * @return true if signature matches request
     * @throws IOException
     * @throws SignatureException
     */
    public boolean verify(Signature verification, byte[] signature)
            throws IOException, SignatureException {
        if (this.body == null) {
            return verifyInternal(verification, signature);
        } else {
            verification.update(body);
            return verification.verify(signature);
        }
    }

}
