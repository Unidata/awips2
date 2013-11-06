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
package com.raytheon.uf.edex.ogc.common.output;

import java.awt.image.RenderedImage;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.ByteBuffer;
import java.util.Iterator;

import javax.imageio.ImageIO;
import javax.imageio.ImageWriter;
import javax.imageio.stream.ImageOutputStream;
import javax.servlet.http.HttpServletResponse;

import com.raytheon.uf.edex.ogc.common.OgcException;
import com.raytheon.uf.edex.ogc.common.OgcException.Code;
import com.raytheon.uf.edex.ogc.common.OgcResponse;
import com.raytheon.uf.edex.ogc.common.OgcResponse.ErrorType;

/**
 * Utility methods for sending OGC HTTP response output
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 28, 2012            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class OgcResponseOutput {

    public static void output(OgcResponse response, IOgcHttpResponse httpRes)
			throws Exception {
		switch (response.getType()) {
		case TEXT:
			sendText(response, httpRes);
			break;
		case IMAGE:
			sendImage(response, httpRes);
			break;
		case BYTE:
		case MULTIPART:
			sendByteArray(response, httpRes);
			break;
		default:
			throw new Exception("Unsupported type: " + response.getType());
		}
	}


    protected static void checkError(OgcResponse response,
            IOgcHttpResponse httpRes) {
        int code = getErrorCode(response.getError());
		httpRes.setStatus(code);
	}

    public static int getErrorCode(ErrorType error) {
        int code;
        switch (error) {
        case INT_ERR:
            code = HttpServletResponse.SC_INTERNAL_SERVER_ERROR;
            break;
        case BAD_REQ:
            code = HttpServletResponse.SC_BAD_REQUEST;
            break;
        case NOT_IMPLEMENTED:
            code = HttpServletResponse.SC_NOT_IMPLEMENTED;
            break;
        case NONE:
            code = HttpServletResponse.SC_OK;
            break;
        default:
            throw new IllegalArgumentException("Unsupported error type: "
                    + error);
        }
        return code;
    }

    public static void sendText(OgcResponse response, IOgcHttpResponse httpRes)
            throws Exception {
        OutputStream out = httpRes.getOutputStream();
        sendText(response, httpRes, out);
	}

    /**
     * Send text when output stream has already be retrieved. Closes output
     * stream.
     * 
     * @param response
     * @param httpRes
     * @param out
     * @throws Exception
     */
    public static void sendText(OgcResponse response, IOgcHttpResponse httpRes,
            OutputStream out) throws Exception {
        httpRes.setContentType(response.getMimetype().toString());
        checkError(response, httpRes);
        Object obj = response.getBody();
        try {
            if (obj instanceof byte[]) {
                // UTF8 bytes
                httpRes.setCharacterEncoding("UTF-8");
                out.write((byte[]) obj);
                out.flush();
            } else {
                Writer writer = new OutputStreamWriter(out);
                writer.write(obj.toString());
                writer.flush();
            }
        } catch (Exception e) {
            httpRes.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
            throw e;
        } finally {
            if (out != null) {
                out.close();
            }
        }
    }

    public static void sendImage(OgcResponse response, IOgcHttpResponse httpRes)
            throws Exception {
        String mimetype = response.getMimetype().toString();
		checkError(response, httpRes);
		httpRes.setContentType(mimetype);
		ImageOutputStream out = null;
		try {
			Iterator<?> it = ImageIO.getImageWritersByMIMEType(mimetype);
			if (!it.hasNext()) {
				throw new OgcException(Code.InvalidFormat,
						"Format not supported: " + mimetype);
			}
			ImageWriter writer = (ImageWriter) it.next();
			out = ImageIO.createImageOutputStream(httpRes.getOutputStream());
			writer.setOutput(out);
			writer.write((RenderedImage) response.getBody());
		} catch (Exception e) {
			httpRes.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
			throw e;
		} finally {

			if (out != null) {
				out.close();
			}
		}
	}

	public static void sendByteArray(OgcResponse response,
            IOgcHttpResponse httpRes) throws Exception {
        httpRes.setContentType(response.getMimetype().toString());
		checkError(response, httpRes);
		Object obj = response.getBody();
		byte[] arr;
		if (obj instanceof byte[]) {
			arr = (byte[]) obj;
		} else if (obj instanceof ByteBuffer) {
			arr = ((ByteBuffer) obj).array();
		} else {
			throw new Exception("Unsupported class: " + obj.getClass());
		}
        OutputStream out = null;
		try {
			out = httpRes.getOutputStream();
			out.write(arr);
            out.flush();
		} catch (Exception e) {
			httpRes.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
			throw e;
		} finally {
			if (out != null) {
				out.close();
			}
		}
	}
}
