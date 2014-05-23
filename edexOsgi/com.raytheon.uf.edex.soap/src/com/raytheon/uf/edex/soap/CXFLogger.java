/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.soap;

import java.io.InputStream;

import org.apache.cxf.helpers.IOUtils;
import org.apache.cxf.interceptor.Fault;
import org.apache.cxf.interceptor.LoggingMessage;
import org.apache.cxf.io.CachedOutputStream;
import org.apache.cxf.io.DelegatingInputStream;
import org.apache.cxf.message.Message;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;


/**
 * Web Services interceptor that logs incoming requests
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * September, 2013         behemmi     Initial creation
 * May 23, 2014 3199       bclement    moved to edex.soap from edex.log
 * 
 * </pre>
 * 
 * @author behemmi
 * @version 1.0
 */
public class CXFLogger extends org.apache.cxf.interceptor.LoggingInInterceptor {

	protected IUFStatusHandler log = UFStatus.getHandler(this.getClass());
	
	@Override
	public void handleMessage(Message message) throws Fault {
        if (writer != null || 
        		RequestLogController.getInstance().shouldLogRequestsInfo() &&
        		log.isPriorityEnabled(RequestLogController.getInstance().getRequestLogLevel())) {
            logging(message);
        }
    }

	/**
	 * implement custom content for incoming requests in our own log format
	 * @param message
	 * @throws Fault
	 */
	protected void logging(Message message) throws Fault {
		if (message.containsKey(LoggingMessage.ID_KEY)) {
            return;
        }
        String id = (String)message.getExchange().get(LoggingMessage.ID_KEY);
        if (id == null) {
            id = LoggingMessage.nextId();
            message.getExchange().put(LoggingMessage.ID_KEY, id);
        }
        message.put(LoggingMessage.ID_KEY, id);
        final LoggingMessage buffer 
            = new LoggingMessage("Inbound Message\n--------------------------", id);

        Integer responseCode = (Integer)message.get(Message.RESPONSE_CODE);
        if (responseCode != null) {
            buffer.getResponseCode().append(responseCode);
        }

        String encoding = (String)message.get(Message.ENCODING);

        if (encoding != null) {
            buffer.getEncoding().append(encoding);
        }
        String httpMethod = (String)message.get(Message.HTTP_REQUEST_METHOD);
        if (httpMethod != null) {
            buffer.getHttpMethod().append(httpMethod);
        }
        String ct = (String)message.get(Message.CONTENT_TYPE);
        if (ct != null) {
            buffer.getContentType().append(ct);
        }
        Object headers = message.get(Message.PROTOCOL_HEADERS);

        if (headers != null) {
            buffer.getHeader().append(headers);
        }
        String uri = (String)message.get(Message.REQUEST_URL);
        if (uri != null) {
            buffer.getAddress().append(uri);
            String query = (String)message.get(Message.QUERY_STRING);
            if (query != null) {
                buffer.getAddress().append("?").append(query);
            }
        }
        
        if (!isShowBinaryContent() && isBinaryContent(ct)) {
            buffer.getMessage().append(BINARY_CONTENT_MESSAGE).append('\n');
            log.handle(RequestLogController.getInstance().getRequestLogLevel(), 
            		buffer.toString());
            return;
        }
        
        InputStream is = message.getContent(InputStream.class);
        if (is != null) {
            CachedOutputStream bos = new CachedOutputStream();
            if (threshold > 0) {
                bos.setThreshold(threshold);
            }
            try {
                // use the appropriate input stream and restore it later
                InputStream bis = is instanceof DelegatingInputStream 
                    ? ((DelegatingInputStream)is).getInputStream() : is;
                
                IOUtils.copyAndCloseInput(bis, bos);
                bos.flush();
                bis = bos.getInputStream();
                
                // restore the delegating input stream or the input stream
                if (is instanceof DelegatingInputStream) {
                    ((DelegatingInputStream)is).setInputStream(bis);
                } else {
                    message.setContent(InputStream.class, bis);
                }

                if (bos.getTempFile() != null) {
                    //large thing on disk...
                    buffer.getMessage().append("\nMessage (saved to tmp file):\n");
                    buffer.getMessage().append("Filename: " + bos.getTempFile().getAbsolutePath() + "\n");
                }
                if (bos.size() > limit) {
                    buffer.getMessage().append("(message truncated to " + limit + " bytes)\n");
                }
                writePayload(buffer.getPayload(), bos, encoding, ct); 
                    
                bos.close();
            } catch (Exception e) {
                throw new Fault(e);
            }
        }
        log.handle(RequestLogController.getInstance().getRequestLogLevel(), 
        		buffer.toString());
	}

}
