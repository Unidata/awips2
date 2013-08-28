/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.wcs;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.OutputStream;
import java.security.InvalidParameterException;
import java.util.Map;

import javax.servlet.http.HttpServletResponse;

import org.apache.camel.Exchange;
import org.apache.camel.Processor;
import org.apache.cxf.helpers.IOUtils;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.ogc.common.BasicFileStore;
import com.raytheon.uf.edex.wcs.format.NetCdfFormatter;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 25, 2013            bclement     Initial creation
 *
 * </pre>
 *
 * @author bclement
 * @version 1.0	
 */
public class CoverageStoreEndpoint implements Processor {

    private final BasicFileStore store;

    private final IUFStatusHandler log = UFStatus.getHandler(this.getClass());

    public static final String ID_HEADER = "id";

    /**
     * @param store
     */
    public CoverageStoreEndpoint(BasicFileStore store) {
        this.store = store;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.apache.camel.Processor#process(org.apache.camel.Exchange)
     */
    @Override
    public void process(Exchange ex) throws Exception {
        HttpServletResponse response = ex.getIn().getBody(
                HttpServletResponse.class);
        Map<String, Object> headers = ex.getIn().getHeaders();

        OutputStream out = null;
        try {
            String id = (String) headers.get(ID_HEADER);
            File cov = getCoverage(id);
            response.setContentType(NetCdfFormatter.CONTENT_TYPE);
            out = response.getOutputStream();
            sendFile(out, cov);
            out.flush();
            store.remove(id);
        } catch (InvalidParameterException e) {
            response.sendError(HttpServletResponse.SC_BAD_REQUEST);
        } catch (FileNotFoundException e) {
            response.sendError(HttpServletResponse.SC_NOT_FOUND);
        } catch (Exception e) {
            log.error("Problem retrieving file from store", e);
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        } finally {
            if (out != null) {
                out.close();
            }
        }
    }

    /**
     * Retrieve file from store
     * 
     * @param id
     * @return
     * @throws FileNotFoundException
     *             if file is not in store
     */
    private File getCoverage(String id) throws FileNotFoundException {
        File rval = store.getFile(id);
        if (rval == null) {
            throw new FileNotFoundException("No file in store with id: " + id);
        }
        return rval;
    }

    /**
     * Stream file to out
     * 
     * @param out
     * @param f
     * @throws Exception
     */
    private void sendFile(OutputStream out, File f) throws Exception {
        FileInputStream in = null;
        try {
            in = new FileInputStream(f);
            IOUtils.copy(in, out);
        } finally {
            if (in != null) {
                in.close();
            }
        }
    }

}
