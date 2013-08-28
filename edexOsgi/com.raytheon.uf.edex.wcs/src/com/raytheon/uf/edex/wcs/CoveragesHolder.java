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

import java.util.Map;

import net.opengis.wcs.v_1_1_2.CoveragesType;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 19, 2013            bclement     Initial creation
 *
 * </pre>
 *
 * @author bclement
 * @version 1.0	
 */
public class CoveragesHolder {

    protected CoveragesType metadata;

    protected Map<String, byte[]> data;

    protected String contentType;

    /**
     * @param metadata
     * @param data
     */
    public CoveragesHolder(CoveragesType metadata, Map<String, byte[]> data) {
        super();
        this.metadata = metadata;
        this.data = data;
    }

    /**
     * 
     */
    public CoveragesHolder() {
        // TODO Auto-generated constructor stub
    }

    /**
     * @return the metadata
     */
    public CoveragesType getMetadata() {
        return metadata;
    }

    /**
     * @param metadata
     *            the metadata to set
     */
    public void setMetadata(CoveragesType metadata) {
        this.metadata = metadata;
    }

    /**
     * @return the data
     */
    public Map<String, byte[]> getData() {
        return data;
    }

    /**
     * @param data
     *            the data to set
     */
    public void setData(Map<String, byte[]> data) {
        this.data = data;
    }

    /**
     * @return the contentType
     */
    public String getContentType() {
        return contentType;
    }

    /**
     * @param contentType
     *            the contentType to set
     */
    public void setContentType(String contentType) {
        this.contentType = contentType;
    }

}
