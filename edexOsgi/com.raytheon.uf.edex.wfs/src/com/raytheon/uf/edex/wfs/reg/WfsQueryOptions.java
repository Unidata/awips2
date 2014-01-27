/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.wfs.reg;

/**
 * options for wfs query
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 13, 2012            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */

public class WfsQueryOptions {

    protected WfsQueryResults.ResultType type;

    protected boolean gatherMetadata = false;

    protected String gmlVersion = "3.2.1";

    /**
     * 
     */
    public WfsQueryOptions(WfsQueryResults.ResultType type) {
        this.type = type;
    }

    /**
     * @return the type
     */
    public WfsQueryResults.ResultType getType() {
        return type;
    }

    /**
     * @param type
     *            the type to set
     */
    public void setType(WfsQueryResults.ResultType type) {
        this.type = type;
    }

    /**
     * @return the gatherMetadata
     */
    public boolean isGatherMetadata() {
        return gatherMetadata;
    }

    /**
     * @param gatherMetadata
     *            the gatherMetadata to set
     */
    public void setGatherMetadata(boolean gatherMetadata) {
        this.gatherMetadata = gatherMetadata;
    }

    /**
     * @return the gmlVersion
     */
    public String getGmlVersion() {
        return gmlVersion;
    }

    /**
     * @param gmlVersion
     *            the gmlVersion to set
     */
    public void setGmlVersion(String gmlVersion) {
        this.gmlVersion = gmlVersion;
    }

}
