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

import java.util.Date;
import java.util.List;

/**
 * Results wrapper for wfs queries
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

public class WfsQueryResults {

    public static enum ResultType {
        JAXB, SIMPLE
    };

    protected final ResultType type;

    protected List<?> results;

    protected Date oldestResult;

    protected Date latestResult;

    /**
     * 
     */
    public WfsQueryResults(List<?> results, ResultType type) {
        this.type = type;
        this.results = results;
    }

    public WfsQueryResults(ResultType type) {
        this.type = type;
    }

    /**
     * @return the oldestResult
     */
    public Date getOldestResult() {
        return oldestResult;
    }

    /**
     * @param oldestResult
     *            the oldestResult to set
     */
    public void setOldestResult(Date oldestResult) {
        this.oldestResult = oldestResult;
    }

    /**
     * @return the latestResult
     */
    public Date getLatestResult() {
        return latestResult;
    }

    /**
     * @param latestResult
     *            the latestResult to set
     */
    public void setLatestResult(Date latestResult) {
        this.latestResult = latestResult;
    }

    /**
     * @return the type
     */
    public ResultType getType() {
        return type;
    }

    /**
     * @return the results
     */
    public List<?> getResults() {
        return results;
    }

    /**
     * @param results
     *            the results to set
     */
    public void setResults(List<?> results) {
        this.results = results;
    }

}
