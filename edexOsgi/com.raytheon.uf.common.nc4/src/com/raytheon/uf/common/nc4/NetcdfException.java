/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.common.nc4;

import edu.mit.ll.netcdf.LLNetcdfException;

/**
 * Exception used in Netcdf interface
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 5, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class NetcdfException extends Exception{

    private static final long serialVersionUID = -508018999572379516L;

    protected int errorCode;

    /**
     * 
     */
    public NetcdfException(LLNetcdfException llex) {
        this(llex.getError());
    }

    public NetcdfException(int errorCode) {
        super(NcConstants.ERROR_MAP.get(errorCode));
    }

    /**
     * @param message
     * @param cause
     */
    public NetcdfException(String message, Throwable cause) {
        super(message, cause);
    }

    /**
     * @param message
     */
    public NetcdfException(String message) {
        super(message);
    }

    /**
     * @param cause
     */
    public NetcdfException(Throwable cause) {
        super(cause);
    }

    /**
     * @return the errorCode
     */
    public int getErrorCode() {
        return errorCode;
    }

    /**
     * @param errorCode
     *            the errorCode to set
     */
    public void setErrorCode(int errorCode) {
        this.errorCode = errorCode;
    }

}
