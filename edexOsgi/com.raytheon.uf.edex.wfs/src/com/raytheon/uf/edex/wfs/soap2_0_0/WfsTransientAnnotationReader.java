/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.wfs.soap2_0_0;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.ogc.common.jaxb.TransientAnnotationReader;

/**
 * Adds transient annotations to external classes used by WFS so they can be
 * marshalled in the SOAP response
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 21, 2012            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */

public class WfsTransientAnnotationReader extends TransientAnnotationReader {

	private IUFStatusHandler log = UFStatus.getHandler(this.getClass());

    /**
     * 
     */
    public WfsTransientAnnotationReader() {
        try {
            addTransientField(Throwable.class.getDeclaredField("stackTrace"));
            addTransientMethod(Throwable.class
                    .getDeclaredMethod("getStackTrace"));
        } catch (Exception e) {
            log.error("Unable to add transients for wfs JAXB", e);
            throw new RuntimeException(e);
        }
    }
}
