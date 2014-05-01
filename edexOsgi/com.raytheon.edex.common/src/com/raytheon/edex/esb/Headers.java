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
package com.raytheon.edex.esb;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 5, 2009            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class Headers implements Serializable, Iterable<String> {

    private static final long serialVersionUID = 1L;

    private Map<String, Object> headerMap = new HashMap<String, Object>();

    /**
     * 
     * @param key
     * @return
     */
    public Object get(String headerName) {
        return headerMap.get(headerName);
    }

    public void put(String headerName, Object value) {
        headerMap.put(headerName, value);
    }

    /**
     * Returns an Iterator to the header names contained in this Header. The
     * order of returned names is not specified.
     * 
     * @return An Iterator to the Header names.
     */
    @Override
    public Iterator<String> iterator() {
        return headerMap.keySet().iterator();
    }

}
