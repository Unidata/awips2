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
package com.raytheon.uf.common.dataaccess.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataaccess.IDataRequest;
import com.raytheon.uf.common.dataaccess.exception.MissingRequiredIdentifierException;

/**
 * 
 * An abstract data factory that can be used by implementing IGridDataFactories
 * or IGeometryDataFactories.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 13, 2012            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public abstract class AbstractDataFactory {

    /**
     * Returns the identifiers that must be set on a request for the request to
     * be processed
     * 
     * @return the required identifiers
     */
    public abstract String[] getRequiredIdentifiers();

    /**
     * Validates that a request is compatible with the factory
     * 
     * @param request
     *            the request to validate
     */
    public void validateRequest(IDataRequest<?> request) {
        String[] required = getRequiredIdentifiers();
        List<String> missing = null;
        if (required != null && required.length > 0) {
            Map<String, Object> identifiers = request.getIdentifiers();
            if (identifiers != null) {
                for (String s : required) {
                    if (!identifiers.containsKey(s)) {
                        if (missing == null) {
                            missing = new ArrayList<String>(required.length);
                        }
                        missing.add(s);
                    }
                }
            } else {
                missing = Arrays.asList(required);
            }
        }

        if (missing != null) {
            StringBuilder sb = new StringBuilder();
            sb.append("Request of ");
            sb.append(request.getDatatype());
            sb.append(" data is missing identifiers: ");
            Iterator<String> itr = missing.iterator();
            while (itr.hasNext()) {
                sb.append(itr.next());
                if (itr.hasNext()) {
                    sb.append(", ");
                }
            }
            throw new MissingRequiredIdentifierException(sb.toString());
        }
    }
}
