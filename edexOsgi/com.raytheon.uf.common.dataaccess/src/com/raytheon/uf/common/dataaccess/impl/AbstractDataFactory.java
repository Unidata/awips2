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

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Map;

import com.raytheon.uf.common.dataaccess.IDataFactory;
import com.raytheon.uf.common.dataaccess.IDataRequest;
import com.raytheon.uf.common.dataaccess.exception.InvalidIdentifiersException;

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
 * Feb 14, 2013 1614       bsteffen    Refactor data access framework to use
 *                                     single request.
 * Feb 19, 2012 1552       mpduff      Implement IDataFactory.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public abstract class AbstractDataFactory implements IDataFactory {

    /**
     * Returns the identifiers that must be set on a request for the request to
     * be processed
     * 
     * @return the required identifiers
     */
    public String[] getRequiredIdentifiers() {
        return null;
    }

    /**
     * Return the complete set of all valid identifiers for a request, or null
     * if there is no well defined set or if no validation should occur.
     * 
     * @return the valid identifiers.
     */
    public String[] getValidIdentifiers() {
        return null;
    }

    /**
     * Validates that a request is compatible with the factory
     * 
     * @param request
     *            the request to validate
     */
    public void validateRequest(IDataRequest request) {
        String[] required = getRequiredIdentifiers();
        Collection<String> missing = Collections.emptySet();
        Collection<String> invalid = Collections.emptySet();
        Map<String, Object> identifiers = request.getIdentifiers();
        if (identifiers != null) {
            if (required != null) {
                missing = new HashSet<String>(Arrays.asList(required));
                missing.removeAll(identifiers.keySet());
            }
            String[] valid = getValidIdentifiers();
            if (valid != null) {
                invalid = new HashSet<String>(identifiers.keySet());
                invalid.removeAll(Arrays.asList(valid));
            }
        } else if (required != null) {
            missing = Arrays.asList(required);
        }

        if (!missing.isEmpty() || !invalid.isEmpty()) {
            throw new InvalidIdentifiersException(request.getDatatype(),
                    missing, invalid);
        }
    }

}
