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
package com.raytheon.uf.common.dataaccess.exception;

import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;

/**
 * An exception for when a request is sent that a factory cannot process because
 * of unrecognized or missing identifiers.
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

public class InvalidIdentifiersException extends DataAccessException {

    private static final long serialVersionUID = 1L;

    private final String datatype;

    private final Collection<String> missingIdentifiers;

    private final Collection<String> invalidIdentifiers;

    public InvalidIdentifiersException(String datatype,
            Collection<String> missingIdentifiers,
            Collection<String> invalidIdentifiers) {
        // Life is easier if we can assume these are non null.
        if (missingIdentifiers == null) {
            missingIdentifiers = Collections.emptyList();
        }
        if (invalidIdentifiers == null) {
            invalidIdentifiers = Collections.emptyList();
        }
        if (missingIdentifiers.isEmpty() && invalidIdentifiers.isEmpty()) {
            throw new IllegalArgumentException(this.getClass().getSimpleName()
                    + " must be contain either invalid or missing identifiers.");
        }
        this.datatype = datatype;
        this.missingIdentifiers = missingIdentifiers;
        this.invalidIdentifiers = invalidIdentifiers;
    }

    @Override
    public String getMessage() {
        StringBuilder sb = new StringBuilder();
        sb.append("Request of ");
        sb.append(getDatatype());
        sb.append(" data ");
        if (!missingIdentifiers.isEmpty()) {
            sb.append("is missing identifiers: ");
            Iterator<String> itr = missingIdentifiers.iterator();
            while (itr.hasNext()) {
                sb.append(itr.next());
                if (itr.hasNext()) {
                    sb.append(", ");
                }
            }
            if (!invalidIdentifiers.isEmpty()) {
                sb.append(" and ");
            }

        }
        if (!invalidIdentifiers.isEmpty()) {
            if (missingIdentifiers.isEmpty())
            sb.append("has invalid identifiers: ");
            Iterator<String> itr = invalidIdentifiers.iterator();
            while (itr.hasNext()) {
                sb.append(itr.next());
                if (itr.hasNext()) {
                    sb.append(", ");
                }
            }
        }
        return sb.toString();
    }

    public String getDatatype() {
        return datatype;
    }

    public Collection<String> getMissingIdentifiers() {
        return missingIdentifiers;
    }

    public Collection<String> getInvalidIdentifiers() {
        return invalidIdentifiers;
    }

}
