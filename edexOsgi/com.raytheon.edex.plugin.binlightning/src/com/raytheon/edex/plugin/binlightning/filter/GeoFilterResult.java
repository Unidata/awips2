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
package com.raytheon.edex.plugin.binlightning.filter;

import java.util.Collection;
import java.util.Collections;

import com.vividsolutions.jts.geom.prep.PreparedGeometry;

/**
 * Geographic filter parsing results. Wraps parsed data and any parsing errors.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 10, 2014 3226       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class GeoFilterResult {

    private final Collection<GeoFilterException> errors;

    private final Collection<PreparedGeometry> filters;


        /**
     * @param filters
     */
    public GeoFilterResult(Collection<PreparedGeometry> filters) {
        this(filters, Collections.<GeoFilterException> emptyList());
    }

    /**
     * @param filters
     * @param errors
     */
    public GeoFilterResult(Collection<PreparedGeometry> filters,
            Collection<GeoFilterException> errors) {
        this.filters = filters;
        this.errors = errors;
    }

    /**
     * @return true if any errors were encountered during parsing
     * @see #getErrors()
     */
    public boolean hasErrors() {
        return !errors.isEmpty();
    }

    /**
     * @return the errors
     * @see #hasErrors()
     */
    public Collection<GeoFilterException> getErrors() {
        return errors;
    }

    /**
     * @return the filters
     */
    public Collection<PreparedGeometry> getFilters() {
        return filters;
    }

}
