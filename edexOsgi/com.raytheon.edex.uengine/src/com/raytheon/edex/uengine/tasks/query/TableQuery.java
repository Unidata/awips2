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

package com.raytheon.edex.uengine.tasks.query;

import java.util.List;

import com.raytheon.uf.edex.database.DataAccessLayerException;

/**
 * TableQuery task derived from DbQuery task.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date             Ticket#         Engineer            Description
 * -----------      ----------      ------------        --------------------------
 * 9/24/2007        368             grichard            Initial creation. 
 * 10/3/2007        459             grichard            Guarded `results'.
 * 06/4/2008        875             bphillip            Refactored to use DatabaseQuery
 * 
 * </pre>
 * 
 * @author grichard
 */

public class TableQuery extends DbQuery {

    /**
     * Constructor
     * 
     * @param aClassName
     *            the table to query for
     */
    public TableQuery(String database, String aClassName)
            throws DataAccessLayerException {
        super(database, aClassName);
    }

    public List<?> execute() throws Exception {
        List<?> results = (List<?>) super.execute();

        /*
         * `results' can be a null pointer, so to avoid raising a
         * NullPointerException in the for-loop below, guard the for-loop
         * appropriately. Returning `results' as a null pointer is okay since
         * the uEngine script runner will detect the ReferenceError and provide
         * a descriptive error message in the case when `results' is null.
         */
        return results;
    }

}
