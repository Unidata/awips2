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
package com.raytheon.uf.common.datadelivery.registry.ebxml;

import java.util.List;

import com.raytheon.uf.common.datadelivery.registry.GriddedDataSetMetaData;
import com.raytheon.uf.common.registry.ebxml.IntegerAttribute;

/**
 * Query to retrieve {@link GriddedDataSetMetaData} information.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 17, 2012 0743       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
abstract class GriddedDataSetMetaDataFilterableQuery<RETURN_TYPE>
        extends DataSetMetaDataFilterableQuery<RETURN_TYPE> {

    /**
     * A setter for the queryable attribute cycle. Using this setter will equate
     * to an HQL "equals" query against the specified column name.
     * 
     * @param immutableDate
     *            The value of the date attribute to search for.
     */
    public void setCycle(int cycle) {
        setAttribute(GriddedDataSetMetaData.CYCLE_SLOT, new IntegerAttribute(
                cycle));
    }

    /**
     * A setter for the queryable attribute cycle. Using this setter will equate
     * to an HQL "in list" query against the specified column name.
     * 
     * @param dates
     *            The values of the date attribute to search for.
     */
    public void setCycles(List<Integer> cycles) {
        setAttribute(GriddedDataSetMetaData.CYCLE_SLOT,
                IntegerAttribute.fromIntegers(cycles));
    }
}
