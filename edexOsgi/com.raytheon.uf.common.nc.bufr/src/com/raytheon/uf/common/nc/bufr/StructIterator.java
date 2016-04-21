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
package com.raytheon.uf.common.nc.bufr;

import java.io.IOException;

import ucar.ma2.StructureData;
import ucar.ma2.StructureDataIterator;

/**
 * Wrapper to fix {@link StructureDataIterator}. Needed because calling
 * {@link StructureDataIterator#hasNext()} when there is no more items throws an
 * exception.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 19, 2014 2905       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class StructIterator {

    private final StructureDataIterator iter;

    private final int totalVariable;

    private int count = 0;

    /**
     * @param iter
     * @param totalVariables
     *            total number of items in iter
     */
    public StructIterator(StructureDataIterator iter, int totalVariables) {
        this.iter = iter;
        this.totalVariable = totalVariables;
    }

    /**
     * @return true if there are more elements
     * @throws IOException
     */
    public boolean hasNext() throws IOException {
        return count < totalVariable && iter.hasNext();
    }

    /**
     * @return next element
     * @throws IOException
     */
    public StructureData next() throws IOException {
        ++count;
        return iter.next();
    }

}
