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
package com.raytheon.uf.common.registry.ebxml;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import com.raytheon.uf.common.registry.IMultipleResultFormatter;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

/**
 * Returns an arbitrary number of string results per {@link RegistryObjectType}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 15, 2012 0743       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class StringArrayResultsQuery extends AdhocRegistryQuery<String>
        implements IMultipleResultFormatter<String> {

    private final String[][] resultsToReturn;

    private int index;

    public StringArrayResultsQuery(String[][] resultsToReturn) {
        this.resultsToReturn = resultsToReturn;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Class<String> getObjectType() {
        return String.class;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Class<String> getResultType() {
        return String.class;
    }

    @Override
    public Collection<String> decodeObject(RegistryObjectType registryObjectType) {
        List<String> results = new ArrayList<String>();

        String[] valuesToReturn = resultsToReturn[index++];
        for (String value : valuesToReturn) {
            results.add(value);
        }

        return results;
    }
}