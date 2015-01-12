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
package com.raytheon.uf.common.registry;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.List;

import org.junit.Test;

import com.raytheon.uf.common.registry.BaseQuery;
import com.raytheon.uf.common.registry.RegistryResponse;
import com.raytheon.uf.common.registry.ebxml.AdhocRegistryQuery;

/**
 * Test {@link BaseQuery}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 2, 2012  955        djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class BaseQueryTest {

    private static final BaseQuery<String> BASE_QUERY = new AdhocRegistryQuery<String>() {
        @Override
        public Class<String> getObjectType() {
            return String.class;
        }

        @Override
        public Class<String> getResultType() {
            return String.class;
        }
    };

    @Test
    public void testGetResultsReturnsEmptyListForNullResponseObjects()
    {
        RegistryResponse response = new RegistryResponse();
        response.setRegistryObjects(null);
        
        List<String> results = BASE_QUERY.getResults(response);

        assertTrue(
                "Expected an empty list if the results were null from the response!",
                results.isEmpty());
    }

    @Test
    public void testGetSingleResultReturnsNullForNullResponseObjects() {
        RegistryResponse response = new RegistryResponse();
        response.setRegistryObjects(null);

        String result = BASE_QUERY.getSingleResult(response);

        assertNull("Expected null if the results were null from the response!",
                result);
    }

    @Test
    public void testGetResultsReturnsListOfTypeSafeResultsForNonNullResponseObjects() {
        final Object one = "one";
        final Object two = "two";

        RegistryResponse response = new RegistryResponse();
        response.setRegistryObjects(Arrays.asList(one, two));

        List<String> results = BASE_QUERY.getResults(response);

        assertEquals("Incorrect results collection size!", 2, results.size());
        assertTrue("Didn't find an expected result!", results.contains(one));
        assertTrue("Didn't find an expected result!", results.contains(two));
    }

    @Test
    public void testGetSingleResultReturnsTypeSafeResultForNonNullResponseObjects() {
        final Object one = "one";

        RegistryResponse response = new RegistryResponse();
        response.setRegistryObjects(Arrays.asList(one));

        String result = BASE_QUERY.getSingleResult(response);

        assertEquals("Didn't find the expected result!", one, result);
    }
}
