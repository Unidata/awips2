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
package com.raytheon.uf.edex.registry.ebxml.services.query.types.canonical;

import static org.junit.Assert.assertTrue;

import java.util.List;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ClassificationNodeType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ClassificationSchemeType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;
import com.raytheon.uf.edex.registry.ebxml.services.query.plugins.ClassificationSchemeSelector;
import com.raytheon.uf.edex.registry.ebxml.services.query.types.QueryTest;

/**
 * 
 * Test for the Canonical ClassiciationSchemeSelector query defined by the EBXML
 * 4.0 spec
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 10/8/2013    1682        bphillip    Initial implementation
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class ClassificationSchemeSelectorTest extends QueryTest {

    @Autowired
    private ClassificationSchemeSelector selector;

    @Test
    public void testQuery() throws MsgRegistryException {
        QueryRequest request = createQuery(
                CanonicalQueryTypes.CLASSIFICATION_SCHEME_SELECTOR,
                QueryConstants.CLASSIFICATION_SCHEME_ID,
                "urn:oasis:names:tc:ebxml-regrep:classificationScheme:ActionType");
        List<RegistryObjectType> result = executeQuery(selector, request);

        assertTrue(result.get(0) instanceof ClassificationSchemeType);
        ClassificationSchemeType scheme = (ClassificationSchemeType) result
                .get(0);
        assertTrue(scheme.getClassificationNode().isEmpty());

        for (int i = 1; i < result.size(); i++) {
            assertTrue(result.get(i) instanceof ClassificationNodeType);
        }
    }

}
