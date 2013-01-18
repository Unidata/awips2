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
package com.raytheon.uf.viz.datadelivery.subscription.xml;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.junit.Assert.assertThat;

import org.junit.Test;

import com.raytheon.uf.viz.datadelivery.system.Operator;
import com.raytheon.uf.viz.datadelivery.system.OperatorTypes;
import com.raytheon.uf.viz.datadelivery.utils.NameOperationItems;
import com.raytheon.uf.viz.datadelivery.utils.TypeOperationItems;

/**
 * Test {@link OperatorAdapter}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 14, 2013 1286       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@SuppressWarnings({ "rawtypes" })
public class OperatorAdapterTest {

    @Test
    public void testMarshalUnmarshalOperatorNameOperationItems() {
        verifyOperatorsUnmarshalAsSameOperator(NameOperationItems.values());
    }

    @Test
    public void testMarshalUnmarshalOperatorOperatorTypes() {
        verifyOperatorsUnmarshalAsSameOperator(OperatorTypes.values());
    }

    @Test
    public void testMarshalUnmarshalOperatorTypeOperationItems() {
        verifyOperatorsUnmarshalAsSameOperator(TypeOperationItems.values());
    }

    private void verifyOperatorsUnmarshalAsSameOperator(Operator... operators) {
        for (Operator operator : operators) {
            Operator expected = OperatorAdapter.fromString(OperatorAdapter
                    .toString(operator));
            assertThat(operator, is(sameInstance(expected)));
        }
    }

}
