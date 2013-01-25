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

import javax.xml.bind.JAXBException;

import org.junit.Test;

import com.raytheon.uf.common.datadelivery.registry.SubscriptionFixture;
import com.raytheon.uf.common.units.DataSizeUnit;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.viz.datadelivery.system.CreateEditRuleDlg.FreqUnitOptions;
import com.raytheon.uf.viz.datadelivery.system.OperatorTypes;
import com.raytheon.uf.viz.datadelivery.system.OpsNetFieldNames;

/**
 * Test {@link LatencyRulesXML}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 14, 2013 1286       djohnson     Initial creation
 * Jan 17, 2013 1357       mpduff       DataSizeUnits was moved.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class LatencyRuleXMLTest {

    /**
     * Rule data size units were being written out as "Byte" but expected to be
     * BYTE when running matches.
     * 
     * @throws JAXBException
     */
    @Test
    public void testDataSizeUnitCanBeUsedInMatches() throws JAXBException {
        LatencyRuleXML ruleXml = new LatencyRuleXML();
        ruleXml.setLatency(10);
        ruleXml.setRuleField(OpsNetFieldNames.SIZE.toString());
        ruleXml.setRuleName("ruleName");
        ruleXml.setRuleOperator(OperatorTypes.GREATER_THAN);
        ruleXml.setRuleUnit(DataSizeUnit.BYTE.getUnit());
        ruleXml.setRuleValue("10");

        ruleXml.matches(SubscriptionFixture.INSTANCE.get(),
                CollectionUtil.asSet(1, 2));
    }

    @Test
    public void testFrequencyUnitCanBeUsedInMatches() throws JAXBException {
        LatencyRuleXML ruleXml = new LatencyRuleXML();
        ruleXml.setLatency(10);
        ruleXml.setRuleField(OpsNetFieldNames.FREQUENCY.toString());
        ruleXml.setRuleName("ruleName");
        ruleXml.setRuleOperator(OperatorTypes.GREATER_THAN);
        ruleXml.setRuleUnit(FreqUnitOptions.MIN.getOperation());
        ruleXml.setRuleValue("10");

        ruleXml.matches(SubscriptionFixture.INSTANCE.get(),
                CollectionUtil.asSet(1, 2));
    }
}
