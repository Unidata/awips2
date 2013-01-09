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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Set;
import java.util.TreeSet;

import org.junit.Before;
import org.junit.Test;

import com.raytheon.uf.common.datadelivery.registry.DataType;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.viz.datadelivery.system.CreateEditRuleDlg.FreqUnitOptions;
import com.raytheon.uf.viz.datadelivery.system.OperatorTypes;
import com.raytheon.uf.viz.datadelivery.system.OpsNetFieldNames;
import com.raytheon.uf.viz.datadelivery.utils.DataSizeUtil;
import com.raytheon.uf.viz.datadelivery.utils.NameOperationItems;
import com.raytheon.uf.viz.datadelivery.utils.TypeOperationItems;

/**
 * Test {@link RuleXML}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 3, 2013    1420     mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class RuleXMLTest {
    private Subscription sub;

    @Before
    public void setUp() {
        sub = new Subscription();
        sub.setDataSetName("GFS");
        sub.setDataSetType(DataType.GRID);
        sub.setDataSetSize(100);
    }

    @Test
    public void testMatchesDataSetNameRule() {
        LatencyRuleXML rule = new LatencyRuleXML();
        rule.setRuleValue("GFS");
        rule.setRuleField(OpsNetFieldNames.NAME.getFieldName());
        rule.setRuleOperator(NameOperationItems.LIKE.getOperation());

        assertTrue("Matches Data Set Name failed", rule.matches(sub, null));
    }

    @Test
    public void testMatchesDataSetNameRuleFails() {
        LatencyRuleXML rule = new LatencyRuleXML();
        rule.setRuleValue("GFS2");
        rule.setRuleField(OpsNetFieldNames.NAME.getFieldName());
        rule.setRuleOperator(NameOperationItems.LIKE.getOperation());

        assertFalse("Matches Data Set Name false positive",
                rule.matches(sub, null));
    }

    @Test
    public void testMatchesDataSetTypeInRule() {
        LatencyRuleXML rule = new LatencyRuleXML();
        rule.setRuleValue("GRID,OBS");
        rule.setRuleField(OpsNetFieldNames.TYPE.getFieldName());
        rule.setRuleOperator(TypeOperationItems.IN.getOperation());

        assertTrue("Matches Data Type In Failed", rule.matches(sub, null));
    }

    @Test
    public void testMatchesDataSetTypeNotInRule() {
        LatencyRuleXML rule = new LatencyRuleXML();
        rule.setRuleValue("FAKE");
        rule.setRuleField(OpsNetFieldNames.TYPE.getFieldName());
        rule.setRuleOperator(TypeOperationItems.NOT_IN.getOperation());

        assertTrue("Matches Data Type Not In Failed", rule.matches(sub, null));
    }

    @Test
    public void testMatchesSizeEqualsInKB() {
        LatencyRuleXML rule = new LatencyRuleXML();
        rule.setRuleValue(String.valueOf(100));
        rule.setRuleField(OpsNetFieldNames.SIZE.getFieldName());
        rule.setRuleOperator(OperatorTypes.EQUAL.getOperation());
        rule.setRuleUnit(DataSizeUtil.KB.getUnit());

        assertTrue("Matches Dataset Size Equals Failed",
                rule.matches(sub, null));
    }

    @Test
    public void testMatchesSizeEqualsInMB() {
        System.out.println("Here 1");
        LatencyRuleXML rule = new LatencyRuleXML();
        rule.setRuleValue(String.valueOf(100));
        rule.setRuleField(OpsNetFieldNames.SIZE.getFieldName());
        rule.setRuleOperator(OperatorTypes.EQUAL.getOperation());
        rule.setRuleUnit(DataSizeUtil.MB.getUnit());

        sub.setDataSetSize(1024 * 100);
        assertTrue("Matches Dataset Size Equals Failed",
                rule.matches(sub, null));
    }

    @Test
    public void testMatchesSizeEqualsInGB() {
        System.out.println("Here 2");
        LatencyRuleXML rule = new LatencyRuleXML();
        rule.setRuleValue(String.valueOf(100));
        rule.setRuleField(OpsNetFieldNames.SIZE.getFieldName());
        rule.setRuleOperator(OperatorTypes.EQUAL.getOperation());
        rule.setRuleUnit(DataSizeUtil.GB.getUnit());

        sub.setDataSetSize(100 * 1024 * 1024);
        assertTrue("Matches Dataset Size Equals Failed",
                rule.matches(sub, null));
    }

    // The other operator types are tested in OperatorTypesTest.java

    @Test
    public void testMatchesFrequencyMinutes() {
        LatencyRuleXML rule = new LatencyRuleXML();
        rule.setRuleValue(String.valueOf(60));
        rule.setRuleField(OpsNetFieldNames.FREQUENCY.getFieldName());
        rule.setRuleOperator(OperatorTypes.EQUAL.getOperation());
        rule.setRuleUnit(FreqUnitOptions.MIN.getOperation());

        Set<Integer> cycles = new TreeSet<Integer>();
        cycles.add(0);
        cycles.add(1);

        assertTrue("Matches Dataset Frequency Minutes Equals Failed",
                rule.matches(sub, cycles));
    }

    @Test
    public void testMatchesFrequencyHours() {
        LatencyRuleXML rule = new LatencyRuleXML();
        rule.setRuleValue(String.valueOf(1));
        rule.setRuleField("Dataset Frequency");
        rule.setRuleOperator(OperatorTypes.EQUAL.getOperation());
        rule.setRuleUnit(FreqUnitOptions.HOURS.getOperation());

        Set<Integer> cycles = new TreeSet<Integer>();
        cycles.add(0);
        cycles.add(1);

        assertTrue("Matches Dataset Frequency Minutes Equals Failed",
                rule.matches(sub, cycles));
    }
}
