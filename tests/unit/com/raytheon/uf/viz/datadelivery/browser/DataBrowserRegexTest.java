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
package com.raytheon.uf.viz.datadelivery.browser;

import static org.junit.Assert.assertNull;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.Test;

/**
 * Test the filter functionality of the Dataset Discovery Brower's FilterComps.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 25, 2013   1588     mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class DataBrowserRegexTest {

    private static final List<String> itemList = Arrays.asList("temp",
            "surface", "temp2", "temp3", "temp4", "wndSpd", "wndDir", "pres",
            "dptmp", "dpd");

    private static final String NL = "\n";

    /**
     * 
     */
    @Test
    public void testSingleTermMatch_NoCase_NoExclude() {
        List<String> expected = new ArrayList<String>();
        expected.add("temp");
        expected.add("temp2");
        expected.add("temp3");
        expected.add("temp4");

        String searchTerm = "Emp";
        String[] fullList = itemList.toArray(new String[itemList.size()]);

        List<String> results = DataBrowserUtils.search(searchTerm, fullList,
                true, false, false);

        String msg = checkResults(expected, results);
        assertNull(msg);
    }

    /**
     * 
     */
    @Test
    public void testSingleTermMatch_Case_NoExclude() {
        List<String> expected = new ArrayList<String>();
        expected.add("temp");
        expected.add("temp2");
        expected.add("temp3");
        expected.add("temp4");

        String searchTerm = "emp";
        String[] fullList = itemList.toArray(new String[itemList.size()]);

        List<String> results = DataBrowserUtils.search(searchTerm, fullList,
                true, true, false);

        String msg = checkResults(expected, results);
        assertNull(msg);
    }

    /**
     * 
     */
    @Test
    public void testSingleTermMatch_NoCase_Exclude() {
        List<String> expected = new ArrayList<String>();
        expected.add("dpd");
        expected.add("dptmp");
        expected.add("surface");
        expected.add("wndSpd");
        expected.add("wndDir");
        expected.add("pres");

        String searchTerm = "emp";
        String[] fullList = itemList.toArray(new String[itemList.size()]);

        List<String> results = DataBrowserUtils.search(searchTerm, fullList,
                true, false, true);

        String msg = checkResults(expected, results);
        assertNull(msg);
    }

    /**
     * 
     */
    @Test
    public void testSingleTermMatch_Case_Exclude() {
        List<String> expected = new ArrayList<String>();
        expected.add("dpd");
        expected.add("dptmp");
        expected.add("surface");
        expected.add("wndSpd");
        expected.add("wndDir");
        expected.add("pres");

        String searchTerm = "emp";
        String[] fullList = itemList.toArray(new String[itemList.size()]);

        List<String> results = DataBrowserUtils.search(searchTerm, fullList,
                true, true, true);

        String msg = checkResults(expected, results);
        assertNull(msg);
    }

    /**
     * 
     */
    @Test
    public void testMultipleTermMatchAnyNoCaseNoExclude() {
        List<String> expected = new ArrayList<String>();
        expected.add("temp");
        expected.add("temp2");
        expected.add("temp3");
        expected.add("temp4");

        String searchTerm = "Te 3";
        String[] fullList = itemList.toArray(new String[itemList.size()]);

        List<String> results = DataBrowserUtils.search(searchTerm, fullList,
                true, false, false);

        String msg = checkResults(expected, results);
        assertNull(msg);
    }

    /**
     * 
     */
    @Test
    public void testMultipleTermMatchAllNoCaseNoExclude() {
        List<String> expected = new ArrayList<String>();
        expected.add("temp3");

        String searchTerm = "Te 3";
        String[] fullList = itemList.toArray(new String[itemList.size()]);

        List<String> results = DataBrowserUtils.search(searchTerm, fullList,
                false, false, false);

        String msg = checkResults(expected, results);
        assertNull(msg);
    }

    /**
     * 
     */
    @Test
    public void testMultipleTermMatchAllCase_NoExclude() {
        List<String> expected = new ArrayList<String>();
        expected.add("temp3");

        String searchTerm = "te 3";
        String[] fullList = itemList.toArray(new String[itemList.size()]);

        List<String> results = DataBrowserUtils.search(searchTerm, fullList,
                false, true, false);

        String msg = checkResults(expected, results);
        assertNull(msg);
    }

    /**
     * 
     */
    @Test
    public void testSingleTermMatchAnyNoCase_NoExclude_Wildcard() {
        List<String> expected = new ArrayList<String>();
        expected.add("temp3");

        String searchTerm = "T*3";
        String[] fullList = itemList.toArray(new String[itemList.size()]);

        List<String> results = DataBrowserUtils.search(searchTerm, fullList,
                true, false, false);

        String msg = checkResults(expected, results);
        assertNull(msg);
    }

    /**
     * 
     */
    @Test
    public void testSingleTermMatchAnyCase_NoExclude_Wildcard() {
        List<String> expected = new ArrayList<String>();
        expected.add("temp3");

        String searchTerm = "t*3";
        String[] fullList = itemList.toArray(new String[itemList.size()]);

        List<String> results = DataBrowserUtils.search(searchTerm, fullList,
                true, true, false);

        String msg = checkResults(expected, results);
        assertNull(msg);
    }

    /**
     * 
     */
    @Test
    public void testSingleTermMatchAnyNoCase_Exclude_Wildcard() {
        List<String> expected = new ArrayList<String>();
        expected.add("surface");
        expected.add("wndSpd");
        expected.add("wndDir");
        expected.add("pres");
        expected.add("dptmp");
        expected.add("dpd");

        String searchTerm = "te*3";
        String[] fullList = itemList.toArray(new String[itemList.size()]);

        List<String> results = DataBrowserUtils.search(searchTerm, fullList,
                true, false, true);
        String msg = checkResults(expected, results);
        assertNull(msg);
    }

    /**
     * 
     */
    @Test
    public void testMultipleTermMatchAnyNoCase_NoExclude_Wildcard() {
        List<String> expected = new ArrayList<String>();
        expected.add("temp3");
        expected.add("wndSpd");
        expected.add("wndDir");

        String searchTerm = "t*3 wnd";
        String[] fullList = itemList.toArray(new String[itemList.size()]);

        List<String> results = DataBrowserUtils.search(searchTerm, fullList,
                true, false, false);

        String msg = checkResults(expected, results);
        assertNull(msg);
    }

    /**
     * 
     */
    @Test
    public void testMultipleTermMatchAllNoCase_NoExclude_Wildcard() {
        List<String> expected = new ArrayList<String>();
        expected.add("temp3");

        String searchTerm = "t*3 mp";
        String[] fullList = itemList.toArray(new String[itemList.size()]);

        List<String> results = DataBrowserUtils.search(searchTerm, fullList,
                false, false, false);

        String msg = checkResults(expected, results);
        assertNull(msg);
    }

    /**
     * 
     * @param expected
     *            The expected results
     * @param results
     *            The actual results
     * @return message null if lists match up, otherwise a message stating the
     *         problem with the lists
     */
    private String checkResults(List<String> expected, List<String> actual) {
        if (actual.containsAll(expected) && expected.containsAll(actual)) {
            return null;
        }

        // lists don't match, didn't get back what we expected. Construct an
        // error message
        List<String> extraActual = new ArrayList<String>();
        for (String s : actual) {
            if (!expected.contains(s)) {
                extraActual.add(s);
            }
        }

        List<String> extraExpected = new ArrayList<String>();
        for (String s : expected) {
            if (!actual.contains(s)) {
                extraExpected.add(s);
            }
        }

        StringBuilder sb = new StringBuilder();
        if (extraActual.size() > 0) {
            sb.append("Actual contained additional values:\n");
            for (String s : extraActual) {
                sb.append(s + NL);
            }
        }

        if (extraExpected.size() > 0) {
            sb.append(NL);
            sb.append("Expected contained additional values:\n");
            for (String s : extraExpected) {
                sb.append(s + NL);
            }
        }

        return sb.toString();
    }
}
