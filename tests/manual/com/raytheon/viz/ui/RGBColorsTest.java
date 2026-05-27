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
package com.raytheon.viz.ui;

import com.raytheon.uf.viz.core.RGBColors;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jul 31, 2008				randerso	Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class RGBColorsTest {
    public static void main(String[] args) {
        test("forestgreen");
        test("tomato2");
        test("yellow");
        test("red");
        test("#0bc71e");
        test("#60C7B8");
        test("#e11a00");
    }

    /**
     * @param string
     */
    private static void test(String s) {
        System.out.println("\"" + s + "\": " + RGBColors.getRGBColor(s));
    }
}
