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
package com.raytheon.uf.common.menus.vb;

/**
 * Items for the View, a.k.a Settings menu.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 08, 2018 6355       nabowle     Extracted from VBMenuBarItemsMgr.
 *
 * </pre>
 *
 * @author nabowle
 */
public enum ViewMenu {
    PLANVIEW("Plan view"),
    CROSSSECTION("Cross section"),
    TIMEHEIGHT("Time height"),
    VARVSHGT("Var vs Hgt"),
    SOUNDING("Sounding"),
    TIMESERIES("Time series");

    String displayString;

    ViewMenu(String displayStr) {
        displayString = displayStr;
    }

    public String getDisplayString() {
        return displayString;
    }
}