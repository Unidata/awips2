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

package com.raytheon.viz.core.mode;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.core.ProgramArguments;

/**
 * CAVEMode.
 * 
 * Holds the constants that define the CAVE mode.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ----------	----------	-----------	--------------------------
 * 12/20/07     561         Dan Fitch    Initial Creation.
 * </pre>
 * 
 * @author Dan Fitch
 * @version 1
 */
public enum CAVEMode {

    OPERATIONAL("Operational"), PRACTICE("Practice"), TEST("Test");

    private String displayString;

    private CAVEMode(String displayString) {
        this.displayString = displayString;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Enum#toString()
     */
    @Override
    public String toString() {
        return this.displayString;
    }

    private static Color PRACTICE_BG_COLOR;

    private static Color TEST_BG_COLOR;

    private static Color OPERATIONAL_BG_COLOR;

    private static Color PRACTICE_FG_COLOR;

    private static Color TEST_FG_COLOR;

    private static Color OPERATIONAL_FG_COLOR;

    private static CAVEMode modeAtStartup = CAVEMode.OPERATIONAL;

    public static CAVEMode getMode() {
        return modeAtStartup;
    }

    /**
     * This should NOT be called by end users
     */
    public static void performStartupDuties() {
        String operatingMode = "OPERATIONAL";
        String test = ProgramArguments.getInstance().getString("-mode");
        if (test != null && test.equals("") == false) {
            operatingMode = test;
        }
        try {
            modeAtStartup = CAVEMode.valueOf(operatingMode.toUpperCase());
        } catch (Throwable e2) {
            MessageDialog.openError(new Shell(), "Error starting CAVE",
                    "CAVE will not start in mode: " + operatingMode);
            System.exit(0);
        }

    }

    public static Color getBackgroundColor() {
        return getBackgroundColor(getMode());
    }

    public static Color getBackgroundColor(CAVEMode mode) {
        switch (mode) {
        case PRACTICE:
            if (PRACTICE_BG_COLOR == null) {
                PRACTICE_BG_COLOR = new Color(Display.getDefault(), 255, 165, 0);
            }
            return PRACTICE_BG_COLOR;

        case TEST:
            if (TEST_BG_COLOR == null) {
                TEST_BG_COLOR = Display.getDefault().getSystemColor(
                        SWT.COLOR_BLACK);
            }
            return TEST_BG_COLOR;

        case OPERATIONAL:
            if (OPERATIONAL_BG_COLOR == null) {
                OPERATIONAL_BG_COLOR = Display.getDefault().getSystemColor(
                        SWT.COLOR_WIDGET_BACKGROUND);
            }
            return OPERATIONAL_BG_COLOR;

        default:
            throw new IllegalArgumentException("\"" + mode.name()
                    + "\" is an unrecognized CAVE mode.");
        }
    }

    public static Color getForegroundColor() {
        return getForegroundColor(getMode());
    }

    public static Color getForegroundColor(CAVEMode mode) {
        switch (mode) {
        case PRACTICE:
            if (PRACTICE_FG_COLOR == null) {
                PRACTICE_FG_COLOR = Display.getDefault().getSystemColor(
                        SWT.COLOR_BLACK);
            }
            return PRACTICE_FG_COLOR;

        case TEST:
            if (TEST_FG_COLOR == null) {
                TEST_FG_COLOR = Display.getDefault().getSystemColor(
                        SWT.COLOR_WHITE);
            }
            return TEST_FG_COLOR;

        case OPERATIONAL:
            if (OPERATIONAL_FG_COLOR == null) {
                OPERATIONAL_FG_COLOR = Display.getDefault().getSystemColor(
                        SWT.COLOR_WIDGET_FOREGROUND);
            }
            return OPERATIONAL_FG_COLOR;

        default:
            throw new IllegalArgumentException("\"" + mode.name()
                    + "\" is an unrecognized CAVE mode.");
        }
    }

}
