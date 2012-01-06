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
package com.raytheon.viz.gfe.gridmanager;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jul 2, 2008				danfitch	Initial creation
 * 
 * </pre>
 * 
 * @author danfitch
 * @version 1.0
 */

public enum GridMode {

    NORMAL("Normal"), HISTORY("History"), LASTSAVED("Saved"), LASTMODIFIED(
            "Modified"), LASTPUBLISHED("Published"), LASTSENT("Sent");

    private String displayString;

    private GridMode(String displayString) {
        this.displayString = displayString;
    }

    @Override
    public String toString() {
        return displayString;
    }

    public static GridMode valueFrom(String displayString) {
        GridMode result = null;
        if (displayString.equals(NORMAL.displayString)) {
            result = NORMAL;
        } else if (displayString.equals(HISTORY.displayString)) {
            result = HISTORY;
        } else if (displayString.equals(LASTSAVED.displayString)) {
            result = LASTSAVED;
        } else if (displayString.equals(LASTMODIFIED.displayString)) {
            result = LASTMODIFIED;
        } else if (displayString.equals(LASTPUBLISHED.displayString)) {
            result = LASTPUBLISHED;
        } else if (displayString.equals(LASTSENT.displayString)) {
            result = LASTSENT;
        }

        return result;
    }

}
