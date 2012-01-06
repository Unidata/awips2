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
package com.raytheon.viz.mpe.ui.dialogs;

import com.raytheon.viz.mpe.util.DailyQcUtils.Station;

/**
 * Change and manage display position of text labels for Stations in DQC
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 7, 2011            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class TextPositionMgr {
    private static final int UPPER_LEFT = 0;

    private static final int UPPER_RIGHT = 1;

    private static final int LOWER_LEFT = 2;

    private static final int LOWER_RIGHT = 3;

    public TextPositionMgr() {

    }

    // ---------------------------------------------------------------
    public int getTextPosition(Station selectedStation) {
        int position = LOWER_RIGHT;
        int xadd = selectedStation.xadd;
        int yadd = selectedStation.yadd;

        if (xadd == -1 && yadd == -1) {
            position = UPPER_LEFT;
        } else if (xadd == -1 && yadd == 0) {
            position = LOWER_LEFT;
        } else if (xadd == 0 && yadd == -1) {
            position = UPPER_RIGHT;
        } else if (xadd == 0 && yadd == 0) {
            position = LOWER_RIGHT;
        }

        return position;
    }

    // ---------------------------------------------------------------

    protected void changeStationLocation(int textPositionCode, Station station) {
        int xadd = 0;
        int yadd = 0;

        if (textPositionCode == UPPER_LEFT) {
            xadd = -1;
            yadd = -1;
        }

        else if (textPositionCode == UPPER_RIGHT) {
            xadd = 0;
            yadd = -1;
        }

        else if (textPositionCode == LOWER_LEFT) {
            xadd = -1;
            yadd = 0;
        }

        else if (textPositionCode == LOWER_RIGHT) {
            xadd = 0;
            yadd = 0;
        }

        station.xadd = xadd;
        station.yadd = yadd;

        return;
    }
}
