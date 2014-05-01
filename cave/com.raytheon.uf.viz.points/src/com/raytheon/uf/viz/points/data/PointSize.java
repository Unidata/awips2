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
package com.raytheon.uf.viz.points.data;

/**
 * This class for handling point's font sizes.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 08, 2012 #875       rferrel     Initial Creation.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public enum PointSize {

    SMALL(12), DEFAULT(14), LARGE(18), EXTRA_LARGE(24), XXL(30);

    private static final String POINTS_NAME = " pt";

    private transient int fontSize;

    private transient String readableName;

    PointSize(int fontSize) {
        this.fontSize = fontSize;
        this.readableName = " " + fontSize + POINTS_NAME;
    }

    public int getFontSize() {
        return fontSize;
    }

    /**
     * @param ordinal
     * @return ps
     */
    static public PointSize getPointSize(int ordinal) {
        PointSize newPS = null;
        switch (ordinal) {
        case 0:
            newPS = SMALL;
            break;
        case 1:
            newPS = DEFAULT;
            break;
        case 2:
            newPS = LARGE;
            break;
        case 3:
            newPS = EXTRA_LARGE;
            break;
        case 4:
            newPS = XXL;
            break;
        }
        return newPS;
    }

    /**
     * A descriptive name usable in a combo box item.
     * 
     * @return name
     */
    public String getReadableName() {
        return readableName;
    }
}
