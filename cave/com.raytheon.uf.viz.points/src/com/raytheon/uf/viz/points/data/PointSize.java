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

import com.raytheon.uf.viz.points.PointUtilities;

/**
 * This class handles determining the what size font to use for a point's label.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * October-2010              epolster    Initial Creation.
 * 
 * </pre>
 * 
 * @author epolster
 * @version 1.0
 */

public enum PointSize {
    SMALL(0, PointUtilities.FONT_1), DEFAULT(1, PointUtilities.FONT_2), LARGE(
            2, PointUtilities.FONT_3), EXTRA_LARGE(3, PointUtilities.FONT_4), XXL(
            4, PointUtilities.FONT_5);

    public static final int MIN_ORDINAL = 0;

    public static final int MAX_ORDINAL = 4;

    private int _ordinal = 0;

    private int _fontSize = 0;

    private String POINTS_NAME = " pt";

    PointSize(int ordinal, int fontSize) {
        _ordinal = ordinal;
        _fontSize = fontSize;
    }

    public int getFontSize() {
        return _fontSize;
    }

    public int getOrdinal() {
        return _ordinal;
    }

    static public PointSize getPointSize(int ordinal) {
        PointSize newMS = null;
        switch (ordinal) {
        case 0:
            newMS = SMALL;
            break;
        case 1:
            newMS = DEFAULT;
            break;
        case 2:
            newMS = LARGE;
            break;
        case 3:
            newMS = EXTRA_LARGE;
            break;
        case 4:
            newMS = XXL;
            break;
        }
        return newMS;
    }

    public String getReadableName() {
        String is = Integer.toString(_fontSize);
        is = is.concat(POINTS_NAME);
        return " ".concat(is);
    }

    public boolean isAtMaxSize() {
        return (_ordinal == PointSize.MAX_ORDINAL);
    }

    public boolean isAtMinSize() {
        return (_ordinal == PointSize.MIN_ORDINAL);
    }

    public PointSize getNextHigher() {
        PointSize newMS = null;
        int o = _ordinal;
        if (o == MAX_ORDINAL) {
            newMS = this;
        } else {
            newMS = PointSize.getPointSize(o + 1);
        }
        return newMS;
    }

    public PointSize getNextLower() {
        PointSize newMS = null;
        int o = _ordinal;
        if (o == MIN_ORDINAL) {
            newMS = this;
        } else {
            newMS = PointSize.getPointSize(o - 1);
        }
        return newMS;
    }

    static public int getNumberOfFontSizes() {
        return PointSize.MAX_ORDINAL;
    }
}
