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
package com.raytheon.viz.volumebrowser.vbui;

import com.raytheon.uf.common.menus.vb.ViewMenu;
import com.raytheon.uf.viz.core.rsc.ResourceType;

/**
 *
 * This class contains enumerations used for creating the menu items in the
 * Volume Browser.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jun 09, 2009  2161     lvenable  Initial creation
 * Feb 08, 2018  6355     nabowle   Extracted ViewMenu to a common plugin.
 * Oct 22, 2018  7483     bsteffen  Handle SPACE mode selection on resources
 *                                  instead of editors.
 * 
 * </pre>
 *
 * @author lvenable
 */
public class VBMenuBarItemsMgr {

    public static ResourceType getResourceType(ViewMenu vm) {
        switch (vm) {
        case PLANVIEW:
            return ResourceType.PLAN_VIEW;
        case CROSSSECTION:
            return ResourceType.CROSS_SECTION;
        case SOUNDING:
            return ResourceType.SOUNDING;
        case TIMEHEIGHT:
            return ResourceType.TIME_HEIGHT;
        case TIMESERIES:
            return ResourceType.TIME_SERIES;
        case VARVSHGT:
            return ResourceType.VAR_HEIGHT;
        }
        return null;
    }

    /**
     * Items for the "Space/Time" menu.
     */
    public static enum SpaceTimeMenu {
        TIME("Time"), SPACE("Space");

        public final String displayString;

        SpaceTimeMenu(String displayStr) {
            displayString = displayStr;
        }

        public String getDisplayString() {
            return displayString;
        }

    };

    /**
     * Items for the "Left/Right" menu.
     */
    public static enum LeftRightMenu {
        LEFT("<left"), RIGHT("right>");

        public final String displayString;

        LeftRightMenu(String displayStr) {
            displayString = displayStr;
        }

        public String getDisplayString() {
            return displayString;
        }
    };

    /**
     * Items for the "Points" menu.
     */
    public static enum PointsMenu {
        POINT_A("Point A"),
        POINT_B("Point B"),
        POINT_C("Point C"),
        POINT_D("Point D"),
        POINT_E("Point E"),
        POINT_F("Point F"),
        POINT_G("Point G"),
        POINT_H("Point H"),
        POINT_I("Point I"),
        POINT_J("Point J");

        public final String displayString;

        PointsMenu(String displayStr) {
            displayString = displayStr;
        }

        public String getDisplayString() {
            return displayString;
        }
    };
}
