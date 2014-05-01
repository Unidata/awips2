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

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.uf.viz.d2d.core.time.LoadMode;
import com.raytheon.uf.viz.d2d.ui.time.dialogs.LoadModeSwitcher;

/**
 * 
 * This class contains enumerations used for creating the menu items in the
 * Volume Browser.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 9, 2009  #2161      lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class VBMenuBarItemsMgr {

    /**
     * Items for the View, a.k.a Settings menu.
     */
    public static enum ViewMenu {
        PLANVIEW("Plan view"), CROSSSECTION("Cross section"), TIMEHEIGHT(
                "Time height"), VARVSHGT("Var vs Hgt"), SOUNDING("Sounding"), TIMESERIES(
                "Time series");

        String displayString;

        ViewMenu(String displayStr) {
            displayString = displayStr;
        }

        public String getDisplayString() {
            return displayString;
        }

        public ResourceType getResourceType() {
            switch (this) {
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
    };

    /**
     * Items for the "Space/Time" menu.
     */
    public static enum SpaceTimeMenu {
        TIME("Time", new IMenuItemCallback() {

            @Override
            public void execute() {

                LoadModeSwitcher switcher = new LoadModeSwitcher();
                try {
                    switcher.switchLoadMode(LoadMode.VALID_TIME_SEQ);
                } catch (VizException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }

        }), SPACE("Space", new IMenuItemCallback() {

            @Override
            public void execute() {
                LoadModeSwitcher switcher = new LoadModeSwitcher();
                try {
                    switcher.switchLoadMode(LoadMode.PROG_LOOP);
                } catch (VizException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }

        });

        IMenuItemCallback callback;

        String displayString;

        SpaceTimeMenu(String displayStr, IMenuItemCallback menuItemCallback) {
            displayString = displayStr;
            callback = menuItemCallback;
        }

        public String getDisplayString() {
            return displayString;
        }

        public IMenuItemCallback getCallback() {
            return callback;
        }
    };

    /**
     * Items for the "Left/Right" menu.
     */
    public static enum LeftRightMenu {
        LEFT("<left"), RIGHT("right>");

        String displayString;

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
        POINT_A("Point A"), POINT_B("Point B"), POINT_C("Point C"), POINT_D(
                "Point D"), POINT_E("Point E"), POINT_F("Point F"), POINT_G(
                "Point G"), POINT_H("Point H"), POINT_I("Point I"), POINT_J(
                "Point J");

        String displayString;

        PointsMenu(String displayStr) {
            displayString = displayStr;
        }

        public String getDisplayString() {
            return displayString;
        }
    };
}
