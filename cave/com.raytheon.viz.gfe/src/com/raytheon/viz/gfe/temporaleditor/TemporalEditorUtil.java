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

package com.raytheon.viz.gfe.temporaleditor;

import java.util.List;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.gfe.core.DataManagerUIFactory;
import com.raytheon.viz.gfe.core.IReferenceSetManager;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.gridmanager.GridManager;
import com.raytheon.viz.gfe.gridmanager.GridManagerUtil;

/**
 * Displays the Temporal Editor Data
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 30, 2009  2159     rjpeter   Initial creation.
 * Feb 20, 2015  4051     dgilling  Added determinePointsToUse() to allow
 *                                  TemporalEditor to edit with no active edit
 *                                  area.
 * Feb 07, 2018  6882     randerso  Changed to use ReferenceData.isEmpty()
 *
 * </pre>
 *
 * @author rjpeter
 */

public class TemporalEditorUtil extends GridManagerUtil {

    /** Text Justification */
    @SuppressWarnings("javadoc")
    public enum TextJustify {
        TOP, BOTTOM, LEFT, RIGHT, CENTER
    }

    /**
     * Constructor
     *
     * @param gridManager
     */
    public TemporalEditorUtil(GridManager gridManager) {
        super(gridManager);
    }

    @Override
    protected TimeRange getVisibleTimeRange() {
        return gridManager.getTemporalEditorVisibleTimeRange();
    }

    /**
     * Draw justified text
     *
     * @param gc
     * @param txt
     * @param x
     * @param y
     * @param vertTxtJust
     * @param horzTxtJust
     */
    public static void drawJustifiedText(GC gc, String txt, int x, int y,
            TextJustify vertTxtJust, TextJustify horzTxtJust) {
        Point txtSize = gc.textExtent(txt);
        switch (horzTxtJust) {
        case RIGHT:
            x -= txtSize.x;
            break;
        case CENTER:
            x -= txtSize.x / 2;
            break;
        case LEFT:
        default:
            // do nothing
        }
        switch (vertTxtJust) {
        case TOP:
            y -= txtSize.y;
            break;
        case CENTER:
            y -= txtSize.y / 2;
            break;
        case BOTTOM:
        default:
            // do nothing
        }
        gc.drawText(txt, x, y, true);
    }

    /**
     *
     * @param colorList
     * @param min
     * @param max
     * @param value
     * @return color from color list to be used for value
     */
    public static Color getColorForValue(List<Color> colorList, float min,
            float max, float value) {
        int size = colorList.size() - 1;
        float valPerIndex = size / (max - min);
        int index = Math.round((value - min) * valPerIndex);
        return colorList.get(index);
    }

    /**
     * @param parm
     * @return the title bar text for parm
     */
    public static String getTitleBarText(Parm parm) {
        ParmID pId = parm.getGridInfo().getParmID();
        DatabaseID dbId = pId.getDbId();
        return pId.getParmName() + " " + pId.getParmLevel() + " "
                + dbId.getModelName() + " (" + dbId.getSiteId() + ")";
    }

    /**
     * @param refSet
     * @return the edit area mask based on refset
     */
    public static Grid2DBit determinePointsToUse(final ReferenceData refSet) {
        Grid2DBit pointsToUse;

        if (refSet.isEmpty()) {
            IReferenceSetManager refMgr = DataManagerUIFactory
                    .getCurrentInstance().getRefManager();
            pointsToUse = refMgr.fullRefSet().getGrid();
        } else {
            pointsToUse = refSet.getGrid();
        }

        return pointsToUse;
    }
}
