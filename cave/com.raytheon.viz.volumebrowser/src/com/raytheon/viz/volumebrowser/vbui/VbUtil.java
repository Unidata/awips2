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

import java.awt.Rectangle;

import org.eclipse.ui.IEditorPart;
import org.geotools.geometry.GeneralEnvelope;
import org.opengis.geometry.DirectPosition;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.maps.display.VizMapEditor;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Simple utility class for logic that repeatedly shows up in the volume browser
 * 
 * @author jelkins
 * 
 */
public class VbUtil {

    /**
     * Returns the corner positions of the current map
     * 
     * @return a DirectPosition array containing the lower left, lower right,
     *         upper right, and upper left corner positions of the display at
     *         array positions 0, 1, 2, and 3 respectively. The center position
     *         is returned at array position 4. null is returned when the Corner
     *         Positions cannot be determined.
     */
    public static DirectPosition[] getCornerPositions() {

        DirectPosition lowerLeftCornerDirectPosition = null, lowerRightCornerDirectPosition = null, upperRightCornerDirectPosition = null, upperLeftCornerDirectPosition = null, centerDirectPosition = null;

        try {

            GeneralEnvelope env = null;

            IEditorPart editor = EditorUtil.findEditor(VizMapEditor.EDITOR_ID);
            if (editor != null) {
                env = (GeneralEnvelope) ((AbstractEditor) editor)
                        .getActiveDisplayPane().getDescriptor()
                        .getGridGeometry().getEnvelope();
            } else {
                env = (GeneralEnvelope) new MapDescriptor().getGridGeometry()
                        .getEnvelope();
            }

            MathTransform mt = MapUtil.getTransformToLatLon(env
                    .getCoordinateReferenceSystem());
            lowerLeftCornerDirectPosition = mt.transform(env.getLowerCorner(),
                    null);
            upperRightCornerDirectPosition = mt.transform(env.getUpperCorner(),
                    null);

            upperLeftCornerDirectPosition = env.getUpperCorner();
            upperLeftCornerDirectPosition.setOrdinate(0, env.getMinimum(0));
            upperLeftCornerDirectPosition = mt.transform(
                    upperLeftCornerDirectPosition, null);

            lowerRightCornerDirectPosition = env.getLowerCorner();
            lowerRightCornerDirectPosition.setOrdinate(0, env.getMaximum(0));
            lowerRightCornerDirectPosition = mt.transform(
                    lowerRightCornerDirectPosition, null);

            centerDirectPosition = mt.transform(env.getMedian(), null);

        } catch (Exception e) {
            return null;
        }

        return new DirectPosition[] { lowerLeftCornerDirectPosition,
                lowerRightCornerDirectPosition, upperRightCornerDirectPosition,
                upperLeftCornerDirectPosition, centerDirectPosition };
    }

    /**
     * 
     * @return a Rectangle that covers the entire range of the current map
     */
    public static Rectangle getMapCoverageRectangle() {

        Rectangle rectangle = new Rectangle();

        DirectPosition[] cornerPositions = getCornerPositions();

        double smallestX = cornerPositions[0].getCoordinate()[0], largestX = cornerPositions[0]
                .getCoordinate()[0], smallestY = cornerPositions[0]
                .getCoordinate()[1], largestY = cornerPositions[0]
                .getCoordinate()[1];

        for (DirectPosition cornerPosition : getCornerPositions()) {
            double cornerPositionX = cornerPosition.getCoordinate()[0];
            double cornerPositionY = cornerPosition.getCoordinate()[1];

            smallestX = Math.min(cornerPositionX, smallestX);
            largestX = Math.max(cornerPositionX, largestX);

            smallestY = Math.min(cornerPositionY, smallestY);
            largestY = Math.max(cornerPositionY, largestY);
        }

        rectangle.x = (int) Math.ceil(smallestX);
        rectangle.y = (int) Math.ceil(smallestY);

        rectangle.width = (int) Math.ceil(largestX - smallestX);
        rectangle.height = (int) Math.ceil(largestY - smallestY);

        return rectangle;
    }

    /**
     * 
     * @return the currently instantiated DataListsProdTableComp of the Volume
     *         Browser
     */
    public static DataListsProdTableComp getDataListsProdTableComp() {
        return (VolumeBrowserAction.getVolumeBrowserDlg().getListTableComp());
    }

}
