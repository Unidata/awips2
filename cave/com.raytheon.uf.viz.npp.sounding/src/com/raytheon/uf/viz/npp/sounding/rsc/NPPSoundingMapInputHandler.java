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
package com.raytheon.uf.viz.npp.sounding.rsc;

import java.util.Collection;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPart;

import com.raytheon.uf.common.dataplugin.npp.sounding.NPPSoundingRecord;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.input.InputAdapter;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Input handler for npp sounding availability resource
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 14, 2013            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class NPPSoundingMapInputHandler extends InputAdapter {

    private NPPSoundingMapResource resource;

    private Cursor handCursor;

    private NPPSoundingRecord closestRecord;

    private int downX, downY;

    public NPPSoundingMapInputHandler(NPPSoundingMapResource resource) {
        this.resource = resource;
        Display display = Display.getCurrent();
        handCursor = new Cursor(display, SWT.CURSOR_HAND);
    }

    public void dispose() {
        handCursor.dispose();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.InputAdapter#handleMouseMove(int, int)
     */
    @Override
    public boolean handleMouseMove(int x, int y) {
        boolean wasClosest = closestRecord != null;
        closestRecord = null;

        if (resource.isEditable()) {
            Collection<NPPSoundingRecord> records = resource
                    .getCurrentRecords();
            if (records != null) {
                double radius = resource.getRadius();
                Coordinate c = new Coordinate(x, y);
                double bestDist = Double.MAX_VALUE;
                for (NPPSoundingRecord record : records) {
                    double lat = record.getLatitude();
                    double lon = record.getLongitude();
                    double[] pixel = resource.getResourceContainer()
                            .translateInverseClick(new Coordinate(lon, lat));
                    Coordinate p = new Coordinate(pixel[0], pixel[1]);
                    double dist = p.distance(c);
                    if (dist < bestDist && dist < radius) {
                        closestRecord = record;
                        bestDist = dist;
                    }
                }
            }
        }

        if (wasClosest && closestRecord == null) {
            getShell().setCursor(null);
        } else if (!wasClosest && closestRecord != null) {
            getShell().setCursor(handCursor);
        }

        return super.handleMouseMove(x, y);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.InputAdapter#handleMouseDown(int, int,
     * int)
     */
    @Override
    public boolean handleMouseDown(int x, int y, int mouseButton) {
        if (mouseButton == 1) {
            downX = x;
            downY = y;
        }
        return super.handleMouseDown(x, y, mouseButton);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.InputAdapter#handleMouseUp(int, int, int)
     */
    @Override
    public boolean handleMouseUp(int x, int y, int mouseButton) {
        int downX = this.downX;
        int downY = this.downY;
        this.downX = this.downY = -1;
        if (closestRecord != null && mouseButton == 1 && downX == x
                && downY == y) {
            resource.loadSoundingResource(closestRecord);
            closestRecord = null;
            return true;
        }
        return super.handleMouseUp(x, y, mouseButton);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.input.InputAdapter#handleMouseExit(org.eclipse.swt
     * .widgets.Event)
     */
    @Override
    public boolean handleMouseExit(Event event) {
        closestRecord = null;
        getShell().setCursor(null);
        return super.handleMouseExit(event);
    }

    private Shell getShell() {
        IDisplayPaneContainer container = resource.getResourceContainer();
        if (container instanceof IWorkbenchPart) {
            return ((IWorkbenchPart) container).getSite().getShell();
        } else {
            return VizWorkbenchManager.getInstance().getCurrentWindow()
                    .getShell();
        }
    }

}
