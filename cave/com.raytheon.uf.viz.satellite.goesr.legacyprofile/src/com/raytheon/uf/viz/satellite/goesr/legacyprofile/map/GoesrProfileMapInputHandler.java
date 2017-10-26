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
package com.raytheon.uf.viz.satellite.goesr.legacyprofile.map;

import gov.noaa.nws.ncep.ui.nsharp.display.NsharpSkewTPaneDescriptor;
import gov.noaa.nws.ncep.ui.nsharp.display.NsharpSkewTPaneDisplay;

import java.util.HashMap;

import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPart;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.viz.core.DescriptorMap;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.satellite.goesr.legacyprofile.GoesrLegacyProfileResourceData;
import com.raytheon.viz.ui.BundleProductLoader;
import com.raytheon.viz.ui.UiUtil;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.input.InputAdapter;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Input handler for GOESR Legacy Moisture/Temperature profiles availability
 * resource
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 30, 2015  4335     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GoesrProfileMapInputHandler extends InputAdapter {

    private GoesrProfileMapResource resource;

    private Cursor handCursor;

    private boolean overDataPoint = false;

    private int downX, downY;

    public GoesrProfileMapInputHandler(GoesrProfileMapResource resource) {
        this.resource = resource;
        Display display = Display.getCurrent();
        handCursor = new Cursor(display, SWT.CURSOR_HAND);
    }

    public void dispose() {
        handCursor.dispose();
    }

    @Override
    public boolean handleMouseMove(int x, int y) {
        boolean wasData = overDataPoint;
        overDataPoint = false;

        if (resource.isEditable()) {
            Coordinate sampleCoord = resource.getResourceContainer()
                    .translateClick(x, y);
            if (sampleCoord != null) {
                overDataPoint = resource.isDataPoint(sampleCoord);
            }
        }

        if (wasData && !overDataPoint) {
            getShell().setCursor(null);
        } else if (!wasData && overDataPoint) {
            getShell().setCursor(handCursor);
        }

        return super.handleMouseMove(x, y);
    }

    @Override
    public boolean handleMouseDown(int x, int y, int mouseButton) {
        if (mouseButton == 1) {
            downX = x;
            downY = y;
        }
        return super.handleMouseDown(x, y, mouseButton);
    }

    @Override
    public boolean handleMouseUp(int x, int y, int mouseButton) {
        int downX = this.downX;
        int downY = this.downY;
        this.downX = this.downY = -1;
        if (overDataPoint && mouseButton == 1 && downX == x && downY == y) {
            HashMap<String, RequestConstraint> metadataMap = new HashMap<>(
                    resource.getResourceData().getMetadataMap());
            metadataMap.put("physicalElement", new RequestConstraint(
                    "V%P-%hPa", ConstraintType.LIKE));
            GoesrLegacyProfileResourceData resourceData = new GoesrLegacyProfileResourceData();
            resourceData.setMetadataMap(metadataMap);
            resourceData.setSoundingType("GOES");
            resourceData.setCoordinate(resource.getResourceContainer()
                    .translateClick(x, y));
            ResourcePair pair = new ResourcePair();
            pair.setResourceData(resourceData);
            pair.setLoadProperties(new LoadProperties());
            NsharpSkewTPaneDisplay display = new NsharpSkewTPaneDisplay();
            display.setDescriptor(new NsharpSkewTPaneDescriptor());
            display.getDescriptor().getResourceList().add(pair);
            String editorId = DescriptorMap.getEditorId(display.getDescriptor()
                    .getClass().getName());
            AbstractEditor editor = UiUtil.createOrOpenEditor(editorId,
                    display.cloneDisplay());
            Bundle b = new Bundle();
            b.setDisplays(new AbstractRenderableDisplay[] { display });
            Job j = new BundleProductLoader(editor, b);
            j.schedule();
            return true;
        }
        return super.handleMouseUp(x, y, mouseButton);
    }

    @Override
    public boolean handleMouseExit(Event event) {
        overDataPoint = false;
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
