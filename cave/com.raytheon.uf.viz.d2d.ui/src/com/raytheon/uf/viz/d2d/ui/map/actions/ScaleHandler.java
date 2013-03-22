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

package com.raytheon.uf.viz.d2d.ui.map.actions;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.maps.scales.MapScales;
import com.raytheon.uf.viz.core.maps.scales.MapScales.MapScale;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.d2d.core.map.D2DMapRenderableDisplay;
import com.raytheon.viz.ui.EditorUtil;

/**
 * Load the scale bundle and merge it into the existing bundle
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Jul 24, 2007             randerso    Initial Creation.
 * Oct 21, 2008   #1450     randerso    Fixed to support multipane editors
 * Mar 21, 2013       1638  mschenke    Changed map scales not tied to d2d
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class ScaleHandler extends AbstractHandler {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ScaleHandler.class);

    public ScaleHandler() {
    }

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        String scale = event.getParameter("scale");

        setScale(scale);

        return null;
    }

    /**
     * @param scale
     */
    public static void setScale(String scale) {
        // retrieve the existing map descriptor
        IDisplayPaneContainer editor = EditorUtil.getActiveVizContainer();
        if (editor == null) {
            try {
                editor = new NewMapEditor().execute(null);
            } catch (ExecutionException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error creating new map editor for scale: " + scale);
            }
        }
        setScale(editor, scale);
    }

    /**
     * Set the scale on the container passed in
     * 
     * @param editor
     * @param scale
     */
    public static void setScale(IDisplayPaneContainer editor, String scale) {
        if (editor == null) {
            statusHandler.handle(Priority.PROBLEM,
                    "Could not set scale on null editor");
        }
        MapScale mapScale = MapScales.getInstance().getScaleByName(scale);
        if (mapScale == null) {
            statusHandler.handle(Priority.PROBLEM,
                    "Could not find scale with name: " + scale);
        } else {
            File scaleBundle = mapScale.getFile();
            if (scaleBundle == null || scaleBundle.exists() == false
                    || scaleBundle.isFile() == false) {
                statusHandler.handle(Priority.PROBLEM,
                        "Could not find scale file: " + mapScale.getFileName());
            } else {
                try {
                    Object obj = SerializationUtil
                            .jaxbUnmarshalFromXmlFile(scaleBundle);
                    if (obj instanceof Bundle == false) {
                        throw new SerializationException(
                                "Deserialized object is not of type "
                                        + Bundle.class);
                    }

                    Bundle b = (Bundle) obj;
                    AbstractRenderableDisplay[] displays = b.getDisplays();
                    if (displays.length != 1) {
                        throw new VizException(
                                "Scale bundle must have exactly 1 renderable display, "
                                        + scaleBundle.getAbsolutePath()
                                        + " had " + displays.length);
                    }
                    if (displays[0] instanceof D2DMapRenderableDisplay == false) {
                        throw new VizException("Bundle, "
                                + scaleBundle.getAbsolutePath()
                                + ", is not a valid D2D map scale bundle");
                    }

                    D2DMapRenderableDisplay rd = (D2DMapRenderableDisplay) displays[0];

                    for (IDisplayPane pane : editor.getDisplayPanes()) {
                        D2DMapRenderableDisplay existingDisplay = (D2DMapRenderableDisplay) pane
                                .getRenderableDisplay();
                        IMapDescriptor existingMD = existingDisplay
                                .getDescriptor();

                        // set the projection
                        existingMD.setGridGeometry(rd.getDescriptor()
                                .getGridGeometry());
                        pane.setZoomLevel(1.0f);
                        pane.scaleToClientArea();

                        // set the scale name
                        existingDisplay.setScale(mapScale.getDisplayName());

                        // remove the existing map resources
                        List<ResourcePair> remove = new ArrayList<ResourcePair>();
                        for (ResourcePair rp : existingMD.getResourceList()) {
                            // if resource is not a map layer add it to the new
                            // md
                            if (rp.getProperties().isMapLayer()) {
                                remove.add(rp);
                            }
                        }
                        existingMD.getResourceList().removeAll(remove);

                        // add the new map resources
                        for (ResourcePair rp : rd.getDescriptor()
                                .getResourceList()) {
                            if (rp.getProperties().isMapLayer()) {
                                if (rp.getResource() == null) {
                                    rp.instantiateResource(existingMD);
                                }
                                existingMD.getResourceList().add(rp);
                            }
                        }
                    }

                    VizGlobalsManager.getCurrentInstance().updateUI(editor);
                } catch (SerializationException e) {
                    statusHandler.handle(
                            Priority.PROBLEM,
                            "Error deserializing file: "
                                    + scaleBundle.getAbsolutePath(), e);
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error loading scale bundle", e);
                }
            }
        }
    }

}
