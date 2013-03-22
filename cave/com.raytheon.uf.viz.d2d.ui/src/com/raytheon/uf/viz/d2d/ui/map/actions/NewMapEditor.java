package com.raytheon.uf.viz.d2d.ui.map.actions;

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

import java.io.File;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.maps.scales.MapScales;
import com.raytheon.uf.viz.core.maps.scales.MapScales.MapScale;
import com.raytheon.uf.viz.core.maps.scales.MapScales.PartId;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.viz.ui.UiUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Opens a new map editor
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 11, 2007            chammack    Initial Creation.
 * Mar 21, 2013       1638 mschenke    Changed map scales not tied to d2d
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public class NewMapEditor extends AbstractHandler {
    
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(NewMapEditor.class);

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public AbstractEditor execute(ExecutionEvent arg0)
            throws ExecutionException {
        MapScale editorScale = null;
        String editorId = null;
        for (MapScale scale : MapScales.getInstance().getScales()) {
            for (PartId partId : scale.getPartIds()) {
                if (partId.isView() == false) {
                    editorScale = scale;
                    editorId = partId.getId();
                }
            }
        }

        File bundle = null;
        try {
            bundle = editorScale.getFile();
            Bundle b = Bundle.unmarshalBundle(bundle);
            return UiUtil.createEditor(editorId, b.getDisplays());
        } catch (Exception e) {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "Unable to load bundle, " + bundle + " to screen", e);
        }

        return null;
    }
}
