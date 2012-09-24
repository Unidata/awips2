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
package com.raytheon.uf.viz.d2d.nsharp.display;

import gov.noaa.nws.ncep.ui.nsharp.view.NsharpPaletteWindow;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;

/**
 * 
 * Extends NsharpPaletteWindow but overide load to prevent opening ncmapeditor.
 * Also disable unload since loading and unloading is being handled by time
 * matched resources.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 26, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class D2DNSharpPaletteWindow extends NsharpPaletteWindow {

    @Override
    public void createDataControlGp(Composite parent) {
        super.createDataControlGp(parent);
        unloadBtn.dispose();
        // remove any other listeners.
        for (Listener listener : loadBtn.getListeners(SWT.MouseUp)) {
            loadBtn.removeListener(SWT.MouseUp, listener);
        }
        loadBtn.addListener(SWT.MouseUp, new Listener() {

            @Override
            public void handleEvent(Event event) {
                D2DNsharpHandleArchiveFile.openArchiveFile(getViewSite()
                        .getShell());
            }
        });

    }

}
