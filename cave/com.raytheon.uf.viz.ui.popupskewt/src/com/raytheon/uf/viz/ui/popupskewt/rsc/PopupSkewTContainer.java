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
package com.raytheon.uf.viz.ui.popupskewt.rsc;

import java.util.IdentityHashMap;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;

import com.raytheon.uf.common.sounding.VerticalSounding;
import com.raytheon.uf.common.sounding.adapter.IVerticalSoundingProvider;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.sounding.providers.VerticalSoundingProviderFactory;
import com.raytheon.uf.viz.ui.popupskewt.config.SoundingSource;
import com.raytheon.uf.viz.ui.popupskewt.ui.PopupSkewTDialog;
import com.raytheon.viz.ui.VizWorkbenchManager;

/**
 * Container object for popupskewt
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 2, 2013        2190 mschenke    Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class PopupSkewTContainer {

    private static Map<IDisplayPaneContainer, PopupSkewTContainer> containers = new IdentityHashMap<IDisplayPaneContainer, PopupSkewTContainer>();

    private IDisplayPaneContainer displayContainer;

    private PopupSkewTDialog dialog;

    private SoundingSource soundingSource;

    private IVerticalSoundingProvider soundingProvider;

    private boolean popupSkewTOn = false;

    private int references = 0;

    public synchronized static PopupSkewTContainer createContainer(
            PopupSkewTResource resource) {
        IDisplayPaneContainer rscContainer = resource.getResourceContainer();
        PopupSkewTContainer container = containers.get(rscContainer);
        if (container == null) {
            container = new PopupSkewTContainer(rscContainer);
            containers.put(rscContainer, container);
        }
        container.references += 1;
        return container;
    }

    public synchronized static void disposeContainer(
            PopupSkewTContainer container) {
        container.references -= 1;
        if (container.references == 0) {
            container.closeSkewTDialog();
            containers.remove(container.displayContainer);
        }
    }

    private PopupSkewTContainer(IDisplayPaneContainer displayContainer) {
        this.displayContainer = displayContainer;
    }

    public SoundingSource getSoundingSource() {
        return soundingSource;
    }

    public void setSoundingSource(SoundingSource soundingSource) {
        this.soundingSource = soundingSource;
        if (soundingSource != null) {
            this.soundingProvider = VerticalSoundingProviderFactory
                    .getVerticalSoundingProvider(soundingSource.getType(),
                            soundingSource.getConstraints());
        } else {
            this.soundingProvider = null;
        }
    }

    public IVerticalSoundingProvider getSoundingProvider() {
        return soundingProvider;
    }

    public boolean isPopupSkewTOn() {
        return popupSkewTOn;
    }

    public void setPopupSkewTOn(boolean popupSkewTOn) {
        this.popupSkewTOn = popupSkewTOn;
        if (isPopupSkewTOn() == false) {
            closeSkewTDialog();
        }
    }

    public synchronized void showSkewTDialog() {
        if (dialog == null) {
            dialog = new PopupSkewTDialog(VizWorkbenchManager.getInstance()
                    .getCurrentWindow().getShell());
            dialog.addListener(SWT.Dispose, new Listener() {
                @Override
                public void handleEvent(Event event) {
                    dialog = null;
                    popupSkewTOn = false;
                }
            });
            dialog.open();
        }
    }

    public synchronized void closeSkewTDialog() {
        if (dialog != null) {
            dialog.close();
            dialog = null;
        }
    }

    public synchronized void plotHeight(float height, float temp) {
        showSkewTDialog();
        dialog.plotHeight(height, temp);
    }

    public synchronized void plotSounding(VerticalSounding sounding) {
        showSkewTDialog();
        dialog.setSounding(sounding);
    }
}
