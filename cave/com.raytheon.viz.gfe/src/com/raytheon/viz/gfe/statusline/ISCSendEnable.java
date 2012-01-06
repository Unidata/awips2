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
package com.raytheon.viz.gfe.statusline;

import org.eclipse.jface.action.ContributionItem;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.core.msgs.ISCSendStatusChangedMsg;
import com.raytheon.viz.gfe.core.msgs.Message;
import com.raytheon.viz.gfe.core.msgs.Message.IMessageClient;

/**
 * A status line contribution to display the ISC Send Enable status
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 25, 2010            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class ISCSendEnable extends ContributionItem implements IMessageClient {
    private ImageRegistry registry;

    private Canvas canvas;

    @SuppressWarnings("unchecked")
    public ISCSendEnable() {
        registry = new ImageRegistry();
        registry.put("green", Activator.imageDescriptorFromPlugin(
                Activator.PLUGIN_ID, "icons/green.gif"));
        registry.put("red", Activator.imageDescriptorFromPlugin(
                Activator.PLUGIN_ID, "icons/red.gif"));

        Message.registerInterest(this, ISCSendStatusChangedMsg.class);
    }

    @SuppressWarnings("unchecked")
    @Override
    public void dispose() {
        Message.unregisterInterest(this, ISCSendStatusChangedMsg.class);

        super.dispose();

        if (registry != null) {
            registry.dispose();
            registry = null;
        }

    }

    @Override
    public void fill(Composite parent) {

        Composite comp = new Composite(parent, SWT.NONE);

        comp.setLayout(new GridLayout(2, false));
        GridData layoutData;

        Label label = new Label(comp, SWT.NONE);
        label.setText("ISCxmt:");
        layoutData = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        label.setLayoutData(layoutData);

        canvas = new Canvas(comp, SWT.NONE);
        canvas.setBackground(null);

        Image image = registry.get("green");
        canvas.setBackgroundImage(image);
        Rectangle imageBounds = image.getBounds();
        layoutData = new GridData(imageBounds.width, imageBounds.height);
        canvas.setLayoutData(layoutData);

        update();
    }

    @Override
    public void update() {
        boolean enabled = Message.inquireLastMessage(
                ISCSendStatusChangedMsg.class).isEnabled();
        Image image;
        if (enabled) {
            image = registry.get("green");
        } else {
            image = registry.get("red");
        }
        canvas.setBackgroundImage(image);
        canvas.redraw();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.msgs.Message.IMessageClient#receiveMessage(
     * com.raytheon.viz.gfe.core.msgs.Message)
     */
    @Override
    public void receiveMessage(Message message) {
        if (message instanceof ISCSendStatusChangedMsg) {
            VizApp.runAsync(new Runnable() {

                @Override
                public void run() {
                    update();
                }
            });
        }
    }
}
