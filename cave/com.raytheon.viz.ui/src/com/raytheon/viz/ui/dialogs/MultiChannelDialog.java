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
package com.raytheon.viz.ui.dialogs;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IMultiChannelImageExtension.Channel;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IMultiChannelImageExtension.ChannelData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.capabilities.MultiChannelCapability;

/**
 * Dialog for manipulating multi channel capability
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 3, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class MultiChannelDialog extends CaveSWTDialog {

    private static final String DISABLED = "Disabled";

    private AbstractVizResource<?, ?> resource;

    private MultiChannelCapability capability;

    private String[] items;

    private List<ColorMapSliderComp> sliderComps;

    public MultiChannelDialog(Shell parent, AbstractVizResource<?, ?> resource,
            MultiChannelCapability capability) {
        super(parent);
        this.capability = capability;
        this.resource = resource;
        String[] names = capability.getNames();
        this.items = new String[names.length + 1];
        System.arraycopy(names, 0, items, 1, names.length);
        items[0] = DISABLED;
        this.sliderComps = new ArrayList<ColorMapSliderComp>();
        setText("Channel Options");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        Map<Channel, ChannelData> channelMap = capability.getChannelMap();
        for (Channel c : Channel.values()) {
            addGroup(shell, c, channelMap.get(c));
        }

        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp
                .setLayoutData(new GridData(SWT.CENTER, SWT.FILL, true, true));
        createBottomButtons(buttonComp);
    }

    private void createBottomButtons(Composite buttonComp) {
        buttonComp.setLayout(new GridLayout(2, true));
        Button b = new Button(buttonComp, SWT.PUSH);
        b.setText(IDialogConstants.OK_LABEL);
        b.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                okPressed();
            }
        });
        b.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, false, true));

        b = new Button(buttonComp, SWT.PUSH);
        b.setText(IDialogConstants.CANCEL_LABEL);
        b.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                cancelPressed();
            }
        });
        GridData gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, true);
        gd.widthHint = 100;
        b.setLayoutData(gd);
    }

    private void addGroup(Composite parent, Channel channel,
            final ChannelData data) {
        ColorMapParameters params = null;
        if (data != null) {
            params = data.parameters;
        } else {
            params = new ColorMapParameters();
        }

        Group group = new Group(parent, SWT.SHADOW_ETCHED_IN);
        group.setLayout(new GridLayout(4, false));
        group.setText(channel + ":");
        group.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, true));
        final ColorMapSliderComp cmapSlider = new ColorMapSliderComp(group,
                params);
        cmapSlider.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        sliderComps.add(cmapSlider);

        final Button invertBtn = new Button(group, SWT.CHECK);
        invertBtn.addSelectionListener(new SelectionAdapter() {
            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                data.invert = invertBtn.getSelection();
                resource.issueRefresh();
            }
        });

        if (data != null) {
            invertBtn.setSelection(data.invert);
        }

        Label label = new Label(group, SWT.NONE);
        label.setText("Invert");

        final Combo options = new Combo(group, SWT.DROP_DOWN | SWT.READ_ONLY);
        options.setLayoutData(new GridData(SWT.CENTER, SWT.CENTER, false, false));
        options.setItems(items);
        if (data == null) {
            options.setText(DISABLED);
            enable(group, false);
        } else {
            if (data.name == null) {
                options.setText(DISABLED);
                enable(cmapSlider, false);
            } else {
                options.setText(data.name);
            }
            options.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    if (DISABLED.equals(options.getText())) {
                        enable(cmapSlider, false);
                        data.name = null;
                    } else {
                        enable(cmapSlider, true);
                        data.name = options.getText();
                    }
                    capability.capabilityChanged();
                }
            });
        }
    }

    private void cancelPressed() {
        for (ColorMapSliderComp cmapSlider : sliderComps) {
            cmapSlider.restore();
        }
        close();
    }

    private void okPressed() {
        close();
    }
}
