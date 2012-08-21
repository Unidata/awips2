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
package com.raytheon.uf.viz.truecolor.ui;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
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
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IDisposeListener;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.truecolor.extension.ITrueColorImagingExtension.Channel;
import com.raytheon.uf.viz.truecolor.rsc.TrueColorResourceGroup;
import com.raytheon.uf.viz.truecolor.rsc.TrueColorResourceGroup.DisplayedChannelResource;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.ColorMapSliderComp;

/**
 * Dialog for modifying true color attributes on a dialog
 * 
 * TODO: Update sliders when combo changes
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 16, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class TrueColorDialog extends CaveSWTDialog implements IDisposeListener {

    private static final String DISABLED = "Disabled";

    protected TrueColorResourceGroup resource;

    private List<ColorMapSliderComp> sliderComps;

    private Map<String, DisplayedChannelResource> resourceMap;

    private Map<Channel, DisplayedChannelResource> resourceChannelMap;

    private Map<DisplayedChannelResource, Channel[]> originalChannelMap;

    /**
     * Creates the TrueColorDialog as a window of the parent Shell
     * 
     * @param parent
     * @param resource
     */
    public TrueColorDialog(Shell parent, TrueColorResourceGroup resource) {
        super(parent, SWT.DIALOG_TRIM);
        this.resource = resource;
        this.sliderComps = new ArrayList<ColorMapSliderComp>();
        this.resourceMap = new LinkedHashMap<String, DisplayedChannelResource>();
        this.resourceChannelMap = new HashMap<Channel, DisplayedChannelResource>();
        this.originalChannelMap = new HashMap<DisplayedChannelResource, Channel[]>();
        setText("Composite Options");
        populateItemMap(resource.getChannelResources());
        resource.registerListener(this);
    }

    /**
     * @param channelResources
     */
    private void populateItemMap(
            Collection<DisplayedChannelResource> channelResources) {
        resourceMap.put(DISABLED, null);
        for (DisplayedChannelResource rsc : channelResources) {
            Channel[] channels = rsc.channel.getChannels();
            resourceMap.put(rsc.getDisplayName(), rsc);
            originalChannelMap.put(rsc, channels);
            for (Channel c : channels) {
                resourceChannelMap.put(c, rsc);
            }
        }
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
        for (Channel c : Channel.values()) {
            addGroup(shell, c, resourceChannelMap.get(c));
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

    /**
     * @param shell
     * @param c
     * @param displayedChannelResource
     */
    private void addGroup(Composite parent, final Channel channel,
            DisplayedChannelResource displayedResource) {
        ColorMapParameters params = null;
        if (displayedResource != null) {
            params = displayedResource.resource.getCapability(
                    ColorMapCapability.class).getColorMapParameters();
        } else {
            params = new ColorMapParameters();
        }

        Group group = new Group(parent, SWT.SHADOW_ETCHED_IN);
        group.setLayout(new GridLayout(2, false));
        group.setText(channel + ":");
        group.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, true));
        final ColorMapSliderComp cmapSlider = new ColorMapSliderComp(group,
                params);
        cmapSlider.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        sliderComps.add(cmapSlider);

        final Combo options = new Combo(group, SWT.DROP_DOWN | SWT.READ_ONLY);
        options.setLayoutData(new GridData(SWT.CENTER, SWT.CENTER, false, false));
        options.setItems(resourceMap.keySet().toArray(
                new String[resourceMap.size()]));
        if (displayedResource == null) {
            options.setText(DISABLED);
            enable(group, false);
        } else {
            options.setText(displayedResource.getDisplayName());
            options.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    DisplayedChannelResource channelResource = resourceMap
                            .get(options.getText());
                    enable(cmapSlider, channelResource != null);
                    DisplayedChannelResource oldResource = resourceChannelMap
                            .put(channel, channelResource);
                    if (oldResource != null) {
                        oldResource.removeChannel(channel);
                    }
                    if (channelResource != null) {
                        channelResource.addChannel(channel);
                        // TODO: Update slider?
                    }

                    resource.issueRefresh();
                }
            });
        }
    }

    private void cancelPressed() {
        for (ColorMapSliderComp cmapSlider : sliderComps) {
            cmapSlider.restore();
        }
        for (DisplayedChannelResource rsc : originalChannelMap.keySet()) {
            rsc.setChannels(originalChannelMap.get(rsc));
        }
        close();
    }

    private void okPressed() {
        close();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.IDisposeListener#disposed(com.raytheon.uf
     * .viz.core.rsc.AbstractVizResource)
     */
    @Override
    public void disposed(AbstractVizResource<?, ?> rsc) {
        close();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#disposed()
     */
    @Override
    protected void disposed() {
        super.disposed();
        resource.unregisterListener(this);
    }

}
