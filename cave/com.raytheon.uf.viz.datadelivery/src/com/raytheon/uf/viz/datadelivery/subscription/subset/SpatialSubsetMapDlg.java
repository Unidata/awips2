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
package com.raytheon.uf.viz.datadelivery.subscription.subset;

import java.io.File;
import java.text.DecimalFormat;
import java.text.NumberFormat;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.jts.ReferencedEnvelope;

import com.raytheon.uf.common.datadelivery.registry.EnvelopeUtils;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IRenderableDisplayChangedListener;
import com.raytheon.uf.viz.core.IRenderableDisplayChangedListener.DisplayChangeType;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.maps.display.MapRenderableDisplay;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.IInputHandler.InputPriority;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.datadelivery.common.spatial.SpatialUtils;
import com.raytheon.uf.viz.datadelivery.rsc.DrawBoxResource;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;
import com.raytheon.viz.ui.actions.LoadSerializedXml;
import com.raytheon.viz.ui.dialogs.CaveSWTDialogBase;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;
import com.raytheon.viz.ui.editor.ISelectedPanesChangedListener;
import com.raytheon.viz.ui.panes.PaneManager;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Dialog for selecting an area via a map.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 18, 2012            mpduff      Initial creation.
 * Oct 31, 2012   1278     mpduff      Integrated SpatialUtils and allow for
 *                                     display of already selected area on the map.
 * Dec 07, 2012 1278       bgonzale    added setter for spatialUtils.
 * Dec 10, 2012 1259       bsteffen   Switch Data Delivery from LatLon to referenced envelopes.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class SpatialSubsetMapDlg extends CaveSWTDialogBase implements
        IMultiPaneEditor, BoxListener {
    /** Status Handler */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SpatialSubsetMapDlg.class);

    /** Bundle file */
    private final String BUNDLE_FILE = "/bundles/dataDelivery/dataDeliveryBundle.xml";

    /** PaneManager reference */
    private final PaneManager paneManager;

    /** LoopProperties reference */
    private LoopProperties loopProperties;

    /** Upper left value text field */
    private Text ulValueTxt;

    /** Lower right value text field */
    private Text lrValueTxt;

    /** ISubset callback */
    private final ISubset callback;

    /** Number format for display */
    private final NumberFormat formatter = new DecimalFormat(".0000");

    /** Map composite */
    private Composite mapComp;

    /** Envelope describing full area where requests are possible */
    private ReferencedEnvelope fullEnvelope;

    /** Envelope describing a subsection of fullEnvelope */
    private ReferencedEnvelope subEnvelope;

    /**
     * Constructor
     * 
     * @param shell
     *            the parent shell
     * @param callback
     *            the callback class
     * @param fullEnvelope
     *            the full area where request are possible
     * @param subEnvelope
     *            the initial subset to draw.
     */
    public SpatialSubsetMapDlg(Shell shell, ISubset callback,
            ReferencedEnvelope fullEnvelope, ReferencedEnvelope subEnvelope) {
        super(shell, SWT.DIALOG_TRIM | SWT.MIN, CAVE.INDEPENDENT_SHELL);
        paneManager = new PaneManager();
        loopProperties = new LoopProperties();
        this.callback = callback;
        if (fullEnvelope == null) {
            setFullEnvelope(EnvelopeUtils.WORLD_WIDE_EQ_CYLINDRICAL_CENTER180);
            this.subEnvelope = null;
        } else {
            setFullEnvelope(fullEnvelope);
            this.subEnvelope = subEnvelope;
        }
        this.setText("Subset");
    }

    /*
     * (non-Javadoc)
     *
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        return new GridLayout(1, false);
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayoutData()
     */
    @Override
    protected Object constructShellLayoutData() {
        // return new GridData(500, SWT.DEFAULT);
        return new GridData(SWT.FILL, SWT.DEFAULT, true, false);
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

        createDisplayPane();
        createButtons();

        setCorners();

        ResourceList rscList = paneManager.getActiveDisplayPane()
                .getDescriptor().getResourceList();

        for (ResourcePair rp : rscList) {
            if (rp.getResource() instanceof DrawBoxResource) {
                DrawBoxResource rsc = (DrawBoxResource) rp.getResource();
                rsc.addListener(this);
                rsc.setShell(shell);
                if (subEnvelope != null) {
                    Coordinate ul = EnvelopeUtils
                            .getUpperLeftLatLon(subEnvelope);
                    Coordinate lr = EnvelopeUtils
                            .getLowerRightLatLon(subEnvelope);
                    rsc.setCoordinates(ul, lr);
                    ul = EnvelopeUtils.getUpperLeftLatLon(fullEnvelope);
                    lr = EnvelopeUtils.getLowerRightLatLon(fullEnvelope);
                    rsc.setSpatialUtils(new SpatialUtils(ul, lr));
                }
            }
        }
    }

    /**
     * Create the display pane.
     */
    private void createDisplayPane() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(1, false);

        Group group = new Group(shell, SWT.NONE);
        group.setText(" Subset by Space ");
        group.setLayout(gl);
        group.setLayoutData(gd);

        gd = new GridData(750, 575);
        // gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gl = new GridLayout(1, false);
        mapComp = new Composite(group, SWT.BORDER);
        mapComp.setLayout(gl);
        mapComp.setLayoutData(gd);

        paneManager.initializeComponents(this, mapComp);
        try {
            File defaultBundle = PathManagerFactory.getPathManager()
                    .getStaticFile(BUNDLE_FILE);
            Bundle b = Bundle.unmarshalBundle(defaultBundle, null);
            for (IRenderableDisplay display : b.getDisplays()) {
                if (display instanceof MapRenderableDisplay) {
                    Coordinate center = EnvelopeUtils
                            .getCenterLatLon(fullEnvelope);
                    ((MapRenderableDisplay) display).setMapCenter(new double[] {
                            center.x, center.y });

                }
                if (display.getDescriptor() instanceof IMapDescriptor) {
                    IMapDescriptor desc = (IMapDescriptor) display
                            .getDescriptor();
                    desc.setGridGeometry(getGridGeometry());
                }
            }
            LoadSerializedXml.loadTo(this, b);
            this.refresh();
        } catch (VizException e) {
            // statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
            // e);
            e.printStackTrace();
        }

        gl = new GridLayout(4, false);
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);

        Composite actionComp = new Composite(group, SWT.NONE);
        actionComp.setLayout(gl);
        actionComp.setLayoutData(gd);

        Label ulLabel = new Label(actionComp, SWT.NONE);
        ulLabel.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, true, false));
        ulLabel.setText("Upper Left:");

        // TODO Need to update the box when the text fields change
        ulValueTxt = new Text(actionComp, SWT.BORDER);
        ulValueTxt.setLayoutData(new GridData(150, SWT.DEFAULT));

        Label lrLabel = new Label(actionComp, SWT.NONE);
        lrLabel.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, true, false));
        lrLabel.setText("     Lower Right:");

        lrValueTxt = new Text(actionComp, SWT.BORDER);
        lrValueTxt.setLayoutData(new GridData(150, SWT.DEFAULT));

    }

    /**
     * Create the buttons.
     */
    private void createButtons() {
        Composite btnComp = new Composite(shell, SWT.NONE);

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(2, false);

        btnComp.setLayout(gl);
        btnComp.setLayoutData(gd);

        GridData btnData = new GridData(70, SWT.DEFAULT);
        Button okBtn = new Button(btnComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(btnData);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (handleOk()) {
                    shell.close();
                }
            }
        });

        btnData = new GridData(70, SWT.DEFAULT);
        Button cancelBtn = new Button(btnComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(btnData);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                shell.close();
            }
        });
    }

    /**
     * OK Button action handler.
     *
     * @return true if data are valid
     */
    private boolean handleOk() {
        Coordinate ul = parseCoordinate(ulValueTxt.getText());
        if (ul == null) {
            return false;
        }
        Coordinate lr = parseCoordinate(lrValueTxt.getText());
        if (lr == null) {
            return false;
        }
        subEnvelope = EnvelopeUtils.createSubenvelopeFromLatLon(fullEnvelope,
                ul, lr);
        callback.updateBounds(subEnvelope);
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.IDisplayPaneContainer#getDisplayPanes()
     */
    @Override
    public IDisplayPane[] getDisplayPanes() {
        return paneManager.getDisplayPanes();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.IDisplayPaneContainer#getLoopProperties()
     */
    @Override
    public LoopProperties getLoopProperties() {
        return this.loopProperties;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IDisplayPaneContainer#setLoopProperties(com.
     * raytheon.uf.viz.core.datastructure.LoopProperties)
     */
    @Override
    public void setLoopProperties(LoopProperties loopProperties) {
        this.loopProperties = loopProperties;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IDisplayPaneContainer#getActiveDisplayPane()
     */
    @Override
    public IDisplayPane getActiveDisplayPane() {
        return paneManager.getActiveDisplayPane();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.IDisplayPaneContainer#refresh()
     */
    @Override
    public void refresh() {
        paneManager.refresh();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IDisplayPaneContainer#translateClick(double,
     * double)
     */
    @Override
    public Coordinate translateClick(double x, double y) {
        return paneManager.translateClick(x, y);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IDisplayPaneContainer#translateInverseClick(
     * com.raytheon.uf.viz.core.Coordinate)
     */
    @Override
    public double[] translateInverseClick(Coordinate c) {
        return paneManager.translateInverseClick(c);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.IDisplayPaneContainer#
     * addRenderableDisplayChangedListener
     * (com.raytheon.uf.viz.core.IRenderableDisplayChangedListener)
     */
    @Override
    public void addRenderableDisplayChangedListener(
            IRenderableDisplayChangedListener displayChangedListener) {
        paneManager.addRenderableDisplayChangedListener(displayChangedListener);

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.IDisplayPaneContainer#
     * removeRenderableDisplayChangedListener
     * (com.raytheon.uf.viz.core.IRenderableDisplayChangedListener)
     */
    @Override
    public void removeRenderableDisplayChangedListener(
            IRenderableDisplayChangedListener displayChangedListener) {
        paneManager
                .removeRenderableDisplayChangedListener(displayChangedListener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.IDisplayPaneContainer#
     * notifyRenderableDisplayChangedListeners
     * (com.raytheon.uf.viz.core.IDisplayPane,
     * com.raytheon.uf.viz.core.drawables.IRenderableDisplay,
     * com.raytheon.uf.viz
     * .core.IRenderableDisplayChangedListener.DisplayChangeType)
     */
    @Override
    public void notifyRenderableDisplayChangedListeners(IDisplayPane pane,
            IRenderableDisplay display, DisplayChangeType type) {
        paneManager
                .notifyRenderableDisplayChangedListeners(pane, display, type);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IDisplayPaneContainer#registerMouseHandler(com
     * .raytheon.uf.viz.core.rsc.IInputHandler,
     * com.raytheon.uf.viz.core.rsc.IInputHandler.InputPriority)
     */
    @Override
    public void registerMouseHandler(IInputHandler handler,
            InputPriority priority) {
        paneManager.registerMouseHandler(handler, priority);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IDisplayPaneContainer#registerMouseHandler(com
     * .raytheon.uf.viz.core.rsc.IInputHandler)
     */
    @Override
    public void registerMouseHandler(IInputHandler handler) {
        paneManager.registerMouseHandler(handler);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IDisplayPaneContainer#unregisterMouseHandler
     * (com.raytheon.uf.viz.core.rsc.IInputHandler)
     */
    @Override
    public void unregisterMouseHandler(IInputHandler handler) {
        paneManager.unregisterMouseHandler(handler);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.editor.IMultiPaneEditor#getNumberofPanes()
     */
    @Override
    public int getNumberofPanes() {
        return paneManager.getNumberofPanes();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.IMultiPaneEditor#setSelectedPane(java.lang
     * .String, com.raytheon.uf.viz.core.IDisplayPane)
     */
    @Override
    public void setSelectedPane(String action, IDisplayPane pane) {
        paneManager.setSelectedPane(action, pane);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.IMultiPaneEditor#getSelectedPane(java.lang
     * .String)
     */
    @Override
    public IDisplayPane getSelectedPane(String action) {
        return paneManager.getSelectedPane(action);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.IMultiPaneEditor#getSelectedPanes(java.lang
     * .String)
     */
    @Override
    public IDisplayPane[] getSelectedPanes(String action) {
        return paneManager.getSelectedPanes(action);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.IMultiPaneEditor#isSelectedPane(java.lang.
     * String, com.raytheon.uf.viz.core.IDisplayPane)
     */
    @Override
    public boolean isSelectedPane(String action, IDisplayPane pane) {
        return paneManager.isSelectedPane(action, pane);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.IMultiPaneEditor#addSelectedPaneChangedListener
     * (com.raytheon.viz.ui.editor.ISelectedPanesChangedListener)
     */
    @Override
    public void addSelectedPaneChangedListener(
            ISelectedPanesChangedListener listener) {
        paneManager.addSelectedPaneChangedListener(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.IMultiPaneEditor#removeSelectedPaneChangedListener
     * (com.raytheon.viz.ui.editor.ISelectedPanesChangedListener)
     */
    @Override
    public void removeSelectedPaneChangedListener(
            ISelectedPanesChangedListener listener) {
        paneManager.removeSelectedPaneChangedListener(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.IMultiPaneEditor#addPane(com.raytheon.uf.viz
     * .core.drawables.IRenderableDisplay)
     */
    @Override
    public IDisplayPane addPane(IRenderableDisplay renderableDisplay) {
        return paneManager.addPane(renderableDisplay);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.IMultiPaneEditor#removePane(com.raytheon.uf
     * .viz.core.IDisplayPane)
     */
    @Override
    public void removePane(IDisplayPane pane) {
        paneManager.removePane(pane);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.IMultiPaneEditor#hidePane(com.raytheon.uf.
     * viz.core.IDisplayPane)
     */
    @Override
    public void hidePane(IDisplayPane pane) {
        paneManager.hidePane(pane);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.IMultiPaneEditor#showPane(com.raytheon.uf.
     * viz.core.IDisplayPane)
     */
    @Override
    public void showPane(IDisplayPane pane) {
        paneManager.showPane(pane);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.editor.IMultiPaneEditor#displayedPaneCount()
     */
    @Override
    public int displayedPaneCount() {
        return paneManager.displayedPaneCount();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.editor.IMultiPaneEditor#clear()
     */
    @Override
    public void clear() {
        paneManager.clear();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.datadelivery.test.subscription.subset.BoxListener
     * #cornerPointsChanged
     * (com.raytheon.uf.viz.datadelivery.test.subscription.subset
     * .BoxChangeEvent)
     */
    @Override
    public void cornerPointsChanged(BoxChangeEvent event) {
        Coordinate[] coordinates = (Coordinate[]) event.getSource();

        subEnvelope = EnvelopeUtils.createSubenvelopeFromLatLon(
fullEnvelope,
                coordinates[0],
                coordinates[1]);
        setCorners();
    }

    /**
     * Set the corners.
     */
    private void setCorners() {
        Coordinate ul = null;
        Coordinate lr = null;
        if (subEnvelope != null) {
            ul = EnvelopeUtils.getUpperLeftLatLon(subEnvelope);
            lr = EnvelopeUtils.getLowerRightLatLon(subEnvelope);
        } else {
            ul = EnvelopeUtils.getUpperLeftLatLon(fullEnvelope);
            lr = EnvelopeUtils.getLowerRightLatLon(fullEnvelope);
        }
        if (ul != null) {
            ulValueTxt.setText(formatter.format(ul.x) + ", "
                    + formatter.format(ul.y));
        }

        if (lr != null) {
            lrValueTxt.setText(formatter.format(lr.x) + ", "
                    + formatter.format(lr.y));
        }
    }

    /**
     * Get a grid Geometry with for the descriptor.
     * 
     * @return
     */
    private GridGeometry2D getGridGeometry() {
        double aspect = fullEnvelope.getWidth() / fullEnvelope.getHeight();
        int mapWidth = (aspect > 1.0 ? (int) Math.round(aspect * 10000) : 10000);
        int mapHeight = (aspect > 1.0 ? 10000 : (int) Math
                .round(10000 / aspect));

        return new GridGeometry2D(new GeneralGridEnvelope(new int[] { 0, 0 },
                new int[] { mapWidth, mapHeight }, false), fullEnvelope);
    }

    /**
     * Parse and validate the points string.
     * 
     * @param str
     *            the String of point values
     * 
     * @return a Coordinate if points are valid
     */
    private Coordinate parseCoordinate(String str) {
        if (str == null || str.length() == 0) {
            DataDeliveryUtils.showMessage(getShell(), SWT.OK, "Invalid Entry",
                    "Lat/Lon values are required");
            return null;
        }

        if (!str.contains(",")) {
            DataDeliveryUtils.showMessage(getShell(), SWT.OK, "Invalid Entry",
                    str + " is invalid");
            return null;
        }

        String[] parts = str.split(",");
        if (parts == null || parts.length != 2) {
            DataDeliveryUtils.showMessage(getShell(), SWT.OK, "Invalid Entry",
                    str + " is invalid");
            return null;
        }

        try {
            double x = Double.parseDouble(parts[0]);
            double y = Double.parseDouble(parts[1]);
            Coordinate c = new Coordinate(x, y);
            // Allow up to 5% error to handle rounding problems with formatted
            // LatLon values.
            if (!EnvelopeUtils.envelopeContainsLatLon(fullEnvelope, c, 0.05)) {
                    return null;
            }
            return c;
        } catch (NumberFormatException e) {
            DataDeliveryUtils.showMessage(getShell(), SWT.OK, "Invalid Entry",
                    "Invalid Lat/Lon values entered: " + str);
            return null;
        }
    }

    public ReferencedEnvelope getSubEnvelope() {
        return subEnvelope;
    }

    public void setSubEnvelope(ReferencedEnvelope subEnvelope) {
        this.subEnvelope = subEnvelope;
    }

    public void setFullEnvelope(ReferencedEnvelope fullEnvelope) {
        this.fullEnvelope = fullEnvelope;
    }

}
