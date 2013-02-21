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
package com.raytheon.viz.gfe.temporaleditor;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.PreferenceConstants;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IParmManager;
import com.raytheon.viz.gfe.core.ISpatialDisplayManager;
import com.raytheon.viz.gfe.core.msgs.IActivatedParmChangedListener;
import com.raytheon.viz.gfe.core.msgs.IDisplayedParmListChangedListener;
import com.raytheon.viz.gfe.core.msgs.IGridVisibilityChangedListener;
import com.raytheon.viz.gfe.core.msgs.IParmIDChangedListener;
import com.raytheon.viz.gfe.core.msgs.Message;
import com.raytheon.viz.gfe.core.msgs.Message.IMessageClient;
import com.raytheon.viz.gfe.core.msgs.TEOverlayModeChangedMsg;
import com.raytheon.viz.gfe.core.msgs.TEweModeChangedMsg;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.gridmanager.GridManager;

/**
 * TemporalEditor widget containing a TemporalEditorBar for each displayed
 * unit/parm
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 30, 2009 2159       rjpeter      Initial creation
 * Nov 14, 2012 1298       rferrel     Remove no longer used reference to MoveWeatherElementDialog.
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class TemporalEditor extends Composite implements IMessageClient {
    public enum StatisticsMode {
        ABSOLUTE, MODERATED, STANDARD_DEVIATION
    }

    public enum TemporalEditorMode {
        ALL("All Weather Elements"), ALL_NOISC("All Weather Elements w/o ISC"), MUTABLE(
                "Fcst Elements Only"), ACTIVE("Active Element Only"), VISIBLE(
                "Visible Elements Only");

        private String displayString;

        TemporalEditorMode(String displayString) {
            this.displayString = displayString;
        }

        @Override
        public String toString() {
            return displayString;
        }
    }

    private ActivatedParmChangedListener activeParmListener = new ActivatedParmChangedListener();

    private GridVisibilityChangedListener visibilityListener = new GridVisibilityChangedListener();

    private DisplayedParmListChangedListener displayedParmListener = new DisplayedParmListChangedListener();

    private IParmIDChangedListener parmIdChangeListener = new ParmIDChangedListener();

    private PropertyChangeListener propChangeListener = new PropertyChangeListener();

    private TemporalEditorMode teMode;

    protected ScrolledComposite parent;

    private GridManager gridManager;

    private List<AbstractTemporalEditorBar> teBarList = new ArrayList<AbstractTemporalEditorBar>();

    private Map<Parm, AbstractTemporalEditorBar> parmToTEBar = new HashMap<Parm, AbstractTemporalEditorBar>();

    private Map<String, AbstractTemporalEditorBar> unitToGridBar = new HashMap<String, AbstractTemporalEditorBar>();

    private Map<String, List<AbstractTemporalEditorBar>> unitToMovedGridBarList = new HashMap<String, List<AbstractTemporalEditorBar>>();

    private TemporalEditorUtil teUtil;

    private StatisticsMode statMode = StatisticsMode.ABSOLUTE;

    private int moderatedMin = 15;

    private int moderatedMax = 15;

    private float stdDevMin = 1;

    private float stdDevMax = 1;

    private long deactivateTime = 0;

    /**
     * 
     */
    @SuppressWarnings("unchecked")
    public TemporalEditor(ScrolledComposite parent, GridManager gridManager) {
        super(parent, SWT.NONE);

        this.parent = parent;
        GridData gridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gridData.heightHint = 2000;
        GridLayout gridLayout = new GridLayout(1, false);
        gridLayout.marginHeight = 0;
        gridLayout.marginWidth = 0;
        gridLayout.verticalSpacing = 0;
        gridLayout.horizontalSpacing = 0;
        setLayout(gridLayout);
        setLayoutData(gridData);

        teUtil = new TemporalEditorUtil(gridManager);
        this.gridManager = gridManager;
        DataManager dataMgr = gridManager.getDataManager();
        dataMgr.getParmManager().addDisplayedParmListChangedListener(
                displayedParmListener);
        teMode = Message.inquireLastMessage(TEweModeChangedMsg.class).getMode();
        statMode = StatisticsMode
                .valueOf(GFEPreference
                        .getPreference(PreferenceConstants.GFE_TEMPORAL_EDITOR_STATISTICS_MODE));
        moderatedMin = GFEPreference
                .getIntPreference(PreferenceConstants.GFE_TEMPORAL_EDITOR_STATISTICS_MODE_MODERATED_MIN);
        moderatedMax = GFEPreference
                .getIntPreference(PreferenceConstants.GFE_TEMPORAL_EDITOR_STATISTICS_MODE_MODERATED_MAX);
        stdDevMin = (float) GFEPreference
                .getDoublePreference(PreferenceConstants.GFE_TEMPORAL_EDITOR_STATISTICS_MODE_SD_MIN);
        stdDevMax = (float) GFEPreference
                .getDoublePreference(PreferenceConstants.GFE_TEMPORAL_EDITOR_STATISTICS_MODE_SD_MAX);

        Message.registerInterest(this, TEweModeChangedMsg.class);
        GFEPreference.addPropertyChangeListener(propChangeListener);
        dataMgr.getParmManager().addParmIDChangedListener(parmIdChangeListener);
        addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                for (AbstractTemporalEditorBar bar : teBarList) {
                    bar.dispose();
                }

                DataManager dm = TemporalEditor.this.gridManager
                        .getDataManager();
                dm.getParmManager().removeDisplayedParmListChangedListener(
                        displayedParmListener);
                dm.getParmManager().removeParmIDChangedListener(
                        parmIdChangeListener);
                GFEPreference.removePropertyChangeListener(propChangeListener);
                Message.unregisterInterest(TemporalEditor.this,
                        TEweModeChangedMsg.class);
                ISpatialDisplayManager sdm = dm.getSpatialDisplayManager();
                sdm.removeGridVisibilityChangedListener(visibilityListener);
                sdm.removeActivatedParmChangedListener(activeParmListener);
            }
        });
        handleTEMode();
    }

    /**
     * 
     */
    public void resize() {
        VizApp.runSync(new Runnable() {
            @Override
            public void run() {
                layout();
                Point pt = computeSize(SWT.DEFAULT, SWT.DEFAULT);
                GridData gridData = (GridData) getLayoutData();
                gridData.heightHint = pt.y;
                setLayoutData(gridData);
                parent.setMinSize(pt);
                parent.layout();
            }
        });
    }

    /**
     * Adds a parm to the TE. Parm is checked to make sure it is not already
     * displayed.
     * 
     * @param parm
     */
    protected void addParm(Parm parm) {
        if (parm != null && !parmToTEBar.containsKey(parm)) {
            TimeSeries ts = new TimeSeries(parm);

            // if bar with same unit already exists, append to it
            AbstractTemporalEditorBar bar;
            GridType gridType = parm.getGridInfo().getGridType();
            boolean overlay = Message.inquireLastMessage(
                    TEOverlayModeChangedMsg.class).isEnabled();

            // Discrete/Wx parms always get their own bar to start with
            if (!overlay || GridType.DISCRETE.equals(gridType)
                    || GridType.WEATHER.equals(gridType)) {
                bar = AbstractTemporalEditorBar.instanceFor(this, teUtil, parm,
                        ts);
            } else {
                String unit = parm.getGridInfo().getUnitString();

                if (unitToGridBar.containsKey(unit)) {
                    bar = unitToGridBar.get(unit);
                    bar.addParm(parm, ts);
                } else {
                    bar = AbstractTemporalEditorBar.instanceFor(this, teUtil,
                            parm, ts);
                }
            }

            addBar(parm, bar);
        }
    }

    /**
     * 
     * @param parm
     */
    protected void addBar(Parm parm, AbstractTemporalEditorBar bar) {
        if (parm != null && !parmToTEBar.containsKey(parm)) {
            // if bar with same unit already exists, append to it
            String unit = parm.getGridInfo().getUnitString();

            if (!teBarList.contains(bar)) {
                teBarList.add(bar);
            }

            Collections.sort(teBarList);

            for (AbstractTemporalEditorBar teBar : teBarList) {
                teBar.resetLocation();
            }
            resize();

            AbstractTemporalEditorBar unitBar = unitToGridBar.get(unit);

            if (unitBar != null) {
                if (!unitBar.equals(bar)) {
                    List<AbstractTemporalEditorBar> barList = unitToMovedGridBarList
                            .get(unit);

                    if (barList == null) {
                        barList = new ArrayList<AbstractTemporalEditorBar>();
                        unitToMovedGridBarList.put(unit, barList);
                    }

                    if (!barList.contains(bar)) {
                        barList.add(bar);
                    }
                }
            } else {
                unitToGridBar.put(unit, bar);
            }

            parmToTEBar.put(parm, bar);

            redraw();
        }
    }

    /**
     * 
     * @param parm
     * @param sourceBar
     * @param destBar
     */
    public void moveParm(Parm parm, AbstractTemporalEditorBar sourceBar,
            AbstractTemporalEditorBar destBar) {
        if (!sourceBar.equals(destBar)) {
            TEParmDisplayAttributes dispAtt = sourceBar
                    .getParmDisplayAttributes(parm);
            TimeSeries ts = sourceBar.getTimeSeriesForParm(parm);

            if (destBar == null) {
                destBar = AbstractTemporalEditorBar.instanceFor(this, teUtil,
                        parm, ts);
            }

            destBar.addParm(parm, ts);
            destBar.setParmDisplayAttributes(parm, dispAtt);
            removeParm(parm);
            addBar(parm, destBar);
        }
    }

    /**
     * 
     * @param parm
     */
    protected void removeParm(Parm parm) {
        if (parmToTEBar.containsKey(parm)) {
            AbstractTemporalEditorBar bar = parmToTEBar.remove(parm);

            // remove the parm
            bar.removeParm(parm);

            // check if the bar been depleted of parms
            if (bar.getParms().size() == 0) {
                teBarList.remove(bar);

                // remove it from the unitToGridBar mapping
                String unit = parm.getGridInfo().getUnitString();

                if (unitToGridBar.get(unit).equals(bar)) {
                    unitToGridBar.remove(unit);

                    // add the next with the correct unit??
                    List<AbstractTemporalEditorBar> barList = unitToMovedGridBarList
                            .get(unit);
                    if (barList != null && barList.size() > 0) {
                        // grab the first item of the moved list
                        unitToGridBar.put(unit, barList.remove(0));

                        if (barList.size() == 0) {
                            unitToMovedGridBarList.remove(unit);
                        }
                    }
                } else {
                    // was not the original bar, remove it from the moved
                    // list
                    List<AbstractTemporalEditorBar> barList = unitToMovedGridBarList
                            .get(unit);
                    barList.remove(bar);
                    if (barList.size() == 0) {
                        unitToMovedGridBarList.remove(unit);
                    }
                }

                bar.dispose();
                resize();
            }

            redraw();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.swt.widgets.Control#computeSize(int, int)
     */
    @Override
    public Point computeSize(int hint, int hint2) {
        int y = 0;
        for (AbstractTemporalEditorBar bar : teBarList) {
            if (!bar.isDisposed()) {
                y += bar.getHeight();
            }
        }
        int x = hint;
        if (hint == SWT.DEFAULT) {
            x = getClientArea().x;
        }
        return new Point(x, y);
    }

    /**
     * 
     */
    @Override
    public void redraw() {
        if (!isDisposed()) {
            super.redraw();

            VizApp.runSync(new Runnable() {

                @Override
                public void run() {
                    for (AbstractTemporalEditorBar bar : teBarList) {
                        bar.redraw();
                    }
                }
            });
        }
    }

    /**
     * 
     * @param parm
     * @param sourceBar
     * @return
     */
    public List<AbstractTemporalEditorBar> getCombinableBars(Parm parm,
            AbstractTemporalEditorBar sourceBar) {
        List<AbstractTemporalEditorBar> barList = new ArrayList<AbstractTemporalEditorBar>();
        String unit = parm.getGridInfo().getUnitString();

        if (unitToGridBar.containsKey(unit)) {
            barList.add(unitToGridBar.get(unit));
        }

        if (unitToMovedGridBarList.containsKey(unit)) {
            barList.addAll(unitToMovedGridBarList.get(unit));
        }

        barList.remove(sourceBar);
        return barList;
    }

    public StatisticsMode getMode() {
        return statMode;
    }

    public int getModeratedMin() {
        return moderatedMin;
    }

    public int getModeratedMax() {
        return moderatedMax;
    }

    public float getStdDevMin() {
        return stdDevMin;
    }

    public float getStdDevMax() {
        return stdDevMax;
    }

    public int getHorizOffset() {
        // SCALE_WIDTH + 2, +2 accounts for boundary
        return AbstractTemporalEditorBar.SCALE_WIDTH + 2;
    }

    /**
     * 
     */
    private void handleTEMode() {
        DataManager dm = gridManager.getDataManager();
        ISpatialDisplayManager sdm = dm.getSpatialDisplayManager();
        IParmManager ipm = dm.getParmManager();
        sdm.removeGridVisibilityChangedListener(visibilityListener);
        sdm.removeActivatedParmChangedListener(activeParmListener);

        // default to all parm currently in te
        final List<Parm> parmsToAdd = new ArrayList<Parm>();

        switch (teMode) {
        case ACTIVE:
            sdm.addActivatedParmChangedListener(activeParmListener);
            Parm parm = sdm.getActivatedParm();

            if (parm != null) {
                parmsToAdd.add(parm);
            }
            break;
        case ALL:
            for (Parm parmToAdd : ipm.getDisplayedParms()) {
                parmsToAdd.add(parmToAdd);
            }
            break;
        case ALL_NOISC:
            // add any non ISC parms not already displayed
            for (Parm parmAdd : ipm.getDisplayedParms()) {
                if (!parmAdd.getParmID().getDbId().getModelName().equals("ISC")) {
                    parmsToAdd.add(parmAdd);
                }
            }
            break;
        case MUTABLE:
            // add any mutable parms not already displayed
            for (Parm parmAdd : ipm.getDisplayedParms()) {
                if (parmAdd.isMutable()) {
                    parmsToAdd.add(parmAdd);
                }
            }
            break;
        case VISIBLE:
            sdm.addGridVisibilityChangedListener(visibilityListener);
            for (Parm parmToAdd : sdm.getCurrentlyEnabledParms()) {
                parmsToAdd.add(parmToAdd);
            }
            break;
        }

        final ArrayList<Parm> parmsToRemove = new ArrayList<Parm>(
                parmToTEBar.keySet());
        parmsToRemove.removeAll(parmsToAdd);

        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                for (Parm parmToRemove : parmsToRemove) {
                    removeParm(parmToRemove);
                }

                for (Parm parmToAdd : parmsToAdd) {
                    addParm(parmToAdd);
                }
            }
        });
    }

    public void activate() {
        deactivateTime = 0;
    }

    public void deactivate() {
        deactivateTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"))
                .getTimeInMillis();
    }

    /**
     * 
     * @param activate
     * @return
     */
    public boolean checkDeactive() {
        if (deactivateTime > 0) {
            long currentTime = Calendar
                    .getInstance(TimeZone.getTimeZone("GMT")).getTimeInMillis();

            if ((currentTime - deactivateTime) > 60000) {
                return true;
            }
        }

        return false;
    }

    private class ActivatedParmChangedListener implements
            IActivatedParmChangedListener {
        @Override
        public void activatedParmChanged(Parm newParm) {
            ArrayList<Parm> parmsToDelete = new ArrayList<Parm>();
            // make separate list from keyset to prevent concurrent
            // modification
            for (Parm parmToProcess : parmToTEBar.keySet()) {
                if (!parmToProcess.equals(newParm)) {
                    parmsToDelete.add(parmToProcess);
                }
            }

            for (Parm parmToDelete : parmsToDelete) {
                removeParm(parmToDelete);
            }

            addParm(newParm);
        }
    }

    private class GridVisibilityChangedListener implements
            IGridVisibilityChangedListener {
        /**
         * Callback that grid visibility has changed.
         * 
         * @param parm
         *            parm whose visibility has changed
         * @param visible
         *            true if parm is visible
         * @param makeOnlyVisible
         *            true if only this parm is visible
         */
        public void gridVisibilityChanged(Parm parm, boolean visible,
                boolean makeOnlyVisible) {
            if (makeOnlyVisible) {
                ArrayList<Parm> parmsToDelete = new ArrayList<Parm>();
                // make separate list from keyset to prevent concurrent
                // modification
                for (Parm parmToProcess : parmToTEBar.keySet()) {
                    if (!parmToProcess.equals(parm)) {
                        parmsToDelete.add(parmToProcess);
                    }
                }

                for (Parm parmToDelete : parmsToDelete) {
                    removeParm(parmToDelete);
                }
            }

            if (visible) {
                addParm(parm);
            } else {
                removeParm(parm);
            }
        }
    }

    private class DisplayedParmListChangedListener implements
            IDisplayedParmListChangedListener {
        @Override
        public void displayedParmListChanged(Parm[] parms, Parm[] deletions,
                Parm[] additions) {
            for (Parm parm : deletions) {
                removeParm(parm);
            }

            // additions handled by teMode
            switch (teMode) {
            case ALL:
                for (Parm parmAdd : additions) {
                    addParm(parmAdd);
                }
                break;
            case ALL_NOISC:
                for (Parm parmAdd : additions) {
                    if (!parmAdd.getParmID().getDbId().getModelName()
                            .equals("ISC")) {
                        addParm(parmAdd);
                    }
                }
                break;
            case MUTABLE:
                for (Parm parmAdd : additions) {
                    if (parmAdd.isMutable()) {
                        addParm(parmAdd);
                    }
                }
                break;
            default:
                // do nothing
            }

            Rectangle r = TemporalEditor.this.parent.getClientArea();
            TemporalEditor.this.parent.setMinSize(TemporalEditor.this
                    .computeSize(r.width, SWT.DEFAULT));
            redraw();
        }
    }

    private class PropertyChangeListener implements
            org.eclipse.jface.util.IPropertyChangeListener {
        @Override
        public void propertyChange(PropertyChangeEvent event) {
            String id = event.getProperty();
            if (id.equals(PreferenceConstants.GFE_TEMPORAL_EDITOR_STATISTICS_MODE)) {
                statMode = StatisticsMode
                        .valueOf(GFEPreference
                                .getPreference(PreferenceConstants.GFE_TEMPORAL_EDITOR_STATISTICS_MODE));
                redraw();
            } else if (id
                    .equals(PreferenceConstants.GFE_TEMPORAL_EDITOR_STATISTICS_MODE_MODERATED_MIN)) {
                moderatedMin = GFEPreference
                        .getIntPreference(PreferenceConstants.GFE_TEMPORAL_EDITOR_STATISTICS_MODE_MODERATED_MIN);
                redraw();
            } else if (id
                    .equals(PreferenceConstants.GFE_TEMPORAL_EDITOR_STATISTICS_MODE_MODERATED_MAX)) {
                moderatedMax = GFEPreference
                        .getIntPreference(PreferenceConstants.GFE_TEMPORAL_EDITOR_STATISTICS_MODE_MODERATED_MAX);
                redraw();
            } else if (id
                    .equals(PreferenceConstants.GFE_TEMPORAL_EDITOR_STATISTICS_MODE_SD_MIN)) {
                stdDevMin = (float) GFEPreference
                        .getDoublePreference(PreferenceConstants.GFE_TEMPORAL_EDITOR_STATISTICS_MODE_SD_MIN);
                redraw();
            } else if (id
                    .equals(PreferenceConstants.GFE_TEMPORAL_EDITOR_STATISTICS_MODE_SD_MAX)) {
                stdDevMax = (float) GFEPreference
                        .getDoublePreference(PreferenceConstants.GFE_TEMPORAL_EDITOR_STATISTICS_MODE_SD_MAX);
                redraw();
            }
        }
    }

    public class ParmIDChangedListener implements IParmIDChangedListener {

        /*
         * (non-Javadoc)
         * 
         * @see
         * com.raytheon.viz.gfe.core.msgs.IParmIDChangedListener#parmIDChanged
         * (com.raytheon.viz.gfe.core.parm.Parm,
         * com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID)
         */
        @Override
        public void parmIDChanged(Parm parm, ParmID newParmID) {
            redraw();
        }

    }

    @Override
    public void receiveMessage(Message message) {
        if (message instanceof TEweModeChangedMsg) {
            TemporalEditorMode mode = ((TEweModeChangedMsg) message).getMode();
            if (!mode.equals(this.teMode)) {
                TemporalEditor.this.teMode = mode;
                handleTEMode();
            }
        }
    }
}
