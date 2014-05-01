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
package com.raytheon.viz.radar.ui;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.preference.IPersistentPreferenceStore;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.localization.HierarchicalPreferenceStore;
import com.raytheon.viz.radar.Activator;
import com.raytheon.viz.radar.IRadarConfigListener;
import com.raytheon.viz.radar.rsc.image.RadarSRMResource.SRMSource;

/**
 * Singleton controls for the values needed by radar and SCAN
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class RadarDisplayManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarDisplayManager.class);

    public enum TrackTypes {
        NONE("no tracks"), PAST("past"), FORECAST("forecast"), PAST_AND_FORECAST(
                "past & fcst");

        private String s;

        TrackTypes(String s) {
            this.s = s;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Enum#toString()
         */
        @Override
        public String toString() {
            return this.s;
        }

        public static TrackTypes fromString(String s) {
            TrackTypes trackType = null;
            for (TrackTypes type : TrackTypes.values()) {
                if (type.toString().equals(s)) {
                    trackType = type;
                    break;
                }
            }
            return trackType;
        }

    };

    private Job saveJob = new Job("Saving Radar Config") {

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            try {
                Activator.getDefault().getPreferenceStore().save();
            } catch (IOException e) {
                statusHandler.handle(Priority.SIGNIFICANT,
                        "Unable to save data");
            }
            return Status.OK_STATUS;
        }

        @Override
        public boolean shouldSchedule() {
            return Activator.getDefault().getPreferenceStore().needsSaving();
        }

        @Override
        public boolean shouldRun() {
            return Activator.getDefault().getPreferenceStore().needsSaving();
        }

    };

    private ArrayList<IRadarConfigListener> configListeners;

    private RadarDisplayControls currentValues;

    private static RadarDisplayManager manager = null;

    private RadarDisplayManager() {
        configListeners = new ArrayList<IRadarConfigListener>();
        saveJob.setSystem(true);
        Activator.getDefault().getPreferenceStore()
                .addPropertyChangeListener(new IPropertyChangeListener() {

                    @Override
                    public void propertyChange(PropertyChangeEvent event) {
                        retrieveDefaultSettings();
                    }
                });
    }

    /**
     * Singleton pattern for _values creating a singleton holding values to pass
     * through jvm
     * 
     * @return
     */
    public static RadarDisplayManager getInstance() {
        if (manager == null) {
            manager = new RadarDisplayManager();
            manager.retrieveDefaultSettings();
        }
        return manager;
    }

    /**
     * @return The currentValues that contains the current settings for
     *         displaying radar products
     */
    public RadarDisplayControls getCurrentSettings() {
        return currentValues;
    }

    /**
     * @return the configListeners
     */
    public ArrayList<IRadarConfigListener> getConfigListeners() {
        return configListeners;
    }

    /**
     * @param configListeners
     *            the configListeners to set
     */
    public void setConfigListeners(
            ArrayList<IRadarConfigListener> configListeners) {
        this.configListeners = configListeners;
    }

    public void addListener(IRadarConfigListener newListener) {
        synchronized (configListeners) {
            configListeners.add(newListener);
        }
    }

    public void removeListener(IRadarConfigListener currListener) {
        synchronized (configListeners) {
            if (configListeners.contains(currListener)) {
                configListeners.remove(currListener);
            }
        }
    }

    public void displayConfigUpdated() {
        save();
        synchronized (configListeners) {
            List<IRadarConfigListener> listeners = new ArrayList<IRadarConfigListener>(
                    configListeners);
            for (IRadarConfigListener currListener : listeners) {
                currListener.updateConfig();
            }
        }
    }

    private void save() {
        if (currentValues == null) {
            return;
        }
        HierarchicalPreferenceStore prefs = Activator.getDefault()
                .getPreferenceStore();

        prefs.setValue("STI_NumStorms", currentValues.getStiNumStorms());
        prefs.setValue("STI_TrackType", currentValues.getStiTrackType().name());
        prefs.setValue("HI_POHLow", currentValues.getHiPOHLow());
        prefs.setValue("HI_POHHigh", currentValues.getHiPOHHigh());
        prefs.setValue("HI_POSHLow", currentValues.getHiPOSHLow());
        prefs.setValue("HI_POSHHigh", currentValues.getHiPOSHHigh());
        prefs.setValue("TVS_ShowElevated", currentValues.isTvsShowElevated());
        prefs.setValue("DMD_ShowExtrapolated",
                currentValues.isDmdMdTvsShowExtrapolated());
        prefs.setValue("DMD_MinFeatureStrength",
                currentValues.getDmdMinFeatureStrength());
        prefs.setValue("DMD_ShowOverlapping",
                currentValues.isDmdShowOverlapping());
        prefs.setValue("DMD_TrackType", currentValues.getDmdTrackType().name());
        prefs.setValue("SRM_Source", currentValues.getSrmSource().name());
        prefs.setValue("SRM_Direction", currentValues.getSrmDir());
        prefs.setValue("SRM_Speed", currentValues.getSrmSpeed());
        // Put the IO on a different thread to avoid hanging.
        saveJob.schedule();
    }

    /**
     * Reads the stored settings from the config.xml and uses these values to
     * populate the currentValues
     */
    private void retrieveDefaultSettings() {

        IPersistentPreferenceStore prefs = Activator.getDefault()
                .getPreferenceStore();

        RadarDisplayControls currentVals = currentValues;
        if (currentVals == null) {
            // The first time we retrieve, do not save again by leaving
            // currentValues null
            currentVals = new RadarDisplayControls();
        }
        // Load the current values from the preferences store
        currentVals.setStiNumStorms(prefs.getInt("STI_NumStorms"));
        currentVals.setStiTrackType(TrackTypes.valueOf(prefs
                .getString("STI_TrackType")));
        currentVals.setHiPOHLow(prefs.getInt("HI_POHLow"));
        currentVals.setHiPOHHigh(prefs.getInt("HI_POHHigh"));
        currentVals.setHiPOSHLow(prefs.getInt("HI_POSHLow"));
        currentVals.setHiPOSHHigh(prefs.getInt("HI_POSHHigh"));
        currentVals.setTvsShowElevated(prefs.getBoolean("TVS_ShowElevated"));
        currentVals.setDmdMdTvsShowExtrapolated(prefs
                .getBoolean("DMD_ShowExtrapolated"));
        currentVals.setDmdMinFeatureStrength(prefs
                .getInt("DMD_MinFeatureStrength"));
        currentVals.setDmdShowOverlapping(prefs
                .getBoolean("DMD_ShowOverlapping"));
        currentVals.setDmdTrackType(TrackTypes.valueOf(prefs
                .getString("DMD_TrackType")));
        currentVals.setSrmSource(SRMSource.valueOf(prefs
                .getString("SRM_Source")));
        currentVals.setSrmDir(prefs.getInt("SRM_Direction"));
        currentVals.setSrmSpeed(prefs.getInt("SRM_Speed"));
        currentValues = currentVals;
    }

}
