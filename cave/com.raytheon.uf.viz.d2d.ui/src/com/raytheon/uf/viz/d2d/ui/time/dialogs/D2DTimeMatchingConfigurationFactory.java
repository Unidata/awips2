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
package com.raytheon.uf.viz.d2d.ui.time.dialogs;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.DataTime.FLAG;
import com.raytheon.uf.viz.core.comm.PerspectiveSpecificLoadProperties;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.d2d.core.D2DLoadProperties;
import com.raytheon.uf.viz.d2d.core.time.AbstractTimeMatchingConfigurationFactory;
import com.raytheon.uf.viz.d2d.core.time.D2DTimeMatcher;
import com.raytheon.uf.viz.d2d.core.time.LoadMode;
import com.raytheon.uf.viz.d2d.core.time.TimeMatchingConfiguration;
import com.raytheon.uf.viz.d2d.ui.actions.TimeOptionsAction;

/**
 * Manages time matching interactive configurations.
 * 
 * 
 * <pre>
 * 
 * Dialogs:
 *      Select Forecast
 *      Select Forecast and Inventory Times
 *      Select Forecast, Offset And Tolerance
 *      Select Offset and Tolerance
 *      Select Valid Time and Time Resolution
 *      
 * List Boxes in the Dialogs:
 *      Forecast Time
 *      Offset
 *      Tolerance
 *      Inventory Time
 *      Valid Time
 *      Time Resolution
 * 
 * </pre>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 5, 2009            bgonzale     Initial creation
 * Oct 5, 2015  4934      bsteffen     Do not re-use datatimes for overlays in
 *                                     VALID_TIME_SEQ
 * Mar 3, 2016  5436      bsteffen     Do not throw illegal state exception
 *                                     when in an unexpected state.
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

public class D2DTimeMatchingConfigurationFactory extends
        AbstractTimeMatchingConfigurationFactory {

    private TimeOptionsAction timeOptionsAction;

    /*
     * When loading multiple resources all resources will be clones of
     * currentConfig until resetMultiload is called by the time matcher.
     */
    protected TimeMatchingConfiguration currentConfig = null;

    /**
     * Create the time matching configuration.
     * 
     * <pre>
     * Derived from
     * OB9/D2D/tclFXA/load-mgr.tcl
     *      load_depict_keys { keys }
     *      load_get_needed_info { keys inventoryNeeded }
     * </pre>
     */
    @Override
    public TimeMatchingConfiguration getConfiguration(LoadProperties loadProps,
            D2DTimeMatcher matcher, DataTime[] availableTimes,
            IDescriptor descriptor) throws VizException {

        PerspectiveSpecificLoadProperties perspProps = loadProps
                .getPerspectiveProperty();
        D2DLoadProperties d2dProps = null;
        if (perspProps instanceof D2DLoadProperties) {
            d2dProps = (D2DLoadProperties) perspProps;
        } else {
            d2dProps = new D2DLoadProperties();
            loadProps.setPerspectiveProperty(d2dProps);
            if (currentConfig != null) {
                d2dProps.setTimeConfig(currentConfig.clone());
            }
        }

        TimeMatchingConfiguration config = d2dProps.getTimeConfig();

        if (config.isCancel()) {
            return null;
        }
        if (config.getLoadMode() == null) {
            config.setLoadMode(matcher.getLoadMode());
        }

        LoadMode nextLoadMode = config.getLoadMode();

        switch (config.getLoadMode()) {
        case INVENTORY:
            if (config.getDataTimes() == null) {
                config = selectForecastAndInventoryTimes(loadProps, matcher,
                        availableTimes, descriptor);
            } else {
                List<DataTime> bestTimes = new ArrayList<DataTime>(
                        config.getDataTimes().length);
                for (DataTime targetTime : config.getDataTimes()) {
                    DataTime bestTime = null;
                    for (DataTime availableTime : availableTimes) {
                        if (availableTime.equals(targetTime)) {
                            bestTime = availableTime;
                            break;
                        }
                        if (availableTime.getMatchValid() == targetTime
                                .getMatchValid()) {
                            if (bestTime == null) {
                                bestTime = availableTime;
                            } else {
                                if (availableTime.getMatchFcst() == targetTime
                                        .getMatchFcst()) {
                                    if (bestTime.getMatchFcst() != targetTime
                                            .getMatchFcst()) {
                                        bestTime = availableTime;
                                    }
                                }
                            }
                        }
                    }
                    if (bestTime != null) {
                        bestTimes.add(bestTime);
                    }
                }
                config.setDataTimes(bestTimes.toArray(new DataTime[bestTimes
                        .size()]));
            }
            break;
        case DPROG_DT:
        case PROG_LOOP:
            if (hasForecasts(availableTimes) && config.getForecast() == null) {
                config = selectForecastAndResolution(loadProps, matcher,
                        availableTimes, descriptor);
            }
            if (config != null) {
                nextLoadMode = LoadMode.FCST_TIME_MATCH;
            }
            break;
        case SLOT:
            if (config.getForecast() == null && hasForecasts(availableTimes)) {
                config = selectForecastAndResolution(loadProps, matcher,
                        availableTimes, descriptor);
            }
            break;
        case ANALYSIS_LOOP:
        case LATEST:
        case NO_BACKFILL:
        case PREVIOUS_MODEL_RUN:
        case PREVIOUS_VALID_TIME_SEQ:
        case RANGE_MATCH:
        case STD:
        case VALID_TIME_SEQ:
            if (timeOptionsAction != null && matcher.isTimeOptionsSelected()) {
                config = selectValidTimeAndTimeResolution(loadProps, matcher,
                        availableTimes, descriptor);
            }
            break;
        case FORCED:
        case FCST_TIME_MATCH:
            // You chould not be here
        default:
            // No Dialogs
            break;
        }
        if (config != null && !config.isCancel()) {
            toggleTimeOptionsCheckbox(matcher);
        }

        d2dProps.setTimeConfig(config);

        currentConfig = config.clone();
        currentConfig.setLoadMode(nextLoadMode);
        if (nextLoadMode == LoadMode.VALID_TIME_SEQ) {
            /*
             * delta for the first resource means space between frames, for
             * overlays it means an offset from the time match basis, we don't
             * want to be offset.
             */
            currentConfig.setDelta(null);
            /*
             * Overlay resources should just time match against the basis, not necessarily use the exact same times.
             */
            currentConfig.setDataTimes(null);
        }
        if (nextLoadMode != config.getLoadMode()) {
            changeLoadMode(nextLoadMode);
        }
        return config;
    }

    /**
     * Create the time matching configuration for an overlay (already have a
     * time matching basis.)
     * 
     * <pre>
     * Derived from
     * OB9/D2D/tclFXA/load-mgr.tcl
     *      load_depict_keys { keys }
     *      load_get_needed_info { keys inventoryNeeded }
     * </pre>
     */
    @Override
    public TimeMatchingConfiguration getOverlayConfiguration(
            LoadProperties loadProps, D2DTimeMatcher matcher,
            DataTime[] availableTimes, IDescriptor descriptor)
            throws VizException {
        PerspectiveSpecificLoadProperties perspProps = loadProps
                .getPerspectiveProperty();
        D2DLoadProperties d2dProps = null;
        if (perspProps instanceof D2DLoadProperties) {
            d2dProps = (D2DLoadProperties) perspProps;
        } else {
            d2dProps = new D2DLoadProperties();
            loadProps.setPerspectiveProperty(d2dProps);
            if (currentConfig != null) {
                d2dProps.setTimeConfig(currentConfig.clone());
            }
        }

        TimeMatchingConfiguration config = d2dProps.getTimeConfig();

        if (config.isCancel()) {
            return null;
        }
        if (config.getLoadMode() == null) {
            config.setLoadMode(matcher.getLoadMode());
        }

        LoadMode nextLoadMode = config.getLoadMode();

        switch (config.getLoadMode()) {
        case INVENTORY:
            // Open Dialog when choosing products in VB
            if (config.getDataTimes() == null
                    || descriptor.getFramesInfo().getFrameCount() == 0) {
                config = selectForecastAndInventoryTimes(loadProps, matcher,
                        availableTimes, descriptor);

            } else {
                DataTime[] configTimes = config.getDataTimes();
                List<DataTime> bestTimes = new ArrayList<DataTime>(
                        configTimes.length);
                for (DataTime targetTime : configTimes) {
                    DataTime bestTime = null;
                    long validDist = 0;
                    if (!targetTime.getUtilityFlags().contains(FLAG.FCST_USED)) {
                        /*
                         * for non forecast products(Satellite) accept the
                         * closest record within two minutes.
                         */
                        validDist = 120000;
                    }
                    for (DataTime availableTime : availableTimes) {
                        if (availableTime.equals(targetTime)) {
                            bestTime = availableTime;
                            break;
                        }
                        long dist = Math.abs(availableTime.getMatchValid()
                                - targetTime.getMatchValid());
                        if (dist <= validDist) {
                            if (bestTime == null) {
                                validDist = dist;
                                bestTime = availableTime;
                            } else {
                                if (availableTime.getMatchFcst() == targetTime
                                        .getMatchFcst()) {
                                    if (bestTime.getMatchFcst() != targetTime
                                            .getMatchFcst()) {
                                        bestTime = availableTime;
                                    }
                                }
                            }
                        }
                    }
                    if (bestTime != null) {
                        bestTimes.add(bestTime);
                    }
                }
                config.setDataTimes(bestTimes.toArray(new DataTime[bestTimes
                        .size()]));
            }
            break;

        case SLOT:
        case FORCED:
            if (config.getForecast() == null && hasForecasts(availableTimes)) {
                config = selectForecastAndResolution(loadProps, matcher,
                        availableTimes, descriptor);
            }
            break;
        case PROG_LOOP:
            config = selectForecastOffsetAndTolerance(loadProps, matcher,
                    availableTimes, descriptor);
            break;
        case ANALYSIS_LOOP:
        case FCST_TIME_MATCH:
        case LATEST:
        case NO_BACKFILL:
        case PREVIOUS_MODEL_RUN:
        case PREVIOUS_VALID_TIME_SEQ:
        case RANGE_MATCH:
        case STD:
        case VALID_TIME_SEQ:
            if (timeOptionsAction != null && matcher.isTimeOptionsSelected()) {
                config = selectOffsetAndTolerance(loadProps, matcher,
                        availableTimes, descriptor);
            }
            break;
        case DPROG_DT:
            // How did you get Here
            nextLoadMode = LoadMode.FCST_TIME_MATCH;
        default:
            break;
        }

        if (config != null && !config.isCancel()) {
            toggleTimeOptionsCheckbox(matcher);
        }
        d2dProps.setTimeConfig(config);

        currentConfig = config.clone();
        currentConfig.setLoadMode(nextLoadMode);
        if (nextLoadMode != config.getLoadMode()) {
            changeLoadMode(nextLoadMode);
        }
        return config;
    }

    private boolean hasForecasts(DataTime[] availableTimes) {
        boolean haveForecasts = false;
        for (DataTime dataTime : availableTimes) {
            haveForecasts = (dataTime.getUtilityFlags()
                    .contains(FLAG.FCST_USED));
            if (haveForecasts) {
                break;
            }
        }
        return haveForecasts;
    }

    private void toggleTimeOptionsCheckbox(D2DTimeMatcher timeMatcher) {
        if (timeOptionsAction != null) {
            // update the toggle that activated this action
            Display display = PlatformUI.getWorkbench().getDisplay();
            display.syncExec(new Runnable() {
                @Override
                public void run() {
                    timeOptionsAction.setEnabled(false);
                }
            });
            timeOptionsAction = null;
            timeMatcher.setTimeOptionsSelected(false);
        }
    }

    private TimeMatchingConfiguration selectValidTimeAndTimeResolution(
            LoadProperties loadProps, D2DTimeMatcher timeMatcher,
            DataTime[] availableTimes, IDescriptor descriptor)
            throws VizException {
        return TimeMatchingDialogOpener.runDialog(
                ValidTimeAndTimeResolutionDialog.class, loadProps, timeMatcher,
                availableTimes, descriptor);
    }

    private TimeMatchingConfiguration selectOffsetAndTolerance(
            final LoadProperties loadProps, D2DTimeMatcher timeMatcher,
            final DataTime[] availableTimes, final IDescriptor descriptor)
            throws VizException {
        return TimeMatchingDialogOpener.runDialog(
                "Select Offset And Tolerance", OffsetAndToleranceDialog.class,
                loadProps, timeMatcher, availableTimes, descriptor);
    }

    private TimeMatchingConfiguration selectForecastAndInventoryTimes(
            LoadProperties loadProps, D2DTimeMatcher timeMatcher,
            DataTime[] availableTimes, IDescriptor descriptor)
            throws VizException {
        return TimeMatchingDialogOpener.runDialog(
                ForecastAndInventoryTimeDialog.class, loadProps, timeMatcher,
                availableTimes, descriptor);
    }

    private TimeMatchingConfiguration selectForecastOffsetAndTolerance(
            LoadProperties loadProps, D2DTimeMatcher timeMatcher,
            DataTime[] availableTimes, IDescriptor descriptor)
            throws VizException {
        return TimeMatchingDialogOpener.runDialog(
                "Select Forecast, Offset And Tolerance",
                OffsetAndToleranceDialog.class, loadProps, timeMatcher,
                availableTimes, descriptor);
    }

    private TimeMatchingConfiguration selectForecastAndResolution(
            LoadProperties loadProps, D2DTimeMatcher timeMatcher,
            DataTime[] availableTimes, IDescriptor descriptor)
            throws VizException {
        return TimeMatchingDialogOpener.runDialog(
                ForecastAndResolutionDialog.class, loadProps, timeMatcher,
                availableTimes, descriptor);
    }

    /**
     * @param timeOptionsAction
     *            the timeOptionsAction to set
     */
    public void setTimeOptionsAction(TimeOptionsAction timeOptionsAction) {
        this.timeOptionsAction = timeOptionsAction;
    }

    public void changeLoadMode(LoadMode mode) throws VizException {
        LoadModeSwitcher switcher = new LoadModeSwitcher();
        switcher.switchLoadMode(mode);
    }

    @Override
    public void resetMultiload() {
        this.currentConfig = null;
    }

}
