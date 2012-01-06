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
import java.util.Date;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.gfe.core.IReferenceSetManager;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.msgs.IGridDataChangedListener;
import com.raytheon.viz.gfe.core.msgs.IReferenceSetChangedListener;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.sampler.HistSample;
import com.vividsolutions.jts.geom.Envelope;

/**
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 30, 2009 2159       rjpeter      Initial creation
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class TimeSeries {
    private ReferenceSetChangedListener refSetListener = new ReferenceSetChangedListener();

    private GridDataChangedListener gridDataListener = new GridDataChangedListener();

    private SortedMap<TimeRange, HistSample> timeRangeToSampleMapping = new TreeMap<TimeRange, HistSample>();

    private List<ITimeSeriesChangedListener> listeners = new ArrayList<ITimeSeriesChangedListener>();

    private Parm parm;

    private Grid2DBit sampleArea;

    private float resolution = 1;

    public TimeSeries(Parm parm) {
        this.parm = parm;
        this.parm.getListeners().addGridChangedListener(gridDataListener);
        IReferenceSetManager refSetMgr = parm.getDataManager().getRefManager();
        refSetMgr.addReferenceSetChangedListener(refSetListener);
        sampleArea = refSetMgr.getActiveRefSet().getGrid();
        generateSamples();
    }

    public Parm getParm() {
        return parm;
    }

    public void checkRemoveListeners() {
        if (listeners.size() < 1) {
            IReferenceSetManager refSetMgr = parm.getDataManager()
                    .getRefManager();
            refSetMgr.removeReferenceSetChangedListener(refSetListener);
            parm.getListeners().removeGridChangedListener(gridDataListener);
        }
    }

    public void generateSamples() {
        timeRangeToSampleMapping.clear();

        if (sampleArea != null && sampleArea.isAnyBitsSet()) {
            IGridData[] gridDataArray = parm.getGridInventory();

            for (IGridData gridData : gridDataArray) {
                gridData.populate();
                if (gridData.isValid()) {
                    IGridSlice gridSlice = gridData.getGridSlice();
                    TimeRange validTime = gridSlice.getValidTime();
                    HistSample sample = new HistSample(gridSlice, sampleArea,
                            false, false);
                    timeRangeToSampleMapping.put(validTime, sample);
                    resolution = sample.getResolution();
                }
            }
        }

        fireChangeListeners();
    }

    public void generateSamples(TimeRange tr) {
        // clear sample for the given range
        ArrayList<TimeRange> samplesToRemove = new ArrayList<TimeRange>();
        for (TimeRange trSamples : timeRangeToSampleMapping.keySet()) {
            if (trSamples.overlaps(tr)) {
                samplesToRemove.add(trSamples);
            }
        }

        for (TimeRange trToRemove : samplesToRemove) {
            timeRangeToSampleMapping.remove(trToRemove);
        }

        if (sampleArea != null && sampleArea.isAnyBitsSet()) {
            IGridData[] gridDataArray = parm.getGridInventory(tr);

            for (IGridData gridData : gridDataArray) {
                gridData.populate();
                if (gridData.isValid()) {
                    IGridSlice gridSlice = gridData.getGridSlice();
                    TimeRange validTime = gridSlice.getValidTime();
                    HistSample sample = new HistSample(gridSlice, sampleArea,
                            false, false);
                    timeRangeToSampleMapping.put(validTime, sample);
                    resolution = sample.getResolution();
                }
            }
        }

        fireChangeListeners();
    }

    public HistSample getSampleForDate(Date date) {
        for (TimeRange timeRange : timeRangeToSampleMapping.keySet()) {
            if (timeRange.contains(date)) {
                return timeRangeToSampleMapping.get(timeRange);
            }
        }

        return null;
    }

    /**
     * Samples returned sorted by time range
     * 
     * @param tr
     * @return
     */
    public List<HistSample> getSamplesForTimeRange(TimeRange tr) {
        List<HistSample> samples = new ArrayList<HistSample>();
        for (TimeRange timeRange : timeRangeToSampleMapping.keySet()) {
            if (timeRange.overlaps(tr)) {
                samples.add(timeRangeToSampleMapping.get(timeRange));
            }
        }

        return samples;
    }

    public float getResolution() {
        return resolution;
    }

    public void addTimeSeriesChangeListener(ITimeSeriesChangedListener listener) {
        if (listener != null) {
            listeners.add(listener);
        }
    }

    public void removeTimeSeriesChangeListener(
            ITimeSeriesChangedListener listener) {
        if (listener != null) {
            listeners.remove(listener);
        }
    }

    private void fireChangeListeners() {
        for (ITimeSeriesChangedListener listener : listeners) {
            listener.timeSeriesChanged(this);
        }
    }

    private class ReferenceSetChangedListener implements
            IReferenceSetChangedListener {
        @Override
        public void referenceSetChanged(ReferenceData refSet,
                ArrayList<Envelope> domains) {
            sampleArea = refSet.getGrid();

            VizApp.runAsync(new Runnable() {
                @Override
                public void run() {
                    generateSamples();
                }
            });
        }
    }

    private class GridDataChangedListener implements IGridDataChangedListener {
        @Override
        public void gridDataChanged(ParmID parm, final TimeRange timeRange) {
            VizApp.runAsync(new Runnable() {
                @Override
                public void run() {
                    generateSamples(timeRange);
                }
            });
        }
    }
}
