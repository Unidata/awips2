package com.raytheon.viz.warnings.rsc;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;
import com.raytheon.uf.common.dataplugin.warning.PracticeWarningRecord;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.warnings.DateUtil;

/**
 * 
 * Top level watches, warnings, and advisory resource that contains the code
 * that is shared by all below resources
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 3, 2011            jsanchez     Initial creation
 * Aug 5, 2011            njensen       Refactored maps
 * Aug 22, 2011  10631   njensen  Major refactor
 * May 31, 2012 DR14992  mgamazaychikov Changed the order of strings in the
 * 										String array returned from getText method
 * Jun 04, 2012 DR14992  mgamazaychikov Reversed the previous changes
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public abstract class AbstractWWAResource extends
        AbstractVizResource<WWAResourceData, MapDescriptor> implements
        IResourceDataChanged {

    private static final DataTime[] dataTimes = AbstractVizResource.TIME_AGNOSTIC
            .toArray(new DataTime[0]);

    protected static final SimpleDateFormat DEFAULT_FORMAT = new SimpleDateFormat(
            "HHmm'Z'");

    protected static final SimpleDateFormat LONG_FORMAT = new SimpleDateFormat(
            "HH:mm'Z' EEE ddMMMyy");

    protected static final SimpleDateFormat DAY_FORMAT = new SimpleDateFormat(
            "HH:mm'Z' EEE");

    protected List<AbstractWarningRecord> recordsToLoad;

    public AbstractWWAResource(WWAResourceData data, LoadProperties props) {
        super(data, props);
        this.recordsToLoad = new ArrayList<AbstractWarningRecord>();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getDataTimes()
     */
    @Override
    public DataTime[] getDataTimes() {
        return dataTimes;
    }

    @Override
    public boolean isTimeAgnostic() {
        return true;
    }

    public synchronized void addRecord(PluginDataObject[] pdos)
            throws VizException {
        for (PluginDataObject pdo : pdos) {
            if (pdo instanceof AbstractWarningRecord) {
                AbstractWarningRecord record = (AbstractWarningRecord) pdo;
                String officeid = record.getOfficeid();
                if (!resourceData.getMetadataMap().containsKey("officeid")
                        || resourceData.getMetadataMap().get("officeid")
                                .getConstraintValue().contains(officeid)) {
                    this.recordsToLoad.add((AbstractWarningRecord) pdo);
                }
            }
        }
    }

    protected String[] getText(AbstractWarningRecord record, double mapWidth) {
        String vid = record.getPhensig();
        String phen = record.getPhen();
        DateUtil du = new DateUtil();
        String[] textToPrint = new String[] { "", "", "", "" };

        textToPrint[0] = record.getProductClass();
        if ((vid != null && phen != null)
                && (vid.equals("TO.A") || vid.equals("SV.A")
                        || phen.equals("FL") || phen.equals("FA"))) {
            textToPrint[0] += "." + vid;
        }
        textToPrint[0] += "." + record.getEtn();

        textToPrint[1] = record.getPil();

        SimpleDateFormat startFormat = DEFAULT_FORMAT;
        SimpleDateFormat endFormat = DEFAULT_FORMAT;
        if (mapWidth == 0) {
            startFormat = LONG_FORMAT;
            endFormat = DAY_FORMAT;
        } else if (mapWidth <= 200) {
            startFormat = DAY_FORMAT;
            endFormat = DAY_FORMAT;
        }

        synchronized (startFormat) {
            textToPrint[2] = "Valid "
                    + du.format(record.getStartTime().getTime(), startFormat);
        }
        synchronized (endFormat) {
            textToPrint[3] = "Thru "
                    + du.format(record.getEndTime().getTime(), endFormat);
        }

        return textToPrint;
    }

    protected PluginDataObject[] getWarningRecordArray() {
        CAVEMode caveMode = CAVEMode.getMode();
        boolean isOperational = (CAVEMode.OPERATIONAL.equals(caveMode)
                || CAVEMode.TEST.equals(caveMode) ? true : false);
        if (isOperational) {
            return resourceData.records
                    .toArray(new AbstractWarningRecord[resourceData.records
                            .size()]);
        } else {
            return resourceData.records
                    .toArray(new PracticeWarningRecord[resourceData.records
                            .size()]);
        }
    }

}
