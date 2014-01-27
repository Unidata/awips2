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

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.geotools.geometry.jts.ReferencedEnvelope;

import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.DataLevelType;
import com.raytheon.uf.common.datadelivery.registry.DataType;
import com.raytheon.uf.common.datadelivery.registry.Parameter;
import com.raytheon.uf.common.datadelivery.registry.PointDataSet;
import com.raytheon.uf.common.datadelivery.registry.PointTime;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.datadelivery.retrieval.util.PointDataSizeUtils;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.SizeUtil;
import com.raytheon.uf.viz.datadelivery.common.xml.AreaXML;
import com.raytheon.uf.viz.datadelivery.subscription.subset.xml.PointTimeXML;
import com.raytheon.uf.viz.datadelivery.subscription.subset.xml.SubsetXML;
import com.raytheon.uf.viz.datadelivery.subscription.subset.xml.TimeXML;

/**
 * {@link SubsetManagerDlg} for point data sets.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 04, 2013    223     mpduff      Initial creation.
 * Jun 11, 2013   2064     mpduff      Fix editing of subscriptions.
 * Jun 14, 2013   2108     mpduff      Refactored DataSizeUtils and 
 *                                     implement subset size.
 * Sep 05, 2013   2335     mpduff      Fix times for adhoc point queries.
 * Sep 10, 2013   2351     dhladky     Finished adhoc queries
 * Sep 16, 2013   2383     bgonzale    Start time precedes end time.
 * Oct 10, 2013   1797     bgonzale    Refactored registry Time objects.
 * Oct 11, 2013   2386     mpduff      Refactor DD Front end.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class PointSubsetManagerDlg extends SubsetManagerDlg {
    /** Status Handler */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(PointSubsetManagerDlg.class);

    private final String TIMING_TAB_TEXT = "Retrieval Interval";

    /** Point data size utility */
    private PointDataSizeUtils dataSize;

    /** The point subset tab */
    private PointTimeSubsetTab timingTabControls;

    /**
     * Constructor.
     * 
     * @param shell
     *            parent shell
     * @param loadDataSet
     *            load data set flag
     * @param subscription
     *            Subscription object
     */
    public PointSubsetManagerDlg(Shell shell, boolean loadDataSet,
            Subscription subscription) {
        super(shell, loadDataSet, subscription);
        setTitle();
    }

    /**
     * Constructor.
     * 
     * @param shell
     *            parent shell
     * @param dataSet
     *            the data set
     * @param loadDataSet
     *            load data set flag
     * @param subsetXml
     *            the subset xml object
     */
    public PointSubsetManagerDlg(Shell shell, PointDataSet dataSet,
            boolean loadDataSet, SubsetXML subsetXml) {
        super(shell, loadDataSet, dataSet);
        this.dataSet = dataSet;
        this.subsetXml = subsetXml;
        setTitle();
    }

    /**
     * Constructor.
     * 
     * @param shell
     *            the parent shell
     * @param dataSet
     *            the data set
     */
    public PointSubsetManagerDlg(Shell shell, PointDataSet dataSet) {
        super(shell, dataSet);
        this.dataSet = dataSet;
        setTitle();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    void createTabs(TabFolder tabFolder) {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(1, false);

        TabItem timingTab = new TabItem(tabFolder, SWT.NONE);
        timingTab.setText(TIMING_TAB_TEXT);
        timingTab.setData("valid", false);
        Composite timingComp = new Composite(tabFolder, SWT.NONE);
        timingComp.setLayout(gl);
        timingComp.setLayoutData(gd);
        timingTab.setControl(timingComp);
        timingTabControls = new PointTimeSubsetTab(timingComp, this, shell);

        TabItem spatialTab = new TabItem(tabFolder, SWT.NONE);
        spatialTab.setText(SPATIAL_TAB);
        Composite spatialComp = new Composite(tabFolder, SWT.NONE);
        spatialComp.setLayout(gl);
        spatialComp.setLayoutData(gd);
        spatialTab.setControl(spatialComp);
        spatialTabControls = new SpatialSubsetTab(spatialComp, dataSet, this);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void updateDataSize() {
        if (!initialized) {
            return;
        }

        if (dataSize == null) {
            this.dataSize = new PointDataSizeUtils((PointDataSet) dataSet);
        }

        ReferencedEnvelope env = spatialTabControls.getEnvelope();
        int interval = timingTabControls.getDataRetrievalInterval();

        // Update the data set size label text.
        this.sizeLbl.setText(SizeUtil.prettyByteSize(dataSize
                .getDataSetSizeInBytes(env, interval)));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Time setupDataSpecificTime(Time newTime, Subscription sub) {
        PointTime newTimePoint = (PointTime) newTime;

        // Format must be set before setting the dates.
        newTimePoint.setFormat(dataSet.getTime().getFormat());
        int interval = timingTabControls.getSaveInfo()
                .getDataRetrievalInterval();
        Calendar cal = TimeUtil.newGmtCalendar();
        newTimePoint.setInterval(interval);

        newTimePoint.setEnd(cal.getTime());
        cal.add(Calendar.MINUTE, interval * -1);
        newTimePoint.setStart(cal.getTime());

        sub.setLatencyInMinutes(interval);
        return newTimePoint;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected PointTimeXML getTimeXmlFromSubscription() {
        // TODO Auto-generated method stub
        return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Subscription populateSubscription(Subscription sub, boolean create) {
        sub.setProvider(dataSet.getProviderName());
        sub.setDataSetName(dataSet.getDataSetName());
        sub.setDataSetType(dataSet.getDataSetType());
        sub.setDataSetName(dataSet.getDataSetName());

        Time newTime = new PointTime();
        newTime = setupDataSpecificTime(newTime, sub);
        sub.setTime(newTime);

        Coverage cov = new Coverage();
        cov.setEnvelope(dataSet.getCoverage().getEnvelope());
        setCoverage(sub, cov);

        List<Parameter> paramList = new ArrayList<Parameter>();
        Map<String, Parameter> paramMap = dataSet.getParameters();
        List<DataLevelType> levelTypeList = new ArrayList<DataLevelType>();
        levelTypeList.add(new DataLevelType(DataLevelType.LevelType.SFC));
        for (String key : paramMap.keySet()) {
            Parameter p = paramMap.get(key);
            p.setDataType(DataType.POINT);
            p.setLevelType(levelTypeList);
            paramList.add(p);
        }

        sub.setParameter(paramList);

        if (dataSize == null) {
            this.dataSize = new PointDataSizeUtils((PointDataSet) dataSet);
        }

        sub.setDataSetSize(dataSize.getDataSetSizeInKb(sub));

        return sub;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void loadFromSubsetXML(SubsetXML subsetXml) {
        super.loadFromSubsetXML(subsetXml);

        PointTimeXML time = (PointTimeXML) subsetXml.getTime();
        this.timingTabControls.setDataRetrievalInterval(time
                .getDataRetrievalInterval());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.datadelivery.subscription.subset.SubsetManagerDlg
     * #loadFromSubscription
     * (com.raytheon.uf.common.datadelivery.registry.Subscription)
     */
    @Override
    protected void loadFromSubscription(Subscription subscription) {
        super.loadFromSubscription(subscription);

        PointTimeXML time = new PointTimeXML();
        time.setDataRetrievalInterval(((PointTime) subscription.getTime())
                .getInterval());

        this.timingTabControls.setDataRetrievalInterval(time
                .getDataRetrievalInterval());

        AreaXML area = new AreaXML();
        ReferencedEnvelope envelope = this.subscription.getCoverage()
                .getEnvelope();
        ReferencedEnvelope requestEnvelope = this.subscription.getCoverage()
                .getRequestEnvelope();

        if (requestEnvelope != null && !requestEnvelope.isEmpty()) {
            area.setEnvelope(requestEnvelope);
        } else {
            area.setEnvelope(envelope);
        }
        spatialTabControls.setDataSet(this.dataSet);
        spatialTabControls.populate(area);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected TimeXML getDataTimeInfo() {
        return timingTabControls.getSaveInfo();
    }
}
