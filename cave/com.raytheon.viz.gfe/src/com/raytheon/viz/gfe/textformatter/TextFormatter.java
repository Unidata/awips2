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
package com.raytheon.viz.gfe.textformatter;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import jep.JepException;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.status.IPerformanceStatusHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.PerformanceStatus;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.dialogs.formatterlauncher.ConfigData;
import com.raytheon.viz.gfe.dialogs.formatterlauncher.ConfigData.ProductStateEnum;
import com.raytheon.viz.gfe.sampler.SamplerGridSliceCache;
import com.raytheon.viz.gfe.tasks.AbstractGfeTask;

/**
 * Controls the python execution of text products and retrieving information
 * from the product definitions. Runs the python in a separate thread to keep
 * from locking up the UI.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 29, 2008            njensen     Initial creation
 * Oct 15, 2008            njensen     Split python running to
 *                                     non-UI thread
 * Dec 1, 2010    6130     ryu         Set proper state and invoke callback
 * May 29, 2014   2841     randerso    Handle failure to queue due to pending limit
 * Apr 20, 2015  4027      randerso    Renamed ProductStateEnum with an initial capital
 * Jul 28, 2015  4263      dgilling    Support changes to FormatterScriptFactory,
 *                                     get DataManager instance via constructor.
 * Aug 20, 2015  #4749     dgilling    Add cleanUp.
 * Aug 26, 2015  #4804     dgilling    Support ability to run TextFormatters 
 *                                     from SmartScript.
 * Dec 08, 2015  #5129     dgilling    Pass IFPClient to getVarDict.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class TextFormatter extends AbstractGfeTask {
    private static final int UI_THREAD_ID = 1;

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TextFormatter.class);

    private final IPerformanceStatusHandler perfLog = PerformanceStatus
            .getHandler("GFE:");

    private TextProductFinishListener listener;

    private Map<String, Object> argMap;

    private ConfigData.ProductStateEnum state;

    private DataManager dataMgr;

    /**
     * Constructor
     * 
     * @param productName
     *            Name of the python module to execute.
     * @param vtecMode
     *            Single character code for VTEC mode--Operational,
     *            eXperimental, Test, etc.
     * @param databaseID
     *            String form of the {@code DatabaseID} of the source database.
     * @param vtecActiveTable
     *            Name of the active table to use for hazard information.
     * @param drtTime
     *            DRT time to use in YYYYMMDD_HHmm format.
     * @param testMode
     *            Whether or not to execute in test VTEC mode.
     * @param finish
     *            Listener to send status updates to.
     * @param dataMgr
     *            the {@code DataManager} instance to use.
     */
    public TextFormatter(String productName, String vtecMode,
            String databaseID, String vtecActiveTable, String drtTime,
            int testMode, TextProductFinishListener finish, DataManager dataMgr) {
        this(productName, vtecMode, databaseID, null, vtecActiveTable, drtTime,
                testMode, finish, dataMgr);
    }

    /**
     * Constructor that allows the varDict to be pre-supplied. Useful for
     * executing formatters where no GUI popups are desired.
     * 
     * @param productName
     *            Name of the python module to execute.
     * @param vtecMode
     *            Single character code for VTEC mode--Operational,
     *            eXperimental, Test, etc.
     * @param databaseID
     *            String form of the {@code DatabaseID} of the source database.
     * @param varDict
     *            String form of the formatter's variable dictionary.
     * @param vtecActiveTable
     *            Name of the active table to use for hazard information.
     * @param drtTime
     *            DRT time to use in YYYYMMDD_HHmm format.
     * @param testMode
     *            Whether or not to execute in test VTEC mode.
     * @param finish
     *            Listener to send status updates to.
     * @param dataMgr
     *            the {@code DataManager} instance to use.
     */
    public TextFormatter(String productName, String vtecMode,
            String databaseID, String varDict, String vtecActiveTable,
            String drtTime, int testMode, TextProductFinishListener finish,
            DataManager dataMgr) {
        super(productName);

        String addr = null;
        try {
            addr = InetAddress.getLocalHost().getHostName();
        } catch (UnknownHostException e) {
            addr = UUID.randomUUID().toString();
        }

        this.dataMgr = dataMgr;

        argMap = new HashMap<String, Object>();
        argMap.put("testMode", testMode);
        argMap.put(ArgDictConstants.DATABASE_ID, databaseID);
        argMap.put(ArgDictConstants.SITE, this.dataMgr.getSiteID());
        argMap.put(ArgDictConstants.FORECAST_LIST, productName);
        argMap.put("username", System.getProperty("user.name") + ":" + addr);
        argMap.put("dataMgr", this.dataMgr);
        argMap.put(ArgDictConstants.VTEC_MODE, vtecMode);
        argMap.put(ArgDictConstants.VTEC_ACTIVE_TABLE, vtecActiveTable);
        argMap.put("drtTime", drtTime);
        if (!StringUtil.isEmptyString(varDict)) {
            argMap.put(ArgDictConstants.CMDLINE_VARDICT, varDict);
        }

        listener = finish;
        this.state = ConfigData.ProductStateEnum.Queued;
    }

    @Override
    public void doRun() {
        FormatterScript script = null;
        String forecast = null;
        try {
            state = ConfigData.ProductStateEnum.Running;
            listener.startProgressBar(state);

            argMap.put("logFile", getLogFile().getAbsolutePath());
            script = new FormatterScriptFactory().createPythonScript();

            String productName = (String) argMap
                    .get(ArgDictConstants.FORECAST_LIST);
            String issuedBy = dataMgr.getTextProductMgr().getIssuedBy();
            String dbId = (String) argMap.get(ArgDictConstants.DATABASE_ID);

            if (!argMap.containsKey(ArgDictConstants.CMDLINE_VARDICT)) {
                String varDict = getVarDict(productName, dataMgr, dbId,
                        issuedBy, script);
                argMap.put(ArgDictConstants.CMDLINE_VARDICT, varDict);
            }

            String varDict = (String) argMap
                    .get(ArgDictConstants.CMDLINE_VARDICT);
            if (varDict != null) {
                ITimer timer = TimeUtil.getTimer();
                timer.start();
                forecast = (String) script.execute(argMap);
                timer.stop();
                perfLog.logDuration("Text Formatter " + productName,
                        timer.getElapsedTime());
            } else {
                forecast = "Formatter canceled";
            }

            state = ConfigData.ProductStateEnum.Finished;
        } catch (Throwable t) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error generating text product", t);
            state = ConfigData.ProductStateEnum.Failed;
        } finally {
            SamplerGridSliceCache.remove(this.getId());
            SamplerGridSliceCache.remove(UI_THREAD_ID);
            productFinished(forecast);
            if (script != null) {
                script.dispose();
            }
        }
    }

    private void productFinished(final String text) {
        listener.stopProgressBar(state);
        listener.textProductFinished(text, state);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.tasks.AbstractGfeTask#cancel()
     */
    @Override
    public void cancel() {
        this.status = TaskStatus.CANCELED;

        // TODO: this is deprecated and probably not a good thing to do.
        // we really need to get formatters running in their own process
        // so we can kill them safely
        if (this.state.equals(ProductStateEnum.Queued)
                || this.state.equals(ProductStateEnum.New)) {
            state = ConfigData.ProductStateEnum.Failed;
            productFinished(null);

            this.finishedTime = SimulatedTime.getSystemTime().getTime();
            this.taskCanceled();
        } else {
            this.stop();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.tasks.AbstractGfeTask#getCommand()
     */
    @Override
    public String getCommand() {
        /*
         * runIFPText -t "ZFP_OAX" -h localhost -p 98000000 -d
         * OAX_GRID__Official_00000000_0000 -u "fxa" -S -V
         * "{('Product Issuance','productIssuance'): 'Morning', (Issued By, 'issuedBy'): None}"
         * -a active
         */
        StringBuilder sb = new StringBuilder();

        sb.append("runIFPText");
        sb.append(" -t \"").append(argMap.get(ArgDictConstants.FORECAST_LIST))
                .append('"');
        sb.append(" -d ").append(argMap.get(ArgDictConstants.DATABASE_ID));
        sb.append(" -u \"").append(argMap.get("username")).append('"');
        sb.append(" -V \"")
                .append(argMap.get(ArgDictConstants.CMDLINE_VARDICT))
                .append('"');
        sb.append(" -a ")
                .append(argMap.get(ArgDictConstants.VTEC_ACTIVE_TABLE));

        return sb.toString();
    }

    private String getVarDict(String productName, DataManager dataManager,
            String dbId, String issuedBy, FormatterScript script)
            throws JepException {
        Map<String, Object> map = new HashMap<String, Object>(5, 1f);
        map.put("paths", GfePyIncludeUtil.getTextProductsIncludePath());
        map.put("dspName",
                dataManager.getTextProductMgr().getDisplayName(productName));
        map.put("dataMgr", dataManager);
        map.put("ifpClient", dataManager.getClient().getPythonClient());
        map.put("issuedBy", issuedBy);
        map.put("dataSource", new DatabaseID(dbId).getModelName());

        String varDict = (String) script.execute("getVarDict", map);
        return varDict;
    }

    @Override
    public void cleanUp() {
        super.cleanUp();
        dataMgr = null;
        listener = null;
    }
}
