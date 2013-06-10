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
import java.util.UUID;

import com.raytheon.uf.common.status.IPerformanceStatusHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.PerformanceStatus;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.dialogs.formatterlauncher.ConfigData;
import com.raytheon.viz.gfe.dialogs.formatterlauncher.ConfigData.productStateEnum;
import com.raytheon.viz.gfe.sampler.SamplerGridSliceCache;
import com.raytheon.viz.gfe.tasks.AbstractGfeTask;

/**
 * Controls the python execution of text products and retrieving information
 * from the product definitions. Runs the python in a separate thread to keep
 * from locking up the UI.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * May 29, 2008             njensen     Initial creation
 * Oct 15, 2008             njensen     Split python running to
 *                                      non-UI thread
 * Dec 1, 2010    6130      ryu         Set proper state and invoke callback
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

    private HashMap<String, Object> argMap;

    private ConfigData.productStateEnum state;

    /**
     * Constructor
     */
    public TextFormatter(String productName, String vtecMode,
            String databaseID, String varDict, String vtecActiveTable,
            String drtTime, int testMode, TextProductFinishListener finish) {
        super(productName);
        String addr = null;

        try {
            addr = InetAddress.getLocalHost().getHostName();
        } catch (UnknownHostException e) {
            addr = UUID.randomUUID().toString();
        }

        argMap = new HashMap<String, Object>();
        argMap.put("testMode", testMode);
        argMap.put(ArgDictConstants.DATABASE_ID, databaseID);
        argMap.put(ArgDictConstants.SITE, DataManager.getCurrentInstance()
                .getSiteID());
        argMap.put(ArgDictConstants.FORECAST_LIST, productName);
        argMap.put("username", System.getProperty("user.name") + ":" + addr);
        argMap.put("dataMgr", DataManager.getCurrentInstance());
        argMap.put(ArgDictConstants.VTEC_MODE, vtecMode);
        argMap.put(ArgDictConstants.CMDLINE_VARDICT, varDict);
        argMap.put(ArgDictConstants.VTEC_ACTIVE_TABLE, vtecActiveTable);
        argMap.put("drtTime", drtTime);
        listener = finish;
        this.state = ConfigData.productStateEnum.Queued;
    }

    @Override
    public void doRun() {
        FormatterScript script = null;
        String forecast = null;
        try {
            VizApp.runSyncIfWorkbench(new Runnable() {
                @Override
                public void run() {
                    state = ConfigData.productStateEnum.Running;
                    listener.startProgressBar(state);
                }
            });

            argMap.put("logFile", getLogFile().getAbsolutePath());
            script = FormatterScriptFactory.buildFormatterScript();
            ITimer timer = TimeUtil.getTimer();
            timer.start();
            forecast = (String) script.execute(argMap);
            timer.stop();
            String productName = (String) argMap
                    .get(ArgDictConstants.FORECAST_LIST);
            perfLog.logDuration("Text Formatter " + productName,
                    timer.getElapsedTime());

            state = ConfigData.productStateEnum.Finished;
        } catch (Throwable t) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error generating text product", t);
            state = ConfigData.productStateEnum.Failed;
        } finally {
            SamplerGridSliceCache.remove(this.getId());
            SamplerGridSliceCache.remove(UI_THREAD_ID);
            cleanUp(forecast);
            if (script != null) {
                script.dispose();
            }
        }
    }

    private void cleanUp(final String text) {
        VizApp.runSyncIfWorkbench(new Runnable() {
            @Override
            public void run() {
                listener.textProductFinished(text, state);
                listener.stopProgressBar(state);
            }
        });
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
        if (this.state.equals(productStateEnum.Queued)) {
            state = ConfigData.productStateEnum.Failed;
            cleanUp(null);

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
}
