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
package com.raytheon.edex.plugin.gfe.server.handler;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.InputMismatchException;
import java.util.NoSuchElementException;
import java.util.Scanner;
import java.util.TimeZone;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfigManager;
import com.raytheon.uf.common.dataplugin.gfe.request.GetSelectTimeRangeRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.time.SelectTimeRange;
import com.raytheon.uf.common.dataplugin.gfe.time.SelectTimeRange.Mode;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.util.FileUtil;

/**
 * Request handler for <code>GetSelectTimeRangeRequest</code>.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 1, 2012             dgilling    Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class GetSelectTimeRangeHandler implements
        IRequestHandler<GetSelectTimeRangeRequest> {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GetSelectTimeRangeHandler.class);

    private static final String FILE_EXT = ".SELECTTR";

    private static final String FILE_PATH = FileUtil.join("gfe", "text",
            "selecttr");

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest
     * (com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public ServerResponse<TimeRange> handleRequest(
            GetSelectTimeRangeRequest request) throws Exception {
        ServerResponse<TimeRange> retVal = new ServerResponse<TimeRange>();

        // So because EDEX has no concept of USER LocalizationLevel and the
        // request type allows the user to specify the site id for the SITE
        // localization level, we've got to build these contexts manually and
        // parse through the results ourselves...
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        int numContexts = (request.getWorkstationID().getUserName()
                .equals("SITE") ? 3 : 4);
        LocalizationContext[] contexts = new LocalizationContext[numContexts];
        // building the array with the contexts in USER->SITE->...->BASE order
        // allows the file we really want to appear sooner in the list of
        // results
        if (contexts.length == 4) {
            contexts[0] = pathMgr.getContext(LocalizationType.COMMON_STATIC,
                    LocalizationLevel.USER);
            contexts[0]
                    .setContextName(request.getWorkstationID().getUserName());
        }
        contexts[numContexts - 3] = pathMgr.getContextForSite(
                LocalizationType.COMMON_STATIC, request.getSiteID());
        contexts[numContexts - 2] = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.CONFIGURED);
        contexts[numContexts - 2].setContextName(request.getSiteID());
        contexts[numContexts - 1] = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);

        LocalizationFile[] files = pathMgr.listFiles(contexts, FILE_PATH,
                new String[] { FILE_EXT }, false, true);
        LocalizationFile trFile = null;
        for (LocalizationFile lf : files) {
            String lfName = lf.getFile().getName();
            if (lfName.equals(request.getName() + FILE_EXT)) {
                trFile = lf;
                break;
            }
        }

        if (trFile != null) {
            TimeZone localTZ = TimeZone
                    .getTimeZone(IFPServerConfigManager
                            .getServerConfig(request.getSiteID())
                            .getTimeZones().get(0));
            SelectTimeRange selectTR = loadTimeRange(trFile, localTZ);
            if (selectTR != null) {
                TimeRange tr = selectTR.toTimeRange();
                retVal.setPayload(tr);
            } else {
                retVal.addMessage("Could not read SelectTimeRange "
                        + request.getName());
            }
        } else {
            retVal.addMessage("Could not find a SelectTimeRange named "
                    + request.getName());
        }

        return retVal;
    }

    private SelectTimeRange loadTimeRange(LocalizationFile lf, TimeZone localTZ) {
        File file = lf.getFile();

        Scanner input = null;
        try {
            input = new Scanner(file);

            int start = input.nextInt();
            int end = input.nextInt();

            Mode mode = Mode.LT;
            if (input.hasNextInt()) {
                mode = Mode.values()[input.nextInt()];
            }

            TimeZone timeZone = (mode == Mode.ZULU ? TimeZone
                    .getTimeZone("GMT") : localTZ);

            SelectTimeRange range = new SelectTimeRange(FileUtil.unmangle(file
                    .getName().replace(FILE_EXT, "")), start, end, mode, lf
                    .getContext().getLocalizationLevel(), timeZone);
            return range;
        } catch (FileNotFoundException fileNotFound) {
            statusHandler.handle(Priority.PROBLEM, "Could not open SelectTR: "
                    + file.getAbsolutePath(), fileNotFound);
        } catch (InputMismatchException inputMismatch) {
            statusHandler.handle(
                    Priority.PROBLEM,
                    "Invalid value found within SelectTR: "
                            + file.getAbsolutePath(), inputMismatch);
        } catch (NoSuchElementException noElement) {
            statusHandler.handle(
                    Priority.PROBLEM,
                    "Prematurely reached end of SelectTR: "
                            + file.getAbsolutePath(), noElement);
        } catch (Exception e) {
            statusHandler.handle(
                    Priority.PROBLEM,
                    "Unhandled exception occurred reading SelectTR: "
                            + file.getAbsolutePath(), e);
        } finally {
            if (input != null) {
                input.close();
            }
        }

        return null;
    }
}
