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
package com.raytheon.viz.gfe.core.internal;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import com.raytheon.uf.common.dataplugin.gfe.time.SelectTimeRange;
import com.raytheon.uf.common.dataplugin.gfe.time.SelectTimeRange.Mode;
import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.viz.gfe.GFEServerException;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.ISelectTimeRangeManager;
import com.raytheon.viz.gfe.core.msgs.SelectTimeRangesChangedMsg;

/**
 * Class which manages the selection time range definitions that are stored on
 * the server.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 3, 2009       #3135 randerso    Initial creation
 * Aug 1, 2012       #965  dgilling    Change location of SelectTimeRange.
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
public class SelectTimeRangeManager implements ISelectTimeRangeManager,
        ILocalizationFileObserver {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SelectTimeRangeManager.class);

    private static final String FILE_PATH = FileUtil.join("gfe", "text",
            "selecttr");

    private static final String FILE_EXT = ".SELECTTR";

    private static final IPathManager pathManager = PathManagerFactory
            .getPathManager();

    private static final TimeZone ZULU = TimeZone.getTimeZone("GMT");

    protected DataManager dataManager;

    private TimeZone timeZone;

    private String[] inventory;

    private Map<String, SelectTimeRange> rangeMap;

    private LocalizationFile selectTRDir;

    public SelectTimeRangeManager(DataManager dataManager) {
        this.dataManager = dataManager;
        // Find the referenced time zone
        timeZone = TimeZone.getTimeZone("GMT");
        try {
            timeZone = TimeZone.getTimeZone(dataManager.getClient()
                    .getDBGridLocation().getTimeZone());
        } catch (GFEServerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to retrieve GFE time zone, using GMT", e);
        }

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        selectTRDir = pathMgr.getLocalizationFile(pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE),
                FILE_PATH);
        selectTRDir.addFileUpdatedObserver(this);

        fileUpdated(null);
    }

    @Override
    public void dispose() {
        // unregister for notification of SELECTTR updates
        selectTRDir.removeFileUpdatedObserver(this);

    }

    @Override
    public String[] inventory() {
        return inventory;
    }

    private SelectTimeRange loadTimeRange(LocalizationFile lf) {
        File file = lf.getFile();

        BufferedReader in = null;
        try {
            in = new BufferedReader(new FileReader(file));
            String[] s = in.readLine().split("\\s+");
            int start = Integer.parseInt(s[0]);
            int end = Integer.parseInt(s[1]);
            Mode mode = Mode.LT;
            if (s.length > 2) {
                mode = Mode.values()[Integer.parseInt(s[2])];
            }

            SelectTimeRange range = new SelectTimeRange(FileUtil.unmangle(file
                    .getName().replace(FILE_EXT, "")), start, end, mode, lf
                    .getContext().getLocalizationLevel(),
                    (mode.equals(Mode.ZULU) ? ZULU : timeZone));
            return range;
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error reading SELECTTR file " + file.getAbsolutePath(), e);
        } finally {
            if (in != null) {
                try {
                    in.close();
                } catch (IOException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error closing file " + file.getAbsolutePath(), e);
                }
            }
        }
        return null;
    }

    @Override
    public SelectTimeRange getRange(String name) {
        return rangeMap.get(name);
    }

    @Override
    public void save(String name, int start, int end, Mode mode) {
        LocalizationFile lf = pathManager.getLocalizationFile(pathManager
                .getContext(LocalizationType.COMMON_STATIC,
                        LocalizationLevel.USER), FileUtil.join(FILE_PATH,
                FileUtil.mangle(name) + FILE_EXT));

        File file = lf.getFile();
        BufferedWriter out = null;
        try {
            out = new BufferedWriter(new FileWriter(file));
            out.write(String.format("%d %d %d", start, end, mode.ordinal()));
            out.close();
            lf.save();
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error writing SELECTTR file " + file.getAbsolutePath(), e);
        } finally {
            try {
                out.close();
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM, "Error closing file "
                        + file.getAbsolutePath(), e);
            }
        }
    }

    @Override
    public void remove(String name) {
        LocalizationFile lf = pathManager.getLocalizationFile(pathManager
                .getContext(LocalizationType.COMMON_STATIC,
                        LocalizationLevel.USER), FileUtil.join(FILE_PATH,
                FileUtil.mangle(name) + FILE_EXT));
        try {
            lf.delete();
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error deleting SELECTTR file "
                            + lf.getFile().getAbsolutePath(), e);
        }
    }

    @Override
    public void fileUpdated(FileUpdatedMessage message) {
        LocalizationFile[] files = pathManager.listStaticFiles(FILE_PATH,
                new String[] { FILE_EXT }, false, true);

        List<SelectTimeRange> ranges = new ArrayList<SelectTimeRange>(
                files.length);
        for (LocalizationFile lf : files) {
            SelectTimeRange range = loadTimeRange(lf);
            if (range != null) {
                ranges.add(range);
            }
        }
        Collections.sort(ranges);

        String[] inv = new String[ranges.size()];
        Map<String, SelectTimeRange> map = new HashMap<String, SelectTimeRange>();
        int i = 0;
        for (SelectTimeRange range : ranges) {
            inv[i++] = range.getName();
            map.put(range.getName(), range);
        }

        synchronized (this) {
            this.inventory = inv;
            this.rangeMap = map;
        }

        new SelectTimeRangesChangedMsg().send();
    }
}
