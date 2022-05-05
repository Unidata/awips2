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
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;
import java.util.TreeMap;

import com.raytheon.uf.common.dataplugin.gfe.time.SelectTimeRange;
import com.raytheon.uf.common.dataplugin.gfe.time.SelectTimeRange.Mode;
import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.viz.gfe.core.ISelectTimeRangeManager;
import com.raytheon.viz.gfe.core.msgs.SelectTimeRangesChangedMsg;

/**
 * Class which manages the selection time range definitions that are stored on
 * the server.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Dec 03, 2009  3135     randerso  Initial creation
 * Aug 01, 2012  965      dgilling  Change location of SelectTimeRange.
 * Aug 06, 2013  1561     njensen   Use pm.listFiles() instead of
 *                                  pm.listStaticFiles()
 * Sep 08, 2014  3592     randerso  Changed to use new pm listStaticFiles()
 * Nov 18, 2015  5129     dgilling  Support new IFPClient.
 * Feb 05, 2016  5242     dgilling  Remove calls to deprecated Localization
 *                                  APIs.
 * Feb 28, 2018  7062     randerso  Sort inventory by time range
 *
 * </pre>
 *
 * @author randerso
 */
public class SelectTimeRangeManager
        implements ISelectTimeRangeManager, ILocalizationFileObserver {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SelectTimeRangeManager.class);

    private static final String FILE_PATH = FileUtil.join("gfe", "text",
            "selecttr");

    private static final String FILE_EXT = ".SELECTTR";

    private static final TimeZone ZULU = TimeZone.getTimeZone("GMT");

    private IPathManager pathManager;

    private TimeZone timeZone;

    private Map<String, SelectTimeRange> rangeMap;

    private LocalizationFile selectTRDir;

    /**
     * Constructor
     *
     * @param timeZone
     *            the site's time zone
     */
    public SelectTimeRangeManager(TimeZone timeZone) {
        // Find the referenced time zone
        this.timeZone = timeZone;

        this.pathManager = PathManagerFactory.getPathManager();
        selectTRDir = pathManager.getLocalizationFile(
                pathManager.getContext(LocalizationType.COMMON_STATIC,
                        LocalizationLevel.BASE),
                FILE_PATH);
        selectTRDir.addFileUpdatedObserver(this);

        loadInventory();
    }

    @Override
    public void dispose() {
        // unregister for notification of SELECTTR updates
        selectTRDir.removeFileUpdatedObserver(this);

    }

    @Override
    public String[] inventory() {
        List<SelectTimeRange> ranges = new ArrayList<>(this.rangeMap.values());
        Collections.sort(ranges);
        String[] inventory = new String[ranges.size()];
        for (int i = 0; i < ranges.size(); i++) {
            inventory[i] = ranges.get(i).getName();
        }
        return inventory;
    }

    private SelectTimeRange loadTimeRange(ILocalizationFile lf) {
        String rangeName = rangeNameFromFileName(lf.getPath());

        try (BufferedReader in = new BufferedReader(
                new InputStreamReader(lf.openInputStream()))) {
            String[] s = in.readLine().split("\\s+");
            int start = Integer.parseInt(s[0]);
            int end = Integer.parseInt(s[1]);
            Mode mode = Mode.LT;
            if (s.length > 2) {
                mode = Mode.values()[Integer.parseInt(s[2])];
            }

            SelectTimeRange range = new SelectTimeRange(rangeName, start, end,
                    mode, lf.getContext().getLocalizationLevel(),
                    (mode.equals(Mode.ZULU) ? ZULU : timeZone));
            return range;
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error reading SELECTTR file " + lf.toString(), e);
        }

        return null;
    }

    @Override
    public SelectTimeRange getRange(String name) {
        return rangeMap.get(name);
    }

    @Override
    public void save(String name, int start, int end, Mode mode) {
        ILocalizationFile lf = pathManager.getLocalizationFile(
                pathManager.getContext(LocalizationType.COMMON_STATIC,
                        LocalizationLevel.USER),
                FileUtil.join(FILE_PATH, FileUtil.mangle(name) + FILE_EXT));

        try (SaveableOutputStream lfStream = lf.openOutputStream();
                BufferedWriter out = new BufferedWriter(
                        new OutputStreamWriter(lfStream))) {
            out.write(String.format("%d %d %d", start, end, mode.ordinal()));
            out.close();
            lfStream.save();
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error writing SELECTTR file " + lf.toString(), e);
        }
    }

    @Override
    public void remove(String name) {
        ILocalizationFile lf = pathManager.getLocalizationFile(
                pathManager.getContext(LocalizationType.COMMON_STATIC,
                        LocalizationLevel.USER),
                FileUtil.join(FILE_PATH, FileUtil.mangle(name) + FILE_EXT));
        try {
            lf.delete();
        } catch (LocalizationException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error deleting SELECTTR file " + lf.toString(), e);
        }
    }

    @Override
    public void fileUpdated(FileUpdatedMessage message) {
        SelectTimeRange range;

        switch (message.getChangeType()) {
        case ADDED:
        case UPDATED:
            LocalizationFile lf = pathManager.getLocalizationFile(
                    message.getContext(), message.getFileName());
            range = loadTimeRange(lf);
            if (range != null) {
                SelectTimeRange existing = this.rangeMap.get(range.getName());
                if ((existing == null) || (existing.getLevel()
                        .compareTo(range.getLevel()) <= 0)) {
                    this.rangeMap.put(range.getName(), range);
                }
            }
            break;

        case DELETED:
            LocalizationFile[] files = pathManager.listStaticFiles(
                    LocalizationType.COMMON_STATIC, message.getFileName(),
                    new String[] { FILE_EXT }, false, true);

            if (files.length > 0) {
                // grab the first, should only be one
                range = loadTimeRange(files[0]);
                if (range != null) {
                    this.rangeMap.put(range.getName(), range);
                }
            } else {
                String rangeName = rangeNameFromFileName(message.getFileName());
                this.rangeMap.remove(rangeName);
            }
            break;

        default:
            statusHandler.error("Unexpected FileChangeType received: "
                    + message.getChangeType().name());
            break;
        }

        new SelectTimeRangesChangedMsg().send();
    }

    private void loadInventory() {
        LocalizationFile[] files = pathManager.listStaticFiles(
                LocalizationType.COMMON_STATIC, FILE_PATH,
                new String[] { FILE_EXT }, false, true);

        List<SelectTimeRange> ranges = new ArrayList<>(files.length);
        for (LocalizationFile lf : files) {
            SelectTimeRange range = loadTimeRange(lf);
            if (range != null) {
                ranges.add(range);
            }
        }
        Collections.sort(ranges);

        Map<String, SelectTimeRange> map = new TreeMap<>();
        for (SelectTimeRange range : ranges) {
            map.put(range.getName(), range);
        }

        synchronized (this) {
            this.rangeMap = map;
        }

        new SelectTimeRangesChangedMsg().send();
    }

    private String rangeNameFromFileName(String fileName) {
        File file = new File(fileName);
        String rangeName = FileUtil
                .unmangle(file.getName().replace(FILE_EXT, ""));
        return rangeName;
    }

    @Override
    public TimeZone getTimeZone() {
        return timeZone;
    }
}
