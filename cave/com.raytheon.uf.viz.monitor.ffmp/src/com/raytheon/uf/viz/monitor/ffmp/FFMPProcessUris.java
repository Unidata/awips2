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
package com.raytheon.uf.viz.monitor.ffmp;

import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.NavigableMap;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;

import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager.SOURCE_TYPE;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Processes the FFMP URIs. Extracted from FFMPMonitor.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 7, 2013            njensen     Initial creation
 * Jul 15, 2013  2184     dhladky     Removed all HUC's but ALL
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class FFMPProcessUris {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFMPProcessUris.class);

    private final NavigableMap<Date, List<String>> furiMap;

    private final String fsiteKey;

    private final String fsourceName;

    private final Date fbarrierTime;

    private final FFMPMonitor ffmpMonitor;

    public FFMPProcessUris(FFMPMonitor ffmpMonitor,
            NavigableMap<Date, List<String>> uriMap, String siteKey,
            String sourceName, Date barrierTime) {
        this.furiMap = uriMap;
        this.fsiteKey = siteKey;
        this.fbarrierTime = barrierTime;
        this.fsourceName = sourceName;
        this.ffmpMonitor = ffmpMonitor;
    }

    public void run(IProgressMonitor monitor) {
        if (furiMap != null) {
            SourceXML source = ffmpMonitor.getSourceConfig().getSource(
                    fsourceName);
            boolean isGuidance = false;
            if (source != null
                    && source.getSourceType().equals(
                            SOURCE_TYPE.GUIDANCE.getSourceType())) {
                isGuidance = true;
            }
            List<String> loadedUris = ffmpMonitor.getLoadedUris(fsiteKey,
                    fsourceName);
            Set<FFMPRecord> populatedRecords = new HashSet<FFMPRecord>();
            for (List<String> uris : furiMap.descendingMap().values()) {
                for (String uri : uris) {
                    if (uri == null || loadedUris.contains(uri)) {
                        continue;
                    }
                    FFMPRecord record = new FFMPRecord(uri);
                    if (record.getDataTime().getRefTime().after(fbarrierTime)
                            || isGuidance) {
                        try {
                            record = ffmpMonitor.populateFFMPRecord(uri,
                                    fsiteKey, fsourceName);
                            if (record != null) {
                                populatedRecords.add(record);
                                if (source != null) {
                                    record.setExpiration(source
                                            .getExpirationMinutes(fsiteKey));
                                    record.setRate(source.isRate());
                                }
                            }
                        } catch (Exception e) {
                            statusHandler.handle(Priority.PROBLEM,
                                    "FFMP Can't retrieve FFMP URI, " + uri, e);
                        }
                    }
                }
            }

            monitor.beginTask(null, populatedRecords.size());
            for (FFMPRecord record : populatedRecords) {
                record.getBasinData().loadNow();
                monitor.worked(1);
            }
        }
    }

}
