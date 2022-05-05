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
package com.raytheon.uf.viz.thinclient.localization;

import java.io.File;
import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;

import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.checksum.ChecksumIO;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.viz.core.localization.BundleScanner;
import com.raytheon.uf.viz.core.localization.CAVELocalizationAdapter;
import com.raytheon.uf.viz.thinclient.Activator;
import com.raytheon.uf.viz.thinclient.preferences.ThinClientPreferenceConstants;

/**
 * The ThinClientLocalizationAdapter reduces the number of requests for
 * localization data from the server. It makes sure files are only downloaded
 * from the server if they do not exist on the local file system.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 09, 2011            njensen     Initial creation
 * Aug 13, 2013       2033 mschenke    Changed to search all plugins when 
 *                                     CAVE_STATIC BASE context searched
 * May 29, 2015       4532 bsteffen    Always use super when sync job is running.
 * Nov 30, 2015       4834 njensen     Remove LocalizationOpFailedException
 * Jun 22, 2017       6339 njensen     Updated method signature of listDirectory()
 * Aug 07, 2017       6379 njensen     Set checksum of ListResponses
 * 
 * </pre>
 * 
 * @author njensen
 */

public class ThinClientLocalizationAdapter extends CAVELocalizationAdapter
        implements IPropertyChangeListener {

    /**
     * Whenever there is a sync job running, always call super to allow it to go
     * to the server.
     */
    private final AtomicInteger syncJobsRunning = new AtomicInteger(0);

    private boolean useRemoteFiles = true;

    public ThinClientLocalizationAdapter() {
        super();
        IPreferenceStore store = Activator.getDefault().getPreferenceStore();
        useRemoteFiles = !store.getBoolean(
                ThinClientPreferenceConstants.P_DISABLE_REMOTE_LOCALIZATION);
        store.addPropertyChangeListener(this);
    }

    @Override
    public void retrieve(LocalizationFile file) throws LocalizationException {
        if (syncJobsRunning.get() > 0) {
            super.retrieve(file);
            return;
        }

        File localFile = file.getFile(false);
        if (!localFile.exists() || localFile.length() == 0) {
            super.retrieve(file);
        }
    }

    @Override
    public ListResponse[] listDirectory(LocalizationContext[] contexts,
            String path, String fileExtension, boolean recursive,
            boolean filesOnly) throws LocalizationException {
        if (shouldUseRemoteFiles()) {
            return super.listDirectory(contexts, path, fileExtension, recursive,
                    filesOnly);
        }

        Set<LocalizationContext> ctxsToSearch = new LinkedHashSet<>();
        for (LocalizationContext context : contexts) {
            ctxsToSearch.add(context);
            if (isCaveStaticBase(context) && context.getContextName() == null) {
                for (String bundle : BundleScanner
                        .getListOfBundles(BUNDLE_LOCALIZATION_DIR)) {
                    ctxsToSearch.add(new LocalizationContext(
                            context.getLocalizationType(),
                            context.getLocalizationLevel(), bundle));
                }
            }
        }

        // TODO: Check for preference to only use locally available files
        List<ListResponse> responses = new ArrayList<>();
        for (LocalizationContext context : ctxsToSearch) {
            // Scan local file system for files in directory structure
            File file = getPath(context, "");
            if (file == null || !file.exists()) {
                continue;
            }
            List<String> paths = buildPaths(path, file, fileExtension,
                    recursive, filesOnly);
            for (String p : paths) {
                File localFile = new File(file, p);
                ListResponse response = new ListResponse();
                response.context = context;
                response.isDirectory = localFile.isDirectory();
                response.protectedLevel = null;
                response.existsOnServer = false;
                response.fileName = p;
                response.date = new Date(localFile.lastModified());
                response.checkSum = ChecksumIO.getFileChecksum(localFile,
                        false);
                responses.add(response);
            }
        }
        return responses.toArray(new ListResponse[responses.size()]);

    }

    @Override
    public ListResponse[] getLocalizationMetadata(LocalizationContext[] context,
            String fileName) throws LocalizationException {
        if (shouldUseRemoteFiles()) {
            return super.getLocalizationMetadata(context, fileName);
        }
        List<ListResponse> responses = new ArrayList<>(context.length);
        for (LocalizationContext ctx : context) {
            ListResponse response = new ListResponse();
            response.checkSum = null;
            response.context = ctx;
            response.existsOnServer = false;
            response.fileName = fileName;
            response.protectedLevel = null;
            File file = getPath(ctx, fileName);
            if (file == null) {
                response.isDirectory = false;
                response.date = null;

            } else {
                response.isDirectory = file.isDirectory();
                response.date = new Date(file.lastModified());
            }
            responses.add(response);
        }
        return responses.toArray(new ListResponse[responses.size()]);
    }

    @Override
    public boolean exists(LocalizationFile file) {
        if (shouldUseRemoteFiles()) {
            return super.exists(file);
        }

        return file.getFile().exists();
    }

    private boolean shouldUseRemoteFiles() {
        return useRemoteFiles || syncJobsRunning.get() > 0;
    }

    @Override
    public void propertyChange(PropertyChangeEvent event) {
        if (ThinClientPreferenceConstants.P_DISABLE_REMOTE_LOCALIZATION
                .equals(event.getProperty())) {
            useRemoteFiles = !Boolean
                    .valueOf(String.valueOf(event.getNewValue()));
        } else if (ThinClientPreferenceConstants.P_SYNC_REMOTE_LOCALIZATION
                .equals(event.getProperty())) {
            boolean sync = Boolean.valueOf(String.valueOf(event.getNewValue()));
            if (sync) {
                syncJobsRunning.incrementAndGet();
            } else {
                syncJobsRunning.decrementAndGet();
            }
        }
    }

}
