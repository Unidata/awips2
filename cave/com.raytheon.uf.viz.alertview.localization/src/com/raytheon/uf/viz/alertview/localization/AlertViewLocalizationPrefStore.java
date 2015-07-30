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
package com.raytheon.uf.viz.alertview.localization;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;

import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationFileOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.viz.alertview.AlertViewPrefStore;

/**
 * 
 * An {@link AlertViewPrefStore} that stores preferences in
 * {@link LocalizationFile}s.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Jun 26, 2015  4474     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class AlertViewLocalizationPrefStore implements AlertViewPrefStore,
        ILocalizationFileObserver {

    private static final String BASE_PATH = "alertView"
            + IPathManager.SEPARATOR;

    private final Set<AlertViewPrefListener> listeners = new CopyOnWriteArraySet<>();

    private IPathManager pathManager;

    protected LocalizationFile getRootFile() {
        LocalizationContext context = pathManager.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        return pathManager.getLocalizationFile(context, BASE_PATH);
    }

    public void setPathManager(IPathManager pathManager) {
        this.pathManager = pathManager;
        getRootFile().addFileUpdatedObserver(this);
    }

    public void unsetPathManager(IPathManager pathManager) {
        if (pathManager == this.pathManager) {
            getRootFile().removeFileUpdatedObserver(this);
            this.pathManager = null;
        }
    }

    @Override
    public InputStream readConfigFile(String fileName) throws IOException {
        LocalizationFile file = pathManager.getStaticLocalizationFile(
                LocalizationType.COMMON_STATIC, BASE_PATH + fileName);
        if (file == null) {
            return null;
        } else {
            try {
                return file.openInputStream();
            } catch (LocalizationException e) {
                throw new IOException(e);
            }
        }
    }

    @Override
    public OutputStream writeConfigFile(String fileName) throws IOException {
        LocalizationContext context = pathManager.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.USER);
        LocalizationFile file = pathManager.getLocalizationFile(context,
                BASE_PATH + fileName);
        if (file == null) {
            return null;
        } else {
            try {
                return new SavingOutputStream(file.openOutputStream());
            } catch (LocalizationException e) {
                throw new IOException(e);
            }
        }
    }

    @Override
    public void addListener(AlertViewPrefListener listener) {
        listeners.add(listener);
    }

    @Override
    public void removeListener(AlertViewPrefListener listener) {
        listeners.remove(listener);
    }

    @Override
    public void fileUpdated(FileUpdatedMessage message) {
        String fileName = message.getFileName().substring(BASE_PATH.length());
        for (AlertViewPrefListener listener : listeners) {
            listener.prefFileChanged(fileName);
        }
    }

    private class SavingOutputStream extends OutputStream {

        private final LocalizationFileOutputStream stream;

        public SavingOutputStream(LocalizationFileOutputStream stream) {
            this.stream = stream;
        }

        @Override
        public void close() throws IOException {
            try {
                stream.closeAndSave();
            } catch (LocalizationOpFailedException e) {
                throw new IOException(e);
            }
        }

        @Override
        public void write(int b) throws IOException {
            stream.write(b);
        }

        @Override
        public void write(byte[] b) throws IOException {
            stream.write(b);
        }

        @Override
        public void write(byte[] b, int off, int len) throws IOException {
            stream.write(b, off, len);
        }

    }

}
