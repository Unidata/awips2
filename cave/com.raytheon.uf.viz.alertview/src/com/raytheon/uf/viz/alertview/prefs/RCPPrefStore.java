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
package com.raytheon.uf.viz.alertview.prefs;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.osgi.framework.Bundle;
import org.osgi.framework.FrameworkUtil;

import com.raytheon.uf.viz.alertview.AlertViewPrefStore;

/**
 * 
 * A simple {@link AlertViewPrefStore} that stores preferences in the state area
 * that the Eclipse Runtime Platform provides for this bundle. When no user file
 * is available the default version of the file is loaded from within this
 * bundle.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Jun 17, 2015  4474     bsteffen  Initial creation
 * Aug 18, 2015  3806     njensen   Renamed config stream methods
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class RCPPrefStore implements AlertViewPrefStore {

    private final Bundle bundle;

    private final IPath root = new Path("defaultPrefs");

    private final Set<AlertViewPrefListener> listeners = new CopyOnWriteArraySet<>();

    public RCPPrefStore() {
        this.bundle = FrameworkUtil.getBundle(getClass());
    }

    @Override
    public InputStream openConfigInputStream(String fileName)
            throws IOException {
        IPath userPath = Platform.getStateLocation(bundle).append(fileName);
        File file = userPath.toFile();
        if (file.exists()) {
            return new FileInputStream(file);
        } else {
            return FileLocator.openStream(bundle, root.append(fileName), false);
        }
    }

    @Override
    public OutputStream openConfigOutputStream(String fileName)
            throws FileNotFoundException {
        IPath userPath = Platform.getStateLocation(bundle).append(fileName);
        return new NotificationOutputStream(userPath.toFile());
    }

    @Override
    public void addListener(AlertViewPrefListener listener) {
        listeners.add(listener);
    }

    @Override
    public void removeListener(AlertViewPrefListener listener) {
        listeners.remove(listener);
    }

    private class NotificationOutputStream extends FileOutputStream {

        private final String fileName;

        public NotificationOutputStream(File file) throws FileNotFoundException {
            super(file);
            this.fileName = file.getName();
        }

        @Override
        public void close() throws IOException {
            super.close();
            for (AlertViewPrefListener listener : listeners) {
                listener.prefFileChanged(fileName);
            }
        }

    }

}
