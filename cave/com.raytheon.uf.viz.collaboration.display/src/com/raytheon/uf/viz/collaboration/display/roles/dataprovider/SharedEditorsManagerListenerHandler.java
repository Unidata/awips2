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
package com.raytheon.uf.viz.collaboration.display.roles.dataprovider;

import java.util.LinkedHashSet;
import java.util.Set;

import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Manages Listeners for SharedEditorsManager events.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 6, 2012            bgonzale     Initial creation
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */
public class SharedEditorsManagerListenerHandler {

    Set<ISharedEditorsManagerListener> listeners = new LinkedHashSet<ISharedEditorsManagerListener>();

    /**
     * Event for an editor unshared.
     * 
     * @param editor
     */
    public void fireEditorRemoved(AbstractEditor editor) {
        ISharedEditorsManagerListener[] temp;

        synchronized (listeners) {
            temp = listeners
                    .toArray(new ISharedEditorsManagerListener[listeners.size()]);
        }
        if (temp != null) {
            for (ISharedEditorsManagerListener listener : temp) {
                listener.removeEditor(editor);
            }
        }
    }

    /**
     * Event for an editor shared.
     * 
     * @param editor
     */
    public void fireEditorShared(AbstractEditor editor) {
        ISharedEditorsManagerListener[] temp;

        synchronized (listeners) {
            temp = listeners
                    .toArray(new ISharedEditorsManagerListener[listeners.size()]);
        }
        if (temp != null) {
            for (ISharedEditorsManagerListener listener : temp) {
                listener.shareEditor(editor);
            }
        }
    }

    public void addListener(ISharedEditorsManagerListener listener) {
        synchronized (listener) {
            listeners.add(listener);
        }
    }

    public void removeListener(ISharedEditorsManagerListener listener) {
        synchronized (listener) {
            listeners.remove(listener);
        }
    }
}
