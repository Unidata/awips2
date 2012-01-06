package com.raytheon.uf.viz.monitor.ffmp.ui.listeners;

import java.util.EventObject;

import com.raytheon.uf.viz.monitor.ffmp.ui.rsc.FFMPLoaderStatus;

/**
 * 
 * FFMP loader event updates
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 31, 2011 8661       dhladky     Initial creation
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 * 
 */

public class FFMPLoaderEvent extends EventObject {

    /**
     * 
     */
    private static final long serialVersionUID = 135784L;

    public FFMPLoaderEvent(FFMPLoaderStatus status) {
        super(status);
    }
}
