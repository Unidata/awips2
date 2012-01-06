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
package com.raytheon.viz.volumebrowser.datacatalog;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 24, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class AvailableDataRequest {
    private boolean cancel = false;

    private final BlockingQueue<String> sourceQueue;

    private final BlockingQueue<String> fieldQueue;

    private final BlockingQueue<String> planeQueue;

    private final String[] selectedSources;

    private final String[] selectedFields;

    private final String[] selectedPlanes;

    public AvailableDataRequest(String[] selectedSources,
            String[] selectedFields, String[] selectedPlanes) {
        this.selectedSources = selectedSources;
        this.selectedFields = selectedFields;
        this.selectedPlanes = selectedPlanes;
        this.sourceQueue = new LinkedBlockingQueue<String>();
        this.fieldQueue = new LinkedBlockingQueue<String>();
        this.planeQueue = new LinkedBlockingQueue<String>();
    }

    public String[] getSelectedSources() {
        return selectedSources;
    }

    public String[] getSelectedFields() {
        return selectedFields;
    }

    public String[] getSelectedPlanes() {
        return selectedPlanes;
    }

    public void addAvailableSource(String source) {
        sourceQueue.add(source);
    }

    public void addAvailableField(String field) {
        fieldQueue.add(field);
    }

    public void addAvailablePlane(String plane) {
        planeQueue.add(plane);
    }

    public String getAvailableSource() {
        return sourceQueue.poll();
    }

    public String getAvailableField() {
        return fieldQueue.poll();
    }

    public String getAvailablePlane() {
        return planeQueue.poll();
    }

    public boolean isCanceled() {
        return cancel;
    }

    public void cancel() {
        this.cancel = true;
    }

    public boolean anyAvailable() {
        return !sourceQueue.isEmpty() || !planeQueue.isEmpty()
                || !fieldQueue.isEmpty();
    }

}