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

import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

/**
 * 
 * Data structure allowing async communication between the background thread
 * that is determining which menu items should be available and the man UI
 * thread which is updating the menu items to show availability.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * May 24, 2011  9493     bsteffen  Initial creation
 * Feb 01, 2016  5275     bsteffen  Track all available items and add to queue
 *                                  only once.
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

    private final Set<String> availableSources;

    private final Set<String> availableFields;

    private final Set<String> availablePlanes;

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
        this.availableSources = new HashSet<>();
        this.availableFields = new HashSet<>();
        this.availablePlanes = new HashSet<>();
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
        if (availableSources.add(source)) {
            sourceQueue.add(source);
        }
    }

    public void addAvailableField(String field) {
        if (availableFields.add(field)) {
            fieldQueue.add(field);
        }
    }

    public void addAvailablePlane(String plane) {
        if (availablePlanes.add(plane)) {
            planeQueue.add(plane);
        }
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