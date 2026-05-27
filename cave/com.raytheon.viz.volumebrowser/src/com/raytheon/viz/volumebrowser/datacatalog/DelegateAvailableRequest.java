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

/**
 * Delegate objection for AvailableDataRequests
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 28, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DelegateAvailableRequest extends AvailableDataRequest {

    protected AvailableDataRequest request;

    public DelegateAvailableRequest(AvailableDataRequest request) {
        super(request.getSelectedSources(), request.getSelectedFields(),
                request.getSelectedPlanes());
        this.request = request;
    }

    @Override
    public void addAvailableSource(String source) {
        request.addAvailableSource(source);
    }

    @Override
    public void addAvailableField(String field) {
        request.addAvailableField(field);
    }

    @Override
    public void addAvailablePlane(String plane) {
        request.addAvailablePlane(plane);
    }

    @Override
    public String getAvailableSource() {
        return request.getAvailableSource();
    }

    @Override
    public String getAvailableField() {
        return request.getAvailableField();
    }

    @Override
    public String getAvailablePlane() {
        return request.getAvailablePlane();
    }

    @Override
    public boolean isCanceled() {
        return request.isCanceled();
    }

    @Override
    public void cancel() {
        request.cancel();
    }

    @Override
    public boolean anyAvailable() {
        return request.anyAvailable();
    }

}
