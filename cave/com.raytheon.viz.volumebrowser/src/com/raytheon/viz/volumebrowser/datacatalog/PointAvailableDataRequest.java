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
 * 
 * Wraps an AvailableDataRequest but automatically adds Objective Analysis
 * sources to any sources that are available.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 25, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class PointAvailableDataRequest extends DelegateAvailableRequest {

    public PointAvailableDataRequest(AvailableDataRequest request) {
        super(modifySelectedSources(request));
        this.request = request;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.volumebrowser.datacatalog.DelegateAvailableRequest#
     * addAvailableSource(java.lang.String)
     */
    @Override
    public void addAvailableSource(String source) {
        super.addAvailableSource(source);
        super.addAvailableSource(source + "OA");
    }

    private static AvailableDataRequest modifySelectedSources(
            AvailableDataRequest request) {
        String[] selectedSources = request.getSelectedSources();
        if (selectedSources != null) {
            for (int i = 0; i < selectedSources.length; i++) {
                if (selectedSources[i].endsWith("OA")) {
                    selectedSources[i] = selectedSources[i].substring(0,
                            selectedSources[i].length() - 2);
                } else {
                    selectedSources[i] = selectedSources[i];
                }
            }
        }
        return request;
    }

}
