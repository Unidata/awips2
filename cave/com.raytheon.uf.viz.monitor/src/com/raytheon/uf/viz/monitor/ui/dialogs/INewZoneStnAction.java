package com.raytheon.uf.viz.monitor.ui.dialogs;

/**
 * Interface used for action callbacks for the Monitor Area Configure Dlg
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#      Engineer    Description
 * ------------ ----------   ----------- --------------------------
 * Feb  9, 2011  7854, 7855   zhao        Initial creation
 * Apr 24, 2014  3054         skorolev    Add an error message box action.
 * 
 * </pre>
 * 
 * @author zhao
 * @version 1.0
 */
public interface INewZoneStnAction {
    /**
     * Action to add a new zone to Monitor Area.
     */
    public void addNewZoneAction(String id, String lat, String log);

    public boolean isExistingZone(String id);

    /**
     * Action to add a new station to Monitor Area
     */
    public void addNewStationAction(String stnWithType);

    public boolean isExistingStation(String stnWithType);

    /**
     * Action to show an error message box for invalid Latitude and Longitude
     * values.
     */
    public void latLonErrorMsg(String latStr, String lonStr);
}
