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
package com.raytheon.viz.texteditor.alarmalert.util;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.raytheon.uf.common.dataplugin.text.alarms.AlarmAlertProduct;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 14, 2009            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class AlarmAlertLists {

    private List<AlarmAlertProduct> filteredProducts;

    private List<AlarmAlertProduct> currentAlarms;

    private List<ICurrentAlarmListener> currentAlarmListeners = new ArrayList<ICurrentAlarmListener>();

    private static AlarmAlertLists _instance = null;

    private AlarmAlertLists() {
        filteredProducts = new ArrayList<AlarmAlertProduct>();
        currentAlarms = new ArrayList<AlarmAlertProduct>();
    }

    public static synchronized AlarmAlertLists getInstance() {
        if (_instance == null) {
            _instance = new AlarmAlertLists();
        }
        return _instance;
    }

    /**
     * @return the filteredProducts
     */
    public List<AlarmAlertProduct> getFilteredProducts() {
        return filteredProducts;
    }

    /**
     * @param filteredProducts
     *            the filteredProducts to set
     */
    public void setFilteredProducts(List<AlarmAlertProduct> filteredProducts) {
        this.filteredProducts = filteredProducts;
    }

    /**
     * @return the currentAlarms
     */
    public List<AlarmAlertProduct> getCurrentAlarms() {
        return currentAlarms;
    }

    /**
     * @param currentAlarms
     *            the currentAlarms to set
     */
    public void setCurrentAlarms(List<AlarmAlertProduct> currentAlarms) {
        this.currentAlarms = currentAlarms;
    }

    public void addListener(ICurrentAlarmListener ical) {
        currentAlarmListeners.add(ical);
    }

    public void removeListener(ICurrentAlarmListener ical) {
        currentAlarmListeners.remove(ical);
    }

    /**
     * Takes in the product and fires off to tell the current alarm queue to
     * update
     * 
     * @param aap
     */
    public void fireNewCurrentAlarmEvent(AlarmAlertProduct aap) {
        CurrentAlarmEvent cae = new CurrentAlarmEvent(aap);
        Iterator<ICurrentAlarmListener> iter = currentAlarmListeners.iterator();
        while (iter.hasNext()) {
            iter.next().currentAlarmChanged(cae);
        }
    }
}
