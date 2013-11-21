package com.raytheon.uf.viz.datadelivery.system;

/**
 * System status refresh notifier interface.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 14, 2013  2387      skorolev    Initial creation
 * 
 * </pre>
 * 
 * @author skorolev
 * @version 1.0
 */
public interface ISystemStatusListener {

    /**
     * Refresh system status for Registers and Providers.
     */
    void statusRefresh();

    /**
     * Refresh time label for next refresh.
     */
    void timeLabelRefresh();
}
