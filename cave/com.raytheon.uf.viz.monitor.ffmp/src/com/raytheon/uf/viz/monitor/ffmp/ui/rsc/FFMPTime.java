package com.raytheon.uf.viz.monitor.ffmp.ui.rsc;

/**
 * Time object
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 22 April, 2012 2521          dhladky     Initial creation
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */


public class FFMPTime {
	
	public double time = 0.0;
    public boolean split = false;
    
    public FFMPTime(double time, boolean split) {
        this.time = time;
        this.split = split;
    }
   
    public double getTime() {
        return time;
    }
   
    public boolean isSplit() {
        return split;
    }
   
}

