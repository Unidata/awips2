package com.raytheon.uf.viz.monitor.ffmp.ui.rsc;

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

/**
 * Time object
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 22 April, 2012 2521          dhladky     Initial creation
 * 02/01/13     1569        D. Hladky   Added constants
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

