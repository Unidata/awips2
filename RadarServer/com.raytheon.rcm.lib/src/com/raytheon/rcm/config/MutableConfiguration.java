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
package com.raytheon.rcm.config;

import com.raytheon.rcm.event.RadarEventListener;

/**
 * Interface to configurations that can be changed at runtime.
 *
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ...
 * 2014-02-03   DR 14762   D. Friedman Support arbitrary config files.
 * </pre>
 * 
 */
public interface MutableConfiguration {
    public RadarEventListener getConfigurationEventTarget();
    public void setConfigurationEventTarget(RadarEventListener configurationEventTarget);

    // TODO: need to return usable error messages...
    public boolean setGlobalConfig(Globals globals);
    public boolean setRadarConfig(RadarConfig rc);
    public boolean addRadarConfig(RadarConfig rc);
    public boolean removeRadarConfig(String radarID);
    public boolean storeConfigFile(String name, byte[] data);
}
