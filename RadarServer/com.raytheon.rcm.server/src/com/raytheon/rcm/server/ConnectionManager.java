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
package com.raytheon.rcm.server;


/**
 * High-level interface to the RPG communication services used by various 
 * components (RadarEventListeners) of the Radar Server.
 */
public interface ConnectionManager {
	/** Requests a connection to an RPG.
	 * <p>The connection attempt may be delayed if all dial-out resources are
	 * in use.
	 * @param radarID specifies the RPG
	 */
	public void connectRadar(String radarID);
	/** Disconnects from an RPG.
	 * 
	 * @param radarID specifies the RPG
	 */
	public void disconnectRadar(String radarID);
	/**
	 * Send a message to an RPG.
	 * 
	 * @param radarID The RPG
	 * @param msg A message encoded in Nexrad format. If either the source or 
	 * destination ID is zero, it will be changed to the appropriate value.
	 * {@code msg} must not be immutable in this case as its contents will be 
	 * changed.
	 */
	public void sendMessageToRadar(String radarID, byte[] msg);
}
