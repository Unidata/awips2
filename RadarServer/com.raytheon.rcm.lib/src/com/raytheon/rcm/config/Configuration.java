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

import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;

import com.raytheon.rcm.message.GraphicProduct.PDB;
import com.raytheon.rcm.request.RpsList;


/** 
 * Encapsulates the parameters that affect the operation of the Radar Server.
 * This includes a few site-specific identifiers, a set of records describing 
 * RPGs that the system can connect to, and a database mapping radar products
 * types to WMO headings.
 * 
 * <p>
 * TODO: Make observable. 
 */

public interface Configuration {
	public Collection<String> getConfiguredRadarList();
	public RadarConfig getConfigForRadar(String radarID);
	public int getPupId();
	public int getRegionCode();
	public String getWmoSiteID();
	public boolean isCollectionEnabled();
	public boolean isTdwrCollectionLimited();
	public String getEdexEndpoint();
	public String getAwips1Endpoint();
	public EndpointConfig getEndpointConfig();
	public ProductDistributionInfo getProductDistInfo(String radarID, PDB pdb);
	public ProductDistributionInfo getProductDistInfo(String radarID, int messageCode);
	public boolean isDecompressProducts();

	/** Retrieves the national component of an RPS list for the given radar and
	 * conditions.
	 * 
	 * <p>
	 * This is not simply a config value retrieval.  It can be template 
	 * processing operation in the case of TDWRs.
	 * 
	 * <p>
	 * If an error occurs, it is logged and null is returned.
	 * 
	 * @param radarID
	 * @param opMode
	 * @param vcp
	 * @param cuts The list of elevation angles for the VCP.  This is needed for
	 * TDWRs.
	 * @return An RPS list or null if there was an error.  The returned value 
	 * must not be modified.
	 */
	public RpsList getNationalRpsList(String radarID, int opMode, int vcp, int[] cuts);
	
	public RpsList getLocalRpsList(String radarID, int opMode, int vcp, int[] cuts);

	/*
	public RpsList getRecoveryRpsList(String radarID);
	public void removeRecoverRpsList(String radarID);	
	public void storeRecoveryRpsList(String radarID, RpsList rpsList);
	*/
	public InputStream getPersistedData(String name) throws IOException;
	public void putPersistedData(String name, byte[] data) throws IOException;
	public void removePersistedData(String name);
	
	public InputStream getDropInData(String name) throws IOException;
	
	public void setLocalRpsList(String radarID, RpsList list) throws IOException;
}
