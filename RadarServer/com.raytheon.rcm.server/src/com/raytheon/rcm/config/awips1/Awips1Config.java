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
package com.raytheon.rcm.config.awips1;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;

import com.raytheon.rcm.config.Configuration;
import com.raytheon.rcm.config.EndpointConfig;
import com.raytheon.rcm.config.ProductDistInfoDB;
import com.raytheon.rcm.config.ProductDistributionInfo;
import com.raytheon.rcm.config.RadarConfig;
import com.raytheon.rcm.message.GraphicProduct.PDB;
import com.raytheon.rcm.request.RpsList;
import com.raytheon.rcm.server.Log;


public class Awips1Config implements Configuration {
	private HashMap<String, Awips1RadarConfig> radars = new HashMap<String, Awips1RadarConfig>();

	private int pupId;
	
	// Used for collection/dissemination
	private int regionCode;
	private String wmoSiteID;
	private boolean collectionEnabled; // Global enable-central-collection flag
	private boolean tdwrCollectionLimited;
	
	private ProductDistInfoDB prodDistInfoDB;
	
	private boolean decompressProducts;
	private String edexEndpoint;
	private String awips1Endpoint;
	
	public Collection<String> getConfiguredRadarList() {
		ArrayList<String> result = new ArrayList<String>();
		result.addAll(radars.keySet());
		return result;
	}
	
	public RadarConfig getConfigForRadar(String radarID) {
		return radars.get(radarID.toLowerCase());
	}

	public int getPupId() {
		return pupId;
	}

	public int getRegionCode() {
		return regionCode;
	}

	public String getWmoSiteID() {
		return wmoSiteID;
	}

	public boolean isCollectionEnabled() {
		return collectionEnabled;
	}

	public boolean isTdwrCollectionLimited() {
		return tdwrCollectionLimited;
	}
	
	public ProductDistributionInfo getProductDistInfo(String radarID, PDB pdb) {
		RadarConfig rc = getConfigForRadar(radarID);
		if (prodDistInfoDB != null && rc != null)
			return prodDistInfoDB.getProductDistInfo(rc, pdb.productCode, pdb);
		else
			return null;
	}
	
	public ProductDistributionInfo getProductDistInfo(String radarID, int messageCode) {
		RadarConfig rc = getConfigForRadar(radarID);
		if (prodDistInfoDB != null && rc != null)
			return prodDistInfoDB.getProductDistInfo(rc, messageCode, null);
		else
			return null;
	}
	
	// Setters are not part of the public interface...
	
	public void setRadars(HashMap<String, Awips1RadarConfig> radars) {
		this.radars = radars;
	}

	public void setPupId(int pupId) {
		this.pupId = pupId;
	}

	public void setRegionCode(int regionCode) {
		this.regionCode = regionCode;
	}

	public void setWmoSiteID(String wmoSiteID) {
		this.wmoSiteID = wmoSiteID;
	}

	public void setCollectionEnabled(boolean collectionEnabled) {
		this.collectionEnabled = collectionEnabled;
	}

	public void setTdwrCollectionLimited(boolean tdwrCollectionLimited) {
		this.tdwrCollectionLimited = tdwrCollectionLimited;
	}

	// Should be considered to be (sub) package access
	public ProductDistInfoDB getProdDistInfoDB() {
		return prodDistInfoDB;
	}

	public void setProdDistInfoDB(ProductDistInfoDB prodDistInfoDB) {
		this.prodDistInfoDB = prodDistInfoDB;
	}

	public boolean isDecompressProducts() {
		return decompressProducts;
	}

	public void setDecompressProducts(boolean decompressProducts) {
		this.decompressProducts = decompressProducts;
	}
	
	@Override
	public RpsList getNationalRpsList(String radarID, int opMode, int vcp, int[] cuts) {
		Awips1RadarConfig rc = radars.get(radarID);
		if (rc != null)
			return rc.getNationalRpsList(opMode, vcp, cuts);
		else
			return null;
	}

	@Override
	public RpsList getLocalRpsList(String radarID, int opMode, int vcp, int[] cuts) {
		Awips1RadarConfig rc = radars.get(radarID);
		if (rc != null)
			return rc.getLocalRpsList(opMode, vcp);
		else
			return null;
	}

	public String getEdexEndpoint() {
		return edexEndpoint;
	}

	public void setEdexEndpoint(String edexEndpoint) {
		this.edexEndpoint = edexEndpoint;
	}

	public String getAwips1Endpoint() {
		return awips1Endpoint;
	}
	
	public EndpointConfig getEndpointConfig() {
		return null;
	}

	public void setAwips1Endpoint(String awips1Endpoint) {
		this.awips1Endpoint = awips1Endpoint;
	}

	@Override
	public void setLocalRpsList(String radarID, RpsList list)
			throws IOException {
		throw new IOException("Storing RPS lists not support for AWIPS-1 style configuration.");
	}

	private static final String persistingNotSupproted =
		"Persisting configuration data is not supported under the AWIPS-1 configuration system";
	
	private static final String dropInsNotSupproted =
        "Drop-in configuration data is not supported under the AWIPS-1 configuration system";

	@Override
	public InputStream getPersistedData(String name) throws IOException {
		throw new FileNotFoundException(persistingNotSupproted);
	}

	@Override
	public void putPersistedData(String name, byte[] data) throws IOException {
		String msg = persistingNotSupproted;  
		Log.error(msg);
		throw new IOException(new UnsupportedOperationException(msg));
	}

	@Override
	public void removePersistedData(String name) {
		// nothing
	}

    @Override
    public InputStream getDropInData(String name) throws IOException {
        String msg = persistingNotSupproted;  
        Log.error(msg);
        throw new IOException(new UnsupportedOperationException(msg));
    }

}
