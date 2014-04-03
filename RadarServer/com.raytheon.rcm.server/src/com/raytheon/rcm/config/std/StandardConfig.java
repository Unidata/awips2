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
package com.raytheon.rcm.config.std;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.util.Collection;
import java.util.HashMap;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import com.raytheon.rcm.config.Configuration;
import com.raytheon.rcm.config.EndpointConfig;
import com.raytheon.rcm.config.Globals;
import com.raytheon.rcm.config.MutableConfiguration;
import com.raytheon.rcm.config.ProductDistributionInfo;
import com.raytheon.rcm.config.RadarConfig;
import com.raytheon.rcm.config.RadarType;
import com.raytheon.rcm.config.StandardProductDistInfoDB;
import com.raytheon.rcm.config.Util;
import com.raytheon.rcm.config.awips1.Awips1RpsListUtil;
import com.raytheon.rcm.event.ConfigEvent;
import com.raytheon.rcm.event.ConfigEvent.Category;
import com.raytheon.rcm.event.RadarEventListener;
import com.raytheon.rcm.message.GSM;
import com.raytheon.rcm.message.GraphicProduct.PDB;
import com.raytheon.rcm.request.RpsList;
import com.raytheon.rcm.request.RpsXml;
import com.raytheon.rcm.server.Log;


/**
 * Represents the standard configuration model of the AWIPS 2 RadarServer.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ...
 * 2014-02-03   DR 14762   D. Friedman Handle updated NDM config files.
 *                                     Send configuration events.
 * </pre>
 *
 */
public class StandardConfig implements Configuration, MutableConfiguration {

	final String generalPropBase = "com.raytheon.rcm";

	private ConfigRes res;
	private ConfigDoc doc;
	private StandardConfigProvider provider;
	private HashMap<String, RadarConfig> radars = new HashMap<String, RadarConfig>();
	private StandardProductDistInfoDB prodDistInfoDB = new StandardProductDistInfoDB();
    private RadarEventListener configurationEventTarget;
	
	private int regionCodeFromWmoSiteInfo;

	public StandardConfig(ConfigRes res, StandardConfigProvider provider) {
		/* Provider is passed for the purposes of being able to save the 
		 * configuration.  The config I/O handling is already in the provider,
		 * so it easier just to call a 'save' method in the provider rather than
		 * make getters for Unmarshaller, config file name, etc.. 
		 * Maybe save-configuration method should be in the provider class?
		 */
		if (provider == null) // So for now, we require a provider.
			throw new IllegalArgumentException("Provider must be non-null");
		this.provider = provider;
		this.res = res;
	}

	@Override
	public RadarConfig getConfigForRadar(String radarID) {
		return radars.get(radarID);
	}

	@Override
	public Collection<String> getConfiguredRadarList() {
		return radars.keySet();
	}

	StandardProductDistInfoDB getProdDistInfoDB() {
		return prodDistInfoDB;
	}

    public void setProdDistInfoDB(StandardProductDistInfoDB prodDistInfoDB) {
        this.prodDistInfoDB = prodDistInfoDB;
        if (configurationEventTarget != null) {
            ConfigEvent ev = new ConfigEvent(Category.PROD_DISTRIBUTION);
            configurationEventTarget.handleConfigEvent(ev);
        }
    }

	@Override
	public ProductDistributionInfo getProductDistInfo(String radarID, PDB pdb) {
		RadarConfig rc = getConfigForRadar(radarID);
		StandardProductDistInfoDB db = prodDistInfoDB;
		if (db != null && rc != null)
			return db.getProductDistInfo(rc, pdb.productCode, pdb);
		else
			return null;
	}

	@Override
	public ProductDistributionInfo getProductDistInfo(String radarID,
			int messageCode) {
		RadarConfig rc = getConfigForRadar(radarID);
		StandardProductDistInfoDB db = prodDistInfoDB;
		if (db != null && rc != null)
			return db.getProductDistInfo(rc, messageCode, null);
		else
			return null;
	}

	@Override
	public int getPupId() {
		return doc.pupID;
	}

	@Override
	public int getRegionCode() {
		if (doc.regionCode != null)
			return doc.regionCode;
		else
			return regionCodeFromWmoSiteInfo;
	}

	@Override
	public String getWmoSiteID() {
		return doc.wmoSiteID;
	}

	@Override
	public boolean isCollectionEnabled() {
		return doc.collectionEnabled;
	}

	@Override
	public boolean isTdwrCollectionLimited() {
		// TODO Auto-generated method stub
		return doc.tdwrCollectionLimited;
	}
	
	@Override
	public boolean isDecompressProducts() {
		return doc.decompressProducts;
	}

	void setDoc(ConfigDoc doc) {
		this.doc = doc;
	}

	public void setRadars(HashMap<String, RadarConfig> radars) {
		this.radars = radars;
	}

	// TODO: use enum, map
	private static String[] modeNames = new String[3];
	static {
		modeNames[GSM.OP_MODE_CLEAR_AIR] = "clear-air";
		modeNames[GSM.OP_MODE_STORM] = "storm";
		modeNames[GSM.OP_MODE_MAINTENANCE] = "maint";
	}

	@Override
	public RpsList getLocalRpsList(String radarID, int opMode, int vcp, int[] cuts) {
		RadarConfig rc = getConfigForRadar(radarID);
		if (rc == null)
			return null;

		// Two passes: First the local custom list, then the default list
		// TODO: are default lists also in the drop-ins directory?
		for (int i = 0; i < 2; ++i) {
			String resName;
			boolean isDefaultBaselineFile = i == 1;
			try {
				String selectorName = i == 0 ?
						radarID.toUpperCase() : "KXXX";
				if (opMode != GSM.OP_MODE_MAINTENANCE)
					resName = String.format("%s.%s.VCP%d", selectorName,
							modeNames[opMode], vcp);
				else
					resName = String.format("%s.%s", selectorName, 
							modeNames[opMode]);
			} catch (ArrayIndexOutOfBoundsException e) {
				return null;
			}
			
			RpsList rpsList = getRpsListRes(res.getDropInPath(resName), opMode, vcp);
			if (rpsList != null) {
				if (isDefaultBaselineFile && (vcp == 80 || vcp == 90) && cuts != null)
					rpsList = Awips1RpsListUtil.maybeTransformForTDWR(rc, rpsList, cuts);
				return rpsList;
			}
		}
		
		return null;
	}

	// package radarconfig.getypte .. based on id..

	@Override
	public RpsList getNationalRpsList(String radarID, int opMode, int vcp,
			int[] cuts) {
		
		/* The original AWIPS 1 code had some strange way of determining the
		 * appropriate national RPS list based on the "line type" which 
		 * was derived from the maximum RPS list size (!?) which could only
		 * be determined after connecting.
		 * 
		 * However, two types (rpgop and associated -- both x.25 (?)) are no
		 * longer used because x.25 is not used for dedicated connections
		 * anymore.  That leaves us with only tcp-rpgop and spg???.
		 * That means we can differentiate solely on the radar type.
		 * 
		 * This comment belongs somewhere else.
		 */
		
		
		RadarConfig rc = getConfigForRadar(radarID);
		if (rc == null)
			return null;

		RadarType type = Util.getRadarType(rc);

		String resName = null;

		if (type == RadarType.WSR) {
			if (opMode == GSM.OP_MODE_CLEAR_AIR)
				resName = "rps-RPGOP-tcp.clear-air";
			else if (opMode == GSM.OP_MODE_STORM)
				resName = "rps-RPGOP-tcp.storm";
		} else if (type == RadarType.TDWR)
			resName = "rps-SPGOP-tcp.storm";
		if (resName == null)
			return null;

		RpsList rpsList = getRpsListRes(res.getDropInPath(resName), opMode, 0);

		/* We are still tied up with the TDWR template processing logic... */
		return Awips1RpsListUtil.maybeTransformForTDWR(rc, rpsList, cuts);
	}

	private RpsList getRpsListRes(File dropInPath, int opMode, int vcp) {
		if (!dropInPath.isFile()) {
			/* TODO: not quite an error for local list because we can
			 * use the default.  Also, there are currently no RPS lists
			 * for ASRs.
			 */
			Log.errorf("RPS list does not exist: %s", dropInPath);
			return null;
		}

		Exception exc;

		try {
			FileInputStream fis = new FileInputStream(dropInPath);
			ByteBuffer buf;
			try {
				FileChannel fc = fis.getChannel();
				buf = ByteBuffer.allocate((int) fc.size());
				fc.read(buf);
			} finally {
				fis.close();
			}

			return Util.parseRpsListData(buf.array(), opMode, vcp);
		} catch (IOException e) {
			exc = e;
		} catch (JAXBException e) {
			exc = e;
		} catch (RuntimeException e) { // TODO: placement
			exc = e;
		}

		Log.errorf("Failed to load RPS list '%s': %s", dropInPath, exc);
		return null;

	}

	@Override
	public String getAwips1Endpoint() {
		if (doc.awips1Endpoint != null)
			return doc.awips1Endpoint;
		else
			return System.getProperty(generalPropBase + ".awips1RadarEndpoint");
	}
	
	public EndpointConfig getEndpointConfig() {
		return doc.endpointConfig; 
	}

	@Override
	public String getEdexEndpoint() {
		if (doc.edexEndpoint != null)
			return doc.edexEndpoint;
		else
			return System.getProperty(generalPropBase + ".edexRadarEndpoint");
	}

	int getRegionCodeFromWmoSiteInfo() {
		return regionCodeFromWmoSiteInfo;
	}

	void setRegionCodeFromWmoSiteInfo(int regionCodeFromWmoSiteInfo) {
		this.regionCodeFromWmoSiteInfo = regionCodeFromWmoSiteInfo;
		/* There is currently no need to send a configuration event for this
		 * because the value is always queried from the configuration when
		 * it is used.
		 */
	}

	// TODO: Should not have to care about the opMode...
	private static final HashMap<Integer, Integer> vcpToOpMode;
	static {
		vcpToOpMode = new HashMap<Integer, Integer>();
		vcpToOpMode.put(31, GSM.OP_MODE_CLEAR_AIR);
		vcpToOpMode.put(32, GSM.OP_MODE_CLEAR_AIR);
		final int[] stormVcps = { 11, 12, 21, 121, 211, 212, 221, 80, 90 };
		for (int vcp : stormVcps)
			vcpToOpMode.put(vcp, GSM.OP_MODE_STORM);
	}
	
	@Override
	public void setLocalRpsList(String radarID, RpsList list)
			throws IOException {
		int vcp = list.getVcp(); 
		if (vcp < 1)
			throw new IOException("Invalid VCP " + vcp);
		Integer opMode = vcpToOpMode.get(vcp);
		if (opMode == null)
			throw new IOException("Cannot determine operation mode for VCP " + vcp);

		String resName = String.format("%s.%s.VCP%d", radarID.toUpperCase(),
				modeNames[opMode], vcp);
		Marshaller m = RpsXml.getMarshaller();
		File f = res.getDropInPath(resName);
		try {
			synchronized (m) {
				m.marshal(list, f);
			}
		} catch (JAXBException e) {
			throw new IOException(e);
		}
	}

	@Override
	public boolean setGlobalConfig(Globals globals) {
		doc.collectionEnabled = globals.collectionEnabled;
		doc.tdwrCollectionLimited = globals.tdwrCollectionLimited;
		doc.decompressProducts = globals.decompressProducts;
		doc.edexEndpoint = globals.edexEndpoint;
		doc.pupID  = globals.pupID;
		doc.wmoSiteID = globals.wmoSiteID;
		doc.endpointConfig = globals.endpointConfig;

		// doc.regionCode = globals.regionCode; // Setting this directly is not supported...
		doc.regionCode = null;
		provider.updateRegionCode();

        boolean result = saveConfig();
        if (configurationEventTarget != null) {
            ConfigEvent ev = new ConfigEvent(Category.GLOBAL_CONFIG);
            configurationEventTarget.handleConfigEvent(ev);
        }
        return result;
	}

	private boolean saveConfig() {
		Exception exc = null;
		try {
			this.provider.storeConfiguration(this);
		} catch (JAXBException e) {
			exc = e;
		}

		if (exc == null) {
			Log.event("Configuration saved");
			return true;
		} else {
			Log.errorf("Could not save configuration: %s", exc.getMessage());
			return false;
		}
	}

	@Override
	public boolean addRadarConfig(RadarConfig rc) {
		Log.errorf("Adding radars is unsupported");
		return false;
	}

	@Override
	public boolean removeRadarConfig(String radarID) {
		Log.errorf("Removing radars is unsupported");
		return false;
	}

	@Override
	public boolean setRadarConfig(RadarConfig rc) {
		RadarConfig oldConfig = radars.get(rc.getRadarID());
		if (oldConfig != null) {
			radars.put(rc.getRadarID(), rc);

            boolean result = saveConfig();
            if (configurationEventTarget != null) {
                ConfigEvent ev = new ConfigEvent(rc.getRadarID(), oldConfig, rc);
                configurationEventTarget.handleConfigEvent(ev);
            }
            return result;
		} else {
			Log.errorf("Attempt to change configuration of unknown radar '%s'", rc.getRadarID());
			return false;
		}
	}

	@Override
	public InputStream getPersistedData(String name) throws IOException {
		return new FileInputStream(res.getPrivatePath(name));
	}

	@Override
	public void putPersistedData(String name, byte[] data) throws IOException {
		File tmpFile = File.createTempFile("sct", null, res.getPrivateDir());
		try {
			FileOutputStream fos = new FileOutputStream(tmpFile);
			try {
				fos.write(data);
				fos.close();
			} finally {
				try {
					fos.close();
				} catch (IOException e) {
					// nothing
				}
			}
			File targetFile = res.getPrivatePath(name);
			targetFile.delete();
			tmpFile.renameTo(targetFile);
		} finally {
			tmpFile.delete();
		}
	}

	@Override
	public void removePersistedData(String name) {
		res.getPrivatePath(name).delete();
	}

    @Override
    public InputStream getDropInData(String name) throws IOException {
        return new FileInputStream(res.getDropInPath(name));
    }

    /*package*/ void notifyNationalRpsLists() {
        if (configurationEventTarget != null) {
            ConfigEvent ev = new ConfigEvent(Category.NATIONAL_RPS_LISTS);
            configurationEventTarget.handleConfigEvent(ev);
        }
    }

    public boolean storeConfigFile(String name, byte[] data) {
        // Delegate to provider
        return provider.storeNdmConfigFile(name, data);
    }

    public RadarEventListener getConfigurationEventTarget() {
        return configurationEventTarget;
    }

    public void setConfigurationEventTarget(
            RadarEventListener configurationEventTarget) {
        this.configurationEventTarget = configurationEventTarget;
    }

}
