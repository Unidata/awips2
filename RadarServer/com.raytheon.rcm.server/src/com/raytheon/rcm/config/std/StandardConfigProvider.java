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
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.NoSuchElementException;
import java.util.Scanner;
import java.util.regex.Pattern;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import com.raytheon.rcm.config.Configuration;
import com.raytheon.rcm.config.ConfigurationProvider;
import com.raytheon.rcm.config.LinkResource;
import com.raytheon.rcm.config.LinkType;
import com.raytheon.rcm.config.RadarConfig;
import com.raytheon.rcm.config.StandardProductDistInfoDB;
import com.raytheon.rcm.config.awips1.Awips1ConfigProvider;
import com.raytheon.rcm.server.Log;


/**
 * Constructs (and potentially updates) a StandardConfig based on
 * various configuration files.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ...
 * 2014-02-03   DR 14762   D. Friedman Handle updated NDM config files.
 * </pre>
 *
 */
public class StandardConfigProvider implements ConfigurationProvider {
		
    private static String WSR_88D_PROD_LIST_NAME = "prodList.txt";
    private static String TDWR__PROD_LIST_NAME = "tdwrProdList.txt";
    private static String WMO_SITE_INFO_NAME = "wmoSiteInfo.txt";

	private static JAXBContext jaxbContext;
	private static Unmarshaller u;
	private static Marshaller m;
		
	static {
		try {
			jaxbContext = JAXBContext.newInstance(ConfigDoc.class);
			u = jaxbContext.createUnmarshaller();
			m = jaxbContext.createMarshaller();
			m.setProperty("jaxb.formatted.output", true);
			/*
			SchemaOutputResolver s = new SchemaOutputResolver() {

				@Override
				public Result createOutput(String namespaceUri,
						String suggestedFileName) throws IOException {
					return new StreamResult(new File("/tmp", suggestedFileName));
				}
				
			};
			try {
				jaxbContext.generateSchema(s);
			} catch (IOException e) {
				e.printStackTrace(System.err);
			}
			*/
		} catch (JAXBException e) {
			Log.errorf("%s", e);
		}
	}
	
	private ConfigRes res = new ConfigRes();
	
	StandardConfig config = new StandardConfig(res, this);
	
	public StandardConfigProvider() {
		
		ConfigDoc doc = null;
		
		// TODO: if it fails, do we quit or try to continue?...
		try {
			doc = (ConfigDoc) u.unmarshal(getMainConfigFile());
		} catch (JAXBException e) {
			Log.errorf("Error loading configuration: %s", e);
		}
		
		// TODO: ??
		if (doc.wmoSiteID != null)
			doc.wmoSiteID = doc.wmoSiteID.toUpperCase();
		
		config.setDoc(doc);
		
		if (doc.radars != null) {
			HashMap<String,RadarConfig> radars = 
				new HashMap<String,RadarConfig>(doc.radars.length); 
			
			for (RadarConfig rc : doc.radars) {
				/* Only one link type is supported now and this may never change.
				 * Make TCP/WAN the default.
				 */			
				if (rc.getLinkType() == null)
					rc.setLinkType(LinkType.TCP_WAN);
				for (LinkResource lr : rc.getLinkResources())
					if (lr.getLinkType() == null)
						lr.setLinkType(LinkType.TCP_WAN);
				
				radars.put(rc.getRadarID(), rc);
			}
			
			config.setRadars(radars);
		}

		loadProdListDB();

		updateRegionCode();
	}

    private void loadProdListDB() {
        StandardProductDistInfoDB db = new StandardProductDistInfoDB();
        tryAddProdList(db, false, WSR_88D_PROD_LIST_NAME);
        tryAddProdList(db, true, TDWR__PROD_LIST_NAME);
        config.setProdDistInfoDB(db);
    }

    // TODO: Duplicating code from Awips1ConfigProvider
	protected static boolean skipComments(Scanner s) {
		try {
			s.skip("^\\s*(#|//).*$");
		} catch (NoSuchElementException e) {
			// nothing
		}
		return ! s.hasNext(); // Also returns true if it was just a blank line
	}
	
	public void updateRegionCode() {
		try {
			readWmoSiteInfo(res.getNdmFile(WMO_SITE_INFO_NAME));
		} catch (Exception e) {
			Log.errorf("Could not process %s.txt: %s", WMO_SITE_INFO_NAME, e);
		}
	}

	// TODO: duplicates code in Awips1ConfigProvider
	private void readWmoSiteInfo(File f) throws FileNotFoundException {
		String wmoSiteId = config.getWmoSiteID();
		// need wmoSiteId.txt to read this...
		if (wmoSiteId == null || wmoSiteId.length() == 0) {
			Log.errorf("WMO site ID not set");
			return;
		}
		
		Scanner fs = new Scanner(f);
		try {
			while (fs.hasNextLine()) {
				Scanner ls = new Scanner(fs.nextLine());
				if (skipComments(ls))
					continue;
				if (! wmoSiteId.equalsIgnoreCase( ls.next() ))
					continue;
				config.setRegionCodeFromWmoSiteInfo(ls.nextInt());
			}
		} finally {
			fs.close();
		}
	}

	
	private void tryAddProdList(StandardProductDistInfoDB db, boolean isTDWR, String fileName) {
		try {			
			Awips1ConfigProvider.readProdListFile(res.getNdmFile(fileName), db, isTDWR);
		} catch (Exception e) {
			Log.errorf("Could not load product dist info file '%s': %s", fileName, e);
		}
	}
	
	@Override
	public Configuration getConfiguration() {
		return config;
	}

	@Override
	public void refresh() {
		// TODO Auto-generated method stub
		
	}
	
	public static Marshaller getMarshaller() {
		return m;
	}
			
	public void storeConfiguration(StandardConfig config) throws JAXBException {
		ConfigDoc doc = new ConfigDoc();		
		doc.collectionEnabled = config.isCollectionEnabled();
		doc.decompressProducts = config.isDecompressProducts();
		doc.edexEndpoint = config.getEdexEndpoint();
		doc.pupID = config.getPupId();
		doc.regionCode = config.getRegionCode();
		doc.tdwrCollectionLimited = config.isTdwrCollectionLimited();
		doc.wmoSiteID = config.getWmoSiteID();		
		doc.endpointConfig = config.getEndpointConfig(); 
		doc.radars = new RadarConfig[config.getConfiguredRadarList().size()];
		int i = 0;
		for (String id : config.getConfiguredRadarList())
			doc.radars[i++] = config.getConfigForRadar(id);
		
		m.marshal(doc, getMainConfigFile());
	}
	
	private File getMainConfigFile() {
		return new File(res.getPrivateDir(), "config.xml");
	}

    public boolean storeNdmConfigFile(String name, byte[] data) {
        File f = new File(name);
        if (f.getParent() != null) {
            Log.error("Attempt to store NDM config file with a specific directory: " + name);
            return false;
        }

        File path = res.getDropInPath(name);
        try {
            FileOutputStream fos = new FileOutputStream(path);
            try {
                fos.write(data);
            } finally {
                try {
                    fos.close();
                } catch (IOException e) {
                    // ignore
                }
            }
        } catch (IOException e) {
            Log.errorf("Failed to store NDM config file %s: %s", path, e);
            return false;
        }

        Log.eventf("Stored new version of NDM config %s.", name);

        if (WSR_88D_PROD_LIST_NAME.equals(name) ||
                TDWR__PROD_LIST_NAME.equals(name)) {
            loadProdListDB();
        } else if (WMO_SITE_INFO_NAME.equals(name)) {
            updateRegionCode();
        } else if (Pattern.matches("^rps-.*OP.*$", name)) {
            config.notifyNationalRpsLists();
        } else {
            Log.warnf("No action taken for new %s.  You may need to restart for changes to take affect.", name);
        }

        return true;
    }
}
