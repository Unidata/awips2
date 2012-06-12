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

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Scanner;

import com.raytheon.rcm.config.*;
import com.raytheon.rcm.config.awips1.Awips1RpsListUtil;
import com.raytheon.rcm.config.awips1.Awips1RpsListUtil.Selector;
import com.raytheon.rcm.message.GSM;
import com.raytheon.rcm.request.Request;
import com.raytheon.rcm.request.RpsList;
import com.raytheon.rcm.server.Log;


/**
 * Builds a {@link com.raytheon.rcm.config.Configuration} instance based on a set of 
 * configuration files in the AWIPS 1 format and directory structure.
 * 
 * The root directory of the AWIPS 1 directory structure must be specified
 * by the com.raytheon.rcm.config.awips1.resourceRoot property.  It is also 
 * necessary to define the FXA_LOCAL_SITE environment variable or the 
 * com.raytheon.rcm.config.awips1.FXA_LOCAL_SITE property.  
 * 
 * Note: Does not recognize the FILE_SERVER_DEFAULT_PATHS environment variable.
 * 
 * <pre>
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  2009                     dfriedma    Initial version
 *  2012-04-30   DR 14904    D. Friedman Add backup links to dial ORPGs.
 * </pre>
 */
public class Awips1ConfigProvider implements ConfigurationProvider {
	
	final String generalPropBase = "com.raytheon.rcm";
	final String propBase = generalPropBase + ".awips1";

	String resourceRoot;
	String fxaHome;
	String fxaLocnRoot;
	String fxaNatlConfigData;
	String fxaLocalSite;
	String fxaData;
	
	HashMap<String, Awips1RadarConfig> radars;
	HashMap<String, ArrayList<LinkResource>> radarLinkResources;
	ArrayList<File> searchPaths = new ArrayList<File>();
	
	Map<RadarType, RpsList[]> nationalRpsLists = new HashMap<RadarType, RpsList[]>();
	
	public Awips1ConfigProvider() {
		// TODO: first argument of File ctor can be null
		resourceRoot = System.getProperty(propBase + ".resourceRoot", "/");
		fxaHome = getEnv("FXA_HOME");
		if (fxaHome == null)
			fxaHome = "/awips/fxa";
		fxaLocnRoot = getEnv("FXA_LOCALIZATION_ROOT");
		if (fxaLocnRoot == null)
			fxaLocnRoot = fxaHome + "/data/localizationDataSets";
		fxaNatlConfigData = getEnv("FXA_NATL_CONFIG_DATA");
		if (fxaNatlConfigData == null)
			fxaNatlConfigData = fxaHome + "/data/localization";
		fxaData = getEnv("FXA_DATA");
		if (fxaData == null)
			fxaData = "/data/fxa";
		fxaLocalSite = getEnv("FXA_LOCAL_SITE");

		if (fxaLocalSite != null)
			searchPaths.add(new File(rootRel(fxaLocnRoot), fxaLocalSite));
		// else, warn // or use subdir if only one exists
		searchPaths.add(new File(rootRel(fxaNatlConfigData), "nationalData"));
		searchPaths.add(new File(rootRel(fxaHome), "data"));
	}
	
	File rootRel(String f) {
		return new File(resourceRoot, f);
	}

	Awips1Config config;

	public Configuration getConfiguration() {

		config = new Awips1Config();
		
		radars = new HashMap<String, Awips1RadarConfig>(); 

		File f;
		try {
			f = getIfsResource("pupId.txt");
			Scanner s = new Scanner(f);
			try {
				config.setPupId(s.nextInt());
			} finally {
				s.close();
			}
		} catch (Exception e) {
			// TODO: log...
		}

		try {
			f = getIfsResource("radarsInUse.txt");
			readRadarsInUse(f);
		} catch (Exception e) {
			// TODO: log
			e.printStackTrace();
		}
		
		radarLinkResources = new HashMap<String, ArrayList<LinkResource>>();
		for (String name : radars.keySet())
			radarLinkResources.put(name, new ArrayList<LinkResource>());
		
		try {
			f = getIfsResource("wmoSiteInfo.txt");
			readWmoSiteInfo(f);
		} catch (Exception e) {
			Log.errorf("Could not read wmoSiteInfo.txt: %s", e);
		}
		
		tryReadOTRFile("orpgDedicated.txt", false, true);
		tryReadOTRFile("orpgBackups.txt", true, true);
		tryReadOTRFile("orpgOTRs.txt", false, false);
		
		LinkResource[] lr = new LinkResource[0]; 
		for (String name : radars.keySet()) {
			RadarConfig rc = radars.get(name);
			rc.setLinkResources(radarLinkResources.get(name).toArray(lr));
		}

		readNationalRpsLists();
		findRpsLists();
			
		config.setRadars(radars);
		
		StandardProductDistInfoDB db = new StandardProductDistInfoDB();
		tryAddProdList(db, false, "prodList.txt");
		tryAddProdList(db, true, "tdwrProdList.txt");
		
		config.setProdDistInfoDB(db);
		
		boolean globalSendFlag = true;
		try {
			f = new File(new File(rootRel(fxaData), "workFiles"), "RadarStorage.StateInfo");
			Scanner s = new Scanner(f);
			try {
				if (s.nextInt() == 0)
					globalSendFlag = false;
			} finally {
				s.close();
			}
		} catch (FileNotFoundException e) {
			// This is an optional file, so ignore this exception.
		} catch (Exception e) {
			// TODO: log...
		}
		config.setCollectionEnabled(globalSendFlag);
		
		boolean limitTDWRCollectionBandwidth = true;
		try {
			f = new File(new File(rootRel(fxaData), "workFiles"), "tdwrWanFlag.txt");
			Scanner s = new Scanner(f);
			try {
				limitTDWRCollectionBandwidth = s.nextInt() != 0; 
			} finally {
				s.close();
			}
		} catch (FileNotFoundException e) {
			// This is an optional file, so ignore this exception.
		} catch (Exception e) {
			// TODO: log...
		}
		
		config.setTdwrCollectionLimited(limitTDWRCollectionBandwidth);

		String s = getEnv("decompressRadarProducts");
		if (s != null && s.length() > 0)
			config.setDecompressProducts(true);
		
		String pathName;
		
		pathName = System.getProperty(generalPropBase + ".edexRadarEndpoint");
		config.setEdexEndpoint(pathName);

		pathName = System.getProperty(generalPropBase + ".awips1RadarEndpoint");
		config.setAwips1Endpoint(pathName);

		return config;
	}
	
	private void tryAddProdList(StandardProductDistInfoDB db, boolean isTDWR, String fileName) {
		try {			
			readProdListFile(getIfsResource(fileName), db, isTDWR);
		} catch (Exception e) {
			Log.errorf("Could not load product dist info file '%s': %s", fileName, e);
		}
	}
	
	public static void readProdListFile(File f, StandardProductDistInfoDB db, boolean isTDWR) throws NoSuchElementException, FileNotFoundException {
		Scanner fs = new Scanner(f);
		
		try {
			while (fs.hasNext()) {
				Scanner ls = new Scanner(fs.nextLine());
				if (skipComments(ls))
					continue;
				try {
					ProductDistributionInfo info = new ProductDistributionInfo();
					
					int messageCode = ls.nextInt();
					int elev = ls.nextInt();				
					info.setNnn(ls.next());
					info.setTtaai(ls.next());
					
					Awips1ProdDistInfoBuilder.addProdListEntry(f, messageCode, elev, isTDWR, info, db);
				} catch (NoSuchElementException e) {
				}
			}
		} finally {
			fs.close();
		}
	}
	
	private RpsList getNatRpsList(int opMode, String name) {
		Request[] reqs;
		try {
			Scanner s = new Scanner(getIfsResource(name));
			try {
				reqs = Awips1RpsListUtil.parse(s);
			} finally {
				s.close();
			}
		} catch (Exception e) {
			Log.errorf("Unable to load national RPS list '%s'", name);
			reqs = new Request[0];
		}
			
		return new RpsList(opMode, 0, reqs); 
	}
	
	private void readNationalRpsLists() {
		// TODO: what national RPS list to use when in maintenance mode???
		RpsList[] wsrLists = new RpsList[2];
		wsrLists[0] = getNatRpsList(GSM.OP_MODE_CLEAR_AIR, "rps-RPGOP-tcp.clear-air");
		wsrLists[1] = getNatRpsList(GSM.OP_MODE_STORM, "rps-RPGOP-tcp.storm");
		nationalRpsLists.put(RadarType.WSR, wsrLists);
		
		RpsList[] tdwrLists = new RpsList[1];
		tdwrLists[0] = getNatRpsList(GSM.OP_MODE_STORM, "rps-SPGOP-tcp.storm");
		nationalRpsLists.put(RadarType.TDWR, tdwrLists);
	}
	
	private void findRpsLists() {
		// TODO Auto-generated method stub
		Map<String,List<RpsList>> rpsLists = new HashMap<String,List<RpsList>>();
		for (String name : radars.keySet())
			rpsLists.put(name, new ArrayList<RpsList>());
		
		File dir = new File(new File(rootRel(fxaData), "radar"), "lists");
		File[] listing = dir.listFiles();
		if (listing == null) {
			Log.errorf("Cannot read rps list directory '%s'", dir);
			return;
		}
		for (File f : listing) {
			if (! f.isFile())
				continue;
			
			Selector sel = Awips1RpsListUtil.parseName(f.getName());
			if (sel != null && sel.comment == null) {
				RadarConfig rc = radars.get(sel.radar.toLowerCase());
				if (rc == null)
					continue;
				
				Scanner scanner = null;
				RpsList list;
				try {
					try {
						scanner = new Scanner(f); 
					} catch (FileNotFoundException e2) {
						// TODO: log -- what about I/O errors?
						continue;
					}
					list = new RpsList(sel.opMode, sel.vcp, 
							Awips1RpsListUtil.parse(scanner));
				} finally {
					if (scanner != null)
						scanner.close();
				}
				rpsLists.get(rc.getRadarID()).add(list);
			}
		}

		for (String name : radars.keySet()) {
			Awips1RadarConfig rc = radars.get(name);
			rc.setLocalRpsLists(rpsLists.get(name).toArray(new RpsList[0]));
			RpsList[] nats = nationalRpsLists.get(Util.getRadarType(rc));
			if (nats != null)
				rc.setNationalRpsLists(Arrays.copyOf(nats, nats.length));
		}
	}
	
	protected String getEnv(String var) {
		String value = System.getProperty(propBase + "." + var);
		if (value == null)
			value = System.getenv(var);
		return value;
	}

	public void refresh() {
		// TODO Auto-generated method stub

	}

	protected static boolean skipComments(Scanner s) {
		try {
			s.skip("^\\s*(#|//).*$");
		} catch (NoSuchElementException e) {
			// nothing
		}
		return ! s.hasNext(); // Also returns true if it was just a blank line
	}
	
	protected void readRadarsInUse(File f) throws FileNotFoundException {
		Scanner fs = new Scanner(f);
		try {
			while (fs.hasNext()) {
				Scanner ls = new Scanner(fs.nextLine());
				if (skipComments(ls))
					continue;
				try {
					String radarName = ls.next().toLowerCase();
					int nexradId = ls.nextInt();
					int connType = ls.nextInt();
					
					// TODO: dups
					Awips1RadarConfig rc = new Awips1RadarConfig();
					radars.put(radarName, rc);
					rc.setRadarID(radarName);
					rc.setNexradID(nexradId);
					rc.setDedicated(false);
					switch (connType) {
					case 0: // X.25 dial (class2) to non-associated
						// TODO: flag set look for dial resources later
						continue;
						//break;
					case 1:
						// ? X.25 class 1 associated -- no longer supported
						continue;
						//break;
					case 2: // TCP dedicated or X.25 dedicated (but the latter is obsolete)
						rc.setDedicated(true);
						rc.setSendEnvironmentalData(true);
						break;
					case 3: // tcp/ip class 2 to non-associated over wan
						break;
					case 4: // tcp/ip class 1 *and* class2 <-- huh?
						rc.setDedicated(true);
						break;
					default:
						// TODO log unknown type
					}
				} catch (NoSuchElementException e) {
					// TODO log error
				}
			}
		} finally {
			fs.close();
		}
	}

	protected void tryReadOTRFile(String fileName, boolean backup, boolean dedicated) {
		try {
			readOTRFile(getIfsResource(fileName), backup, dedicated);
		} catch (Exception e) {
			Log.errorf("Could not read comms config file '%s': %s", fileName, e);
		}
	}
	
	protected void readOTRFile(File f, boolean backup, boolean dedicated) throws FileNotFoundException {
		Scanner fs = new Scanner(f);

		try {
			while (fs.hasNext()) {
				String line = fs.nextLine();
				Scanner ls = new Scanner(line);
				if (skipComments(ls))
					continue;
				
				try {
					// TODO: comments
					LinkResource lr = new LinkResource();
					String radarName = ls.next().toLowerCase();
					int nexradId = ls.nextInt();
					
					RadarConfig rc = radars.get(radarName.toLowerCase());
					if (rc == null)
						continue;
					if (nexradId != rc.getNexradID()) {
						// warn...
					}
					
					rc.setLinkType(LinkType.TCP_WAN);
					
					String host = ls.next();
					int port = ls.nextInt();
					lr.setLinkType(LinkType.TCP_WAN);
					lr.setLinkAddress(host + ":" + port);
					lr.setLinkIndex(ls.nextInt());
					lr.setTcmPassword(ls.next());
					lr.setDedicated(dedicated);
					
					if (dedicated) {
						lr.setMaxRpsListSize(ls.nextInt());
					} else {
						lr.setUserPassword(ls.next());
						if (ls.hasNext()) {
							lr.setPortPassword(ls.next());
						} else {
							// sometimes the two fields are glommed together
							String field = lr.getUserPassword();
							lr.setUserPassword(field.substring(0, 6));
							lr.setPortPassword(field.substring(6));
						}
					}
					
					radarLinkResources.get(radarName).add(lr);
				} catch (IndexOutOfBoundsException e) {
					// field.substring
					Log.errorf("Bad link config in %s: '%s'", f, line);
				} catch (IllegalArgumentException e) {
					Log.errorf("Bad link config in %s: '%s'", f, line);
					// e.g. Malformed IP address, bad port number...
				} catch (NoSuchElementException e) {
					Log.errorf("Bad link config in %s: '%s'", f, line);
				}
			}			
		} finally {
			fs.close();
		}
	}

	private String getFullName(String namelet, String linespeed)
	{
		// From WmoSiteInfo::getRadarName
		for (String name : radars.keySet()) {
			if (name.substring(1, 4).equalsIgnoreCase(namelet)) {
				int nexradId = radars.get(name).getNexradID();
				if (linespeed.equals("D6")) {
					// TODO: can we really rely on the 3045 number?
					if (nexradId >= 3000 && nexradId <= 3045)
						return name;
				} else {
					if (nexradId < 3000 || (nexradId >= 4000 && nexradId < 5000))
						return name;
				}
			}
		}
		return null;
	}
	
	private void readWmoSiteInfo(File f) throws FileNotFoundException {
		String wmoSiteId = config.getWmoSiteID();
		// need wmoSiteId.txt to read this...
		if (wmoSiteId == null) {
			Scanner s = new Scanner(getIfsResource("wmoSiteId.txt"));
			try {
				wmoSiteId = s.next();
			} finally {
				s.close();
			}
		}
		
		config.setWmoSiteID(wmoSiteId);
		
		Scanner fs = new Scanner(f);

		try {
			while (fs.hasNextLine()) {
				Scanner ls = new Scanner(fs.nextLine());
				if (skipComments(ls))
					continue;
				if (! wmoSiteId.equals( ls.next() ))
					continue;
				config.setRegionCode(ls.nextInt());
				int nRpgs = ls.nextInt(); // naive..
				while (nRpgs-- > 0) {
					String field = ls.next();
					String radarNamelet = field.substring(0, 3);
					String linespeed = field.substring(4, 6);
					boolean sending = field.substring(3, 4).equals("Y");
					/* Logic from WmoSiteInfo::getRadarName except that it uses 
					 * radarsInUse.txt instead of radarMasterInfo.txt. */
					String fullName = getFullName(radarNamelet, linespeed); // maybe return Radarconfig then..
					if (fullName != null) {
						RadarConfig rc = radars.get(fullName);
						rc.setCollectionEnabled(sending);
					}
				}
				break;
			}
		} finally {
			fs.close();
		}
	}

	protected File getIfsResource(String name) throws FileNotFoundException {
		for (File f : searchPaths) {
			File res = new File(f, name);
			if (res.exists())
				return res;
		}
		throw new FileNotFoundException("Cannot find resource \"" + name + "\"");
	}

	/**
	 * How to signal a refresh? Need to set up some kind of listener... Native
	 * signal handler?
	 * 
	 * Two parts: Generate from source Update in response to some signal
	 */
}
