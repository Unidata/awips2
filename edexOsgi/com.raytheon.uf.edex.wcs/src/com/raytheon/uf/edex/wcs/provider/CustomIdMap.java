/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.wcs.provider;

import java.io.File;
import java.io.FileInputStream;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Mapping of custom identifiers to internally generated URNs
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 31, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class CustomIdMap {

	private static final Map<String, String> externalToInternal;

	private static final Map<String, String> internalToExternal;

	private static final IUFStatusHandler log = UFStatus
			.getHandler(CustomIdMap.class);

	static {
		LocalizationFile idMapFile = findConfigFile();
		Map<String, String> map;
		if (idMapFile == null) {
			log.warn("Unable to find custom id mapping properties");
			map = new HashMap<String, String>(0);
		} else {
			map = loadConfig(idMapFile);
		}
		externalToInternal = Collections.unmodifiableMap(map);
		internalToExternal = Collections.unmodifiableMap(reverseMap(map));
	}

	/**
	 * Find configuration file in localization
	 * 
	 * @return null if not found
	 */
	private static LocalizationFile findConfigFile() {
		IPathManager pm = PathManagerFactory.getPathManager();
		LocalizationContext[] searchHierarchy = pm
				.getLocalSearchHierarchy(LocalizationType.EDEX_STATIC);

		for (LocalizationContext ctx : searchHierarchy) {
			LocalizationFile localizationFile = pm.getLocalizationFile(ctx,
					"wcs" + IPathManager.SEPARATOR
							+ "wcsIdentifier.properties");
			if (localizationFile.exists()) {
				return localizationFile;
			}
		}
		return null;
	}

	/**
	 * Load map from config
	 * 
	 * @param idMapFile
	 * @return
	 */
	private static Map<String, String> loadConfig(LocalizationFile idMapFile) {
		Properties props = new Properties();
		File file = idMapFile.getFile();
		try {
			props.load(new FileInputStream(file));
		} catch (Exception e) {
			log.error("Unable to load WCS id map: " + file, e);
			return new HashMap<String, String>(0);
		}
		// TODO validate entries
		Map<String, String> rval = new HashMap<String, String>(props.size());
		for (Entry<Object, Object> e : props.entrySet()) {
			rval.put(e.getKey().toString(), e.getValue().toString());
		}
		return rval;
	}

	/**
	 * Return a map of values to keys.
	 * 
	 * @param map
	 * @return
	 */
	private static Map<String, String> reverseMap(Map<String, String> map) {
		Map<String, String> rval = new HashMap<String, String>(map.size());
		for (Entry<String, String> e : map.entrySet()) {
			rval.put(e.getValue(), e.getKey());
		}
		return rval;
	}

	/**
	 * @param id
	 * @return id if no mapping exists
	 */
	public static String internalToExternal(String id) {
		String rval = internalToExternal.get(id);
		if (rval == null) {
			return id;
		}
		return rval;
	}

	/**
	 * @param id
	 * @return id if no mapping exists
	 */
	public static String externalToInternal(String id) {
		String rval = externalToInternal.get(id);
		if (rval == null) {
			return id;
		}
		return rval;
	}

}
