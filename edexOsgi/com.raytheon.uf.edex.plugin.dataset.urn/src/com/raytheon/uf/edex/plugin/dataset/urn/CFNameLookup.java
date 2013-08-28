/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */

package com.raytheon.uf.edex.plugin.dataset.urn;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Singleton to facilitate the lookup of the CF Standard name from a shortened 
 * name such as those in GRIB processing.  Also maintains a mapping of the CF names 
 * to ontology individuals to facilitate dynamic ontology updates and individual
 * creation.
 * @author behemmi
 *
 */
public class CFNameLookup {
	
	private static CFNameLookup instance;
	
    private IUFStatusHandler log = UFStatus.getHandler(this.getClass());

	private Map<String, String> ncepToCFMapping;

    protected Map<String, String> cfToNcepMapping;
	private Map<String, String> cfToOntologyRelationMapping;
	private Map<String, String> cfToOntologyClassMapping;
	
	private CFNameLookup(CFNameResource resource) {
		ncepToCFMapping = new HashMap<String, String>();
		cfToOntologyRelationMapping = new HashMap<String, String>();
		cfToOntologyClassMapping = new HashMap<String, String>();
        cfToNcepMapping = new HashMap<String, String>();
		
		try {
			if (resource.getConfigFileResource() != null) {
				InputStream configStream = resource.getConfigFileResource().getInputStream();
				BufferedReader in = new BufferedReader(new InputStreamReader(configStream));
				String line = null;
	
				while((line = in.readLine()) != null) {
					if(line.startsWith("#") || line.length() == 0) {
						//comment, ignore
						continue;
					}
					
					//limit splitting to 2 to account for multiple tabs and trim for map insert
				    String[] parts = line.split("\t", 2);
				    if(parts != null && parts.length > 1) {

				    	//check for an Ontology relation after the CF Name
				    	String[] ontRelParts = parts[1].trim().split("\t", 2);
				    	if(ontRelParts != null && ontRelParts.length > 1) {

				    		//check for an Ontology class after the Ontoogy relation
				    		String[] ontClassParts = ontRelParts[1].trim().split("\t", 2);
					    	if(ontClassParts != null && ontClassParts.length > 1) {
					    		//this line has everything, populate all the HashMaps
					    		cfToOntologyClassMapping.put(ontRelParts[0].trim(), ontClassParts[1].trim());
					    		cfToOntologyRelationMapping.put(ontRelParts[0].trim(), ontClassParts[0].trim());
					    		ncepToCFMapping.put(parts[0].toUpperCase().trim(), ontRelParts[0].trim());
                                cfToNcepMapping.put(ontRelParts[0].trim(),
                                        parts[0].toUpperCase().trim());
					    	} else {
					    		//only entries up the relation are present
					    		cfToOntologyRelationMapping.put(ontRelParts[0].trim(), ontRelParts[1].trim());
					    		ncepToCFMapping.put(parts[0].toUpperCase().trim(), ontRelParts[0].trim());
                                cfToNcepMapping.put(ontRelParts[0].trim(),
                                        parts[0].toUpperCase().trim());
                                log.warn("CF Name "
                                        + ontRelParts[0].trim()
                                        + " not mapped to Ontology Class in Config");
					    	}
				    	} else {
				    		//only the cf and ncep entries are on this line
				    		ncepToCFMapping.put(parts[0].toUpperCase().trim(), parts[1].trim());
                            cfToNcepMapping.put(parts[1].trim(), parts[0]
                                    .toUpperCase().trim());
                            log.warn("CF Name " + parts[1].trim()
                                    + " not mapped to Ontology Info in Config");
				    	}
				    } else {
                        log.warn("NCEP Name " + parts[0]
                                + " not mapped to CF Name in Config");
				    }
				}
			} else {
				throw new IOException("Config File Resource null");
			}
		} catch (IOException e) {
            log.error("Configured mapping to CF Names not available", e);
		}
		instance = this;
	}

	public static CFNameLookup getInstance(){
		//expecting instantiation from spring so no 'new' call here 
		//like in the normal singleton pattern
		return instance;
	}

    /**
     * Returns the CF Name if available, defaults to the ncep name otherwise
     * 
     * @param ncepName
     * @return
     */
	public String getCFFromNCEP(String ncepName) {
		String cfName = ncepName;
		if(ncepToCFMapping.containsKey(ncepName.toUpperCase())) {
			cfName = ncepToCFMapping.get(ncepName.toUpperCase());
		} 
		return cfName;
	}
	
    /**
     * Returns the NCEP name if available, defaults to the cf name otherwise
     * 
     * @param cfName
     * @return
     */
    public String getNCEPFromCF(String cfName) {
        String ncepName = cfName;
        if (cfToNcepMapping.containsKey(cfName)) {
            ncepName = cfToNcepMapping.get(cfName).toUpperCase();
        }

        return ncepName;
    }

	/**
	 * Looks up the associated ontology axiom for a given CF Name, returns null
	 * if no entities are associated
	 * @param cfName
	 * @return
	 */
	public String getOntologyRelationFromCF(String cfName) {
		return cfToOntologyRelationMapping.get(cfName);
	}
	
	/**
	 * Looks up the associated ontology class for a given CF Name, returns null
	 * if no entities are associated
	 * @param cfName
	 * @return
	 */
	public String getOntologyclassFromCF(String cfName) {
		return cfToOntologyClassMapping.get(cfName);
	}
	
}
