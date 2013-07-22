/*
 * com.raytheon.edex.plugin.grib.filenameprocessor.NcgribModelNameMap
 * 
 * 29 May 2013
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package com.raytheon.edex.plugin.grib.filenameprocessor;

import java.io.File;
import java.util.HashMap;
import java.util.regex.Pattern;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.SerializationUtil;

/**
 * Class to hold the model name look up map for ncgrib data file
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 05/13			995		B. Yin   	Initial Creation.
 * </pre>
 * 
 * @author B. Yin
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class NcgribModelNameMap  implements ISerializableObject {
	
	/*
	 * Mapping between file name templates and model names
	 */
    private HashMap<String, String> mapping;
    
    /**
     * Load the ncgrib model name mapping between file name templates and model names.
     * @return NcgribModelNameMap
     * @throws Exception
     */
    public static NcgribModelNameMap load() throws Exception{
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext ctx = pathMgr.getContext(
                LocalizationContext.LocalizationType.EDEX_STATIC,
                LocalizationContext.LocalizationLevel.BASE);
                
    	NcgribModelNameMap map = null;
        try {
            File xmlFile = pathMgr.getFile(ctx, "/grib/ncgrib/ncgribModelNameMap.xml");
        	 map = (NcgribModelNameMap) SerializationUtil
                    .jaxbUnmarshalFromXmlFile(NcgribModelNameMap.class, xmlFile.getAbsolutePath());
        } catch (Exception e) {
        	throw e;
        }
        return map;
    }
 
    /**
     * Gets the model name from the mapping table for the specified file name.
     * @param fileName
     * @return model name
     */
    public String getModelName( String fileName ){
    	String model = null;
    	
    	for ( String namePattern : mapping.keySet() ){
    		if ( Pattern.matches( namePattern, fileName)){
    			model = mapping.get(  namePattern );
    			break;
    		}
    	}
    	
    	return model;
    }
}
