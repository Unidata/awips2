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
package com.raytheon.edex.transform.shef;

import static com.raytheon.uf.common.localization.LocalizationContext.LocalizationType.EDEX_STATIC;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBException;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.ohd.AppsDefaults;
//import com.raytheon.edex.transform.shef.MetarToShefTransformer;
import com.raytheon.uf.edex.decodertools.core.filterimpl.AbstractFilterElement;
import com.raytheon.uf.edex.decodertools.core.filterimpl.AbstractObsFilter;
import com.raytheon.uf.edex.decodertools.core.filterimpl.PluginDataObjectFilter;

/**
 * Use information in metarToShefFilter.xml, MetarToShefFilter filters out
 * the metar messages before send the message to MetarToShefTransformer to 
 * encode to a SHEF message. 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date       Ticket# Engineer Description
 * ---------- ------- -------- --------------------------
 * 1/10/2013  15497   wkwock   Initial creation
 * 
 * </pre>
 * 
 * @author wkwock
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class MetarToShefFilter  {
    @XmlElement
    @DynamicSerializeElement
    protected List<MetarToShefRun> metarToShefRun = new ArrayList<MetarToShefRun>();

    private static final String ERROR_1_FMT = "Could not create {%s} context for file \"%s\"";

    private static final String ERROR_2_FMT = "File %s does not exist";

    private static final String METAR_CFG = "metar.cfg";
    
    public static final String FILTERS_DIR = "plugin-filters";
    
    private String metarToShefOptions = AppsDefaults.getInstance().getToken("metar2shef_options");

    private Log logger = LogFactory.getLog(getClass());

    private String filterConfigFile = null;

    public MetarToShefFilter() {
    }

    public MetarToShefFilter(String configFile, String localContext) {
        filterConfigFile = configFile;
        try {
            File filterDir = null;
            IPathManager manager = PathManagerFactory.getPathManager();
            if(manager != null) {
                LocalizationContext context = manager.getContext(EDEX_STATIC, LocalizationLevel.valueOf(localContext));
                if(context != null) {
                    filterDir = manager.getFile(context, FILTERS_DIR);
                    if (filterDir.exists()) {
                        File srcFile = new File(filterDir, filterConfigFile);

                        byte[] data = new byte[(int) srcFile.length()];

                        InputStream stream = getInputStream(srcFile);
                        try {
                            stream.read(data);
                            stream.close();
                            Object obj = SerializationUtil
                            .unmarshalFromXml(new String(data));
                            if (obj instanceof PluginDataObjectFilter){
                                logger.info("Found "+filterConfigFile+" is PluginDataObjectFilter");
                                PluginDataObjectFilter pdof=(PluginDataObjectFilter) obj;
                                MetarToShefRun mtsr= new MetarToShefRun();
                                mtsr.setConfigFileName(METAR_CFG);
                                mtsr.setMetarToShefOptions(metarToShefOptions);
                                mtsr.setFilterElements(pdof.getFilterElements());
                                mtsr.setFilterName(pdof.getFilterName());
                                this.metarToShefRun.add(mtsr);
                            }else if (obj instanceof MetarToShefFilter) {
                            	MetarToShefFilter filter = (MetarToShefFilter) obj;
                            	this.metarToShefRun=filter.metarToShefRun;
                                logger.info("Found "+filterConfigFile+" is MetarToShefFilter");
                           }else {
                        	   logger.error("Found "+filterConfigFile+" is "+obj.getClass().getCanonicalName());
                               createDummyFilter();
                           }
                        } catch (IOException e) {
                            logger.error("Unable to read filter config", e);
                        } catch (JAXBException e) {
                            logger.error("Unable to unmarshall filter config", e);
                        }
                    } else {
                        logger.error(String.format(ERROR_2_FMT,filterDir.getPath()));
                        createDummyFilter();
                    }
                } else {
                    logger.error(String.format(ERROR_1_FMT, localContext,configFile));
                    createDummyFilter();
                }
            } else {
                // Could not create PathManager
            }
        } catch (Exception e) {
            logger.error(
                    "Error creating filter.", e);
            createDummyFilter();
        }

        for (MetarToShefRun mtsr: metarToShefRun){
        	logger.info("Filter name = " + mtsr.getFilterName()+" with config file: "+mtsr.getConfigFileName());
        }
    }

    private PluginDataObject[] filterARun(PluginDataObject[] reports, List<AbstractFilterElement> filterElements) {
        int reportCount = 0;
        if (reports != null) {

            
            for (int i = 0; i < reports.length; i++) {
                PluginDataObject r = null;
                boolean keep = true;
                for (AbstractFilterElement element : filterElements) {
                    r = element.filter(reports[i]);
                    
                    // Only allow keep to be set to true. Once true it stays that way. 
                    if(AbstractObsFilter.INCLUDE_TYPE.equals(element.getFilterType())) {
                        // Did the filter pass?
                        if(r == null) {
                            // If we fail an element, exit now.  
                            keep = false;
                            break;
                        }
                    } else if(AbstractObsFilter.EXCLUDE_TYPE.equals(element.getFilterType())) {
                        if(r != null) {
                            // There was a match, so we want to remove this item.
                            keep = false;
                            // And there's no reason for further checks.
                            break;
                        }
                    }
                }
                if (keep) {
                    reportCount++;
                } else {
                    reports[i] = null;
                }
            }
        }
        if (reportCount == 0) {
            reports = new PluginDataObject[0];
        } else {
            PluginDataObject[] newReports = new PluginDataObject[reportCount];
            int i = 0;
            // Copy in the reports that passed filtering.
            for (PluginDataObject report : reports) {
                if (report != null) {
                    newReports[i++] = report;
                }
            }
            reports = newReports;
        }
        return reports;
    }
    
    /**
     * Apply the list of filters against given input data.
     * 
     */
 //   @Override
    public PluginDataObject[] filter(PluginDataObject[] reports) {
    	PluginDataObject[] resultRpt=null;
    	for (MetarToShefRun mtsr : metarToShefRun) {
        	PluginDataObject[] tmpRprts = reports.clone();
    		resultRpt=filterARun(tmpRprts,mtsr.getFilterElements());
    		if (resultRpt!=null && resultRpt.length>=1) {
    			logger.info("Report matchs in filter "+mtsr.getFilterName());
    			MetarToShefTransformer.setCfgNOption(mtsr.getConfigFileName(),mtsr.getMetarToShefOptions());
    			break ;
    		}
    	}

        return resultRpt;
    }

    private void createDummyFilter() {
        MetarToShefRun mtsr= new MetarToShefRun();
        mtsr.setConfigFileName(METAR_CFG);
        mtsr.setMetarToShefOptions(metarToShefOptions);

        // Add a dummy element.
        AbstractFilterElement dummy = new AbstractFilterElement() {
            @Override
            public PluginDataObject filter(PluginDataObject report) {
                return report;
            }
        };
        dummy.setFilterType(AbstractObsFilter.INCLUDE_TYPE);
        mtsr.getFilterElements().set(0, dummy);
        mtsr.setFilterName("Created Pass-All filter");
        this.metarToShefRun.add(mtsr);
    }

    /**
     * 
     * @param file
     * @return
     */
    private static FileInputStream getInputStream(File file) {
        FileInputStream fis = null;

        try {
            fis = new FileInputStream(file);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
        return fis;
    }
    
    public void addMetarToShefRun(MetarToShefRun element) {
    	metarToShefRun.add(element);
    }

    /**
     * 
     * @return
     */
    public List<MetarToShefRun> getMetarToShefRun() {
        return metarToShefRun;
    }
    
    /**
     * 
     * @param elements
     */
    public void setMetarToShefRun(List<MetarToShefRun> elements) {
    	metarToShefRun = elements;
    }

 }
