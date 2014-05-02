/**
 * This software was developed by HSEB, OHD
 **/
package gov.noaa.nws.ohd.edex.plugin.hydrodualpol;
import gov.noaa.nws.ohd.edex.plugin.hydrodualpol.common.HydroDualPolConfig;


import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import com.raytheon.edex.urifilter.URIFilter;
import com.raytheon.edex.urifilter.URIGenerateMessage;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarsInUseUtil;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.cpgsrv.CompositeProductGenerator;

/**
 *
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 13, 2013            jtDeng     Initial creation
 * 
 * This HydroDualPolGenerator creates data URI filters for dual pol radar products
 * DSA/DPR/DAA, it retrieves these radar records from "metadata" database and HDF5 files
 * once get notified. 
 * It also generates the post-processing DSA/DPR/DAA products in specific format and 
 * stores in IHFS database as inputs to MPE/HPE/HPN programs.
 * </pre>
 * 
 * @author deng2
 * @version 1.0
 */

public class HydroDualPolGenerator extends CompositeProductGenerator {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(HydroDualPolGenerator.class);

    private static final String genName = "HydroDualPol";

    private static final String productType = "hydrodualpol";

    /** Set of icaos to filter for **/
    private Set<String> icaos = null;

    /** public constructor for HydroDualPolGenerator **/
    public HydroDualPolGenerator(String name, String compositeProductType) {
        super(genName, productType);
    }

    /** default thrift constructor **/
    public HydroDualPolGenerator() {
        super(genName, productType);
    }

    @Override
    protected void configureFilters() {

        logger.debug(getGeneratorName() + " process Filter Config...");
        icaos = new HashSet<String>(RadarsInUseUtil.getSite(null,
                RadarsInUseUtil.LOCAL_CONSTANT));
        icaos.addAll(RadarsInUseUtil.getSite(null,
                RadarsInUseUtil.DIAL_CONSTANT));
    }

    @Override
    protected void createFilters() {
    	
    	String header = "HydroDualPolGenerator.createFilters(): ";
        ArrayList<URIFilter> tmp = new ArrayList<URIFilter>(icaos.size());
        Iterator<String> iter = icaos.iterator();

        int radarCount = 0;
        while (iter.hasNext()) {
            String icao = iter.next();
            try {
                tmp.add(new HydroDualPolURIFilter(icao, HydroDualPolURIFilter.daa));
                tmp.add(new HydroDualPolURIFilter(icao, HydroDualPolURIFilter.dpr));
                tmp.add(new HydroDualPolURIFilter(icao, HydroDualPolURIFilter.dsa));
                
                radarCount++;
                statusHandler.handle(Priority.INFO,
                        header + " radar # " + radarCount + " radar id = " + icao);
                
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Couldn't create HydroDualPol URIFilter.." + icao
                                + " is not a known RADAR site.");
                iter.remove();
            }
        }
        filters = tmp.toArray(new HydroDualPolURIFilter[tmp.size()]);
    }

    @Override
    public void generateProduct(URIGenerateMessage genMessage) {
        HydroDualPolConfig hydrodualpol_config = null;
        
        /** test to retrieve the following fields from radar record **/     
        try {
        	hydrodualpol_config = new HydroDualPolConfig(
        			(HydroDualPolURIGenerateMessage) genMessage, this);

        	statusHandler.handle(Priority.INFO, "In HydroDualPol generateProduct...");

        	String productType = hydrodualpol_config.getProductType();

        	if (productType.equalsIgnoreCase("DSA"))
        	{
        		RadarRecord record = hydrodualpol_config.getDSA();

        		if (record != null) 
        		{
        			processDSAProduct(record);
        		}
        		else
        		{
        			statusHandler.handle(Priority.INFO,
        					"DSA product not found...");
        		}
        	}
        	else if (productType.equalsIgnoreCase("DPR"))
        	{

        		RadarRecord record = hydrodualpol_config.getDPR();

        		if (record != null)
        		{
        			processDPRProduct(record);
        		}
        		else
        		{
        			statusHandler.handle(Priority.INFO,
        			"DPR product not found...");
        		}
        		
        	}


        	else if (productType.equalsIgnoreCase("DAA"))
        	{
        		RadarRecord record = hydrodualpol_config.getDAA();

        		if (record != null)
        		{
        			processDAAProduct(record);
        		}
        		else
        		{
        			statusHandler.handle(Priority.INFO,
        					"DAA product not found...");
        		}
        	}
        }
        catch (Exception e)
        {
        	statusHandler.handle(Priority.ERROR, "Can not run HydroDualPol. " +
        			e.getMessage());
        	e.printStackTrace();
        }
    } //end generateProduct()

    
    private void processDAAProduct(RadarRecord record)
    {

    	DAAProductProcessor processor = new DAAProductProcessor(statusHandler);

    	processor.process(record);

    }
    
    private void processDPRProduct(RadarRecord record)
    {

    	DPRProductProcessor processor = new DPRProductProcessor(statusHandler);

    	processor.process(record);

    }

    private void processDSAProduct(RadarRecord record)
    {

    	DSAProductProcessor processor = new DSAProductProcessor(statusHandler);

    	processor.process(record);

    }
    
	@Override
    public boolean isRunning() {
        return getConfigManager().getHydroDualPolState();
    }
	
}
