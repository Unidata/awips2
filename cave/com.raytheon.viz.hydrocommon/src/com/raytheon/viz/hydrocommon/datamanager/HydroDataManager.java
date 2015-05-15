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
package com.raytheon.viz.hydrocommon.datamanager;

import java.lang.reflect.Field;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.TimeZone;

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.data.Rating;
import com.raytheon.viz.hydrocommon.data.RatingShift;

/**
 * Base Data Manager which other data managers should extend.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 22, 2008 1636       askripsky   Initial Creation
 * Sep 09, 2009 2259       mpduff      Added rating shift data
 * April 8, 2015  17338    jingtaoD    add checkAppostrophe method
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */

public abstract class HydroDataManager {

    protected SimpleDateFormat dbFormat = HydroConstants.DATE_FORMAT;

    protected static Map<String, RatingShift> ratingShiftMap = new HashMap<String, RatingShift>();

    protected static Map<String, Rating> ratingMap = new HashMap<String, Rating>();

    /**
     * Private constructor.
     */
    protected HydroDataManager() {
        dbFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
    }

    /**
     * Returns the raw data from the database.
     * 
     * @return The raw data
     * @throws VizException
     */
    protected ArrayList<Object[]> runQuery(String dataQuery) {
        AppsDefaults ad = AppsDefaults.getInstance();
        boolean debug = ad.getBoolean(HydroConstants.DEBUG_HYDRO_DB_TOKEN, false);
        if (debug) {
            System.out.println(ad.getToken(HydroConstants.PGHOST) + ":" + 
                    ad.getToken(HydroConstants.PGPORT) + ":" + 
                    ad.getToken(HydroConstants.DB_NAME));
            System.out.println("Query: " + dataQuery);
        }
        ArrayList<Object[]> data = null;

        try {
            data = (ArrayList<Object[]>) DirectDbQuery.executeQuery(dataQuery,
                    HydroConstants.IHFS, QueryLanguage.SQL);
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        return data;
    }

    /**
     * Returns the raw data from the database in a mapped result set.
     * 
     * @return The raw data
     * @throws VizException
     * @throws VizException
     */
    public QueryResult runMappedQuery(String dataQuery) throws VizException {
        AppsDefaults ad = AppsDefaults.getInstance();
        boolean debug = ad.getBoolean(HydroConstants.DEBUG_HYDRO_DB_TOKEN, false);
        if (debug) {
            System.out.println(ad.getToken(HydroConstants.PGHOST) + ":" + 
                    ad.getToken(HydroConstants.PGPORT) + ":" + 
                    ad.getToken(HydroConstants.DB_NAME));
            System.out.println("Query: " + dataQuery);
        }
        QueryResult data = null;
        
        // check if dataQuery contains any apostrophe, if does, replace it to two single appostrophe
        dataQuery = checkAppostrophe(dataQuery);
        
        data = DirectDbQuery.executeMappedQuery(dataQuery, HydroConstants.IHFS,
                QueryLanguage.SQL);

        return data;
    }

    /**
     * Executes statement with no return value on the database.
     * 
     * @throws VizException
     */
    public void runStatement(String dataQuery) throws VizException {
    	
        AppsDefaults ad = AppsDefaults.getInstance();
        boolean debug = ad.getBoolean(HydroConstants.DEBUG_HYDRO_DB_TOKEN, false);
        if (debug) {
            System.out.println(ad.getToken(HydroConstants.PGHOST) + ":" + 
                    ad.getToken(HydroConstants.PGPORT) + ":" + 
                    ad.getToken(HydroConstants.DB_NAME));
            System.out.println("Query: " + dataQuery);
        }
        
        // check if dataQuery contains any apostrophe, if does, replace it to two single appostrophe
        dataQuery = checkAppostrophe(dataQuery);
                    
        DirectDbQuery.executeStatement(dataQuery, HydroConstants.IHFS,
                        QueryLanguage.SQL);
        
    }
    
    public String checkAppostrophe (String dataQuery) {    	
    	 /* Check if text fields include single apostrophe, if it does, replace to 
        two single apostrophe since it is treated as special char in Postgres */                                       	            
       String newDataQuery = "";
       String PDCDataStr = "";
       int PDC_presetStr_begindex = 0;
       
       /* special handling for PDC*/
       if (dataQuery.toLowerCase().contains("preset_string"))
       {
    	   PDC_presetStr_begindex = dataQuery.toLowerCase().indexOf("'at=");       
           if (PDC_presetStr_begindex  > 0)
           {
    	      PDCDataStr = dataQuery.substring(PDC_presetStr_begindex);
    	      dataQuery = dataQuery.substring(0, PDC_presetStr_begindex);
           }
       }
       
        String[] dataElement = dataQuery.split(", ");
        for (int j = 0; j < dataElement.length; j++)        
        {
        	String elem = dataElement[j];
        	String newSubData = "";        	        	        	
        	String[] subdataElement = elem.split("=");
        	
        	for (int i = 0; i < subdataElement.length; i++)
        	{        	 
        		String subelem = subdataElement[i];
        		String likeStr="";
        		
        		 // handle the where clause contains "like '%'"
        		if (subelem.toUpperCase().contains("WHERE") &&
        			subelem.toUpperCase().contains("LIKE"))	
        		{
        		   	int likeindex = subelem.toUpperCase().indexOf("LIKE");
        			likeStr = subelem.substring(likeindex , subelem.length());
        		   	subelem = subelem.substring(0, likeindex);        		   	
        		}
        		
        	    String endStr = "";
        	    String beginStr = "";
        	    String checkStr;
        	    int startappostraphe, endappostraphe;        	   
        	   
        	    if (subelem.contains("'"))
        	    {
        	       if (subelem.startsWith("$$") && subelem.endsWith("$$"))
        	       {
        	    	   startappostraphe = subelem.indexOf("$$"); 
        		       endappostraphe = subelem.lastIndexOf("$$");
        		       checkStr = subelem.substring(startappostraphe+2, endappostraphe);
        		       if (checkStr.contains("'"))
             	       { 
        		    	   checkStr = checkStr.replace("'", "''");        		    
             		       subelem = "'" + checkStr +"'";
             	       }
        	       }
        	       else 
        		   {        			   
        			   startappostraphe = subelem.indexOf("'"); 
        		       endappostraphe = subelem.lastIndexOf("'");	
        		       checkStr = subelem.substring(startappostraphe+1, endappostraphe);        		           		          		           		          		 
        	           if (checkStr.contains("'"))
        	           {
        	        	   if (startappostraphe > 0)
                  			   beginStr = subelem.substring(0, startappostraphe);
             	    	   if (endappostraphe + 1 < subelem.length())
                  			   endStr = subelem.substring(endappostraphe + 1, subelem.length());
             		     
             	    	   checkStr = checkStr.replace("'", "''");        		    
             		       subelem = beginStr + "'" + checkStr +"'" + endStr; 
        	           }        	    	         	    	 
        	      } 
        	   }
        	           	   
        	   if (i == subdataElement.length -1 )        	   
        	       newSubData = newSubData + subelem + likeStr;
        	   else 
        		   newSubData = newSubData + subelem + "=";
        	}
        	if (j == dataElement.length - 1)        	
        	    newDataQuery = newDataQuery + newSubData;
        	else
        		newDataQuery = newDataQuery + newSubData + ", ";        		        	    	
        }
        
        if (PDC_presetStr_begindex  > 0)
        {
        	if (newDataQuery.toLowerCase().startsWith("insert"))
        		newDataQuery = newDataQuery + ", " + PDCDataStr;
        	else if (newDataQuery.toLowerCase().startsWith("update"))
        		newDataQuery = newDataQuery + PDCDataStr;
        	
        }
    
    	return newDataQuery;
    }

    /**
     * Runs functions defined in the DB
     * 
     * @param functionCall
     * @throws VizException
     */
    public ArrayList<Object[]> execFunction(String functionCall)
            throws VizException {
        return runQuery("SELECT " + functionCall);
    }
}
