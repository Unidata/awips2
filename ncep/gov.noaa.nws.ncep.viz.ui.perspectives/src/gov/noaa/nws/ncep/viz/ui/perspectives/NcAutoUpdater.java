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
package gov.noaa.nws.ncep.viz.ui.perspectives;

import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefnsMngr;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.RecordFactory;
import com.raytheon.uf.viz.core.alerts.AbstractAlertMessageParser;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.comm.Loader;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.updater.DataUpdateTree;
import com.raytheon.viz.alerts.IAlertObserver;

/**
 * NcAutoUpdater
 * 
 * Updates the Natl Cntrs resources as data comes in. This was copied from Raytheon's AutoUpdater 
 * to fix a couple of bugs in their code. 
 * 
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    10/22/10      #307       ghull       Initial Creation based on AutoUpdater
 *    06/07/11      #445       xGuo        Data Manager Performance Improvements
 *                                         process alert message to update data resource
 *    03/21/12      #606       ghull       resources no longer need to be updated
 *    
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class NcAutoUpdater implements IAlertObserver {

    private static final IUFStatusHandler handler = UFStatus.getHandler(NcAutoUpdater.class);
    
    private static final int MAX_ERRORS = 10;

    private static class ParseNatlCntrsAlertMessage extends
            AbstractAlertMessageParser {

        @Override
        public Object parseAlertMessage(AlertMessage message,
                AbstractRequestableResourceData reqResourceData)
                throws VizException {
            Object objectToSend = null;
            Map<String, Object> attribs = new HashMap<String, Object>(
                    message.decodedAlert);
            String dataURI = message.dataURI;
            if (reqResourceData.isUpdatingOnMetadataOnly()) {
                PluginDataObject record = RecordFactory.getInstance()
                        .loadRecordFromUri(dataURI);
                objectToSend = record;

            } else {
                attribs.put("dataURI", message.dataURI);
                objectToSend = Loader.loadData(attribs);
            }
            return objectToSend;
        }
    };

    private static ParseNatlCntrsAlertMessage defaultParser = new ParseNatlCntrsAlertMessage();

    
    protected NcAutoUpdater() {
    }

    
    @Override
    public void alertArrived( Collection<AlertMessage> alertMessages) {
        //Map<AbstractNatlCntrsRequestableResourceData, List<Object>> pdoSendMap = 
        //	   new HashMap<AbstractNatlCntrsRequestableResourceData, List<Object>>();
        int errors = 0;

        // change all the attributes with spaces to '_'s.
        // known attribute with this problem are areaName, hazardType, watchNumber, and
        // reportType,
//        System.out.println("recieved Alert Msg with "+alertMessages.size() + " msgs");
        
        for( AlertMessage message : alertMessages) {
            Map<String, Object> attribs = new HashMap<String,Object>( message.decodedAlert );

//            System.out.println( message.dataURI );
            
            for( String attrName : attribs.keySet() ) {
        		Object attribsObj = attribs.get( attrName );
        		if( attribsObj instanceof String && 
        		          !attrName.equals("dataTime") ) {
        			
        			String attrStr = ((String)attribsObj); 
        		    if( attrStr.indexOf(' ') != -1 ) {   
        		    	
        			   attribs.put(attrName, attrStr.replace(' ', '_') );
//           			System.out.println( "WARNING: auto update message for URI"+ message.dataURI +
//   					"\n     Attribute "+ attrName +" has a space : '"+ (String)attribsObj +"'" );
            		}
            	}
            }
            try {
                // System.out.println("extract took: " + (tZ1 - tZ0));
                java.util.List<AbstractVizResource<?, ?>> rscList = DataUpdateTree
                        .getInstance().searchTree(attribs);

                if (rscList != null && rscList.size() > 0) {

                	// will this work if we first parse the Alert Message and then 
                	// loop thru the rscList??
                	//
                    for (AbstractVizResource<?, ?> r1 : rscList) {
                    	if( !(r1 instanceof AbstractNatlCntrsResource<?, ?>) ) {
                    		continue;
                    	}
                    	
                        if (!(r1.getResourceData() instanceof AbstractNatlCntrsRequestableResourceData)
                                || r1.getResourceData().isFrozen())
                            continue;

                    	AbstractNatlCntrsRequestableResourceData reqResourceData = 
                			(AbstractNatlCntrsRequestableResourceData) r1.getResourceData();

                        AbstractAlertMessageParser parserToUse = null;

                        if ((parserToUse = reqResourceData.getAlertParser()) == null) {
                            parserToUse = defaultParser;
                        }
                        
                        Object objectToSend = parserToUse.parseAlertMessage(
                                message, reqResourceData);
                       
                        if( objectToSend != null ) {
                            reqResourceData.autoUpdate( objectToSend );                    		
                            r1.issueRefresh();                        	
                        }
                        
//                        if (objectToSend != null) {
//                            displayList.add(md);
//                            addToMapList(reqResourceData, pdoSendMap, objectToSend);
//                        }
                    }
                }

            } catch (final Throwable e) {
                if (errors < MAX_ERRORS) {
                    handler.handle(Priority.PROBLEM, "Error performing NC autoupdate", e);
                }
                errors++;
            }
        }
    }

    /*
    private void addToMapList(AbstractNatlCntrsRequestableResourceData data,
            Map<AbstractNatlCntrsRequestableResourceData, List<Object>> map, Object obj) {

        List<Object> list = map.get(data);
        if (list == null) {
            list = new ArrayList<Object>();
            map.put(data, list);
        }
        list.add(obj);
    }
    */
}
