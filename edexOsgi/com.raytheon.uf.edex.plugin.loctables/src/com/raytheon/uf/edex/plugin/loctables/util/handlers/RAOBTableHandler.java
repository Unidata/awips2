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
package com.raytheon.uf.edex.plugin.loctables.util.handlers;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.raytheon.uf.edex.plugin.loctables.util.TableHandler;
import com.raytheon.uf.edex.plugin.loctables.util.store.ObStationRow;
import com.raytheon.uf.edex.plugin.loctables.util.store.PrintStreamStoreStrategy;
import com.raytheon.uf.edex.plugin.loctables.util.store.RowStoreStrategy;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 21, 2010            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public class RAOBTableHandler  extends AbstractTableHandler {

    private static final String SEP = "|";
    
    private Log logger = LogFactory.getLog(getClass());

    /**
     * 
     */
    public RAOBTableHandler(RowStoreStrategy storeStrategy) {
        super("RAOBTable", storeStrategy);
    }
    
    /**
     * @see com.raytheon.uf.edex.plugin.loctables.util.TableHandler#parseLine(java.lang.String)
     */
    @Override
    public ObStationRow parseLine(String data) {
        // 0000004202|BGTL | 76.53333| -68.75000|   77|THULE AB, GREENLAND|GL|RAOB
        // 0000008594|GVAC | 16.73300| -22.95000|   55|SAL, CAPE VERDE|CV|RAOB
        ObStationRow row = null;

        if(data != null) {
            List<String> tokens = new ArrayList<String>(); 
            StringTokenizer st = new StringTokenizer(data,SEP,true);
            String lastToken = null;
            while(st.hasMoreTokens()) {
                
                String token = st.nextToken();
                if(SEP.equals(token)) {
                    if(SEP.equals(lastToken)) {
                        tokens.add("");
                    }
                } else {
                    tokens.add(token.trim());
                }
                lastToken = token;
            }
            if(tokens.size() == 8) {
                Integer wmo = new Integer(tokens.get(0));
                if(wmo != null) {
                    row = new ObStationRow(ObStation.CAT_TYPE_SFC_RAOB);
                    row.setWmoIndex(wmo);
                    row.setStationId(String.format("%05d",wmo));
                    row.setIcao(tokens.get(1));
                    row.setUpperAirElevation(new Integer(tokens.get(4)));
                    Double lat = new Double(tokens.get(2));
                    Double lon = new Double(tokens.get(3));
                    if((lat != null)&&(lon != null)) {
                        row.setUpperAirGeometry(ObStationRow.getPoint(lat, lon));
                        row.setName(tokens.get(5));
                        row.setCountry(tokens.get(6));
                    } else {
                        row = null;
                    }
                }
            } else {
                System.out.println(tokens);
            }
        }
        return row;
    }

    public static final void main(String [] args) {
        
        File file = new File("./utility/edex_static/base/spatialTables/raobStationInfo.txt");
        File fout = new File("./utility/edex_static/base/spatialTables");
        
        
        RowStoreStrategy out = null;
        try {
            out = new PrintStreamStoreStrategy(fout,"common_obs_spatial","sql",4000);
            
            TableHandler handler = new RAOBTableHandler(out);

            handler.processFile(file);

        } catch(Exception e) {
            
        } finally {
            if(out != null) {
                try {
                    out.close();
                } catch(IOException ioe) {
                    
                }
            }
        }
    }
}

