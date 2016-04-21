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
 * Apr 20, 2010            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public class MetarTableHandler extends AbstractTableHandler {

    private Log logger = LogFactory.getLog(getClass());

    /**
     * 
     */
    public MetarTableHandler(RowStoreStrategy storeStrategy) {
        super("MetarTable", storeStrategy);
    }
    
    /**
     * @see com.raytheon.uf.edex.plugin.loctables.util.TableHandler#parseLine(java.lang.String)
     */
    @Override
    public ObStationRow parseLine(String data) {
        //           11111111112222222222333333333344444444445555555555666666666677777777778888888888
        // 012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
        // 0000000000| 5GN | 61.82 |-147.56 |  900|TAHNETA PASS, AK                    |US|MTR
        // 0000070279| 5HN | 60.23 |-146.65 |   56|CAPE HINCHINBROOK, AK               |US|MTR
        ObStationRow row = null;

        if(data != null) {
            String [] parts = data.split("\\|");
            if(parts.length == 8) {
                String s = parts[1].trim(); 
                if(s.length() > 0) {
                    row = new ObStationRow(ObStation.CAT_TYPE_ICAO);
                    row.setStationId(s);
                    row.setIcao(s);
                    StringBuilder sb = new StringBuilder(parts[0]);
                    for(int i = 0;i < sb.length();i++) {
                        if(sb.charAt(i) == '0') {
                            sb.setCharAt(i, ' ');
                        } else {
                            break;
                        }
                    }
                    s = sb.toString().trim();
                    if(s.length() > 0) {
                        Integer wmo = getInt(s, -1);
                        if(wmo >= 0) {
                            row.setWmoIndex(wmo);
                        }
                    }

                    Double lat = getDouble(parts[2].trim(), null);
                    Double lon = getDouble(parts[3].trim(), null);
                    if(lat != null && lon != null) {
                        row.setLocation(ObStationRow.getPoint(lat, lon));
                        s = parts[4].trim();
                        if(s.length() > 0) {
                            Integer elev = new Integer(s);
                            row.setElevation(elev);
                        }
                        
                        s = parts[5].trim();
                        if(s.length() > 0) {
                            row.setName(s);
                        }
                        
                        s = parts[6].trim();
                        if(s.length() > 0) {
                            row.setCountry(s);
                        }
                    } else {
                        row = null;
                    }
                }
            }
        }
        return row;
    }

    public static final void main(String [] args) {
        
//        File file = new File("./utility/edex_static/base/spatialTables/metarStationInfo.txt");
//        File fout = new File("./utility/edex_static/base/spatialTables");
//        
//        
//        RowStoreStrategy out = null;
//        try {
//            out = new PrintStreamStoreStrategy(fout,"common_obs_spatial","sql",4000);
//            
//            TableHandler handler = new MetarTableHandler(out);
//
//            handler.processFile(file);
//
//        } catch(Exception e) {
//            
//        } finally {
//            if(out != null) {
//                try {
//                    out.close();
//                } catch(IOException ioe) {
//                    
//                }
//            }
//        }
    
      RowStoreStrategy out = null;
      try {
          out = new PrintStreamStoreStrategy(System.out);
          
          TableHandler handler = new MetarTableHandler(out);

          ObStationRow row = handler.parseLine("000070279| 5HN | 60.23 |-146.65 |   56|CAPE HINCHINBROOK, AK               |US|MTR");

          System.out.println(row);
          
      } catch(Exception e) {
          
      } finally {
          if(out != null) {
              try {
                  out.close();
              } catch(IOException ioe) {
                  
              }
          }
      }

        
        String [] parts = "000070279| 5HN | 60.23 |-146.65 |   56|CAPE HINCHINBROOK, AK               |US|MTR".split("\\|");
        for(String s : parts) {
            System.out.println(s);
        }
    
    
    
    }

}
