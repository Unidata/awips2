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
import java.util.StringTokenizer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 29, 2010            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class WMOPub9SynopticTableHandler extends AbstractTableHandler {

    private Log logger = LogFactory.getLog(getClass());

    private Pattern NUMERIC = Pattern.compile("\\d{5}");

    /**
     * 
     */
    public WMOPub9SynopticTableHandler(RowStoreStrategy storeStrategy) {
        super("WMOPub9SynopticTable", storeStrategy);
    }

    /**
     * @see com.raytheon.uf.edex.plugin.loctables.util.TableHandler#parseLine(java.lang.String)
     */
    @Override
    public ObStationRow parseLine(String data) {

        ObStationRow row = null;

        if ((data != null) && (data.length() > 79)) {
            // Check for the table header. If so, throw it away.
            if (!data.startsWith("RegionId")) {
                ArrayList<String> tokens = new ArrayList<String>();
                StringTokenizer st = new StringTokenizer(data, "\t", true);

                String lastToken = null;
                String token = null;
                while (st.hasMoreTokens()) {
                    token = st.nextToken();
                    if ("\t".equals(token)) {
                        if (token.equals(lastToken)) {
                            tokens.add("");
                        }
                    } else {
                        tokens.add(token);
                    }
                    lastToken = token;
                }

                // TODO: Change this value!
                if(tokens.size() > 10) {
                    
                    row = new ObStationRow(ObStation.CAT_TYPE_SFC_FXD);

                    String s = tokens.get(0);
                    row.setWmoRegion(new Integer(s));
                    //  1                           RegionId

                    s = tokens.get(1);
                    //  AFRICA / AFRIQUE            RegionName

                    s = tokens.get(2);
                    //  ALGERIA / ALGERIE           Country name or area

                    s = tokens.get(3);
                    //  1030                        Country code

                    s = tokens.get(4);
                    //  121                         stationId

                    s = tokens.get(5);
                    row.setWmoIndex(new Integer(s));
                    row.setStationId(String.format("%05d",row.getWmoIndex()));
                    //  60559                       IndexNbr

                    s = tokens.get(6);
                    //  0                           Sub-index number

                    s = tokens.get(7);
                    //  EL-OUED                     Station name
                    
                    s = tokens.get(8);
                    Double lat = cvtLatLon(s);
                    //  33 30N                      Latitude
                    
                    s = tokens.get(9);
                    Double lon = cvtLatLon(s);
                    //  06 47E                      Longitude
                    
                    if((lat != null) && (lon != null)) {
                        row.setLocation(ObStationRow.getPoint(lat, lon));    

                        s = tokens.get(10);
                        row.setElevation(new Integer(s));
                        //  69                          Elevation
                        
                        s = tokens.get(11);
                        //                              HpFlag (# == approx)
                        s = tokens.get(12);
                        //  64                          Elevation
                        s = tokens.get(13);
                        //                              HpaFlag (# == approx)
                        s = tokens.get(14);
                        
                        //                              PressureDefId
                        s = tokens.get(15);
                        
                        //  X                           SO-1
                        s = tokens.get(16);
                        //  X                           SO-2
                        s = tokens.get(17);
                        //  X                           SO-3
                        s = tokens.get(18);
                        //  X                           SO-4
                        s = tokens.get(19);
                        //  X                           SO-5
                        s = tokens.get(20);
                        //  X                           SO-6
                        s = tokens.get(21);
                        //  X                           SO-7
                        s = tokens.get(22);
                        //  X                           SO-8
                        s = tokens.get(23);
                        //  H00-24                      ObsHrs
                        //*******************************
                        // X, P, R, W, WP, PR, .
                        //*******************************
                        s = tokens.get(24);
                        //  P                           UA-1 
                        s = tokens.get(25);
                        //  .                           UA-2
                        s = tokens.get(26);
                        //  P                           UA-3
                        s = tokens.get(27);
                        //  .                           UA-4
                        s = tokens.get(28);
                        //  A;CLIMAT(C);EVAP;M/B;METAR;SOILTEMP;SPECI;SUNDUR
                    } else {
                        row = null;
                    }
                }
            }
        }
        return row;
    }
    
    public static final void main(String [] args) {
        
        Pattern LATLON = Pattern.compile("(\\d{1,3})(( +\\d{2})( +\\d{2})?)?([NESW])");

        Matcher m = LATLON.matcher("136 49 31E");
        
        if(m.find()) {
            for(int i = 0;i <= m.groupCount();i++) {
                System.out.println(m.group(i));
            }
        }
        
        
        
        File file = new File("./utility/edex_static/base/spatialTables/Pub9volA100426.flatfile");

        RowStoreStrategy out = null;
        try {
            out = new PrintStreamStoreStrategy(System.out);
            TableHandler handler = new WMOPub9SynopticTableHandler(out);
            // WMOPub9SynopticTableHandler handler = new
            // WMOPub9SynopticTableHandler(out);

            handler.processFile(file);

        } catch (Exception e) {
            System.out.println("Error processing data");
            e.printStackTrace();
        } finally {
            if (out != null) {
                try {
                    out.close();
                } catch (IOException ioe) {
                    System.out.println("Error closing store strategy");
                    ioe.printStackTrace();
                }
            }
        }
    }
    
}
