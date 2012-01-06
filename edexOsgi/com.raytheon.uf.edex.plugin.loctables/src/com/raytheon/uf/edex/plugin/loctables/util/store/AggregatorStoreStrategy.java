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
package com.raytheon.uf.edex.plugin.loctables.util.store;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.raytheon.uf.edex.plugin.loctables.util.TableHandler;
import com.raytheon.uf.edex.plugin.loctables.util.handlers.MaritimeTableHandler;
import com.raytheon.uf.edex.plugin.loctables.util.handlers.MesonetTableHandler;
import com.raytheon.uf.edex.plugin.loctables.util.handlers.MetarTableHandler;
import com.raytheon.uf.edex.plugin.loctables.util.handlers.PirepTableHandler;
import com.raytheon.uf.edex.plugin.loctables.util.handlers.RAOBTableHandler;
import com.raytheon.uf.edex.plugin.loctables.util.handlers.SynopticLandTableHandler;

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

public class AggregatorStoreStrategy  extends PrintStreamStoreStrategy {

    private Map<String,ObStationRow> locMap = new HashMap<String,ObStationRow>();
    
    /**
     * 
     * @param file
     */
    public AggregatorStoreStrategy(File file) throws IOException {
        super(file);
    }

    /**
     * 
     * @param file
     */
    public AggregatorStoreStrategy(PrintStream stream) {
        super(stream);
    }
    
    /**
     * 
     * @param file
     */
    public AggregatorStoreStrategy(File path, String name, String ext, int breakFile) {
        super(path, name, ext, breakFile);
    }
    
    /**
     * 
     * @param row
     * @return Was the store successful.
     * @see com.raytheon.uf.edex.plugin.loctables.util.store.RowStoreStrategy#store(com.raytheon.uf.edex.plugin.loctables.util.store.ObStationRow)
     */
    @Override
    public boolean store(ObStationRow row) {
        boolean stored = false;

        String key = null;
        if(row != null) {
            // We may want to fold raob and synoptic surface.
            if (ObStation.CAT_TYPE_SFC_RAOB.equals(row.getCatalogType())) {
                key = ObStation.createGID(ObStation.CAT_TYPE_SFC_FXD, row.getStationId());
                if (locMap.containsKey(key)) {
                    ObStationRow aggregate = locMap.get(key);
                    aggregate.setUpperAirElevation(row.getUpperAirElevation());
                    aggregate.setUpperAirGeometry(row.getUpperAirGeometry());
                    if (aggregate.getIcao() == null) {
                        aggregate.setIcao(row.getIcao());
                    }
                }
                // There wasn't a land synoptic site for this id.
                key = row.getGid();
                if (!locMap.containsKey(key)) {
                    locMap.put(key,row);
                }
            } else if (ObStation.CAT_TYPE_SFC_FXD.equals(row.getCatalogType())) {
                key = ObStation.createGID(ObStation.CAT_TYPE_SFC_RAOB, row.getStationId());
                if (locMap.containsKey(key)) {
                    ObStationRow aggregate = locMap.get(key);
                    row.setUpperAirElevation(aggregate.getUpperAirElevation());
                    row.setUpperAirGeometry(aggregate.getUpperAirGeometry());
                    if (row.getIcao() == null) {
                        row.setIcao(aggregate.getIcao());
                    }
                }
                key = row.getGid();
                if (!locMap.containsKey(key)) {
                    locMap.put(key,row);
                }
            } else {
                key = row.getGid();
                if (!locMap.containsKey(key)) {
                    locMap.put(key,row);
                }
            }
        }
        return stored;
    }

    /**
     * 
     */
    private void checkICAOs() {

        ArrayList<ObStationRow> rows = new ArrayList<ObStationRow>();
        
        for(ObStationRow row : locMap.values()) {
            if (ObStation.CAT_TYPE_SFC_FXD.equals(row.getCatalogType())) {
                // This synoptic has an associated ICAO, check to see if it is in the ICAOs
                String icao = row.getIcao();
                if(icao != null) {
                    String key = ObStation.createGID(ObStation.CAT_TYPE_ICAO, icao);
                    if (!locMap.containsKey(key)) {
                        ObStationRow icaoRow = new ObStationRow(ObStation.CAT_TYPE_ICAO);
                        icaoRow.setIcao(icao);
                        icaoRow.setStationId(icao);
                        icaoRow.setWmoIndex(row.getWmoIndex());
                        icaoRow.setWmoRegion(row.getWmoRegion());

                        icaoRow.setCountry(row.getCountry());
                        icaoRow.setState(row.getState());
                        
                        icaoRow.setElevation(row.getElevation());
                        icaoRow.setLocation(row.getLocation());
                        
                        icaoRow.setRbsnIndicator(row.getRbsnIndicator());
                        
                        rows.add(icaoRow);
                    }
                }
            }
        } // for
        for(ObStationRow row : rows) {
            locMap.put(row.getGid(),row);
        }
    }
    
    /**
     * Closes this aggregator. This method must be called so the 
     * aggregated row information is written to the output.
     * @see java.io.Closeable#close()
     */
    @Override
    public void close() throws IOException {
        // Reconcile some issues.
        checkICAOs();
        for(ObStationRow row : locMap.values()) {
            super.store(row);
        }
        super.close();
    }
    
    
    public static final void main(String [] args) {
        
        File fileA = new File("./utility/edex_static/base/spatialTables/raobStationInfo.txt");
        File fileB = new File("./utility/edex_static/base/spatialTables/CMANStationInfo.txt");
        File fileC = new File("./utility/edex_static/base/spatialTables/maritimeStationInfo.txt");
        File fileD = new File("./utility/edex_static/base/spatialTables/metarStationInfo.txt");
        File fileE = new File("./utility/edex_static/base/spatialTables/pirepsTable.txt");
        File fileF = new File("./utility/edex_static/base/spatialTables/synopticStationTable.txt");
        File fileG = new File(
                "./utility/edex_static/base/spatialTables/mesonetStationInfo.txt");
        
        File fout = new File("./utility/edex_static/base/spatialTables");
        
        RowStoreStrategy out = null;
        try {
            out = new AggregatorStoreStrategy(fout,"common_obs_spatial_","sql",4000);
            
            TableHandler handler = new RAOBTableHandler(out);
            handler.processFile(fileA);

            handler = new MaritimeTableHandler(out);
            handler.processFile(fileB);

            handler = new MaritimeTableHandler(out);
            handler.processFile(fileC);

            handler = new MetarTableHandler(out);
            handler.processFile(fileD);

            handler = new PirepTableHandler(out);
            handler.processFile(fileE);

            handler = new SynopticLandTableHandler(out);
            handler.processFile(fileF);

            handler = new MesonetTableHandler(out);
            handler.processFile(fileG);

        } catch(Exception e) {
            System.out.println("Error processing data");
            e.printStackTrace();
        } finally {
            if(out != null) {
                try {
                    out.close();
                } catch(IOException ioe) {
                    System.out.println("Error closing store strategy");
                    ioe.printStackTrace();
                }
            }
        }
    }
}
