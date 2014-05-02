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
package com.raytheon.viz.mpe.ui.dialogs.gagetable;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.ohd.AppsDefaults;

/**
 * Manage the Gage Table Product information.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 1, 2009  2476      mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class GageTableProductManager {
    public static final String MPE_RMOSAIC = "RMOSAIC";

    public static final String MPE_AVGRMOSAIC = "AVGRMOSAIC";

    public static final String MPE_MAXRMOSAIC = "MAXRMOSAIC";

    public static final String MPE_BMOSAIC = "BMOSAIC";

    public static final String MPE_LMOSAIC = "LMOSAIC";

    public static final String MPE_GAGEONLY = "GAGEONLY";

    public static final String MPE_MMOSAIC = "MMOSAIC";

    public static final String MPE_MLMOSAIC = "MLMOSAIC";

    public static final String MPE_SATPRE = "SATPRE";

    public static final String MPE_LSATPRE = "LSATPRE";

    public static final String MPE_SRMOSAIC = "SRMOSAIC";

    public static final String MPE_SGMOSAIC = "SGMOSAIC";

    public static final String MPE_SRGMOSAIC = "SRGMOSAIC";

    public static final String MPE_P3LMOSAIC = "P3LMOSAIC";
    
    //-------------------------------------------
    // Dual Pol Fields
    
    public static final String MPE_RDMOSAIC = "RDMOSAIC";
    
    public static final String MPE_BDMOSAIC = "BDMOSAIC";
    
    public static final String MPE_MDMOSAIC = "MDMOSAIC";
    
    public static final String MPE_LDMOSAIC = "LDMOSAIC";
    
    public static final String MPE_MLDMOSAIC = "MLDMOSAIC";
    
    public static final String MPE_AVGRDMOSAIC = "AVGRDMOSAIC";
    
    public static final String MPE_MAXRDMOSAIC = "MAXRDMOSAIC";
    
    public static final String MPE_SRDMOSAIC = "SRDMOSAIC";
    
    public static final String MPE_SRDGMOSAIC = "SRDGMOSAIC";
    
    //----------------------------------------------

    public static final String MPE_BESTQPE = "xmrg";

    public static final String MPE_RFCBMOSAIC = "RFCBMOSAIC";

    public static final String MPE_RFCMMOSAIC = "RFCMMOSAIC";

    public static final String MPE_RFCMOSAIC = "RFCMOSAIC";

    public static final String MPE_INDEX = "INDEX";

    // Q2 products
    public static final String MPE_QMOSAIC = "QMOSAIC";

    public static final String MPE_LQMOSAIC = "LQMOSAIC";

    public static final String MPE_MLQMOSAIC = "MLQMOSAIC";

    private static GageTableProductManager instance = null;

    private List<GageTableColumn> gageTableColumnList = null;

    /**
     * Holds GageTableColumn data that include the product descriptors for
     * available data types.
     */
    private Map<String, GageTableColumn> gageTableProductColumnMap = null;

    /**
     * Lookup from product name to prefix.
     */
    private Map<String, String> productNameLookup = new HashMap<String, String>();

    private List<GageTableColumn> availableGageTableColumnList = null;

    private List<GageTableColumn> originalColumns = null;

    /** Columns selected in the dialog */
    private List<GageTableColumn> selectedColumns = null;

    private ArrayList<GageTableListener> gageTableListenerList = new ArrayList<GageTableListener>();

    // private List<Map<String, Integer>> columnOrderList = new
    // ArrayList<Map<String, Integer>>();
    // private List<Map<String, Boolean>> sortOrderList = new
    // ArrayList<Map<String, Boolean>>();

    /**
     * Private Constructor.
     */
    private GageTableProductManager() {
        gageTableColumnList = new ArrayList<GageTableColumn>();
        gageTableProductColumnMap = new HashMap<String, GageTableColumn>();
        availableGageTableColumnList = new ArrayList<GageTableColumn>();
        originalColumns = new ArrayList<GageTableColumn>();
        selectedColumns = new ArrayList<GageTableColumn>();

        populateDataStructures();

        /*
         * Populate lists of available MPE Products, those are being created by
         * MPEFieldGen.
         */
        determineAvailableProducts();

        orderAvailableProductsToMatchPrecipFieldsMenu();

        // Generate the non-data columns first
        String[] baseColNames = GageTableConstants.BASE_COLUMNS;
        for (int i = 0; i < baseColNames.length; i++) {
            GageTableColumn col = new GageTableColumn(null);
            col.setName(baseColNames[i]);
            col.setDataColumn(false);
            selectedColumns.add(col);
            System.out.println("Added Base column " + col.getName());
        }

        // Copy available to selected since it's the first time
        for (int i = 0; i < availableGageTableColumnList.size(); i++) {
            selectedColumns.add(availableGageTableColumnList.get(i));
        }
    }

    /**
     * Get an instance of this class.
     * 
     * @return An instance of this class
     */
    public static synchronized GageTableProductManager getInstance() {
        if (instance == null) {
            instance = new GageTableProductManager();
        }

        return instance;
    }

    /**
     * Populate the Gage Table product descriptor data objects.
     */
    private void populateDataStructures() {
        GageTableProductDescriptor radarMosaic = new GageTableProductDescriptor(
                "Radar Mosaic", MPE_RMOSAIC, "Radar Mosaic", "mpe_rmosaic_dir",
                null);
        GageTableColumn column = new GageTableColumn(radarMosaic);
        column.setProductDescriptor(radarMosaic);
        gageTableColumnList.add(column);
        gageTableProductColumnMap.put(radarMosaic.getProductFilenamePrefix(),
                column);
        productNameLookup.put(radarMosaic.getProductName(),
                radarMosaic.getProductFilenamePrefix());

        GageTableProductDescriptor avgRadarMosaic = new GageTableProductDescriptor(
                "Average Radar Mosaic", MPE_AVGRMOSAIC, "Average Radar Mosaic",
                "mpe_avgrmosaic_dir", null);
        column = new GageTableColumn(avgRadarMosaic);
        column.setProductDescriptor(avgRadarMosaic);
        gageTableColumnList.add(column);
        gageTableProductColumnMap.put(
                avgRadarMosaic.getProductFilenamePrefix(), column);
        productNameLookup.put(avgRadarMosaic.getProductName(),
                avgRadarMosaic.getProductFilenamePrefix());

        GageTableProductDescriptor maxRadarMosaic = new GageTableProductDescriptor(
                "Maximum Radar Mosaic", MPE_MAXRMOSAIC, "Maximum Radar Mosaic",
                "mpe_maxrmosaic_dir", null);
        column = new GageTableColumn(maxRadarMosaic);
        column.setProductDescriptor(maxRadarMosaic);
        gageTableColumnList.add(column);
        gageTableProductColumnMap.put(
                maxRadarMosaic.getProductFilenamePrefix(), column);
        productNameLookup.put(maxRadarMosaic.getProductName(),
                maxRadarMosaic.getProductFilenamePrefix());

        GageTableProductDescriptor fieldBiasMosaic = new GageTableProductDescriptor(
                "Field Bias Mosaic", MPE_BMOSAIC, "Field Bias Mosaic",
                "mpe_bmosaic_dir", new String[] { MPE_RMOSAIC });
        column = new GageTableColumn(fieldBiasMosaic);
        column.setProductDescriptor(fieldBiasMosaic);
        gageTableColumnList.add(column);
        gageTableProductColumnMap.put(
                fieldBiasMosaic.getProductFilenamePrefix(), column);
        productNameLookup.put(fieldBiasMosaic.getProductName(),
                fieldBiasMosaic.getProductFilenamePrefix());

        GageTableProductDescriptor localbiasMosaic = new GageTableProductDescriptor(
                "Local Bias Mosaic", MPE_LMOSAIC, "Local Bias Mosaic",
                "mpe_lmosaic_dir", new String[] { MPE_RMOSAIC });
        column = new GageTableColumn(localbiasMosaic);
        column.setProductDescriptor(localbiasMosaic);
        gageTableColumnList.add(column);
        gageTableProductColumnMap.put(
                localbiasMosaic.getProductFilenamePrefix(), column);
        productNameLookup.put(localbiasMosaic.getProductName(),
                localbiasMosaic.getProductFilenamePrefix());

        GageTableProductDescriptor gageOnlyAnalysis = new GageTableProductDescriptor(
                "Gage Only Analysis", MPE_GAGEONLY, "Gage Only Analysis",
                "mpe_gageonly_dir", new String[] { MPE_RMOSAIC });
        column = new GageTableColumn(gageOnlyAnalysis);
        column.setProductDescriptor(gageOnlyAnalysis);
        gageTableColumnList.add(column);
        gageTableProductColumnMap.put(
                gageOnlyAnalysis.getProductFilenamePrefix(), column);
        productNameLookup.put(gageOnlyAnalysis.getProductName(),
                gageOnlyAnalysis.getProductFilenamePrefix());

        GageTableProductDescriptor multiSensorMosaic = new GageTableProductDescriptor(
                "Multi-sensor Mosaic", MPE_MMOSAIC, "Multi-Sensor Mosaic",
                "mpe_mmosaic_dir", new String[] { MPE_RMOSAIC, "MMOSAIC" });
        column = new GageTableColumn(multiSensorMosaic);
        column.setProductDescriptor(multiSensorMosaic);
        gageTableColumnList.add(column);
        gageTableProductColumnMap.put(
                multiSensorMosaic.getProductFilenamePrefix(), column);
        productNameLookup.put(multiSensorMosaic.getProductName(),
                multiSensorMosaic.getProductFilenamePrefix());

        GageTableProductDescriptor localBiasMultiSensorMosaic = new GageTableProductDescriptor(
                "Local Bias Multi-sensor Mosaic", MPE_MLMOSAIC,
                "Local Bias Multi-sensor Mosaic", "mpe_mlmosaic_dir",
                new String[] { MPE_RMOSAIC, MPE_LMOSAIC });
        column = new GageTableColumn(localBiasMultiSensorMosaic);
        column.setProductDescriptor(localBiasMultiSensorMosaic);
        gageTableColumnList.add(column);
        gageTableProductColumnMap.put(
                localBiasMultiSensorMosaic.getProductFilenamePrefix(), column);
        productNameLookup.put(localBiasMultiSensorMosaic.getProductName(),
                localBiasMultiSensorMosaic.getProductFilenamePrefix());

        GageTableProductDescriptor satelllitePrecipField = new GageTableProductDescriptor(
                "Satellite Precipitation Field", MPE_SATPRE,
                "Satellite Precipitation Field", "mpe_satpre_dir", null);
        column = new GageTableColumn(satelllitePrecipField);
        column.setProductDescriptor(satelllitePrecipField);
        gageTableColumnList.add(column);
        gageTableProductColumnMap.put(
                satelllitePrecipField.getProductFilenamePrefix(), column);
        productNameLookup.put(satelllitePrecipField.getProductName(),
                satelllitePrecipField.getProductFilenamePrefix());

        GageTableProductDescriptor localBiasSatellitePrecipField = new GageTableProductDescriptor(
                "Local Bias Satellite Precipitation Field", MPE_LSATPRE,
                "Local Bias Satellite Precipitation Field", "mpe_lsatpre_dir",
                new String[] { MPE_RMOSAIC, MPE_LMOSAIC });
        column = new GageTableColumn(localBiasSatellitePrecipField);
        column.setProductDescriptor(localBiasSatellitePrecipField);
        gageTableColumnList.add(column);
        gageTableProductColumnMap.put(
                localBiasSatellitePrecipField.getProductFilenamePrefix(),
                column);
        productNameLookup.put(localBiasSatellitePrecipField.getProductName(),
                localBiasSatellitePrecipField.getProductFilenamePrefix());

        GageTableProductDescriptor satelliteRadarMosaic = new GageTableProductDescriptor(
                "Satellite Radar Mosaic", MPE_SRMOSAIC,
                "Satellite Radar Mosaic", "mpe_srmosaic_dir", new String[] {
                        MPE_LSATPRE, MPE_LMOSAIC });
        column = new GageTableColumn(satelliteRadarMosaic);
        column.setProductDescriptor(satelliteRadarMosaic);
        gageTableColumnList.add(column);
        gageTableProductColumnMap.put(
                satelliteRadarMosaic.getProductFilenamePrefix(), column);
        productNameLookup.put(satelliteRadarMosaic.getProductName(),
                satelliteRadarMosaic.getProductFilenamePrefix());

        GageTableProductDescriptor satelliteGageMosaic = new GageTableProductDescriptor(
                "Satellite Gage Mosaic", MPE_SGMOSAIC, "Satellite Gage Mosaic",
                "mpe_sgmosaic_dir", new String[] { MPE_LSATPRE });
        column = new GageTableColumn(satelliteGageMosaic);
        column.setProductDescriptor(satelliteGageMosaic);
        gageTableColumnList.add(column);
        gageTableProductColumnMap.put(
                satelliteGageMosaic.getProductFilenamePrefix(), column);
        productNameLookup.put(satelliteGageMosaic.getProductName(),
                satelliteGageMosaic.getProductFilenamePrefix());

        GageTableProductDescriptor satelliteRadarGageMosaic = new GageTableProductDescriptor(
                "Satellite Radar Gage Mosaic", MPE_SRGMOSAIC,
                "Satellite Radar Gage Mosaic", "mpe_srgmosaic_dir",
                new String[] { MPE_LSATPRE, MPE_LMOSAIC });
        column = new GageTableColumn(satelliteRadarGageMosaic);
        column.setProductDescriptor(satelliteRadarGageMosaic);
        gageTableColumnList.add(column);
        gageTableProductColumnMap.put(
                satelliteRadarGageMosaic.getProductFilenamePrefix(), column);
        productNameLookup.put(satelliteRadarGageMosaic.getProductName(),
                satelliteRadarGageMosaic.getProductFilenamePrefix());

        //----------------------------------------------------------------------
        // Dual Pol Fields
        
        GageTableProductDescriptor DPradarMosaic= new GageTableProductDescriptor(
                "DP Radar Mosaic", MPE_RDMOSAIC, "DP Radar Mosaic",
                "mpe_rdmosaic_dir", new String[] { MPE_RMOSAIC, "RDMOSAIC" });
        column = new GageTableColumn(DPradarMosaic);
        column.setProductDescriptor(DPradarMosaic);
        gageTableColumnList.add(column);
        gageTableProductColumnMap.put(
                DPradarMosaic.getProductFilenamePrefix(), column);
        productNameLookup.put(DPradarMosaic.getProductName(),
                DPradarMosaic.getProductFilenamePrefix());
        
        GageTableProductDescriptor DPfieldBiasMosaic = new GageTableProductDescriptor(
                "DP Field Bias Radar Mosaic", MPE_BDMOSAIC, "DP Field Bias Radar Mosaic",
                "mpe_bdmosaic_dir", new String[] { MPE_RMOSAIC, "BDMOSAIC" });
        column = new GageTableColumn(DPfieldBiasMosaic);
        column.setProductDescriptor(DPfieldBiasMosaic);
        gageTableColumnList.add(column);
        gageTableProductColumnMap.put(
                DPfieldBiasMosaic.getProductFilenamePrefix(), column);
        productNameLookup.put(DPfieldBiasMosaic.getProductName(),
                DPfieldBiasMosaic.getProductFilenamePrefix());
        
        GageTableProductDescriptor DPlocalBiasMosaic = new GageTableProductDescriptor(
                "DP Local Bias Radar Mosaic", MPE_LDMOSAIC, "DP Local Bias Radar Mosaic",
                "mpe_ldmosaic_dir", new String[] { MPE_RMOSAIC, "LDMOSAIC" });
        column = new GageTableColumn(DPlocalBiasMosaic);
        column.setProductDescriptor(DPlocalBiasMosaic);
        gageTableColumnList.add(column);
        gageTableProductColumnMap.put(
                DPlocalBiasMosaic.getProductFilenamePrefix(), column);
        productNameLookup.put(DPlocalBiasMosaic.getProductName(),
                DPlocalBiasMosaic.getProductFilenamePrefix());
        
        GageTableProductDescriptor DPmultiSensorMosaic = new GageTableProductDescriptor(
                "DP Field Bias Multisensor Radar Mosaic", MPE_MDMOSAIC,
                "DP Field Bias Multisensor Radar Mosaic",
                "mpe_mdmosaic_dir", new String[] { MPE_RMOSAIC, "MDMOSAIC" });
        column = new GageTableColumn(DPmultiSensorMosaic);
        column.setProductDescriptor(DPmultiSensorMosaic);
        gageTableColumnList.add(column);
        gageTableProductColumnMap.put(
                DPmultiSensorMosaic.getProductFilenamePrefix(), column);
        productNameLookup.put(DPmultiSensorMosaic.getProductName(),
                DPmultiSensorMosaic.getProductFilenamePrefix());
        
        GageTableProductDescriptor DPlocalBiasMultiSensorMosaic = new GageTableProductDescriptor(
                "DP Local Bias Multisensor Radar Mosaic", MPE_MLDMOSAIC,"DP Local Bias Multisensor Radar Mosaic",
                "mpe_mldmosaic_dir", new String[] { MPE_RMOSAIC, "MLDMOSAIC" });
        column = new GageTableColumn(DPlocalBiasMultiSensorMosaic);
        column.setProductDescriptor(DPlocalBiasMultiSensorMosaic);
        gageTableColumnList.add(column);
        gageTableProductColumnMap.put(
                DPlocalBiasMultiSensorMosaic.getProductFilenamePrefix(), column);
        productNameLookup.put(DPlocalBiasMultiSensorMosaic.getProductName(),
                DPlocalBiasMultiSensorMosaic.getProductFilenamePrefix());
        
        GageTableProductDescriptor DPavgRadarMosaic = new GageTableProductDescriptor(
                "DP Avg Radar Mosaic", MPE_AVGRDMOSAIC, "DP Avg Radar Mosaic",
                "mpe_avgrdmosaic_dir", new String[] { MPE_RMOSAIC, "AVGRDMOSAIC" });
        column = new GageTableColumn(DPavgRadarMosaic);
        column.setProductDescriptor(DPavgRadarMosaic);
        gageTableColumnList.add(column);
        gageTableProductColumnMap.put(
                DPavgRadarMosaic.getProductFilenamePrefix(), column);
        productNameLookup.put(DPavgRadarMosaic.getProductName(),
                DPavgRadarMosaic.getProductFilenamePrefix());
        
        GageTableProductDescriptor DPmaxRadarMosaic = new GageTableProductDescriptor(
                "DP Max Radar Mosaic", MPE_MAXRDMOSAIC, "DP Max Radar Mosaic",
                "mpe_maxrdmosaic_dir", new String[] { MPE_RMOSAIC, "MAXRDMOSAIC" });
        column = new GageTableColumn(DPmaxRadarMosaic);
        column.setProductDescriptor(DPmaxRadarMosaic);
        gageTableColumnList.add(column);
        gageTableProductColumnMap.put(
                DPmaxRadarMosaic.getProductFilenamePrefix(), column);
        productNameLookup.put(DPmaxRadarMosaic.getProductName(),
                DPmaxRadarMosaic.getProductFilenamePrefix());
        
        GageTableProductDescriptor DPsatelliteRadarMosaic = new GageTableProductDescriptor(
                "DP Satellite Radar Mosaic", MPE_SRDMOSAIC, "DP Satellite Radar Mosaic",
                "mpe_srdmosaic_dir", new String[] { MPE_RMOSAIC, "SRDMOSAIC" });
        column = new GageTableColumn(DPsatelliteRadarMosaic);
        column.setProductDescriptor(DPsatelliteRadarMosaic);
        gageTableColumnList.add(column);
        gageTableProductColumnMap.put(
                DPsatelliteRadarMosaic.getProductFilenamePrefix(), column);
        productNameLookup.put(DPsatelliteRadarMosaic.getProductName(),
                DPsatelliteRadarMosaic.getProductFilenamePrefix());
        
        GageTableProductDescriptor DPsatelliteRadarGageMosaic = new GageTableProductDescriptor(
                "DP Satellite Radar Gage Mosaic", MPE_SRDGMOSAIC, "DP Satellite Radar Gage Mosaic",
                "mpe_srdgmosaic_dir", new String[] { MPE_RMOSAIC, "SRDGMOSAIC" });
        column = new GageTableColumn(DPsatelliteRadarGageMosaic);
        column.setProductDescriptor(DPsatelliteRadarGageMosaic);
        gageTableColumnList.add(column);
        gageTableProductColumnMap.put(
                DPsatelliteRadarGageMosaic.getProductFilenamePrefix(), column);
        productNameLookup.put(DPsatelliteRadarGageMosaic.getProductName(),
                DPsatelliteRadarGageMosaic.getProductFilenamePrefix());
   
        //----------------------------------------------------------------------
        
        GageTableProductDescriptor traiangulatedRadarMosaic = new GageTableProductDescriptor(
                "Triangulated Radar Mosaic", MPE_P3LMOSAIC,
                "Triangulated Radar Mosaic", "mpe_p3lmosaic_dir",
                new String[] { MPE_RMOSAIC });
        column = new GageTableColumn(traiangulatedRadarMosaic);
        column.setProductDescriptor(traiangulatedRadarMosaic);
        gageTableColumnList.add(column);
        gageTableProductColumnMap.put(
                traiangulatedRadarMosaic.getProductFilenamePrefix(), column);
        productNameLookup.put(traiangulatedRadarMosaic.getProductName(),
                traiangulatedRadarMosaic.getProductFilenamePrefix());

        GageTableProductDescriptor bestEstimateQPE = new GageTableProductDescriptor(
                "Best Estimate QPE", MPE_BESTQPE, "Best Estimate QPE",
                "mpe_qpe_dir", null);
        column = new GageTableColumn(bestEstimateQPE);
        column.setProductDescriptor(bestEstimateQPE);
        gageTableColumnList.add(column);
        gageTableProductColumnMap.put(
                bestEstimateQPE.getProductFilenamePrefix(), column);
        productNameLookup.put(bestEstimateQPE.getProductName(),
                bestEstimateQPE.getProductFilenamePrefix());

        GageTableProductDescriptor rfcMeanFieldBiasMosaic = new GageTableProductDescriptor(
                "RFC Mean Field Bias Mosaic", MPE_RFCBMOSAIC,
                "RFC Mean Field Bias Mosaic", "mpe_rfcbmosaic_dir",
                new String[] { MPE_RMOSAIC });
        column = new GageTableColumn(rfcMeanFieldBiasMosaic);
        column.setProductDescriptor(rfcMeanFieldBiasMosaic);
        gageTableColumnList.add(column);
        gageTableProductColumnMap.put(
                rfcMeanFieldBiasMosaic.getProductFilenamePrefix(), column);
        productNameLookup.put(rfcMeanFieldBiasMosaic.getProductName(),
                rfcMeanFieldBiasMosaic.getProductFilenamePrefix());

        GageTableProductDescriptor rfcMultiSensorMosaic = new GageTableProductDescriptor(
                "RFC Multi-sensor Mosaic", MPE_RFCMMOSAIC,
                "RFC Multi-sensor Mosaic", "mpe_rfcmmosaic_dir", new String[] {
                        MPE_RMOSAIC, MPE_RFCBMOSAIC });
        column = new GageTableColumn(rfcMultiSensorMosaic);
        column.setProductDescriptor(rfcMultiSensorMosaic);
        gageTableColumnList.add(column);
        gageTableProductColumnMap.put(
                rfcMultiSensorMosaic.getProductFilenamePrefix(), column);
        productNameLookup.put(rfcMultiSensorMosaic.getProductName(),
                rfcMultiSensorMosaic.getProductFilenamePrefix());

        GageTableProductDescriptor rfcBestQPEMosaic = new GageTableProductDescriptor(
                "RFC Best QPE Mosaic", MPE_RFCMOSAIC, "RFC Best QPE Mosaic",
                "mpe_rfcmosaic_dir", null);
        column = new GageTableColumn(rfcBestQPEMosaic);
        column.setProductDescriptor(rfcBestQPEMosaic);
        gageTableColumnList.add(column);
        gageTableProductColumnMap.put(
                rfcBestQPEMosaic.getProductFilenamePrefix(), column);
        productNameLookup.put(rfcBestQPEMosaic.getProductName(),
                rfcBestQPEMosaic.getProductFilenamePrefix());

        // new Q2 products
        GageTableProductDescriptor q2Mosaic = new GageTableProductDescriptor(
                "Q2 Mosaic", MPE_QMOSAIC, "Q2 Mosaic", "mpe_qmosaic_dir", null);
        column = new GageTableColumn(q2Mosaic);
        column.setProductDescriptor(q2Mosaic);
        gageTableColumnList.add(column);
        gageTableProductColumnMap.put(q2Mosaic.getProductFilenamePrefix(),
                column);
        productNameLookup.put(q2Mosaic.getProductName(),
                q2Mosaic.getProductFilenamePrefix());

        GageTableProductDescriptor q2LocalBiasMosaic = new GageTableProductDescriptor(
                "Q2 Local Bias Mosaic", MPE_LQMOSAIC, "Q2 Local Bias Mosaic",
                "mpe_lqmosaic_dir", new String[] { MPE_QMOSAIC });
        column = new GageTableColumn(q2LocalBiasMosaic);
        column.setProductDescriptor(q2LocalBiasMosaic);
        gageTableColumnList.add(column);
        gageTableProductColumnMap.put(
                q2LocalBiasMosaic.getProductFilenamePrefix(), column);
        productNameLookup.put(q2LocalBiasMosaic.getProductName(),
                q2LocalBiasMosaic.getProductFilenamePrefix());

        GageTableProductDescriptor q2MultiSensorMosaic = new GageTableProductDescriptor(
                "Q2 Multi-sensor Mosaic", MPE_MLQMOSAIC,
                "Q2 Multi-sensor Mosaic", "mpe_mlqmosaic_dir", new String[] {
                        MPE_QMOSAIC, MPE_LQMOSAIC });
        column = new GageTableColumn(q2MultiSensorMosaic);
        column.setProductDescriptor(q2MultiSensorMosaic);
        gageTableColumnList.add(column);
        gageTableProductColumnMap.put(
                q2MultiSensorMosaic.getProductFilenamePrefix(), column);
        productNameLookup.put(q2MultiSensorMosaic.getProductName(),
                q2MultiSensorMosaic.getProductFilenamePrefix());
    }

    /**
     * Determine the available products.
     */
    private void determineAvailableProducts() {
        getAvailableProductsFromMPEGenerateList();
        addQPEFieldType();
        addRFCMosaic();
        addBaseRadarMosaic();
    }

    /**
     * Get the available products from the MPE generate list.
     */
    private void getAvailableProductsFromMPEGenerateList() {
        final String MPE_GENERATE_LIST_TOKEN = "mpe_generate_list";
        final String DEFAULT_GENERATE_LIST = MPE_BESTQPE + ", " + MPE_BMOSAIC
                + "," + MPE_GAGEONLY + "," + MPE_LMOSAIC + "," + MPE_LSATPRE
                + "," + MPE_MLMOSAIC + "," + MPE_MMOSAIC + "," + MPE_RMOSAIC
                + "," + MPE_SATPRE 
                + "," + MPE_RDMOSAIC + "," + MPE_BDMOSAIC + "," + MPE_LDMOSAIC
                + "," + MPE_MDMOSAIC + "," + MPE_MLDMOSAIC
                ;

        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        GageTableColumn productColumn;
        String generateList;

        generateList = appsDefaults.getToken(MPE_GENERATE_LIST_TOKEN,
                DEFAULT_GENERATE_LIST);

        // generateList = MPE_BESTQPE + ", " + generateList;
        String[] products = generateList.split(",");

        // Force the addition of the XMRG field. The XMRG field is always
        // generated.
        GageTableColumn qpe = gageTableProductColumnMap.get(MPE_BESTQPE);
        addProductToGenerateList(qpe);

        for (String product : products) {
            productColumn = gageTableProductColumnMap.get(product);

            if (productColumn != null) {
                addProductToGenerateList(productColumn);
                // TODO - Log this
                System.out
                        .println("getAvailableProductsFromMPEGenerateList(): mpeProduct = "
                                + productColumn.getProductDescriptor()
                                        .getProductFilenamePrefix());
            } else {
                // TODO - Log this
                System.out
                        .println("getAvailableProductsFromMPEGenerateList(): "
                                + "Unrecognized mpe_generate_list value "
                                + product + ".");
            }
        }
    }

    /**
     * Add the QPE field type.
     */
    private void addQPEFieldType() {
        final String MPE_QPE_FIELDTYPE_TOKEN = "mpe_qpe_fieldtype";
        final String DEFAULT_QPE_FIELD_TYPE = MPE_MMOSAIC;

        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        String bestQPE = appsDefaults.getToken(MPE_QPE_FIELDTYPE_TOKEN,
                DEFAULT_QPE_FIELD_TYPE);

        GageTableColumn product = gageTableProductColumnMap.get(bestQPE);

        if (product == null) {
            System.out
                    .println("Invalid mpe_qpe_fieldtype variable: " + bestQPE);
            System.out.println("Using default mpe_qpe_fieldtype value: "
                    + DEFAULT_QPE_FIELD_TYPE);
            product = gageTableProductColumnMap.get(DEFAULT_QPE_FIELD_TYPE);
        }

        addProductToGenerateList(product);
    }

    /**
     * Add the RFC Mosaic.
     */
    private void addRFCMosaic() {
        final String GENERATE_AREAL_QPE_TOKEN = "mpe_generate_areal_qpe";
        final String DEFAULT_GENERATE_AREAL_QPE = "OFF";

        // The RFC Mosaic is handled in a special way.
        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        GageTableColumn column;

        String generateArealQPE = appsDefaults.getToken(
                GENERATE_AREAL_QPE_TOKEN, DEFAULT_GENERATE_AREAL_QPE);

        if (generateArealQPE.equalsIgnoreCase("ON")) {
            column = gageTableProductColumnMap.get(MPE_RFCMOSAIC);

            if (column != null) {
                if (!gageTableColumnList.contains(column)) {
                    gageTableColumnList.add(column);
                    checkProductDependencies(column);
                }
            }
        }
    }

    /**
     * Add the base radar mosaic.
     */
    private void addBaseRadarMosaic() {
        final String MPE_BASE_RADAR_MOSAIC_TOKEN = "mpe_base_radar_mosaic";
        final String DEFAULT_BASE_RADAR_MOSAIC = "RMOSAIC";

        // The Base Radar Mosaic is handled in a special way.
        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        GageTableColumn productColumn;

        String baseRadarMosaic = appsDefaults.getToken(
                MPE_BASE_RADAR_MOSAIC_TOKEN, DEFAULT_BASE_RADAR_MOSAIC);

        productColumn = gageTableProductColumnMap.get(baseRadarMosaic);

        if (productColumn != null) {
            if (!gageTableColumnList.contains(productColumn)) {
                gageTableColumnList.add(productColumn);
                checkProductDependencies(productColumn);
            }
        }
    }

    
    //  Should we add code to handle a DP base radar ????
    
    
    /**
     * Add the product to the generate list.
     * 
     * @param productDescriptor
     *            The product to add to the list
     */
    private void addProductToGenerateList(GageTableColumn productColumn) {
        if (productColumn != null) {
            if (!getAvailableGageTableColumnList().contains(productColumn)) {
                getAvailableGageTableColumnList().add(productColumn);
                checkProductDependencies(productColumn);
            }
        }

    }

    /**
     * Check the GageTableProductDescriptor dependencies.
     * 
     * @param product
     *            The product to check dependencies on
     */
    private void checkProductDependencies(GageTableColumn column) {
        String dependency;
        GageTableColumn dependentProductColumn;

        if (column.getProductDescriptor().getDependencies() != null) {
            GageTableProductDescriptor product = column.getProductDescriptor();
            for (int i = 0; i < product.getDependencies().length; i++) {
                dependency = product.getDependencies()[i];
                dependentProductColumn = getGageTableProductColumnMap().get(
                        dependency);

                if (dependentProductColumn != null) {
                    if (!getAvailableGageTableColumnList().contains(
                            dependentProductColumn)) {
                        getAvailableGageTableColumnList().add(
                                dependentProductColumn);
                        checkProductDependencies(dependentProductColumn);
                    }
                } else {
                    // TODO log this statement
                    System.out.println("For product "
                            + product.getProductName()
                            + " unrecognized dependency " + dependency);
                }

            }
        }
    }

    /**
     * The list of available MPE products is initially ordered by how they are
     * specified in the mpe_generate_list token. Reorder the products to match
     * how they appear on the PrecipFields menu.
     */
    private void orderAvailableProductsToMatchPrecipFieldsMenu() {
        // Iterate over the list of all possible MPE Products. It is
        // assumed that this list is in the same order as the items
        // on the MPE Editor PrecipFields menu.
        GageTableColumn product;
        List<GageTableColumn> tempListOfAvailableProductDescriptors;
        tempListOfAvailableProductDescriptors = new ArrayList<GageTableColumn>();
        Iterator<GageTableColumn> gageTableProduct = gageTableColumnList
                .iterator();

        while (gageTableProduct.hasNext()) {
            product = gageTableProduct.next();

            if (availableGageTableColumnList.contains(product)) {
                tempListOfAvailableProductDescriptors.add(product);
            }
        }

        availableGageTableColumnList = tempListOfAvailableProductDescriptors;
    }

    /**
     * @return the gageTableProductDescList
     */
    public List<GageTableColumn> getGageTableProductDescList() {
        return gageTableColumnList;
    }

    /**
     * @param gageTableProductDescList
     *            the gageTableProductDescList to set
     */
    public void setGageTableColumnList(List<GageTableColumn> gageTableColumnList) {
        this.gageTableColumnList = gageTableColumnList;
    }

    /**
     * @return the gageTableProductDescMap
     */
    public Map<String, GageTableColumn> getGageTableProductColumnMap() {
        return gageTableProductColumnMap;
    }

    /**
     * @param gageTableProductDescMap
     *            the gageTableProductDescMap to set
     */
    public void setGageTableProductColumnMap(
            Map<String, GageTableColumn> gageTableProductColumnMap) {
        this.gageTableProductColumnMap = gageTableProductColumnMap;
    }

    /**
     * @return the availableGageTableProductList
     */
    public List<GageTableColumn> getAvailableGageTableColumnList() {
        return availableGageTableColumnList;
    }

    /**
     * @param availableGageTableProductList
     *            the availableGageTableProductList to set
     */
    public void setAvailableGageTableColumnList(
            List<GageTableColumn> availableGageTableColumnList) {
        this.availableGageTableColumnList = availableGageTableColumnList;
    }

    /**
     * @return the originalColumns
     */
    public List<GageTableColumn> getOriginalColumns() {
        return originalColumns;
    }

    /**
     * @param originalColumns
     *            the originalColumns to set
     */
    public void setOriginalColumns(List<GageTableColumn> originalColumns) {
        this.originalColumns = originalColumns;
    }

    /**
     * @return the selectedColumns
     */
    public List<GageTableColumn> getSelectedColumns() {
        return selectedColumns;
    }

    /**
     * @param selectedColumns
     *            the selectedColumns to set
     */
    public void setSelectedColumns(List<GageTableColumn> selectedColumns) {
        setOriginalColumns(this.selectedColumns);
        this.selectedColumns = selectedColumns;

    }

    /**
     * Add a GageTable Listener.
     * 
     * @param sdl
     */
    public void addGageTableListener(GageTableListener gtl) {
        gageTableListenerList.clear();
        gageTableListenerList.add(gtl);
    }

    /**
     * Fire a StationDisplayUpdateEvent.
     */
    public void fireUpdateEvent(GageTableUpdateEvent event) {
        Iterator<GageTableListener> iter = gageTableListenerList.iterator();

        while (iter.hasNext()) {
            (iter.next()).notifyUpdate(event);
        }
    }

    /**
     * Lookup a product name and get the product's prefix.
     * 
     * @param name
     *            The product name to lookup
     * @return The product's prefix
     */
    public String lookupProductPrefix(String name) {
        return productNameLookup.get(name);
    }
}
