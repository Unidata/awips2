package gov.noaa.nws.ncep.edex.plugin.ncgrib;

import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;

public class GridLookupFileName {
    /** The singleton instance of GridLookupFileName **/
    private static GridLookupFileName instance;

    private String[] modelNames = { "cmce", "gefs", "gww", "naefsBC",
            "naefsUS", "naefsAK", "sref" };

    private String[] template = {
            "cmc_gep.*|cmce;cmc_gec.*|cmcec;cmc_geavg.*|cmceMean;cmc_gespr.*|cmceSpread",
            "gec00.*bc.*|gefscBC;gec00.*anl|gefscAnal;gec00.*[0-9]|gefsc;gep.*anl|gefsAnal;gep.*bc.*|gefsBC;geavg.*bc.*|gefsMeanBC;gespr.*bc.*|gefsSpreadBC;gespr.*|gefsSpread",
            "mean.*|gwwMean;probab.*|gwwProb;spread.*|gwwSpread",
            "naefs_geavg.*bc.*|naefsMeanBC;naefs_geavg.*anv.*|naefsMean;naefs_gespr.*bc.*|naefsSpreadBC;naefs_ge10pt.*bc.*|naefs10ptBC;naefs_ge50pt.*bc.*|naefs50ptBC;naefs_ge90pt.*bc.*|naefs90ptBC;naefs_gemode.*bc.*|naefsModeBC",
            "naefs_geavg.*conus.*grib2|naefsMeanUS;naefs_ge10pt.*conus.*grib2|naefs10ptUS;naefs_ge50pt.*conus.*grib2|naefs50ptUS;naefs_ge90pt.*conus.*grib2|naefs90ptUS;naefs_gemode.*conus.*grib2|naefsModeUS",
            "naefs_geavg.*alaska.*grib2|naefsMeanAK;naefs_ge10pt.*alaska.*grib2|naefs10ptAK;naefs_ge50pt.*alaska.*grib2|naefs50ptAK;naefs_ge90pt.*alaska.*grib2|naefs90ptAK;naefs_gemode.*alaska.*grib2|naefsModeAK",
            "nam.*.awip3d00.*tm00|nam40;sref_nam.*ctl1.*|srefNamCtl1;sref_nam.*ctl2.*|srefNamCtl2;sref_nam.*[pn][1-9].*|srefNam;sref_eta.*[pn][1-9].*|srefEta;sref_eta.*ctl1.*|srefEtaCtl1;sref_eta.*ctl2.*|srefEtaCtl2;sref_rsm.*ctl.*|srefRsmCtl;sref_rsm.*[pn][1-9].*|srefRsm;sref_nmm.*ctl.*|srefNmmCtl;sref_nmm.*[pn][1-9].*|srefNmm;sref_em.*ctl.*|srefEmCtl;sref_em.*[pn][1-9].*|srefEm" };

    private final Map<String, String> models;

    public static GridLookupFileName getInstance() {
        if (instance == null) {
            instance = new GridLookupFileName();
        }
        return instance;
    }

    private GridLookupFileName() {
        models = new HashMap<String, String>();

        initModels();
    }

    private void initModels() {
        for (int i = 0; i < modelNames.length; i++) {
            models.put(modelNames[i].toUpperCase(), template[i]);
        }
    }

    public String getModelName(String filename) {
        String modelname = null;

        // if (model.equalsIgnoreCase("gww") && gridId.equalsIgnoreCase("229"))
        // {
        // return modelname;
        // }
        //
        // String template = models.get(model.toUpperCase());
        for (String template : this.template) {
            String[] tokens = template.split(";");
            // System.out.println ( " CMC ensemble " + "!!!\n");

            for (String token : tokens) {
                String[] alias = token.split("\\|");
                if (Pattern.matches(alias[0], filename)) {
                    modelname = alias[1];

                    // perturbation number
                    // String[] pert = filename.split("\\.");
                    // if ( pert[3].startsWith("p") || pert[3].startsWith("n"))
                    // {
                    // nbm.setPerturbationNumber(pert[3]);
                    // }
                    break;
                }
            }
        }
        return modelname;
    }
}
