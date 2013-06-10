package gov.damcat.data;

import java.sql.*;
import java.util.*;
import java.util.Date;
import javax.swing.*;
import gov.dambreak.util.*;
import ohd.hseb.db.*;
import gov.damcat.db.*;


/**
 * This class allows data in the damcat database to be read, edited, and exported
 * to/from the SMPDBK GUI.
 */

public class DBAccess 
{
    private java.sql.Connection conn = null;
    private String DBDriver, DBConnectionString;
    private boolean bConnected = false;
    
    private ohd.hseb.db.Database  _db = null;		// to interface with JDBGen
    
    /**
     * Constructor for DBAccess Class
     *  
     * Creation date: (7/14/2003 12:46:52 PM)
     */
    public DBAccess() {
        bConnected = connect();
    }
    /**
     * Calculate slope using same formula used in Simplified DamBreak program
     * as revised on 10/29/1996
     *
     * Creation date: (1/20/2004 2:14:23 PM)
     * @return float
     * @param dam gov.damcat.data.DamInfo
     */
    private float calculateSlope(DamInfo dam) {
        
        String header = "DBAccess.calculateSlope()";
        // System.out.println(header);
        
        // first set default value for slope
        
        float slope = 0.5f;
        
        // next set constant variables
        
        double m = 0.5;
        double mPlus1 = m + 1.0;
        
        // then set intermediate results in calculation of xl (Length of Reservoir)
        // where xl = (xlFactor1 / xlFactor2) * 6.0 / xlFactor3
        //
        // 		 xlFactor1 = ((mPlus1)*volume*43560)
        //		 xlFactor2 = k*pow(height, mPlus1)
        // 		 xlFactor3 = (1.0 + 4*pow(m, mPlus1))
        //
        //		 k = crestLength / (sqrt(height))
        
        double xl = 0.0;			// length of reservoir in feet
        double xlMiles = 0.0;		// length of reservoir in miles
        double xlFactor1 = 0.0;
        double xlFactor2 = 0.0;
        double xlFactor3 = 0.0;
        double k = 0.0; 
        
        // then initialize dam measurements
        
        double height = 0.0;		// Height of Dam in ft
        double storage = 0.0;		// Storage volume of Dam in Acre-ft
        double crestLength = 0.0;	// Length of Dam in ft
        
        // locate actual dam measurements :
        
        // 	for height
        if (dam.nid_height.floatValue() > 0.0f)
        {
            height = dam.nid_height.floatValue();
        } 
        else 
            if (dam.dam_height.floatValue() > 0.0f)
            {
                height = dam.dam_height.floatValue();
            }
            else
                if (dam.structural_height.floatValue() > 0.0f)
                {
                    height = dam.structural_height.floatValue();
                }
                else
                    if (dam.hydraulic_height.floatValue() > 0.0f)
                    {
                        height = dam.hydraulic_height.floatValue();
                    }
                    
                    // for storage
        if (dam.nid_storage.floatValue() > 0.0f)
        {
            storage = dam.nid_storage.floatValue();
        }
        else
            if (dam.max_storage.floatValue() > 0.0f)
            {
                storage = dam.max_storage.floatValue();
            }
            else
                if (dam.normal_storage.floatValue() > 0.0f)
                {
                    storage = dam.normal_storage.floatValue();
                }
                
                // for crestLength
        if (dam.dam_length.floatValue() > 0.0f)
        {
            crestLength = dam.dam_length.floatValue();
        }
        
        
        // if insufficient measurements, don't bother with calculation, just return default value
        if (height == 0.0 || storage == 0.0 || crestLength == 0.0)
        {
            return slope;
        }
        
        // System.out.println("height = " + height + " storage = " + storage + " crestLength = " + crestLength);
        
        // calculate k = crestLength / (sqrt(height))
        k = crestLength / (Math.sqrt(height));
        // System.out.println("k = " + k);
        
        // calculate xlFactor1 = ((mPlus1)*storage*43560)   
        //									NOTE: 43560 is conversion factor from acre-ft to cubic feet 
        xlFactor1 = mPlus1 * storage * 43560.0;
        // System.out.println("xlFactor1 = " + xlFactor1);
        
        // calculate xlFactor2 = k*pow(height, mPlus1)	
        xlFactor2 = k * Math.pow(height, mPlus1);
        // System.out.println("xlFactor2 = " + xlFactor2);
        
        // calculate xlFactor3 = (1.0 + 4*pow(m, mPlus1))
        xlFactor3 = 1.0 + (4.0 * Math.pow(m, mPlus1));
        // System.out.println("xlFactor3 = " + xlFactor3);
        
        // calculate xl= (xlFactor1 / xlFactor2) * 6.0 / xlFactor3
        xl = ((xlFactor1 / xlFactor2) * 6.0) / xlFactor3;
        // System.out.println("xl = " + xl);
        
        // convert from feet to miles
        xlMiles = xl / 5280.0;
        // System.out.println("xlMiles = " + xlMiles);	
        
        // calculate slope as ft / mile
        double dSlope = height / xlMiles;
        // System.out.println("dSlope = " + dSlope);
        
        // cast from double to float
        Double doubleSlope = new Double(dSlope);	
        slope = doubleSlope.floatValue(); 
        
        return slope;
    }
    /**
     * 
     * Transitioned to JDBGen Structures
     *
     * Creation date: (7/14/2003 12:50:08 PM)
     */
    private boolean connect() {
        
        getConfigInfo();
        
        if (DBDriver == null || DBConnectionString == null)
            return false;
        
        
        // **** JDBGen Logic Goes Here
        try {
            
            _db = new ohd.hseb.db.Database();
            _db.setDriverClassName(DBDriver);
            _db.connect(DBConnectionString);
            conn = _db.getConnection();
            
        } catch (Exception ex) {
            System.out.println("ERROR: Could not connect. (" + DBDriver + ")");
            ex.printStackTrace();        
            return false;
        }
        
        return true;
    }
    /**
     * Insert the method's description here.
     * Creation date: (8/13/2004 4:01:35 PM)
     * @return boolean
     * @param damInfo gov.damcat.data.DamInfo
     */
    public boolean deleteDamFeatures(DamInfo damInfo) {
        String header = "DBAccess.deleteDamFeatures() ";
        
        boolean bError = false;
        
        // JDBGen Code Here
        
        DamFeaturesRecord featuresRow = new DamFeaturesRecord() ;
        DamFeaturesTable featuresTable = new DamFeaturesTable(_db) ;
        
        try {
            featuresRow.setNidid(damInfo.getNidid());
            featuresRow.setOther_dam_name(damInfo.getOther_dam_name());
            featuresRow.setDam_former_name(damInfo.getDam_former_name());
            featuresRow.setStateid(damInfo.getStateid());
            featuresRow.setSection_t_r(damInfo.getSection_t_r());
            featuresRow.setOwner_name(damInfo.getOwner_name());
            featuresRow.setOwner_type(damInfo.getOwner_type());
            featuresRow.setDam_designer(damInfo.getDam_designer());
            featuresRow.setPrivate_on_federal(damInfo.getPrivate_on_federal());
            featuresRow.setDam_type(damInfo.getDam_type());
            featuresRow.setCore(damInfo.getCore());
            featuresRow.setFoundation(damInfo.getFoundation());
            featuresRow.setPurposes(damInfo.getPurposes());
            featuresRow.setYear_completed(damInfo.getYear_completed());
            featuresRow.setYear_modified(damInfo.getYear_modified());
            featuresRow.setEmerg_action_plan(damInfo.getEmerg_action_plan());
            featuresRow.setInspection_date(damInfo.getInspection_date());
            featuresRow.setInspection_freq(damInfo.getInspection_freq());
            featuresRow.setSt_reg_dam(damInfo.getSt_reg_dam());
            featuresRow.setSt_reg_agency(damInfo.getSt_reg_agency());
            featuresRow.setSpillway_type(damInfo.getSpillway_type());
            featuresRow.setOutlet_gates(damInfo.getOutlet_gates());
            featuresRow.setFed_funding(damInfo.getFed_funding());
            featuresRow.setFed_design(damInfo.getFed_design());
            featuresRow.setFed_construction(damInfo.getFed_construction());
            featuresRow.setFed_regulatory(damInfo.getFed_regulatory());
            featuresRow.setFed_inspection(damInfo.getFed_inspection());
            featuresRow.setFed_operation(damInfo.getFed_operation());
            featuresRow.setFed_owner(damInfo.getFed_owner());
            featuresRow.setFed_other(damInfo.getFed_other());
            featuresRow.setSource_agency(damInfo.getSource_agency());
            featuresRow.setTopo_map(damInfo.getTopo_map());
            featuresRow.setComments(damInfo.getComments());
            featuresRow.setPrebreak_avail(damInfo.getPrebreak_avail());
            
            String updatedString = getCurrentTime();
            long updated = DbTimeHelper.getLongTimeFromDateTimeString(updatedString);
            featuresRow.setUpdated(updated);
            
            featuresRow.setSpillway_width(damInfo.getSpillway_width().doubleValue());
            featuresRow.setNumber_locks(damInfo.getNumber_locks().doubleValue());
            featuresRow.setLength_locks(damInfo.getLength_locks().doubleValue());
            featuresRow.setWidth_locks(damInfo.getWidth_locks().doubleValue());
            featuresRow.setDrainage_area(damInfo.getDrainage_area().doubleValue());
            featuresRow.setDam_length(damInfo.getDam_length().doubleValue());
            featuresRow.setDam_height(damInfo.getDam_height().doubleValue());
            
            featuresRow.setStructural_height(damInfo.getStructural_height().doubleValue());
            
            featuresRow.setHydraulic_height(damInfo.getHydraulic_height().doubleValue());
            featuresRow.setNid_height(damInfo.getNid_height().doubleValue());
            featuresRow.setMax_discharge(damInfo.getMax_discharge().doubleValue());
            featuresRow.setVolume_dam(damInfo.getVolume_dam().doubleValue());
            featuresRow.setNormal_storage(damInfo.getNormal_storage().doubleValue());
            featuresRow.setNid_storage(damInfo.getNid_storage().doubleValue());
            featuresRow.setSurface_area(damInfo.getSurface_area().doubleValue());
            featuresRow.setElev(damInfo.getElev().doubleValue());
            
            featuresRow.setReturn_flow_region(damInfo.getReturn_flow_region().intValue());
            
            featuresTable.delete(featuresRow);
            
        } catch (Exception e) {
            JOptionPane.showMessageDialog(null,"Error: Could not delete DamFeatures database table.");
            e.printStackTrace();
            bError = true;
        }
        return !bError;
    }
    /**
     * Delete only the row in the DamMaster table 
     * Creation date: (1/27/2004 12:16:53 PM)
     * @return boolean
     * @param damInfo gov.damcat.data.DamInfo
     */
    public boolean deleteDamMaster(DamInfo damInfo) {
        
        String header = "DBAccess.deleteDamMaster table() ";
        
        boolean bError = false;
        
        
        
        // JDBGen Code Here
        
        DamMasterRecord masterRow = new DamMasterRecord() ;
        DamMasterTable masterTable = new DamMasterTable(_db);
        
        try {
            masterRow.setNidid(damInfo.getNidid());
            masterRow.setDam_name(damInfo.getDam_name());
            masterRow.setCounty(damInfo.getCounty());
            masterRow.setRiver(damInfo.getRiver());
            masterRow.setDownstream_hazard(damInfo.getDownstream_hazard());
            masterRow.setHsa(damInfo.getHsa());
            masterRow.setRfc(damInfo.getRfc());
            masterRow.setMax_storage(damInfo.getMax_storage().doubleValue());
            masterRow.setLongitude_dam(damInfo.getLongitude_dam().doubleValue());     		
            masterRow.setLatitude_dam(damInfo.getLatitude_dam().doubleValue());
            
            masterTable.delete(masterRow);
            
        } catch (Exception e) {
            JOptionPane.showMessageDialog(null,"Error: Could not delete DamMaster database table.");
            e.printStackTrace();
            bError = true;
        }
        return !bError;
    }
    /**
     * Delete row from damcat_down table
     * Creation date: (1/27/2004 12:30:16 PM)
     * @return boolean
     * @param damID java.lang.String
     * @param downEntry gov.damcat.data.DownstreamEntryInfo
     */
    public boolean deleteDownstream(String damID, DownstreamEntryInfo downEntry) {
        
        String header = "DBAccess.deleteDownstream() ";
        // System.out.println(header);
        
        boolean bError = false;
        
        // JDBGen Code Here
        
        DownStreamRecord downRow = new DownStreamRecord();
        DownStreamTable downTable = new DownStreamTable(_db);
        
        try {
            
            DownstreamEntryInfo down = downEntry;
            
            downRow.setNidid(damID);
            
            downRow.setDown_name(down.getDown_name());
            downRow.setComments(down.getComments());
            downRow.setXsec_best_type(down.getXsec_best_type());				
            
            String updatedString = getCurrentTime();
            long updated = DbTimeHelper.getLongTimeFromDateTimeString(updatedString);
            downRow.setUpdated(updated);
            
            downRow.setLongitude(down.getLongitude().doubleValue());
            downRow.setLatitude	(down.getLatitude().doubleValue());
            downRow.setElevation(down.getElevation().doubleValue());
            downRow.setDistance_from_dam(down.getDistance_from_dam().doubleValue());
            downRow.setFlood_flow(down.getFlood_flow().doubleValue());
            downRow.setFlood_depth(down.getFlood_depth().doubleValue());
            downRow.setFlood_width(down.getFlood_width().doubleValue());
            downRow.setMann_oc (down.getMann_oc().doubleValue());	
            
            downTable.delete(downRow);
            
        } catch (Exception e) {
            JOptionPane.showMessageDialog(null,"Error: Could not delete Downstream database table.");
            e.printStackTrace();
            bError = true;
        }
        return !bError;
    }
    /**
     * Delete Dam from damcat_dam table and all dependent tables
     *
     * Navigate DamInfo structure to determine which specific rows 
     *    are to be deleted from dependent tables
     *
     * Creation date: (1/22/2004 6:05:00 PM)
     * @return boolean
     * @param damInfo gov.damcat.data.DamInfo
     */
    public boolean deleteEntireDam(DamInfo damInfo) {
        
        String damID = damInfo.getNidid();
        
        for (int j=0; j<damInfo._outputEntries.size(); j++) {
            
            OutputEntryInfo out = (OutputEntryInfo)damInfo._outputEntries.get(j);
            deleteSdbOut(damID, out);
            
        }
        
        for (int j=0; j<damInfo._crossSectionEntries.size(); j++) {
            
            CrossSectionEntryInfo xs = (CrossSectionEntryInfo)damInfo._crossSectionEntries.get(j);
            deleteSectionPair(damID, xs);
            
        }
        
        for (int j=0; j<damInfo._downstreamEntries.size(); j++) {
            
            DownstreamEntryInfo down = (DownstreamEntryInfo)damInfo._downstreamEntries.get(j);
            deleteDownstream(damID, down);
            
        }
        
        
        for (int j=0; j<damInfo._inputEntries.size(); j++) {
            
            InputEntryInfo in = (InputEntryInfo)damInfo._inputEntries.get(j);
            deleteSdbIn(damID, in);
            
        }
        
        deleteDamFeatures(damInfo);
        
        deleteDamMaster(damInfo);
        
        return false;
    }
    /**
     * Delete row from damcat_in table.
     * Creation date: (1/27/2004 12:23:06 PM)
     * @return boolean
     * @param in gov.damcat.data.InputEntryInfo
     */
    public boolean deleteSdbIn(String damID, InputEntryInfo in) {
        
        String header = "DBAccess.deleteSdbIn():";
        System.out.println(header);
        
        boolean bError = false;
        
        // JDBGen Code Here
        
        SdbInRecord inRow = new SdbInRecord();
        SdbInTable inTable = new SdbInTable(_db);
        
        try {
            
            inRow.setNidid(damID);
            inRow.setSrc(in.getSrc());
            inRow.setScenario(in.getScenario());
            inRow.setComments(in.getComments());
            
            String updatedString = getCurrentTime();
            long updated = DbTimeHelper.getLongTimeFromDateTimeString(updatedString);
            inRow.setUpdated(updated);
            
            inRow.setHde(in.getHde().doubleValue());
            inRow.setBme(in.getBme().doubleValue());
            inRow.setVol(in.getVol().doubleValue());
            inRow.setSa(in.getSa().doubleValue());
            inRow.setTfm(in.getTfm().doubleValue());
            inRow.setQo(in.getQo().doubleValue());
            inRow.setBw(in.getBw().doubleValue());		
            
            inRow.setIdam(in.getIdam().intValue());
            
            inTable.delete(inRow);
            
        } catch (Exception e) {
            JOptionPane.showMessageDialog(null,"Error: Could not delete SdbIn database table.");
            e.printStackTrace();
            bError = true;
        }
        
        return !bError;
    }
    /**
     * Delete row from damcat_out table
     * Creation date: (1/27/2004 12:34:01 PM)
     * @return boolean
     * @param damID java.lang.String
     * @param outEntry gov.damcat.data.OutputEntryInfo
     */
    public boolean deleteSdbOut(String damID, OutputEntryInfo outEntry) {
        
        String header = "DBAccess.deleteSdbOut():";
        System.out.println(header);
        boolean bError = false;
        
        // *** JDBGen Code Here
        
        SdbOutRecord outRow = new SdbOutRecord();
        SdbOutTable outTable = new SdbOutTable(_db);
        
        try {
            
            OutputEntryInfo out = outEntry;
            
            outRow.setNidid(damID);
            
            outRow.setSrc(out.getSrc());
            outRow.setScenario(out.getScenario());
            outRow.setDown_name(out.getDown_name());
            // System.out.println(out.getDown_name());
            
            outRow.setComments(out.getComments());
            
            String updatedString = getCurrentTime();
            long updated = DbTimeHelper.getLongTimeFromDateTimeString(updatedString);
            outRow.setUpdated(updated);
            
            outRow.setSlope(out.getSlope().doubleValue());
            outRow.setMax_flow(out.getMax_flow().doubleValue());
            outRow.setMax_depth(out.getMax_depth().doubleValue());
            outRow.setTime_max_depth(out.getTime_max_depth().doubleValue());
            outRow.setTime_flood(out.getTime_flood().doubleValue());
            outRow.setTime_deflood(out.getTime_deflood().doubleValue());
            
            outTable.delete(outRow);		
            
        } catch (Exception e) {
            JOptionPane.showMessageDialog(null,"Error: Could not delete SdbOut database table.");
            e.printStackTrace();
            bError = true;
        }
        return !bError;
    }
    /**
     * Delete row from damcat_pair table
     * Creation date: (1/27/2004 12:39:41 PM)
     * @return boolean
     * @param damID java.lang.String
     * @param pairEntry gov.damcat.data.CrossSectionEntryInfo
     */
    public boolean deleteSectionPair(String damID, CrossSectionEntryInfo pairEntry) {
        
        String header = "DBAccess.deleteSectionPair():";
        boolean bError = false;
        
        System.out.println(header);
        
        SectionPairRecord pairRow = new SectionPairRecord();
        SectionPairTable pairTable = new SectionPairTable(_db);
        
        try {
            
            
            CrossSectionEntryInfo xs = pairEntry;
            
            pairRow.setNidid(damID);
            
            pairRow.setDown_name(xs.getDown_name());
            pairRow.setXsec_type(xs.getXsec_type());
            
            String updatedString = getCurrentTime();
            long updated = DbTimeHelper.getLongTimeFromDateTimeString(updatedString);
            pairRow.setUpdated(updated);
            
            pairRow.setElev(xs.getElev().doubleValue());
            pairRow.setTw(xs.getTw().doubleValue());
            pairRow.setMann_n(xs.getMann_n().doubleValue());
            pairRow.setInactive_width(xs.getInactive_width().doubleValue());
            
            pairRow.setPair_num(xs.getPair_num().intValue());
            
            pairTable.delete(pairRow);
            
        } catch (Exception e) {
            JOptionPane.showMessageDialog(null,"Error: Could not delete SectionPair database table.");
            e.printStackTrace();
            bError = true;
        }
        
        return !bError;
        
    }
    /**
     * Delete Dam from damcat_dam table
     *
     * NOTE: Delete rules will be cascaded to dependent tables
     *
     * Creation date: (1/27/2004 11:50:57 AM)
     * @return boolean
     * @param damID java.lang.String
     */
    public boolean deleteSelectedDam(String damID) {
        
        String header = "DBAccess.deleteEntireDam() ";
        
        boolean bError = false;
        
        String whereClause = "WHERE nidid = '" + damID.trim() +"'";
        
        // JDBGen Code Here
        
        // DamCat_OutRecord outRow = new DamCat_OutRecord();
        SdbOutTable outTable = new SdbOutTable(_db);
        
        // DamCat_PairRecord pairRow = new DamCat_PairRecord();
        SectionPairTable pairTable = new SectionPairTable(_db);
        
        // DamCat_DownRecord downRow = new DamCat_DownRecord();
        DownStreamTable downTable = new DownStreamTable(_db);
        
        // DamCat_InRecord inRow = new DamCat_InRecord();
        SdbInTable inTable = new SdbInTable(_db);
        
        // DamCat_DamRecord damRow = new DamCat_DamRecord() ;
        DamFeaturesTable featuresTable = new DamFeaturesTable(_db);
        
        DamMasterTable masterTable = new DamMasterTable(_db);
        
        try {
            
            outTable.delete(whereClause);
            
            pairTable.delete(whereClause);
            
            downTable.delete(whereClause);
            
            inTable.delete(whereClause);
            
            featuresTable.delete(whereClause);
            
            masterTable.delete(whereClause);
            
        } catch (Exception e) {
            JOptionPane.showMessageDialog(null,"Error: Could not delete entire dam " + damID);
            e.printStackTrace();
            bError = true;
        }
        
        return !bError; 
        
    }
    /**
     * Build where clause to delete from damcat_output table based on parameters passed
     * To delete ALL scenarios, set scenario to null
     * To delete ALL sources, set src to null
     *
     * NOTE: The delete rules will also be cascaded to damcat_out table 
     *
     * Creation date: (1/27/2004 3:11:57 PM)
     * @return boolean
     * @param damID java.lang.String
     * @param src java.lang.String
     * @param scenario java.lang.String
     */
    public boolean deleteSelectedInputs(String damID, String src, String scenario) {
        
        String header = "DBAccess.deleteSelectedInputs():";
        boolean bError = false;
        
        //System.out.println(header);
        
        SdbInRecord inRow = new SdbInRecord();
        SdbInTable inTable = new SdbInTable(_db);
        
        
        String whereClause = "WHERE nidid = '" + damID.trim() + "'";
        
        if ((src != null))
        {
            String srcClause = " AND src = '" + src.trim() + "'"; 
            whereClause = whereClause + srcClause;
        }
        
        if ((scenario != null)) 
        {
            String scenarioClause = " AND scenario = '" + scenario.trim() + "'"; 
            whereClause = whereClause + scenarioClause;
        }
        
        
        try {
            
            /*
             if(src.startsWith("#"))
             {
             System.out.println("Starts with '#' in DBAccess.deleteSelectedInputs");
             inTable.delete(whereClause);
             }
             else
             {
             // *** first delete corresponding damcat_out entries
              deleteSelectedOutputs(damID, src, scenario, null);
              inTable.delete(whereClause);
              }
              */
            
            // *** first delete corresponding damcat_out entries
            
            deleteSelectedOutputs(damID, src, scenario, null);
            
            inTable.delete(whereClause);
            
        } catch (Exception e) {
            JOptionPane.showMessageDialog(null,"Error: Could not delete selected damcat_input rows.");
            e.printStackTrace();
            bError = true;
        }
        
        return !bError;
    }
    /**
     * Build where clause to delete from damcat_output table based on parameters passed
     * To delete ALL downNames, set downName to null
     * To delete ALL scenarios, set scenario to null
     * To delete ALL sources, set src to null
     *
     * Creation date: (1/27/2004 1:11:58 PM)
     * @return boolean
     * @param damId java.lang.String
     * @param src java.lang.String
     * @param scenario java.lang.String
     * @param downName java.lang.String
     */
    public boolean deleteSelectedOutputs(String damID, String src, String scenario, String downName) {
        
        String header = "DBAccess.deleteSelectedOutputs():";
        boolean bError = false;
        
        //System.out.println(header);
        
        SdbOutRecord outRow = new SdbOutRecord();
        SdbOutTable outTable = new SdbOutTable(_db);
        
        
        String whereClause = "WHERE nidid = '" + damID.trim() + "'";
        
        if ((src != null))
        {
            String srcClause = " AND src = '" + src.trim() + "'"; 
            whereClause = whereClause + srcClause;
        }
        
        if ((scenario != null)) 
        {
            String scenarioClause = " AND scenario = '" + scenario.trim() + "'"; 
            whereClause = whereClause + scenarioClause;
        }
        
        if ((downName != null)) 
        {
            String downClause = " AND down_name = '" + downName.trim() + "'"; 
            whereClause = whereClause + downClause;
        }
        
        try {
            
            outTable.delete(whereClause);
            
        } catch (Exception e) {
            JOptionPane.showMessageDialog(null,"Error: Could not delete selected damcat_output rows.");
            e.printStackTrace();
            bError = true;
        }
        
        return !bError;
    }
    /**
     * Build where clause to delete from damcat_pairs table based on input parameters
     * To delete ALL pairs, set pairNum to -1
     * To delete ALL xsecTypes, set xsecType to null
     * To delete ALL downNames, set downName to null
     *
     * Creation date: (1/27/2004 12:48:35 PM)
     * @return boolean
     * @param damID java.lang.String
     * @param downName java.lang.String
     * @param xsecType java.lang.String
     * @param pairNum int
     */
    public boolean deleteSelectedPairs(String damID, String downName, String xsecType, int pairNum) {
        
        String header = "DBAccess.deleteSelectedPairs():";
        boolean bError = false;
        
        //System.out.println(header);
        
        SectionPairRecord pairRow = new SectionPairRecord();
        SectionPairTable pairTable = new SectionPairTable(_db);
        
        
        String whereClause = "WHERE nidid = '" + damID.trim() + "'";
        
        if ((downName != null))
        {
            String downClause = " AND down_name = '" + downName.trim() + "'"; 
            whereClause = whereClause + downClause;
        }
        
        if ((xsecType != null)) 
        {
            String typeClause = " AND xsec_type = '" + xsecType.trim() + "'"; 
            whereClause = whereClause + typeClause;
        }
        
        if ((pairNum > -1)) 
        {
            String pairString = String.valueOf(pairNum);
            String pairClause = " AND pair_num = '" + pairString + "'"; 
            whereClause = whereClause + pairClause;
        }
        
        try {
            
            pairTable.delete(whereClause);
            
        } catch (Exception e) {
            JOptionPane.showMessageDialog(null,"Error: Could not delete selected damcat_pair rows.");
            e.printStackTrace();
            bError = true;
        }
        
        return !bError;
    }
    /**
     * Build where clause to delete from damcat_down table based on parameters passed
     * To delete ALL downNames, set downName to null
     *
     * NOTE: The delete rules will also be cascaded to damcat_pair and damcat_out tables 
     *
     * Creation date: (1/27/2004 3:01:01 PM)
     * @return boolean
     * @param damID java.lang.String
     * @param downName java.lang.String
     */
    public boolean deleteSelectedPoints(String damID, String downName) {
        
        String header = "DBAccess.deleteSelectedPoints():";
        boolean bError = false;
        
        // System.out.println(header);
        
        DownStreamRecord downRow = new DownStreamRecord();
        DownStreamTable downTable = new DownStreamTable(_db);
        
        
        String whereClause = "WHERE nidid = '" + damID.trim() + "'";
        
        if ((downName != null))
        {
            String downClause = " AND down_name = '" + downName.trim() + "'"; 
            whereClause = whereClause + downClause;
        }
        
        try {
            
            // *** first delete damcat_out and damcat_pair entries
            
            deleteSelectedOutputs(damID, null, null, downName);
            
            deleteSelectedPairs(damID, downName, null, -1);
            
            downTable.delete(whereClause);
            
        } catch (Exception e) {
            JOptionPane.showMessageDialog(null,"Error: Could not delete selected Downstream rows."); 
            e.printStackTrace();
            bError = true;
        }
        return !bError;
    }
    /**
     * Returns true if the dam exists in the damcat_dam table.
     */
    public boolean doesDamExist(String strNIDID) {
        
        // *** JDBGen Code Here
        
        DamMasterRecord masterRow = null;
        DamMasterTable masterTable = new DamMasterTable(_db);
        
        try {
            
            String whereClause = "WHERE nidid='" + strNIDID + "'";
            List rowList = masterTable.select(whereClause);
            
            if (rowList.size() > 0)
            {
                masterRow = (DamMasterRecord) rowList.get(0);
                return true;
            } else
            {
                return false;
            }		
            
        } catch (Exception ex) {
            System.out.println("In DBAccess:doesDamExist");
            ex.printStackTrace();
            return false;
        } 
    }
    /**
     * Returns true if the scenario exists in the damcat_in table.
     */
    private boolean doesScenarioExist(String strNIDID, String src, String scenario) {
        
        // *** JDBGen Code Here
        
        SdbInRecord inRow = null;
        SdbInTable inTable = new SdbInTable(_db);
        
        try {
            
            String whereClause = "WHERE nidid='" + strNIDID + "' AND src='" + src + "' AND scenario='" + scenario + "'";
            List rowList = inTable.select(whereClause);
            
            if (rowList.size() > 0)
            {
                inRow = (SdbInRecord) rowList.get(0);
                return true;
                
            } else
            {
                return false;
            }		
            
        } catch (Exception ex) {
            System.out.println("In DBAccess:doesScenarioExist");
            ex.printStackTrace();
            return false;
        } 
        
        // *** Pre-JDBGen Code Below
        
        /*
         
         Statement stmt = null;
         ResultSet rs = null;
         boolean bError = false;
         boolean bExists = false;
         
         try {
         
         stmt = conn.createStatement();
         String query;
         
         query = "SELECT hde " +
         "FROM damcat_in WHERE nidid='" + strNIDID + "' AND src='" + src + "' AND scenario='" + scenario + "'";
         
         rs = stmt.executeQuery(query);
         
         bExists = rs.next();
         
         } catch (SQLException ex) {
         bError = true;
         System.out.println("ERROR: Could not query database.");
         System.out.println("SQLException: " + ex.getMessage());
         System.out.println("SQLState: " + ex.getSQLState());
         System.out.println("VendorError: " + ex.getErrorCode());
         } catch (Exception ex) {
         ex.printStackTrace();
         } finally {
         if (rs != null) {
         try {
         rs.close();
         } catch (SQLException e) {
         }
         rs = null;
         }
         if (stmt != null) {
         try {
         stmt.close();
         } catch (SQLException e) {
         }
         stmt = null;
         }
         }
         return bExists;
         */
    }
    /**
     * This method will return a full DamInfo structure.
     * Creation date: (7/15/2003 3:03:08 PM)
     * @param dam DamInfo
     */
    public DamInfo fillCompletely(String NIDID) {
        
        DamInfo ret = new DamInfo();
        
        // store NIDID
        ret.nidid = NIDID;
        
        if (!fillDamInfo(ret) || !fillInput(ret) || !fillDownstream(ret)  || !fillCrossSection(ret) || !fillOutput(ret))
            ret._nFilled = 0;
        else
            ret._nFilled = 2;
        
        return ret;
    }
    /**
     * Retrieve rows from damcat_pair table and transfer data to CrossSectionEntryInfo structure 
     * Creation date: (7/15/2003 4:14:10 PM)
     * @param ret DamInfo
     */
    private boolean fillCrossSection(DamInfo ret) {
        
        // *** JDBGen Code Here
        
        
        String whereClause = "WHERE nidid='" + ret.nidid + "'" +
        " ORDER BY down_name, xsec_type, pair_num";
        
        SectionPairRecord pairRow = null;
        SectionPairTable pairTable = new SectionPairTable(_db);
        
        try {
            
            List rowList = pairTable.select(whereClause);
            
            if (rowList.size() > 0)
            {
                ret._crossSectionEntries = new ArrayList();
                
                for (int r = 0; r < rowList.size(); r++) {
                    
                    pairRow = (SectionPairRecord) rowList.get(r);
                    CrossSectionEntryInfo xsEntry = new CrossSectionEntryInfo();
                    
                    
                    xsEntry.down_name		= pairRow.getDown_name();
                    xsEntry.xsec_type		= pairRow.getXsec_type();
                    
                    long updatedLong		= pairRow.getUpdated();
                    xsEntry.updated			= DbTimeHelper.getDateTimeStringFromLongTime(updatedLong);
                    
                    xsEntry.elev			= new Float(pairRow.getElev());
                    xsEntry.tw				= new Float(pairRow.getTw());
                    xsEntry.mann_n			= new Float(pairRow.getMann_n());
                    xsEntry.inactive_width  = new Float(pairRow.getInactive_width());
                    
                    xsEntry.pair_num		= new Integer(pairRow.getPair_num());
                    
                    // System.out.println("DBAccess.fillCrossSection: " + xsEntry.down_name + "-" + xsEntry.xsec_type + "-" + xsEntry.pair_num); 
                    // System.out.println(" VALUES = " + pairRow.getElev() + "," + pairRow.getTw() + "," + pairRow.getMann_n() + "," + pairRow.getInactive_width());
                    ret._crossSectionEntries.add(xsEntry);
                    
                }
                
                return true;
                
            } else
            {
                return true;
            }		
            
        } catch (Exception ex) {
            System.out.println("In DBAccess:fillOutput");
            ex.printStackTrace();
            return false;
        } 
    }
    /**
     * Retrieve rows from damcat_dam table and transfer data to DamInfo structure 
     * Creation date: (7/15/2003 4:15:24 PM)
     * @param ret DamInfo
     */
    private boolean fillDamInfo(DamInfo ret) {
        
        // *** JDBGen Code Here
        
        String whereClause = "WHERE nidid='" + ret.nidid + "'";
        
        DamMasterRecord masterRow = null;
        DamMasterTable masterTable = new DamMasterTable(_db);
        
        DamFeaturesRecord featuresRow = null;
        DamFeaturesTable featuresTable = new DamFeaturesTable(_db);
        
        try {
            
            List rowMasterList = masterTable.select(whereClause);
            
            if (rowMasterList.size() > 0)
            {
                masterRow = (DamMasterRecord) rowMasterList.get(0);
                ret.nidid = masterRow.getNidid().trim();
                ret.dam_name = masterRow.getDam_name();
                ret.county = masterRow.getCounty();
                ret.river = masterRow.getRiver();
                ret.downstream_hazard = masterRow.getDownstream_hazard();
                ret.hsa	= masterRow.getHsa();
                ret.rfc	= masterRow.getRfc();
                ret.max_storage	= new Float(masterRow.getMax_storage());
                ret.longitude_dam = new Float(masterRow.getLongitude_dam());     		
                ret.latitude_dam = new Float(masterRow.getLatitude_dam());
            }
            List rowFeaturesList = featuresTable.select(whereClause);
            
            if (rowFeaturesList.size() > 0)
            {
                featuresRow = (DamFeaturesRecord) rowFeaturesList.get(0);
                ret.nidid = featuresRow.getNidid().trim();
                ret.other_dam_name = featuresRow.getOther_dam_name();
                ret.dam_former_name = featuresRow.getDam_former_name();
                ret.stateid = featuresRow.getStateid();
                ret.section_t_r	= featuresRow.getSection_t_r();		
                ret.owner_name = featuresRow.getOwner_name();
                ret.owner_type = featuresRow.getOwner_type();
                ret.dam_designer = featuresRow.getDam_designer();
                ret.private_on_federal = featuresRow.getPrivate_on_federal();
                ret.dam_type = featuresRow.getDam_type();
                ret.core = featuresRow.getCore();
                ret.foundation = featuresRow.getFoundation();
                ret.purposes = featuresRow.getPurposes();
                ret.year_completed = featuresRow.getYear_completed();
                ret.year_modified = featuresRow.getYear_modified();
                ret.emerg_action_plan = featuresRow.getEmerg_action_plan();
                ret.inspection_date = featuresRow.getInspection_date();
                ret.inspection_freq = featuresRow.getInspection_freq();
                ret.st_reg_dam = featuresRow.getSt_reg_dam();
                ret.st_reg_agency = featuresRow.getSt_reg_agency();
                ret.spillway_type = featuresRow.getSpillway_type();
                ret.outlet_gates = featuresRow.getOutlet_gates();
                ret.fed_funding	= featuresRow.getFed_funding();
                ret.fed_design = featuresRow.getFed_design();
                ret.fed_construction = featuresRow.getFed_construction();
                ret.fed_regulatory = featuresRow.getFed_regulatory();
                ret.fed_inspection = featuresRow.getFed_inspection();
                ret.fed_operation = featuresRow.getFed_operation();
                ret.fed_owner = featuresRow.getFed_owner();
                ret.fed_other = featuresRow.getFed_other();
                ret.source_agency = featuresRow.getSource_agency();
                ret.topo_map = featuresRow.getTopo_map();
                ret.comments = featuresRow.getComments();
                ret.prebreak_avail = featuresRow.getPrebreak_avail();
                
                long updatedLong = featuresRow.getUpdated();
                ret.updated = DbTimeHelper.getDateTimeStringFromLongTime(updatedLong);
                
                ret.spillway_width = new Float(featuresRow.getSpillway_width());
                ret.number_locks = new Float(featuresRow.getNumber_locks());     		
                ret.length_locks = new Float(featuresRow.getLength_locks());
                ret.width_locks	= new Float(featuresRow.getWidth_locks());
                ret.drainage_area = new Float(featuresRow.getDrainage_area());
                ret.dam_length = new Float(featuresRow.getDam_length());
                ret.dam_height = new Float(featuresRow.getDam_height());
                ret.structural_height = new Float(featuresRow.getStructural_height());     		
                ret.hydraulic_height = new Float(featuresRow.getHydraulic_height());
                ret.nid_height = new Float(featuresRow.getNid_height());
                ret.max_discharge = new Float(featuresRow.getMax_discharge());
                ret.volume_dam	= new Float(featuresRow.getVolume_dam());     		
                ret.normal_storage = new Float(featuresRow.getNormal_storage());
                ret.nid_storage	= new Float(featuresRow.getNid_storage());
                ret.surface_area = new Float(featuresRow.getSurface_area());
                ret.elev = new Float(featuresRow.getElev()); 
                
                ret.return_flow_region = new Integer(featuresRow.getReturn_flow_region());
                
                return true;
            } else
            {
                return false;
            }		
            
        } catch (Exception ex) {
            System.out.println("In DBAccess:fillDamInfo");
            ex.printStackTrace();
            return false;
        } 
    }
    /**
     * Read damcat_down table and transfer data to DownstreamEntryInfo structures 
     * Creation date: (7/15/2003 4:13:54 PM)
     * @param ret DamInfo
     */
    private boolean fillDownstream(DamInfo ret) {
        
        // *** JDBGen Code Here
        
        String whereClause = "WHERE nidid='" + ret.nidid + "'" +
        " ORDER BY distance_from_dam";
        
        
        DownStreamRecord downRow = null;
        DownStreamTable downTable = new DownStreamTable(_db);
        
        try {
            
            List rowList = downTable.select(whereClause);
            
            if (rowList.size() > 0)
            {
                ret._downstreamEntries = new ArrayList();
                
                for (int r = 0; r < rowList.size(); r++) {
                    
                    downRow = (DownStreamRecord) rowList.get(r);
                    DownstreamEntryInfo downEntry = new DownstreamEntryInfo();
                    
                    downEntry.down_name			= downRow.getDown_name();
                    downEntry.comments			= downRow.getComments();
                    downEntry.xsec_best_type	= downRow.getXsec_best_type();				
                    
                    long updatedLong			= downRow.getUpdated();
                    downEntry.updated 			= DbTimeHelper.getDateTimeStringFromLongTime(updatedLong);
                    
                    downEntry.longitude			= new Float(downRow.getLongitude());
                    downEntry.latitude			= new Float(downRow.getLatitude());
                    downEntry.elevation			= new Float(downRow.getElevation());
                    downEntry.distance_from_dam	= new Float(downRow.getDistance_from_dam());
                    downEntry.flood_flow		= new Float(downRow.getFlood_flow());
                    downEntry.flood_depth		= new Float(downRow.getFlood_depth());
                    downEntry.flood_width 		= new Float(downRow.getFlood_width());
                    downEntry.mann_oc 			= new Float(downRow.getMann_oc());	
                    
                    ret._downstreamEntries.add(downEntry);
                    
                }
                
                return true;
                
            } else
            {
                return true;
            }		
            
        } catch (Exception ex) {
            System.out.println("In DBAccess:fillDownstream");
            ex.printStackTrace();
            return false;
        }
    }
    /**
     * Read damcat_in table and transfer data to InputEntryInfo structures 
     * Creation date: (7/15/2003 4:14:27 PM)
     * @param ret DamInfo
     */
    private boolean fillInput(DamInfo ret) {
        
        
        // *** JDBGen Code Here
        
        String whereClause = "WHERE nidid='" + ret.nidid + "'" +
        " ORDER BY src, scenario";
        
        SdbInRecord inRow = null;
        SdbInTable inTable = new SdbInTable(_db);
        
        try {
            
            List rowList = inTable.select(whereClause);
            
            if (rowList.size() > 0)
            {
                ret._inputEntries = new ArrayList();
                
                for (int r = 0; r < rowList.size(); r++) {
                    
                    inRow = (SdbInRecord) rowList.get(r);
                    InputEntryInfo inEntry = new InputEntryInfo();
                    
                    inEntry.src			= inRow.getSrc();
                    inEntry.scenario	= inRow.getScenario();
                    inEntry.comments	= inRow.getComments();
                    
                    long updatedLong	= inRow.getUpdated();
                    inEntry.updated = DbTimeHelper.getDateTimeStringFromLongTime(updatedLong);
                    
                    inEntry.hde			= new Float(inRow.getHde());
                    inEntry.bme			= new Float(inRow.getBme());
                    inEntry.vol			= new Float(inRow.getVol());
                    inEntry.sa			= new Float(inRow.getSa());
                    inEntry.tfm			= new Float(inRow.getTfm());
                    inEntry.qo			= new Float(inRow.getQo());
                    inEntry.bw 			= new Float(inRow.getBw());		
                    
                    inEntry.idam		= new Integer(inRow.getIdam());
                    
                    ret._inputEntries.add(inEntry);
                    
                }
                
                return true;
                
            } else
            {
                return true;
            }		
            
        } catch (Exception ex) {
            System.out.println("In DBAccess:fillInput");
            ex.printStackTrace();
            return false;
        } 
    }
    /**
     * Read damcat_out table and transfer data to OutputEntryInfo structures 
     * Creation date: (7/15/2003 4:14:20 PM)
     * @param ret DamInfo
     */
    private boolean fillOutput(DamInfo ret) {
        
        // *** JDBGen Code Here
        
        
        String whereClause = "WHERE nidid='" + ret.nidid + "'" +
        " ORDER BY src, scenario, down_name ";		// ???? check this !!!!
        
        SdbOutRecord outRow = null;
        SdbOutTable outTable = new SdbOutTable(_db);
        
        try {
            
            List rowList = outTable.select(whereClause);
            
            if (rowList.size() > 0)
            {
                ret._outputEntries = new ArrayList();
                
                for (int r = 0; r < rowList.size(); r++) {
                    
                    outRow = (SdbOutRecord) rowList.get(r);
                    OutputEntryInfo outEntry = new OutputEntryInfo();
                    
                    outEntry.src			= outRow.getSrc();
                    outEntry.scenario		= outRow.getScenario();
                    outEntry.down_name		= outRow.getDown_name();				
                    outEntry.comments		= outRow.getComments();
                    
                    long updatedLong		= outRow.getUpdated();
                    outEntry.updated = DbTimeHelper.getDateTimeStringFromLongTime(updatedLong);
                    
                    outEntry.slope			= new Float(outRow.getSlope());
                    outEntry.max_flow		= new Float(outRow.getMax_flow());
                    outEntry.max_depth		= new Float(outRow.getMax_depth());
                    outEntry.time_max_depth = new Float(outRow.getTime_max_depth());
                    outEntry.time_flood		= new Float(outRow.getTime_flood());
                    outEntry.time_deflood	= new Float(outRow.getTime_deflood());
                    
                    
                    ret._outputEntries.add(outEntry);
                    
                }
                
                return true;
                
            } else
            {
                return true;
            }		
            
        } catch (Exception ex) {
            System.out.println("In DBAccess:fillOutput");
            ex.printStackTrace();
            return false;
        }
    }
    /**
     * Find highest "manually" added nidid within given state
     *   and return next available unique nidid
     * 
     * Creation date: (1/22/2004 6:03:20 PM)
     * @return java.lang.String
     * @param stateID java.lang.String
     */
    public String findNextID(String stateID) {
        
        String statePlus99 = stateID + "99";					// user defined nidid's are assigned this range
        
        String whereClause = "where nidid like '" 
            + statePlus99
            + "%'" 
            + " order by nidid desc";	
        
        String highKey = null;
        String highKeyNumeric = null;
        
        int nextKeyNumber = 0;
        int indexNumeric = 0;
        
        String nextKey = null;
        
        DamMasterRecord masterRow = null;
        DamMasterTable masterTable = new DamMasterTable(_db);
        
        try {
            
            List rowList = masterTable.selectNRecords(whereClause, 10);		// just select highest row
            
            if (rowList.size() > 0)
            {
                masterRow = (DamMasterRecord) rowList.get(0);
                
                highKey 				= masterRow.getNidid();			// get ID
                highKeyNumeric = highKey.substring(2).trim();					// isolate numeric portion
                nextKeyNumber = Integer.parseInt(highKeyNumeric) + 1;	// convert to int and add 1
                nextKey = stateID + Integer.toString(nextKeyNumber);	// convert back to String
            } 
            else
            {
                nextKey = statePlus99 + "001";							// first user-defined nidid in this state
            }		
            
        } catch (Exception ex) {
            System.out.println("In DBAccess:findNextID");
            ex.printStackTrace();
            return null;
        } 
        
        return nextKey;
    }
    /**
     * Retrieve ConnectionString and JDBC Driver strings
     * Creation date: (7/14/2003 12:51:55 PM)
     */
    private void getConfigInfo() {
        
        DBConnectionString = PropertyReader.getProperty("CONNECTION_STRING");
        if (DBConnectionString != null)
            DBConnectionString = DBConnectionString.trim();
        
        DBDriver = PropertyReader.getProperty("DBDRIVER");
        if (DBDriver != null)
            DBDriver = DBDriver.trim();
    }
    /**
     * Insert the method's description here.
     * Creation date: (11/7/2003 5:55:29 PM)
     * @return java.lang.String
     */
    public static String getCurrentTime() {
        Timestamp timeSt;
        String curTime = "";
        String curTimeSubstr = "";
        
        timeSt = new Timestamp (new Date().getTime());
        curTime = timeSt.toString();
        int index = curTime.indexOf('.');
        curTimeSubstr = curTime.substring(0, index);
        // System.out.println("Timestamp is: " + curTimeSubstr);
        return curTimeSubstr;
    }
    /**
     * Insert the method's description here.
     * Creation date: (3/23/2004 2:40:38 PM)
     * @return boolean
     */
    public static boolean hasListAllDams() {
        
        // *** The value of "damcrest.hasListAllDams" could be stored
        // in the resources.txt or set as apps defaults token
        String strHasListAllDams = PropertyReader.getProperty("damcrest.hasListAllDams");
        
        if (strHasListAllDams == null)
            return false;
        
        return strHasListAllDams.trim().equalsIgnoreCase("true");
    }
    /**
     * Insert DamCatDam
     * Creation date: (1/22/2004 4:58:37 PM)
     * @return boolean
     * @param damInfo gov.damcat.data.DamInfo
     */
    public boolean insertDamFeatures(DamInfo damInfo) {
        
        String header = "DBAccess.insertDamFeatures() ";
        //System.out.println(header);
        
        boolean bError = false;
        
        DamFeaturesRecord featuresRow = new DamFeaturesRecord() ;
        DamFeaturesTable featuresTable = new DamFeaturesTable(_db);
        
        try {
            featuresRow.setNidid(damInfo.getNidid());
            featuresRow.setOther_dam_name(damInfo.getOther_dam_name()); 
            featuresRow.setDam_former_name(damInfo.getDam_former_name());
            featuresRow.setStateid(damInfo.getStateid());						featuresRow.setSection_t_r(damInfo.getSection_t_r());		
            featuresRow.setOwner_name(damInfo.getOwner_name());
            featuresRow.setOwner_type(damInfo.getOwner_type());
            featuresRow.setDam_designer(damInfo.getDam_designer());
            featuresRow.setPrivate_on_federal(damInfo.getPrivate_on_federal());
            featuresRow.setDam_type(damInfo.getDam_type());
            featuresRow.setCore(damInfo.getCore());
            featuresRow.setFoundation(damInfo.getFoundation());
            featuresRow.setPurposes(damInfo.getPurposes());
            featuresRow.setYear_completed(damInfo.getYear_completed());
            featuresRow.setYear_modified(damInfo.getYear_modified());
            featuresRow.setEmerg_action_plan(damInfo.getEmerg_action_plan());
            featuresRow.setInspection_date(damInfo.getInspection_date());
            featuresRow.setInspection_freq(damInfo.getInspection_freq());
            featuresRow.setSt_reg_dam(damInfo.getSt_reg_dam());
            featuresRow.setSt_reg_agency(damInfo.getSt_reg_agency());
            featuresRow.setSpillway_type(damInfo.getSpillway_type());
            featuresRow.setOutlet_gates(damInfo.getOutlet_gates());
            featuresRow.setVolume_dam(damInfo.getVolume_dam().doubleValue()); 
            featuresRow.setFed_funding(damInfo.getFed_funding());
            featuresRow.setFed_design(damInfo.getFed_design());
            featuresRow.setFed_construction(damInfo.getFed_construction());
            featuresRow.setFed_regulatory(damInfo.getFed_regulatory());
            featuresRow.setFed_inspection(damInfo.getFed_inspection());
            featuresRow.setFed_operation(damInfo.getFed_operation());
            featuresRow.setFed_owner(damInfo.getFed_owner());
            featuresRow.setFed_other(damInfo.getFed_other());
            featuresRow.setSource_agency(damInfo.getSource_agency());
            featuresRow.setTopo_map(damInfo.getTopo_map());
            featuresRow.setComments(damInfo.getComments());
            featuresRow.setPrebreak_avail(damInfo.getPrebreak_avail());
            
            String updatedString = getCurrentTime();
            long updated = DbTimeHelper.getLongTimeFromDateTimeString(updatedString);
            featuresRow.setUpdated(updated);
            
            featuresRow.setSpillway_width(damInfo.getSpillway_width().doubleValue());
            featuresRow.setNumber_locks(damInfo.getNumber_locks().doubleValue());     		
            featuresRow.setLength_locks(damInfo.getLength_locks().doubleValue());
            featuresRow.setWidth_locks(damInfo.getWidth_locks().doubleValue());
            featuresRow.setDrainage_area(damInfo.getDrainage_area().doubleValue());
            featuresRow.setDam_height(damInfo.getDam_height().doubleValue());
            featuresRow.setDam_length(damInfo.getDam_length().doubleValue());
            featuresRow.setStructural_height(damInfo.getStructural_height().doubleValue());     		
            featuresRow.setHydraulic_height(damInfo.getHydraulic_height().doubleValue());
            featuresRow.setNid_height(damInfo.getNid_height().doubleValue());
            featuresRow.setMax_discharge(damInfo.getMax_discharge().doubleValue());	    		
            featuresRow.setNormal_storage(damInfo.getNormal_storage().doubleValue());
            featuresRow.setNid_storage(damInfo.getNid_storage().doubleValue());
            featuresRow.setSurface_area(damInfo.getSurface_area().doubleValue());
            featuresRow.setElev(damInfo.getElev().doubleValue()); 
            
            featuresRow.setReturn_flow_region(damInfo.getReturn_flow_region().intValue());
            
            featuresTable.insert(featuresRow);
            
        } catch (Exception e) {
            JOptionPane.showMessageDialog(null,"Error: Could not insert into DamFeatures database table\nCheck for duplicate keys.");
            e.printStackTrace();
            bError = true;
            return false;
        }
        return true;
    }
    /**
     * InsertDamMaster 
     * Creation date: (5/4/2004 4:58:37 PM)
     * @return boolean
     * @param damInfo gov.damcat.data.DamInfo
     */
    
    public boolean insertDamMaster(DamInfo damInfo) {
        
        String header = "DBAccess.insertDamMaster() ";
        System.out.println(header);
        
        boolean bError = false;
        
        DamMasterRecord masterRow = new DamMasterRecord() ;
        DamMasterTable masterTable = new DamMasterTable(_db);
        
        try {
            masterRow.setNidid(damInfo.getNidid());
            masterRow.setDam_name(damInfo.getDam_name());
            masterRow.setRiver(damInfo.getRiver());
            masterRow.setCounty(damInfo.getCounty());
            masterRow.setHsa(damInfo.getHsa());
            masterRow.setRfc(damInfo.getRfc());
            masterRow.setLongitude_dam(damInfo.getLongitude_dam().doubleValue());     		
            masterRow.setLatitude_dam(damInfo.getLatitude_dam().doubleValue());
            masterRow.setMax_storage(damInfo.getMax_storage().doubleValue());
            masterRow.setDownstream_hazard(damInfo.getDownstream_hazard());
            
            masterTable.insert(masterRow);
            
        } catch (Exception e) {
            JOptionPane.showMessageDialog(null,"Error: Could not insert into DamMaster database table\nCheck for duplicate keys.");
            e.printStackTrace();
            bError = true;
            return false;
        }
        return true;
    }
    /**
     * Insert the method's description here.
     * Creation date: (1/22/2004 5:01:55 PM)
     * @return boolean
     * @param downEntry gov.damcat.data.DownstreamEntryInfo
     */
    public boolean insertDownstream(String damID, DownstreamEntryInfo downEntry) {
        
        String header = "DBAccess.insertDownstream() ";
        // System.out.println(header);
        
        boolean bError = false;
        
        // JDBGen Code Here
        
        DownStreamRecord downRow = new DownStreamRecord();
        DownStreamTable downTable = new DownStreamTable(_db);
        
        try {
            
            DownstreamEntryInfo down = downEntry;
            
            downRow.setNidid(damID);
            
            downRow.setDown_name(down.getDown_name());
            downRow.setComments(down.getComments());
            downRow.setXsec_best_type(down.getXsec_best_type());				
            
            String updatedString = getCurrentTime();
            long updated = DbTimeHelper.getLongTimeFromDateTimeString(updatedString);
            downRow.setUpdated(updated);
            
            downRow.setLongitude(down.getLongitude().doubleValue());
            downRow.setLatitude	(down.getLatitude().doubleValue());
            downRow.setElevation(down.getElevation().doubleValue());
            downRow.setDistance_from_dam(down.getDistance_from_dam().doubleValue());
            downRow.setFlood_flow(down.getFlood_flow().doubleValue());
            downRow.setFlood_depth(down.getFlood_depth().doubleValue());
            downRow.setFlood_width(down.getFlood_width().doubleValue());
            downRow.setMann_oc (down.getMann_oc().doubleValue());	
            
            downTable.insert(downRow);
            
        } catch (Exception e) {
            JOptionPane.showMessageDialog(null,"Error: Could not insert into Downstream database table.\nCheck for duplicate keys.");
            e.printStackTrace();
            bError = true;
        }
        return !bError;
    }
    /**
     * parse DamInfo structure and insert all rows into DBMS 
     * Creation date: (1/22/2004 5:06:46 PM)
     * @return boolean
     * @param damInfo gov.damcat.data.DamInfo
     */
    public boolean insertEntireDam(DamInfo damInfo) {
        
        String header = "DBAccess.insertEntireDam()";
        //System.out.println(header);
        
        boolean bError = false;
        
        String damID = damInfo.getNidid();
        
        try {
            insertDamMaster(damInfo);
            insertDamFeatures(damInfo);
            
            for (int j=0; j<damInfo._inputEntries.size(); j++) {
                
                InputEntryInfo in = (InputEntryInfo)damInfo._inputEntries.get(j);
                insertSdbIn(damID, in);
                
            }
            
            for (int j=0; j<damInfo._downstreamEntries.size(); j++) {
                
                DownstreamEntryInfo down = (DownstreamEntryInfo)damInfo._downstreamEntries.get(j);
                insertDownstream(damID, down);
                
            }
            
            for (int j=0; j<damInfo._crossSectionEntries.size(); j++) {
                
                CrossSectionEntryInfo xs = (CrossSectionEntryInfo)damInfo._crossSectionEntries.get(j);
                insertSectionPair(damID, xs);
                
            }
            
            for (int j=0; j<damInfo._outputEntries.size(); j++) {
                
                OutputEntryInfo out = (OutputEntryInfo)damInfo._outputEntries.get(j);
                insertSdbOut(damID, out);
                
            }	
            
            
        } catch (Exception e) {
            JOptionPane.showMessageDialog(null,"Error: Could not insert entire dam " + damID);
            e.printStackTrace();
            bError = true;
        }
        
        return !bError; 
        
        
        
    }
    /**
     * Insert the method's description here.
     * Creation date: (1/22/2004 5:00:23 PM)
     * @return boolean
     * @param damID java.lang.String
     * @param inEntry gov.damcat.data.InputEntryInfo
     */
    public boolean insertSdbIn(String damID, InputEntryInfo inEntry) {
        
        
        String header = "DBAccess.insertSdbIn():";
        //System.out.println(header);
        
        boolean bError = false;
        
        // JDBGen Code Here
        
        SdbInRecord inRow = new SdbInRecord();
        SdbInTable inTable = new SdbInTable(_db);
        
        try {		
            
            InputEntryInfo in = inEntry;
            
            inRow.setNidid(damID);
            inRow.setSrc(in.getSrc());
            inRow.setScenario(in.getScenario());
            inRow.setComments(in.getComments());
            
            String updatedString = getCurrentTime();
            long updated = DbTimeHelper.getLongTimeFromDateTimeString(updatedString);
            inRow.setUpdated(updated);
            
            inRow.setHde(in.getHde().doubleValue());
            inRow.setBme(in.getBme().doubleValue());
            inRow.setVol(in.getVol().doubleValue());
            inRow.setSa(in.getSa().doubleValue());
            inRow.setTfm(in.getTfm().doubleValue());
            inRow.setQo(in.getQo().doubleValue());
            inRow.setBw(in.getBw().doubleValue());		
            
            inRow.setIdam(in.getIdam().intValue());
            
            inTable.insert(inRow);
            
        } catch (Exception e) {
            JOptionPane.showMessageDialog(null,"Error: Could not insert into SdbIn database table.\nCheck for duplicate keys.");
            e.printStackTrace();
            bError = true;
        }
        return (!bError);
    }
    /**
     * Insert the method's description here.
     * Creation date: (1/22/2004 5:05:28 PM)
     * @return boolean
     * @param damID java.lang.String
     * @param outEntry gov.damcat.data.OutputEntryInfo
     */
    public boolean insertSdbOut(String damID, OutputEntryInfo outEntry) {
        
        String header = "DBAccess.insertSdbOut():";
        //System.out.println(header);
        boolean bError = false;
        
        // *** JDBGen Code Here
        
        SdbOutRecord outRow = new SdbOutRecord();
        SdbOutTable outTable = new SdbOutTable(_db);
        
        try {
            
            OutputEntryInfo out = outEntry;
            
            outRow.setNidid(damID);
            
            outRow.setSrc(out.getSrc());
            outRow.setScenario(out.getScenario());
            outRow.setDown_name(out.getDown_name());
            // System.out.println(out.getDown_name());
            
            outRow.setComments(out.getComments());
            
            String updatedString = getCurrentTime();
            long updated = DbTimeHelper.getLongTimeFromDateTimeString(updatedString);
            outRow.setUpdated(updated);
            
            outRow.setSlope(out.getSlope().doubleValue());
            outRow.setMax_flow(out.getMax_flow().doubleValue());
            outRow.setMax_depth(out.getMax_depth().doubleValue());
            outRow.setTime_max_depth(out.getTime_max_depth().doubleValue());
            outRow.setTime_flood(out.getTime_flood().doubleValue());
            outRow.setTime_deflood(out.getTime_deflood().doubleValue());
            
            outTable.insert(outRow);		
            
        } catch (Exception e) {
            JOptionPane.showMessageDialog(null,"Error: Could not insert into SdbOut database table.\nCheck for duplicate keys.");
            e.printStackTrace();
            bError = true;
        }
        return !bError;
    }
    /**
     * Insert the method's description here.
     * Creation date: (1/22/2004 5:04:14 PM)
     * @return boolean
     * @param damID java.lang.String
     * @param pairEntry gov.damcat.data.CrossSectionEntryInfo
     */
    public boolean insertSectionPair(String damID, CrossSectionEntryInfo pairEntry) {
        
        String header = "DBAccess.insertSectionPair():";
        boolean bError = false;
        
        //System.out.println(header);
        
        SectionPairRecord pairRow = new SectionPairRecord();
        SectionPairTable pairTable = new SectionPairTable(_db);
        
        try {
            
            
            CrossSectionEntryInfo xs = pairEntry;
            
            pairRow.setNidid(damID);
            
            pairRow.setDown_name(xs.getDown_name());
            pairRow.setXsec_type(xs.getXsec_type());
            
            String updatedString = getCurrentTime();
            long updated = DbTimeHelper.getLongTimeFromDateTimeString(updatedString);
            pairRow.setUpdated(updated);
            
            pairRow.setElev(xs.getElev().doubleValue());
            pairRow.setTw(xs.getTw().doubleValue());
            pairRow.setMann_n(xs.getMann_n().doubleValue());
            pairRow.setInactive_width(xs.getInactive_width().doubleValue());
            
            pairRow.setPair_num(xs.getPair_num().intValue());
            
            pairTable.insert(pairRow);
            
        } catch (Exception e) {
            JOptionPane.showMessageDialog(null,"Error: Could not insert into SectionPair database table.\nCheck for duplicate keys.");
            e.printStackTrace();
            bError = true;
        }
        return !bError;
    }
    /**
     * Returns true if the connection to the database was successful.
     * Creation date: (7/14/2003 12:59:17 PM)
     * @return boolean
     */
    public boolean isConnected() {
        return bConnected;
    }
    /**
     * Test System Property - probably should test other 
     * Creation date: (7/18/2003 2:33:55 PM)
     * @return boolean
     */
    public static boolean isDBEnabled() {
        
        String strEnabled = PropertyReader.getProperty( "damcrest.db_enabled" );
        // *** This currently assumes that the value of "damcrest.db_enabled" 
        // *** in the resources.txt file or set as token 
        // *** accurately reflects the state of the DBMS - that seems like a bad idea !!!!
        // *** It should really test the Connection before returning a "true" 
        
        if (strEnabled == null)
        {
            // System.out.println("strEnabled is null");
            return false;
        }
        // System.out.println("strEnabled is : " + strEnabled);		
        return strEnabled.trim().equalsIgnoreCase("true");
    }
    /**
     * Searches the database for all dams that contain the given text in their
     * corresponding fields.
     * Creation date: (7/14/2003 12:47:45 PM)
     * @return java.util.ArrayLists
     */
    public ArrayList search(
            String field[],
            String oper[],
            String data1[],
            String data2[]) {
        
        // *** JDBGen Code Here
        
        ArrayList searchResults = new ArrayList();
        
        String whereClause = "";
        
        DamMasterRecord masterRow = null;
        DamMasterTable masterTable = new DamMasterTable(_db);
        
        try {
            
            // if field is null, then return all dams
            if (field == null) {
                whereClause = " order by dam_name"; 
                
            } else {
                whereClause = " WHERE ";
                for (int i=0; i<3; i++) {
                    if (field[i] == null)
                        continue;
                    if (field[i].equals("Dam Name"))
                        whereClause += "dam_name ";
                    else if (field[i].equals("River Name"))
                        whereClause += "river ";
                    else if (field[i].equals("County Name"))
                        whereClause += "county ";
                    else if (field[i].equals("NIDID"))
                        whereClause += "nidid ";
                    else if (field[i].equals("HSA"))
                        whereClause += "hsa ";
                    else if (field[i].equals("RFC"))
                        whereClause += "rfc ";
                    else if (field[i].equals("DH"))
                        whereClause += "downstream_hazard ";
                    else if (field[i].equals("Max Storage"))
                        whereClause += "max_storage ";
                    else if (field[i].equals("Latitude"))
                        whereClause += "latitude_dam ";
                    else if (field[i].equals("Longitude"))
                        whereClause += "longitude_dam ";
                    
                    if (oper[i].equals("equals"))
                        whereClause += "= '" + data1[i] + "' AND ";
                    else if (oper[i].equals("contains"))
                        whereClause += "like '%" + data1[i] + "%' AND ";
                    else if (oper[i].equals("is greater than"))
                        whereClause += "> " + data1[i] + " AND ";
                    else if (oper[i].equals("is less than"))
                        whereClause += "< " + data1[i] + " AND ";
                    else if (oper[i].equals("is between")) {
                        float lowFloat = Float.parseFloat(data1[i]), highFloat = Float.parseFloat(data2[i]),temp;
                        if (highFloat < lowFloat) {
                            temp = lowFloat;
                            lowFloat = highFloat;
                            highFloat = temp;
                        }
                        whereClause += "between " + lowFloat + " and " + highFloat + " AND ";
                    }
                }
                if (whereClause.length() >= 2 && !whereClause.substring(whereClause.length()-4,whereClause.length()).equals("AND"))
                    whereClause = whereClause.substring(0,whereClause.length()-4);
                
                whereClause += " order by dam_name";
            }
            
            // System.out.println("whereClause: " + whereClause);
            
            List rowList = masterTable.select(whereClause);
            
            // List rowList = damTable.select(whereClause);
            
            
            if (rowList.size() > 0)
                
                for (int r = 0; r < rowList.size(); r++)
                {
                    masterRow = (DamMasterRecord) rowList.get(r);
                    
                    searchResults.add(new DamInfo(masterRow.getDam_name(),masterRow.getRiver(),masterRow.getCounty(),masterRow.getNidid(),masterRow.getHsa(),masterRow.getRfc(),masterRow.getDownstream_hazard(),new Float(masterRow.getMax_storage()),new Float(masterRow.getLatitude_dam()),new Float(masterRow.getLongitude_dam())));
                }
            
        } catch (Exception ex) {
            System.out.println("In DBAccess:search()");
            ex.printStackTrace();
        }
        
        return searchResults;
    }
    /**
     * Insert the method's description here.
     * Creation date: (3/31/2004 4:52:51 PM)
     */
    public DamInfo searchCommandLine(String nidid)
    {
        DamInfo damInfo = new DamInfo();
        
        String whereClause = "";
        
        DamMasterRecord masterRow = null;
        DamMasterTable masterTable = new DamMasterTable(_db);
        
        try {
            whereClause = "WHERE nidid = '" + nidid + "'";
            
            System.out.println("DBAccess.searchCommandLine() whereClause: " + whereClause);
            List rowList = masterTable.select(whereClause);
            
            if (rowList.size() > 0)
                
                for (int r = 0; r < rowList.size(); r++)
                {
                    masterRow = (DamMasterRecord) rowList.get(r);
                    damInfo.setNidid(masterRow.getNidid());
                    damInfo.setRiver(masterRow.getRiver());
                    damInfo.setCounty(masterRow.getCounty());
                }
            
        } catch (Exception ex) {
            System.out.println("In DBAccess:searchCommandLine()");
            ex.printStackTrace();
        }
        
        return damInfo;
        
    }
    private void trace()
    {
        try
        {
            throw new Exception("trace");
        }
        catch(Exception e)
        {
            e.printStackTrace();
        }
    }
    /**
       * @param damInfo
       * @return
       */
    /**
     * This method writes the relevant data contained in the
     * given damInfo structure to the DamFeatures database table.
     * Creation date: (7/21/2003 8:00:09 AM)
     * @param damInfo DamInfo
     */
    public boolean updateDamFeatures(DamInfo damInfo) {
        String header = "DBAccess.updateDamFeatures() ";
        //System.out.println(header);
        
        boolean bError = false;
        
        // JDBGen Code Here
        
        DamFeaturesRecord featuresRow = new DamFeaturesRecord() ;
        DamFeaturesTable featuresTable = new DamFeaturesTable(_db);
        
        try {
            featuresRow.setNidid(damInfo.getNidid().trim());
            featuresRow.setOther_dam_name(damInfo.getOther_dam_name()); 				
            featuresRow.setDam_former_name(damInfo.getDam_former_name());
            featuresRow.setStateid(damInfo.getStateid());
            featuresRow.setSection_t_r(damInfo.getSection_t_r());		
            featuresRow.setOwner_name(damInfo.getOwner_name());
            featuresRow.setOwner_type(damInfo.getOwner_type());
            featuresRow.setDam_designer(damInfo.getDam_designer());
            featuresRow.setPrivate_on_federal(damInfo.getPrivate_on_federal());
            featuresRow.setDam_type(damInfo.getDam_type());
            featuresRow.setCore(damInfo.getCore());
            featuresRow.setFoundation(damInfo.getFoundation());
            featuresRow.setPurposes(damInfo.getPurposes());
            featuresRow.setYear_completed(damInfo.getYear_completed());
            featuresRow.setYear_modified(damInfo.getYear_modified());
            featuresRow.setEmerg_action_plan(damInfo.getEmerg_action_plan());
            featuresRow.setInspection_date(damInfo.getInspection_date());
            featuresRow.setInspection_freq(damInfo.getInspection_freq());
            featuresRow.setSt_reg_dam(damInfo.getSt_reg_dam());
            featuresRow.setSt_reg_agency(damInfo.getSt_reg_agency());
            featuresRow.setSpillway_type(damInfo.getSpillway_type());
            featuresRow.setOutlet_gates(damInfo.getOutlet_gates());
            featuresRow.setVolume_dam(damInfo.getVolume_dam().doubleValue());    
            featuresRow.setFed_funding(damInfo.getFed_funding());
            featuresRow.setFed_design(damInfo.getFed_design());
            featuresRow.setFed_construction(damInfo.getFed_construction());
            featuresRow.setFed_regulatory(damInfo.getFed_regulatory());
            featuresRow.setFed_inspection(damInfo.getFed_inspection());
            featuresRow.setFed_operation(damInfo.getFed_operation());
            featuresRow.setFed_owner(damInfo.getFed_owner());
            featuresRow.setFed_other(damInfo.getFed_other());
            featuresRow.setSource_agency(damInfo.getSource_agency());
            featuresRow.setTopo_map(damInfo.getTopo_map());
            featuresRow.setComments(damInfo.getComments());
            featuresRow.setPrebreak_avail(damInfo.getPrebreak_avail());
            
            String updatedString = getCurrentTime();
            long updated = DbTimeHelper.getLongTimeFromDateTimeString(updatedString);
            featuresRow.setUpdated(updated);
            
            featuresRow.setSpillway_width(damInfo.getSpillway_width().doubleValue());
            featuresRow.setNumber_locks(damInfo.getNumber_locks().doubleValue());     		
            featuresRow.setLength_locks(damInfo.getLength_locks().doubleValue());
            featuresRow.setWidth_locks(damInfo.getWidth_locks().doubleValue());
            featuresRow.setDrainage_area(damInfo.getDrainage_area().doubleValue());
            featuresRow.setDam_length(damInfo.getDam_length().doubleValue());
            featuresRow.setDam_height(damInfo.getDam_height().doubleValue());
            featuresRow.setStructural_height(damInfo.getStructural_height().doubleValue());     		
            featuresRow.setHydraulic_height(damInfo.getHydraulic_height().doubleValue());
            featuresRow.setNid_height(damInfo.getNid_height().doubleValue());
            featuresRow.setMax_discharge(damInfo.getMax_discharge().doubleValue());
            featuresRow.setNormal_storage(damInfo.getNormal_storage().doubleValue());
            featuresRow.setNid_storage(damInfo.getNid_storage().doubleValue());
            featuresRow.setSurface_area(damInfo.getSurface_area().doubleValue());
            featuresRow.setElev(damInfo.getElev().doubleValue()); 
            
            featuresRow.setReturn_flow_region(damInfo.getReturn_flow_region().intValue());
            
            featuresTable.insertOrUpdate(featuresRow);
            
        } catch (Exception e) {
            JOptionPane.showMessageDialog(null,"Error: Could not update DamFeatures database table.");
            e.printStackTrace();
            bError = true;
        }
        
        return !bError;
    }
    /**
     * This method writes the relevant data contained in the
     * given damInfo structure to the DamMaster database table.
     * Creation date: (5/4/2003 4:00:09 PM)
     * @param damInfo DamInfo
     */
    public boolean updateDamMaster(DamInfo damInfo) {
        String header = "DBAccess.updateDamMaster() ";
        //System.out.println(header);
        
        boolean bError = false;
        
        // JDBGen Code Here
        
        DamMasterRecord masterRow = new DamMasterRecord() ;
        DamMasterTable masterTable = new DamMasterTable(_db);
        
        try {
            masterRow.setNidid(damInfo.getNidid().trim());
            masterRow.setDam_name(damInfo.getDam_name());
            masterRow.setCounty(damInfo.getCounty());
            masterRow.setRiver(damInfo.getRiver());
            masterRow.setDownstream_hazard(damInfo.getDownstream_hazard());
            masterRow.setHsa(damInfo.getHsa());
            masterRow.setRfc(damInfo.getRfc());
            masterRow.setMax_storage(damInfo.getMax_storage().doubleValue());
            masterRow.setLongitude_dam(damInfo.getLongitude_dam().doubleValue());     		
            masterRow.setLatitude_dam(damInfo.getLatitude_dam().doubleValue());
            
            masterTable.insertOrUpdate(masterRow);
            
        } catch (Exception e) {
            JOptionPane.showMessageDialog(null,"Error: Could not update DamMaster database table.");
            e.printStackTrace();
            bError = true;
        }
        
        return !bError;
    }
    /**
     * This method writes the relevant data contained in the
     * given damInfo structure to the damcat_down database.
     * Creation date: (7/22/2003 7:51:29 AM)
     * @return boolean
     * @param damInfo damcat.DamInfo
     */
    public boolean updateDownstream(DamInfo damInfo, boolean [] changedArray) {
        
        String header = "DBAccess.updateDownstream() ";
        //System.out.println(header);
        
        boolean bError = false;
        
        // JDBGen Code Here
        
        DownStreamRecord downRow = new DownStreamRecord();
        DownStreamTable downTable = new DownStreamTable(_db);
        
        try {
            for (int j=0; j<damInfo._downstreamEntries.size(); j++) {
                // System.out.println("Changed Array: " + changedArray[j]);
                if (! changedArray[j])
                {
                    continue;
                }
                
                DownstreamEntryInfo down = (DownstreamEntryInfo)damInfo._downstreamEntries.get(j);
                
                downRow.setNidid(damInfo.getNidid());
                
                downRow.setDown_name(down.getDown_name());
                downRow.setComments(down.getComments());
                downRow.setXsec_best_type(down.getXsec_best_type());				
                
                String updatedString = getCurrentTime();
                long updated = DbTimeHelper.getLongTimeFromDateTimeString(updatedString);
                downRow.setUpdated(updated);
                
                downRow.setLongitude(down.getLongitude().doubleValue());
                downRow.setLatitude	(down.getLatitude().doubleValue());
                downRow.setElevation(down.getElevation().doubleValue());
                downRow.setDistance_from_dam(down.getDistance_from_dam().doubleValue());
                downRow.setFlood_flow(down.getFlood_flow().doubleValue());
                downRow.setFlood_depth(down.getFlood_depth().doubleValue());
                downRow.setFlood_width(down.getFlood_width().doubleValue());
                downRow.setMann_oc (down.getMann_oc().doubleValue());	
                
                downTable.insertOrUpdate(downRow);
            }
            
        } catch (Exception e) {
            JOptionPane.showMessageDialog(null,"Error: Could not update Downstream database table.");
            e.printStackTrace();
            bError = true;
        }
        
        return !bError;
    }
    /**
     * This method updates the damcat_in, damcat_out, damcat_down, and
     * damcat_pair tables for the given NIDID with the given model
     * input/output data.
     */
    public void updateModelData(String strNIDID, AnalysisData newData)
    throws Exception
    {
        // sdbin keys:			nidid,src,scenario
        // downstream keys:		nidid,down_name
        // sdbout keys:			nidid,src,scenario,down_name 
        // sectionpair keys:	nidid,down_name,xsec_type,pair_num
        
        String header = "DBAccess.updateModelData(): ";
        // System.out.println(header);
        
        
        DamInfo updatedDam = null;
        
        // Not all the damcat_dam fields will be filled from ModelGUI, so retrieve original DamInfo structure. 	
        if (newData.rootDamInfo.size() > 0)
        {
            updatedDam = (DamInfo) newData.rootDamInfo.get(0);
        }
        else
        {
            updatedDam = new DamInfo();
        }
        
        updatedDam.nidid = strNIDID;
        updatedDam.dam_name = newData.damName;
        updatedDam.river = newData.riverName;
        
        if (!updateDamMaster(updatedDam))
            throw new Exception("Failed to update DamMaster table.");
        
        // *** make new ArrayLists
        
        updatedDam._inputEntries = new ArrayList();
        updatedDam._downstreamEntries = new ArrayList();
        updatedDam._crossSectionEntries = new ArrayList();
        updatedDam._outputEntries = new ArrayList();
        
        // update SdbIn table 
        
        int scenarioCount = newData.scenarios.size();
        
        boolean [] changedScenarios = new boolean [scenarioCount];		
        
        for (int i=0; i<newData.scenarios.size(); i++) {
            
            changedScenarios[i] = false;
            
            ModelScenario scenario = (ModelScenario)newData.scenarios.get(i);
            InputEntryInfo in = new InputEntryInfo();
            
            if (scenario.changeFlag > 0) changedScenarios[i] = true;
            
            in.src = scenario.source;
            in.scenario = scenario.name;
            in.hde = new Float(scenario.HDE);
            in.bme = new Float(scenario.BME);
            in.vol = new Float(scenario.VOL);
            in.sa = new Float(scenario.SA);
            in.qo = new Float(scenario.QO);
            in.bw = new Float(scenario.BW);
            in.tfm = new Float(scenario.TFM);
            in.idam = new Integer(scenario.damType);
            
            updatedDam._inputEntries.add(in);
        }
        
        if (!updateSdbIn(updatedDam, changedScenarios))
        {	System.out.println(header + "updateSdbIn failed");
        throw new Exception("Failed to update SdbIn table.");
        }
        
        // handle any deleted ModelScenarios		
        
        for (int i=0; i<newData.deletedScenarios.size(); i++) {
            
            ModelScenario scenario = (ModelScenario)newData.deletedScenarios.get(i);
            InputEntryInfo in = new InputEntryInfo();
            
            in.src = scenario.source;
            in.scenario = scenario.name;
            in.hde = new Float(scenario.HDE);
            in.bme = new Float(scenario.BME);
            in.vol = new Float(scenario.VOL);
            in.sa = new Float(scenario.SA);
            in.qo = new Float(scenario.QO);
            in.bw = new Float(scenario.BW);
            in.tfm = new Float(scenario.TFM);
            in.idam = new Integer(scenario.damType);
            
            if (!deleteSelectedInputs(strNIDID, scenario.source, scenario.name))
            {	
                System.out.println(header + "deleteSdbIn failed");
                throw new Exception("Failed to delete SdbIn table.");
            }		
        }
        
        // update Downstream table
        
        int downstreamCount = newData.downstream.size();
        
        boolean [] changedDownstreamPoints = new boolean [downstreamCount];		
        
        for (int i=0; i<newData.downstream.size(); i++) {
            
            changedDownstreamPoints[i] = false;
            
            DownstreamPoint dp = (DownstreamPoint)newData.downstream.get(i);
            DownstreamEntryInfo down = new DownstreamEntryInfo();
            
            if (dp.changeFlag > 0) changedDownstreamPoints[i] = true;		
            
            down.down_name = dp.name;
            down.comments = dp.comments;
            down.xsec_best_type = dp.xsecBestType;
            down.distance_from_dam = new Float(dp.distanceToSection);
            
            // *** note that "flood depth" is changed to "flood elevation" before storing in database !   		
            down.flood_depth = new Float(dp.floodDepth + dp.elevation);
            
            down.flood_width = new Float(dp.floodWidth);		
            down.flood_flow = new Float(dp.floodFlow);
            down.latitude = new Float(dp.latitude);
            down.longitude = new Float(dp.longitude);
            down.elevation = new Float(dp.elevation);
            down.mann_oc = new Float(dp.mann_oc);		
            
            updatedDam._downstreamEntries.add(down);	
            
        }
        
        if (!updateDownstream(updatedDam, changedDownstreamPoints))
        {	System.out.println(header + "updateDownstream failed");
        throw new Exception("Failed to update Downstream table.");
        }
        
        // handle any deleted DownstreamPoints	
        
        for (int i=0; i<newData.deletedDownstream.size(); i++) {
            
            DownstreamPoint dp = (DownstreamPoint)newData.deletedDownstream.get(i);
            DownstreamEntryInfo down = new DownstreamEntryInfo();		
            
            down.down_name = dp.name;
            down.comments = dp.comments;
            down.xsec_best_type = dp.xsecBestType;
            down.distance_from_dam = new Float(dp.distanceToSection);
            down.flood_depth = new Float(dp.floodDepth);
            down.flood_width = new Float(dp.floodWidth);		
            down.flood_flow = new Float(dp.floodFlow);
            down.latitude = new Float(dp.latitude);
            down.longitude = new Float(dp.longitude);
            down.elevation = new Float(dp.elevation);
            down.mann_oc = new Float(dp.mann_oc);
            
            if (!deleteSelectedPoints(strNIDID, dp.name))
            {	
                System.out.println(header + "deleteDamcatDown failed");
                throw new Exception("Failed to delete damcat_down.");
            }	
            
            
        }
        
        
        // update SectionPair table
        
        // *** store changeFlags for all DownstreamPoints in same ArrayList
        
        ArrayList changedPairsArrayList = new ArrayList();
        
        for (int i=0; i < newData.downstream.size(); i++) 
        {			
            
            DownstreamPoint dpx = (DownstreamPoint) newData.downstream.get(i);	// first get DownstreamPoint
            
            for (int j = 0; j < dpx.xsections.size(); j++)		// each DownstreamPoint has its own SectionGeometry(s)		
            {
                
                SectionGeometry sg = (SectionGeometry) dpx.xsections.get(j);
                
                Integer changedPair = new Integer(sg.getChangeFlag());
                
                for (int k = 0; k < (sg.getLastRowUsed()+1); k++)	// each SectionGeometry has its own set of Pairs
                {
                    CrossSectionEntryInfo xse = new CrossSectionEntryInfo();
                    
                    changedPairsArrayList.add(changedPair);		// each Pair needs its own flag, 
                    // although they are always changed in sets
                    xse.down_name = dpx.name;
                    xse.xsec_type = sg.getXSType();
                    xse.elev = new Float(sg.getElevationData(k, 0));
                    xse.tw = new Float(sg.getElevationData(k, 1));
                    xse.inactive_width = new Float(sg.getElevationData(k, 2));
                    xse.mann_n = new Float(sg.getElevationData(k, 3));
                    
                    xse.pair_num = new Integer(k);
                    
                    updatedDam._crossSectionEntries.add(xse);
                }
                
            }	
            
        }
        
        // *** now convert ArrayList to array of booleans and try to update
        
        int pairCount = changedPairsArrayList.size();
        
        boolean [] changedPairs = new boolean [pairCount];	
        
        for (int p = 0; p < changedPairsArrayList.size(); p++) 
        {
            Integer thisInteger = (Integer) changedPairsArrayList.get(p);
            int thisValue = thisInteger.intValue();
            if (thisValue > 0)
            {
                changedPairs[p] = true;
                //System.out.println(p + " is changed");
            } else
            {
                changedPairs[p] = false;
                //System.out.println(p + " is not");
            }
        }
        
        if (!updateSectionPair(updatedDam, changedPairs))
        {	System.out.println(header + "updateSectionPair failed");
        throw new Exception("Failed to update SectionPair table.");
        }
        
        // *** handle any deleted SectionGeometries
        
        for (int i=0; i < newData.downstream.size(); i++) 
        {			
            
            DownstreamPoint dpx = (DownstreamPoint) newData.downstream.get(i);	// first get DownstreamPoint
            
            for (int j = 0; j < dpx.deletedXsections.size(); j++)			
            {
                
                SectionGeometry sg = (SectionGeometry) dpx.xsections.get(j);
                
                if (!deleteSelectedPairs(strNIDID,  dpx.name, sg.getXSType(), -1))
                {	
                    System.out.println(header + "deleteSectionPair failed");
                    throw new Exception("Failed to delete SectionPair table.");
                }
                
            }	
            
        }
        
        // *** Finally update SdbOut table
        
        ArrayList changedOutputArrayList = new ArrayList();
        
        for (int s=0; s < newData.scenarios.size(); s++)		// for each model scenario 
        {
            
            ModelScenario scenario = (ModelScenario)newData.scenarios.get(s);
            ModelOutput out = scenario.output;
            
            
            int nSections = out.inputDownstream.size();			// for each downstream point
            
            for (int i=0; i< nSections; i++)
            {
                
                DownstreamPoint down = (DownstreamPoint) out.inputDownstream.get(i);
                OutputEntryInfo outEntry = new OutputEntryInfo();
                
                Integer changedOutput = new Integer(out.changeFlag);
                changedOutputArrayList.add(changedOutput);		// each Output needs its own flag, 
                // although they should always be changed in sets
                
                outEntry.src = scenario.source;
                outEntry.scenario = scenario.name;
                outEntry.down_name = down.name;
                outEntry.comments = "";
                
                outEntry.slope = new Float(calculateSlope(updatedDam));
                outEntry.max_flow = new Float(out.maxFlow[i]);
                //					= out.maxDepth[i];	// NOT USED 
                // **** NOTE that maxElevation is stored in database under maxDepth column name !!! ****  					
                outEntry.max_depth = new Float(out.maxElevation[i]);
                outEntry.time_max_depth = new Float(out.timeMaxDepth[i]);
                outEntry.time_flood = new Float(out.timeFlood[i]);
                outEntry.time_deflood = new Float(out.timeDeflood[i]);	
                
                updatedDam._outputEntries.add(outEntry);
            }
            
            
        }
        
        // *** convert ArrayList of Integers to array of booleans
        
        int outputCount = changedOutputArrayList.size();
        boolean [] changedOutputs = new boolean [outputCount];
        
        for (int a = 0; a < changedOutputArrayList.size(); a++) 
        {
            Integer thisInteger = (Integer) changedOutputArrayList.get(a);
            int thisValue = thisInteger.intValue();
            if (thisValue > 0)
            {
                changedOutputs[a] = true;
                // System.out.println(a + " is changed");
            } else
            {
                changedOutputs[a] = false;
                // System.out.println(a + " is not");
            }
        }
        
        if (!updateSdbOut(updatedDam, changedOutputs))
        {	System.out.println(header + "updateSdbOut failed");
        throw new Exception("Failed to update SdbOut table.");
        }
    }
    /**
     * This method writes the relevant data contained in the
     * given damInfo strucutre to the damcat_in database.
     *
     * NOTE: Currently, it will only update existing scenarios
     * in the table. If the given DamInfo structure has a scenario
     * that is not currently in the database, the method will return
     * false.  The method should be changed to add any new scenario
     * that is not currently represented in the database.  This is
     * the same for all the DBAccess::updateDamcat*(DamInfo) methods.
     *
     */
    public boolean updateSdbIn(DamInfo damInfo, boolean[] changedArray) {
        
        String header = "DBAccess.updateSdbIn():";
        //System.out.println(header);
        
        boolean bError = false;
        
        // JDBGen Code Here
        
        SdbInRecord inRow = new SdbInRecord();
        SdbInTable inTable = new SdbInTable(_db);
        
        try {
            for (int j=0; j<damInfo._inputEntries.size(); j++) {
                // System.out.println("Changed Array: " + changedArray[j]);
                if (! changedArray[j])
                {
                    continue;
                }
                
                InputEntryInfo in = (InputEntryInfo)damInfo._inputEntries.get(j);
                
                inRow.setNidid(damInfo.getNidid());
                inRow.setSrc(in.getSrc());
                inRow.setScenario(in.getScenario());
                inRow.setComments(in.getComments());
                
                String updatedString = getCurrentTime();
                long updated = DbTimeHelper.getLongTimeFromDateTimeString(updatedString);
                inRow.setUpdated(updated);
                
                inRow.setHde(in.getHde().doubleValue());
                inRow.setBme(in.getBme().doubleValue());
                inRow.setVol(in.getVol().doubleValue());
                inRow.setSa(in.getSa().doubleValue());
                inRow.setTfm(in.getTfm().doubleValue());
                inRow.setQo(in.getQo().doubleValue());
                inRow.setBw(in.getBw().doubleValue());		
                
                inRow.setIdam(in.getIdam().intValue());
                
                inTable.insertOrUpdate(inRow);
            }
            
        } catch (Exception e) {
            JOptionPane.showMessageDialog(null,"Error: Could not update SdbIn database table.");
            e.printStackTrace();
            bError = true;
        }
        
        return !bError;
    }
    /**
     * This method writes the relevant data contained in the
     * given damInfo structure to the damcat_out database table.
     * Creation date: (7/22/2003 8:44:02 AM)
     * @return boolean
     * @param damInfo damcat.DamInfo
     */
    public boolean updateSdbOut(DamInfo damInfo, boolean [] changedArray) {
        
        String header = "DBAccess.updateSdbOut():";
        //System.out.println(header);
        boolean bError = false;
        
        // *** JDBGen Code Here
        
        SdbOutRecord outRow = new SdbOutRecord();
        SdbOutTable outTable = new SdbOutTable(_db);
        
        try {
            for (int j=0; j<damInfo._outputEntries.size(); j++) {
                
                if (! changedArray[j])
                {
                    continue;
                }
                
                OutputEntryInfo out = (OutputEntryInfo)damInfo._outputEntries.get(j);
                
                outRow.setNidid(damInfo.getNidid());
                
                outRow.setSrc(out.getSrc());
                outRow.setScenario(out.getScenario());
                outRow.setDown_name(out.getDown_name());
                // System.out.println(out.getDown_name());
                
                outRow.setComments(out.getComments());
                
                String updatedString = getCurrentTime();
                long updated = DbTimeHelper.getLongTimeFromDateTimeString(updatedString);
                outRow.setUpdated(updated);
                
                outRow.setSlope(out.getSlope().doubleValue());
                outRow.setMax_flow(out.getMax_flow().doubleValue());
                outRow.setMax_depth(out.getMax_depth().doubleValue());
                outRow.setTime_max_depth(out.getTime_max_depth().doubleValue());
                outRow.setTime_flood(out.getTime_flood().doubleValue());
                outRow.setTime_deflood(out.getTime_deflood().doubleValue());
                
                outTable.insertOrUpdate(outRow);		
            }
            
        } catch (Exception e) {
            JOptionPane.showMessageDialog(null,"Error: Could not update SdbOut database table.");
            e.printStackTrace();
            bError = true;
        }
        
        return !bError;
    }
    /**
     * This method writes the relevant data contained in the
     * given damInfo structure to the damcat_pair database.
     * Creation date: (7/22/2003 8:37:35 AM)
     * @return boolean
     * @param damInfo damcat.DamInfo
     */
    public boolean updateSectionPair(DamInfo damInfo, boolean [] changedArray) {
        
        // *** JDBGen Code Here
        
        String header = "DBAccess.updateSectionPair()";
        //System.out.println(header);
        
        boolean bError = false;
        
        SectionPairRecord pairRow = new SectionPairRecord();
        SectionPairTable pairTable = new SectionPairTable(_db);
        
        try {
            for (int j=0; j<damInfo._crossSectionEntries.size(); j++) {
                
                if (! changedArray[j])
                {
                    // System.out.println("No changes for " + j);
                    continue;
                }
                CrossSectionEntryInfo xs = (CrossSectionEntryInfo)damInfo._crossSectionEntries.get(j);
                
                pairRow.setNidid(damInfo.getNidid());
                
                pairRow.setDown_name(xs.getDown_name());
                pairRow.setXsec_type(xs.getXsec_type());
                
                String updatedString = getCurrentTime();
                long updated = DbTimeHelper.getLongTimeFromDateTimeString(updatedString);
                pairRow.setUpdated(updated);
                
                pairRow.setElev(xs.getElev().doubleValue());
                pairRow.setTw(xs.getTw().doubleValue());
                pairRow.setMann_n(xs.getMann_n().doubleValue());
                pairRow.setInactive_width(xs.getInactive_width().doubleValue());
                
                pairRow.setPair_num(xs.getPair_num().intValue());
                
                pairTable.insertOrUpdate(pairRow);
            }
            
        } catch (Exception e) {
            JOptionPane.showMessageDialog(null,"Error: Could not update SectionPair database table.");
            e.printStackTrace();
            bError = true;
        }
        
    return !bError;
}
}
