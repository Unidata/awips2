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
package com.raytheon.viz.hydrocommon.data;

import java.util.ArrayList;
import java.util.Calendar;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.colorscalemgr.ColorScaleData;
import com.raytheon.viz.hydrocommon.colorscalemgr.HydroColorManager;

/**
 * river data point for summary and profile.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 08 NOV 2008  1628       dhladky      Initial creation 
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 *
 */
public class RiverDataPoint {

   private String lid = null;
   private String locName = null;
   private String county = null;
   private String state = null;
   private double elevation = HydroConstants.MISSING_VALUE;
   private double lat = HydroConstants.MISSING_VALUE;
   private double lon = HydroConstants.MISSING_VALUE;
   private String hsa = null;
   private String riverID = null;
   private String riverName = null;
   private String streamName = null;
   private double mile = HydroConstants.MISSING_VALUE;
   private double zeroDatum = HydroConstants.MISSING_VALUE;
   private String tide = null;
   private double bankFull = HydroConstants.MISSING_VALUE;
   private double actionStage = HydroConstants.MISSING_VALUE;
   private double floodStage = HydroConstants.MISSING_VALUE;
   private double floodFlow = HydroConstants.MISSING_VALUE;
   private double actionFlow = HydroConstants.MISSING_VALUE;
   private String primaryPE = null;
   private String proximity = null;
   private String reach = null;
   private double minor_stage = HydroConstants.MISSING_VALUE;
   private double moderate_stage = HydroConstants.MISSING_VALUE;
   private double major_stage = HydroConstants.MISSING_VALUE;
   private double minor_flow = HydroConstants.MISSING_VALUE;
   private double moderate_flow = HydroConstants.MISSING_VALUE;
   private double major_flow = HydroConstants.MISSING_VALUE;
   private double crestValue = HydroConstants.MISSING_VALUE;
   private int crestFlow = HydroConstants.MISSING_VALUE;
   private Calendar cresttime = null;
   private double obsValue = HydroConstants.MISSING_VALUE;
   private Calendar obstime = null;
   private double fcstValue = HydroConstants.MISSING_VALUE;
   private Calendar fcsttime = null;
   private HydroColorManager colorManager = HydroColorManager.getInstance();
   
   /**
    * This is the riverstatus threat index. It may have the following values:
    * "R" - MOFO stage/flow is at or above flood stage/flow. Red. 
    * "Y" - MOFO stage/flow is at or above action stage/flow. Yellow. 
    * "G" - MOFO stage/flow is below action stage/flow. Green. 
    * "M" - MOFO stage/flow is available, but action or flood stage is missing.
    * "Z" - Threat is not available. Missing.
    */
   private ThreatIndex threatIndex = null;

   public static enum ThreatIndex {
       /*
        * "R" - MOFO stage/flow is at or above flood stage/flow. Red. 
        * "Y" - MOFO stage/flow is at or above action stage/flow. Yellow. 
        * "G" - MOFO stage/flow is below action stage/flow. Green. 
        * "M" - MOFO stage/flow is available, but action or flood stage is missing.
        * "Z" - Threat is not available. Missing.
        */
       THREAT_MISSING_DATA("Z"),
       THREAT_MISSING_STAGE("M"),
       THREAT_NONE("G"),
       THREAT_ACTION("Y"),
       THREAT_FLOOD("R");
       
       private String threatIndex;
       
       ThreatIndex(String value) {
           threatIndex = value;
       }
       
       public String getThreatIndex() {
           return threatIndex;
       }
   }

   public RiverDataPoint (String lid) {
      setLid(lid);
   }
   
   /**
    * public constructor for river data point
    * @param data
    */
   public RiverDataPoint (Object[] data) {
      // We only populate the first 24 fields at inception
      if (data[0] != null) {
         setLid((String)data[0]);
      }
      if (data[1] != null) {
         setLocName((String)data[1]);
      }
      if (data[2] != null) {
         setCounty((String)data[2]);
      }
      if (data[3] != null) {
         setState((String)data[3]);
      }
      if (data[4] != null) {
         setElevation((Double)data[4]);
      }
      if (data[5] != null) {
         setHsa((String)data[5]);
      }
      if (data[6] != null) {
         setLat((Double)data[6]);
      }
      if (data[7] != null) {
         setLon((Double)data[7]);
      }
      if (data[8] != null) {
         setRiverID((String)data[8]);
      }
      if (data[9] != null) {
         setRiverName((String)data[9]);
      }
      if (data[10] != null) {
         setStreamName((String)data[10]);
      }
      if (data[11] != null) {
         setMile((Double)data[11]);
      }
      if (data[12] != null) {
         setZeroDatum((Double)data[12]);
      }
      if (data[13] != null) {
         setTide((String)data[13]);
      }
      if (data[14] != null) {
         setBankFull((Double)data[14]);
      }
      if (data[15] != null) {
         setActionStage((Double)data[15]);
      }
      if (data[16] != null) {
         setFloodStage((Double)data[16]);
      }
      if (data[17] != null) {
         setFloodFlow((Double)data[17]);
      }
      if (data[18] != null) {
         setActionFlow((Double)data[18]);
      }
      if (data[19] != null) {
         setPrimaryPE((String)data[19]);
      }
      if (data[20] != null) {
         setProximity((String)data[20]);
      }
      if (data[21] != null) {
         setReach((String)data[21]);
      }
      if (data[22] != null) {
         setMinorStage((Double)data[22]);
      }
      if (data[23] != null) {
         setModerateStage((Double)data[23]);
      }
      if (data[24] != null) {
         setMajorStage((Double)data[24]);
      }
      if (data[25] != null) {
         setMinorFlow((Double)data[25]);
      }
      if (data[26] != null) {
         setModerateFlow((Double)data[26]);
      }
      if (data[27] != null) {
         setMajorFlow((Double)data[27]);
      }
   }
   /**
    * 
    * @param lid
    */
   public void setLid(String lid) {
      this.lid = lid;
   }
   /**
    * 
    * @return
    */
   public String getLid() {
      return lid;
   }
   /**
    * 
    * @param locName
    */
   public void setLocName(String locName) {
      this.locName = locName;
   }
   /**
    * 
    * @return
    */
   public String getLocName() {
      return locName;
   }
   /**
    * 
    * @return
    */
   public String getCounty() {
      return county;
   }
   /**
    * 
    * @param county
    */
   public void setCounty(String county) {
      this.county = county;
   }
   /**
    * 
    * @return
    */
   public String getState() {
      return state;
   }
   /**
    * 
    * @param state
    */
   public void setState(String state) {
      this.state = state;
   }
   /**
    * 
    * @return
    */
   public String getHsa() {
      return hsa;
   }
   /**
    * 
    * @param state
    */
   public void setHsa(String hsa) {
      this.hsa = hsa;
   }
   /**
    * 
    * @param riverID
    */
   public void setRiverID(String riverID) {
      this.riverID = riverID;
   }
   /**
    * 
    * @return
    */
   public String getRiverID() {
      return riverID;
   }
   /**
    * 
    * @param riverName
    */
   public void setRiverName(String riverName) {
      this.riverName = riverName;
   }
   /**
    * 
    * @return
    */
   public String getRiverName() {
      return riverName;
   }
   /**
    * 
    * @param riverName
    */
   public void setStreamName(String streamName) {
      this.streamName = streamName;
   }
   /**
    * 
    * @return
    */
   public String getStreamName() {
      return streamName;
   }
   /**
    * 
    * @param mile
    */
   public void setMile(double mile) {
      this.mile = mile;
   }
   /**
    * 
    * @return
    */
   public double getMile() {
      return mile;
   }
   /**
    * 
    * @param elevation
    */
   public void setElevation(double elevation) {
      this.elevation = elevation;
   }
   /**
    * 
    * @return
    */
   public double getElevation() {
      return elevation;
   }
   /**
    * 
    * @param latitude
    */
   public void setLat(double lat) {
      this.lat = lat;
   }
   /**
    * 
    * @return
    */
   public double getlat() {
      return lat;
   }
   /**
    * 
    * @param longitude
    */
   public void setLon(double lon) {
      this.lon = lon;
   }
   /**
    * 
    * @return
    */
   public double getlon() {
      return lon;
   }
   /**
    * 
    * @param actionFlow
    */
   public void setActionFlow(double actionFlow) {
      this.actionFlow = actionFlow;
   }
   /**
    * 
    * @return
    */
   public double getActionFlow() {
      return actionFlow;
   }
   /**
    * 
    * @param zeroDatum
    */
   public void setZeroDatum(Double zeroDatum) {
      this.zeroDatum = zeroDatum;
   }
   /**
    * 
    * @return
    */
   public double getZeroDatum() {
      return zeroDatum;
   }
   /**
    * 
    * @param bankFull
    */
   public void setBankFull(Double bankFull) {
      this.bankFull = bankFull;
   }
   /**
    * 
    * @return
    */
   public double getBankFull() {
      return bankFull;
   }
   /**
    * 
    * @param tide
    */
   public void setTide(String tide) {
      this.tide = tide;
   }
   /**
    * 
    * @return
    */
   public String getTide() {
      return tide;
   }
   /**
    * 
    * @param minor_stage
    */
   public void setMinorStage(double minor_stage) {
      this.minor_stage = minor_stage;
   }
   /**
    * 
    * @return
    */
   public double getMinorStage() {
      return minor_stage;
   }
   /**
    * 
    * @param moderate_stage
    */
   public void setModerateStage(double moderate_stage) {
      this.moderate_stage = moderate_stage;
   }
   /**
    * 
    * @return
    */
   public double getModerateStage() {
      return moderate_stage;
   }
   /**
    * 
    * @param major_stage
    */
   public void setMajorStage(double major_stage) {
      this.major_stage = major_stage;
   }
   /**
    * 
    * @return
    */
   public double getMajorStage() {
      return major_stage;
   }
   /**
    * 
    * @param minor_flow
    */
   public void setMinorFlow(double minor_flow) {
      this.minor_flow = minor_flow;
   }
   /**
    * 
    * @return
    */
   public double getMinorFlow() {
      return minor_flow;
   }
   /**
    * 
    * @param moderate_flow
    */
   public void setModerateFlow(double moderate_flow) {
      this.moderate_flow = moderate_flow;
   }
   /**
    * 
    * @return
    */
   public double getModerateFlow() {
      return moderate_flow;
   }
   /**
    * 
    * @param major_flow
    */
   public void setMajorFlow(double major_flow) {
      this.major_flow = major_flow;
   }
   /**
    * 
    * @return
    */
   public double getMajorFlow() {
      return major_flow;
   }
   /**
    * 
    * @param actionStage
    */
   public void setActionStage(double actionStage) {
      this.actionStage = actionStage;
   }
   /**
    * 
    * @return
    */
   public double getActionStage() {
      return actionStage;
   }
   /**
    * 
    * @param flood_stage
    */
   public void setFloodStage(double floodStage) {
      this.floodStage = floodStage;
   }
   /**
    * 
    * @return
    */
   public double getFloodStage() {
      return floodStage;
   }
   /**
    * 
    * @param floodFlow
    */
   public void setFloodFlow(double floodFlow) {
      this.floodFlow = floodFlow;
   }
   /**
    * 
    * @return
    */
   public double getFloodFlow() {
      return floodFlow;
   }

   /**
    * 
    * @param reach
    */
   public void setReach(String reach) {
      this.reach = reach;
   }
   /**
    * 
    * @return
    */
   public String getReach() {
      return reach;
   }
   /**
    * 
    * @param reach
    */
   public void setPrimaryPE(String primaryPE) {
      this.primaryPE = primaryPE;
   }
   /**
    * 
    * @return
    */
   public String getPrimaryPE() {
      return primaryPE;
   }
   /**
    * 
    * @param reach
    */
   public void setProximity(String proximity) {
      this.proximity = proximity;
   }
   /**
    * 
    * @return
    */
   public String getProximity() {
      return proximity;
   }
   /**
    * 
    * @param obsValue
    */
   public void setObsValue(double obsValue) {
      this.obsValue = obsValue;
   }
   /**
    * 
    * @return
    */
   public double getObsValue() {
      return obsValue;
   }
   /**
    * 
    * @param major_stage
    */
   public void setFcstValue(double fcstValue) {
      this.fcstValue = fcstValue;
   }
   /**
    * 
    * @return
    */
   public double getFcstValue() {
      return fcstValue;
   }
   
   /**
    * 
    * @param time
    */
   public void setFcstTime(Calendar fcsttime) {
      this.fcsttime = fcsttime;
   }
   /**
    * 
    * @return
    */
   public Calendar getFcstTime() {
      return fcsttime;
   }
   /**
    * 
    * @param time
    */
   public void setObsTime(Calendar obstime) {
      this.obstime = obstime;
   }
   /**
    * 
    * @return
    */
   public Calendar getObsTime() {
      return obstime;
   }
   
   /**
    * 
    * @param time
    */
   public void setCrestTime(Calendar cresttime) {
      this.cresttime = cresttime;
   }
   /**
    * 
    * @return
    */
   public Calendar getCrestTime() {
      return cresttime;
   }
   
   /**
    * 
    * @param crestValue
    */
   public void setCrestValue(double crestValue) {
      this.crestValue = crestValue;
   }
   /**
    * 
    * @return
    */
   public double getCrestValue() {
      return crestValue;
   }
   
   /**
    * 
    * @param crestFlow
    */
   public void setCrestFlow(int crestFlow) {
      this.crestFlow = crestFlow;
   }
   /**
    * 
    * @return
    */
   public int getCrestFlow() {
      return crestFlow;
   }
  
   /**
    * @return the threatIndex
    */
   public ThreatIndex getThreatIndex() {
       return threatIndex;
   }

   /**
    * @param threatIndex the threatIndex to set
    */
   public void setThreatIndex(ThreatIndex threatIndex) {
       this.threatIndex = threatIndex;
   }

   /**
    * Get the color
    * 
    * @return The RGB color
    */
   public RGB getColor() {
       ArrayList<ColorScaleData> colorData = colorManager.getDefaultColorScaleData("Height");
       
       /* Default to No Data */
       RGB returnColor = colorData.get(HydroColorManager.MISSING_INDEX).color;
       
       if (threatIndex == null) {
           return returnColor; 
       }
       
       switch (threatIndex) {
       case THREAT_MISSING_DATA:
           returnColor = colorData.get(HydroColorManager.MISSING_INDEX).color;
           break;
           
       case THREAT_MISSING_STAGE:
           returnColor = colorData.get(HydroColorManager.MISSING_STAGE_INDEX).color;
           break;
           
       case THREAT_NONE:
           returnColor = colorData.get(HydroColorManager.NO_FLOOD_INDEX).color;
           break;
           
       case THREAT_ACTION:
           returnColor = colorData.get(HydroColorManager.NEAR_FLOOD_INDEX).color;
           break;

       case THREAT_FLOOD:
           returnColor = colorData.get(HydroColorManager.FLOOD_INDEX).color;
           break;
           
       default:
           returnColor = colorData.get(HydroColorManager.MISSING_INDEX).color;
           break;
       }
       
       return returnColor;
   }
}
