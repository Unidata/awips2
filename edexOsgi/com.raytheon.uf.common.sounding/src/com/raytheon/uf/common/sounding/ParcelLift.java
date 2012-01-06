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
package com.raytheon.uf.common.sounding;

/**
 * ParcelLift.java Sept 28, 2008
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 28 Sept 2008            dhladky     new class.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class ParcelLift {
   
   private Double startPressure = null;
   private boolean maxTemp = false;
   private String name = null;
   
   public enum PARCEL_TYPE {
      PMAX("PMAX"), SURFACE("Surface"), 
      MEANTEMP("Mean Temp"), USERSELECT("User Select");

      private final String dataName;

      private PARCEL_TYPE(String dataName) {
          this.dataName = dataName;
      }

      public String getDataName() {
          return dataName;
      }
  };
  
  /**
   * public constructor
   * @param name
   * @param startpressure
   */
  public ParcelLift(PARCEL_TYPE type, Double startPressure, boolean maxTemp) {
     
     if (startPressure != null) {
        setStartPressure(startPressure);
        setMaxTemp(maxTemp);
     }
     
     for (PARCEL_TYPE pt: PARCEL_TYPE.values()) {
        if (pt.getDataName().equals(type.getDataName())) {
           setName(type.getDataName());
        }
     }
      
  }
  
  /**
   * Set start presure when needed.
   * @param startPressure
   */
  private void setStartPressure(Double startPressure) {
     this.startPressure = startPressure;
  }
  
  /**
   * Set use fcst mac temp
   * @param startPressure
   */
  private void setMaxTemp(boolean maxTemp) {
     this.maxTemp = maxTemp;
  }
  
  /**
   * Gets the starting pressure when needed.
   * @return double
   */
  public Double getStartPressure() {
     return startPressure;
  }
  
  /**
   * Gets the starting pressure when needed.
   * @return double
   */
  public boolean getMaxTemp() {
     return maxTemp;
  }
  
  /**
   * Sets the name.
   * @param name
   */
  public void setName(String name) {
     this.name = name;
  }
  
  /**
   * Get the name
   * @return
   */
  public String getName() {
     return name;
  }
  
}
