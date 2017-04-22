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
package com.raytheon.viz.hydrocommon.ratingcurve;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import com.raytheon.uf.common.time.SimulatedTime;

public class RatingCurveImport extends ArrayList<RatingCurveData> {
   
   private static final long serialVersionUID = 19086896L;
   public String fileName = null;
   public String lid = null;
   public Calendar cal = null;

   public RatingCurveImport(String fileName, String lid) {
      
      super();
      this.fileName = fileName;
      this.lid = lid;

      cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
      Date date = SimulatedTime.getSystemTime().getTime();
      cal.setTime(date);
   }
   
   /**
    * Override the add method
    * @param stage
    * @param flow
    */
   public void add(double stage, double flow) {
      RatingCurveData rcd = new RatingCurveData(stage, flow);
      this.add(rcd);
   }
}
