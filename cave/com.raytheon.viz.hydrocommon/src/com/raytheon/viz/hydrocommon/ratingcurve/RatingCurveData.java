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

/**
 * This class containing the rating curve data.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 8, 2008				lvenable	Initial creation
 * Nov 24 2008    1628   dhladky   Spiced it up.
 * 
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */
public class RatingCurveData implements Comparable<RatingCurveData>
{
    /**
     * Stage value.
     */
    private double stage = 0.0;
    
    /**
     * Discharge value.
     */
    private double discharge = 0.0;
 
    /**
     * Sort by key.
     */
    private sortBy sortKey;
    
    /**
     * Constructor.
     */
    public RatingCurveData()
    {        
    }
    
    /**
     * Sort by enumeration.
     */
    public enum sortBy {Stage, Discharge};
    
    /**
     * Constructor.
     * @param stage Stage value.
     * @param discharge Discharge value.
     */
    public RatingCurveData(Object[] objects)
    {
       if (objects[0] != null) {
          setStage((Double)objects[0]);
       }
       if (objects[1] != null) {
          setDischarge((Double)objects[1]);
       }
       
       sortKey = sortBy.Stage;
    }
    
    /**
     * Constructor.
     * @param stage Stage value.
     * @param discharge Discharge value.
     */
    public RatingCurveData(double stage, double discharge)
    {
       this.stage = stage;
       this.discharge = discharge;
       
       sortKey = sortBy.Stage;
    }


    /**
     * Get the stage value.
     * @return The stage value.
     */
    public double getStage()
    {
        return stage;
    }

    /**
     * Set the stage value.
     * @param stage The stage value.
     */
    public void setStage(double stage)
    {
        this.stage = stage;
    }

    /**
     * Get the discharge value.
     * @return The discharge value.
     */
    public double getDischarge()
    {
        return discharge;
    }

    /**
     * Set the discharge value.
     * @param discharge The discharge value.
     */
    public void setDischarge(double discharge)
    {
        this.discharge = discharge;
    }
    
    /**
     * toString method thats formats the stage and discharge data into single string for
     * displaying.
     */
    public String toString()
    {
        String str = String.format("%8.2f %16.1f", stage, discharge);
        
        return str;
    }

   @Override
   public int compareTo(RatingCurveData o) {
      // TODO Auto-generated method stub
      int ret = 0;
      if (sortKey.equals(sortBy.Stage)) {
         if (getStage() > o.getStage()) {
            ret = 1;
         }
         else {
            ret = 0;
         }
      }
      
      if (sortKey.equals(sortBy.Discharge)) {
         if (getDischarge() > o.getDischarge()) {
            ret = 1;
         }
         else {
            ret = 0;
         }
      }
      
      return ret;
   }

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		long temp;
		temp = Double.doubleToLongBits(discharge);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		temp = Double.doubleToLongBits(stage);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		return result;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		RatingCurveData other = (RatingCurveData) obj;
		if (Double.doubleToLongBits(discharge) != Double
				.doubleToLongBits(other.discharge))
			return false;
		if (Double.doubleToLongBits(stage) != Double
				.doubleToLongBits(other.stage))
			return false;
		return true;
	}
}
