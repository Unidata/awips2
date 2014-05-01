/**
 * 
 */
package gov.noaa.nws.ncep.viz.rsc.stormtrack.rsc;

import java.util.Date;
import java.util.TreeSet;

/**
 * @author sgilbert
 *
 */
public class StormIdentifier {

	private String model;
	
	private Date reftime;
	
	private String cycloneNum;
	
	/**
	 * @param model
	 * @param reftime
	 * @param cycloneNum
	 */
	public StormIdentifier(String model, Date reftime, String cycloneNum) {
		this.model = model;
		this.reftime = reftime;
		this.cycloneNum = cycloneNum;
	}

	/**
	 * @return the model
	 */
	public String getModel() {
		return model;
	}

	/**
	 * @return the reftime
	 */
	public Date getReftime() {
		return reftime;
	}

	/**
	 * @return the cycloneNum
	 */
	public String getCycloneNum() {
		return cycloneNum;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((cycloneNum == null) ? 0 : cycloneNum.hashCode());
		result = prime * result + ((model == null) ? 0 : model.hashCode());
		result = prime * result + ((reftime == null) ? 0 : reftime.hashCode());
		return result;
	}

	/* (non-Javadoc)
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
		StormIdentifier other = (StormIdentifier) obj;
		if (cycloneNum == null) {
			if (other.cycloneNum != null)
				return false;
		} else if (!cycloneNum.equals(other.cycloneNum))
			return false;
		if (model == null) {
			if (other.model != null)
				return false;
		} else if (!model.equals(other.model))
			return false;
		if (reftime == null) {
			if (other.reftime != null)
				return false;
		} else if (!reftime.equals(other.reftime))
			return false;
		return true;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return "["+model +", "+ reftime.toGMTString() +", " + cycloneNum+ "]";
	}
	
	
}
