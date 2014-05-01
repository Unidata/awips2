package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.measure.quantity.Dimensionless;
import com.raytheon.uf.common.serialization.ISerializableObject;

  public class PercentageOfCloudCover extends AbstractMetParameter implements
  Dimensionless, ISerializableObject {


	/**
	 * 
	 */
	private static final long serialVersionUID = -3939330897502523677L;

	public PercentageOfCloudCover() {
		  super( UNIT );
	}

  }