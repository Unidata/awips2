 package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to the GEMPAK parameter P24C
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize



	public class PressChange24Hr extends AbstractMetParameter implements 
	javax.measure.quantity.Pressure, ISerializableObject {
		/**
		 * 
		 */
		private static final long serialVersionUID = 5580079908204082433L;

		public PressChange24Hr() {
			 super( UNIT );
		}

 	}

	


