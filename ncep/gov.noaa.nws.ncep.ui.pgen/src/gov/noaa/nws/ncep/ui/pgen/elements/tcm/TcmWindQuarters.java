/*
 * gov.noaa.nws.ncep.ui.pgen.elements.tcm.TcmWindQuarters
 * 
 * 6 September 2011
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.elements.tcm;

import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.SinglePointElement;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Implements a class for TCM wind/wave quarters
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 09/11		?			B. Yin   	Initial Creation for TCM
 *
 * </pre>
 * 
 * @author	B. Yin
 */

@XmlAccessorType(XmlAccessType.NONE)
public class TcmWindQuarters extends SinglePointElement implements ITcmWindQuarter, ISerializableObject{

		@XmlElements( { @XmlElement(name = "quatro", type = Double.class) })
        private double[] quatro;
        
		@XmlAttribute
		private int windSpeed; //32 50 64 knots. 0 = 12ft wave
		
		public TcmWindQuarters(){
			
		}
		
        public TcmWindQuarters( Coordinate loc, int spd, double q1, double q2, double q3, double q4 ){
                quatro = new double[4];
                quatro[0] = q1;
                quatro[1] = q2;
                quatro[2] = q3;
                quatro[3] = q4;
                this.setLocation(loc);
                this.windSpeed = spd;
                
        }

        @Override
        public AbstractDrawableComponent copy() {
        	TcmWindQuarters newQuatros = 
        		new TcmWindQuarters( new Coordinate( this.getLocation().x, this.getLocation().y),
        				this.getWindSpeed(),
        				quatro[0], quatro[1], quatro[2], quatro[3]);
        	
              return newQuatros;
        }
        
        public double[] getQuarters() {
                return quatro;
        }

		public void setWindSpeed(int windSpeed) {
			this.windSpeed = windSpeed;
		}

		public int getWindSpeed() {
			return windSpeed;
		}
        
}
