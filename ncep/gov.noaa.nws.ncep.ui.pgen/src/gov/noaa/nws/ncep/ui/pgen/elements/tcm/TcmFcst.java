/*
 * gov.noaa.nws.ncep.ui.pgen.elements.TcmFcst
 * 
 * 6 September 2011
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.elements.tcm;

import java.awt.Color;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

import com.vividsolutions.jts.geom.Coordinate;

import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.SinglePointElement;

/**
 * Implements a class for TCM forecast information
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
public class TcmFcst extends SinglePointElement implements ITcmFcst{

	@XmlAttribute
	private int fcstHr;
	@XmlAttribute
	private int gust;	
	@XmlAttribute
	private int windMax;
	@XmlAttribute
	private int direction;
	@XmlAttribute
	private int speed;
	
	@XmlElements( { @XmlElement(name = "winds", type = TcmWindQuarters.class) })
	private TcmWindQuarters[] winds;
	
	public TcmFcst(){
		
	}
	
	public TcmFcst( Coordinate loc, int fcstHr, double[][] quatros ){
		
		this.fcstHr = fcstHr;
		this.setLocation(loc);
		winds = new TcmWindQuarters[3];
		winds[0] = new TcmWindQuarters( loc, 34, quatros[0][0], quatros[1][0], quatros[2][0], quatros[3][0]);
		winds[1] = new TcmWindQuarters( loc, 50, quatros[0][1], quatros[1][1], quatros[2][1], quatros[3][1]);
		winds[2] = new TcmWindQuarters( loc, 64, quatros[0][2], quatros[1][2], quatros[2][2], quatros[3][2]);

	}
	
	private TcmFcst( TcmWindQuarters[] quatros ){
		winds = quatros;
	}

	@Override
	public AbstractDrawableComponent copy() {
		TcmWindQuarters[] newWinds = new TcmWindQuarters[3];
		newWinds[0] = (TcmWindQuarters) winds[0].copy();
		newWinds[1] = (TcmWindQuarters) winds[1].copy();
		newWinds[2] = (TcmWindQuarters) winds[2].copy();

		TcmFcst newFcst= new TcmFcst( newWinds );
		newFcst.setLocation(this.getLocation());
		newFcst.direction = this.direction;
		newFcst.gust = this.gust;
		newFcst.speed = this.speed;
		newFcst.windMax = this.windMax;
		newFcst.fcstHr = this.fcstHr;
		return newFcst;
	}

	@Override
	public ITcmWindQuarter[] getQuarters() {
		return winds;
	}

	public void setFcstHr(int fcstHr) {
		this.fcstHr = fcstHr;
	}

	public int getFcstHr() {
		return fcstHr;
	}

	public void setGust(int gust) {
		this.gust = gust;
	}

	public int getGust() {
		return gust;
	}

	public void setWindMax(int windMax) {
		this.windMax = windMax;
	}

	public int getWindMax() {
		return windMax;
	}

	public void setDirection(int direction) {
		this.direction = direction;
	}

	public int getDirection() {
		return direction;
	}

	public void setSpeed(int speed) {
		this.speed = speed;
	}

	public int getSpeed() {
		return speed;
	}
}
