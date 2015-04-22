/*
 * gov.noaa.nws.ncep.ui.pgen.elements.tcm.Tcm
 * 
 * 6 September 2011
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.elements.tcm;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.MultiPointElement;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Implements a class for PGEN TCM
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

public class Tcm extends MultiPointElement implements ITcm{

	private String stormName;
	private String stormType;
	private int stormNumber;
	private int advisoryNumber;
	private String basin;
	
	private int eyeSize;
	private int positionAccuracy;
	private boolean correction;
	
	private Calendar time;
	private int centralPressure;
	
	private TcmWindQuarters waveQuatro;
	
	private ArrayList<TcmFcst> tcmFcst;
	
	public Tcm(){
		tcmFcst = new ArrayList<TcmFcst>();
	}
	
	public Tcm(String stormType, int stormNum, int advisoryNum, String name, String basin, 
			int eyeSize, int posAccuracy, boolean corr,
			Calendar time, int pressure){
		this.pgenCategory = "MET";
		this.pgenType = "TCM";
		
		this.basin = basin;
		this.stormType = stormType;
		this.stormNumber = stormNum;
		this.advisoryNumber = advisoryNum;
		this.stormName = name;
		this.centralPressure = pressure;
		
		this.eyeSize = eyeSize;
		this.positionAccuracy = posAccuracy;
		this.correction = corr;
		
		this.setTime(time);
		tcmFcst = new ArrayList<TcmFcst>();
	}
	
	public void addTcmFcst(TcmFcst fcst ){
//		if ( fcst.getFcstHr() == 0 ){
//			waveQuatro = new TcmWindQuatros( fcst.getLocation(), Color.GREEN, 500, 400, 300,250);
//		}
		tcmFcst.add(fcst);
//		if ( linePoints == null ){
//			linePoints = new ArrayList<Coordinate>();
//		}
//		this.getPoints().add( fcst.getLocation());
		
	}
	
	@Override
	public AbstractDrawableComponent copy() {
		Tcm newTcm = new Tcm( this.stormType, this.stormNumber, this.advisoryNumber, 
				this.stormName, this.basin, this.eyeSize, this.positionAccuracy,
				this.isCorrection(),
				this.getTime(), this.centralPressure);
		newTcm.setWaveQuatro((TcmWindQuarters) this.waveQuatro.copy());
		
		for ( TcmFcst fcst : tcmFcst ){
			newTcm.addTcmFcst( (TcmFcst) fcst.copy());
		}
		
		newTcm.setStormType(this.getStormType());
		newTcm.setStormNumber(this.getStormNumber());
		newTcm.setAdvisoryNumber(this.getAdvisoryNumber());
		
		return newTcm;
	}

	@Override
	public double[][] getWindRadius() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<TcmFcst> getTcmFcst() {
		return tcmFcst;
	}
	
	public void setTcmFcst(  List<TcmFcst> fcst ) {
		this.tcmFcst = (ArrayList<TcmFcst>) fcst;
		return;
	}
	
	@Override
	public String getStormName() {
		return stormName;
	}

	@Override
	public int getCentralPressure() {
		return centralPressure;
	}

	public void setTime(Calendar time) {
		this.time = time;
	}
	
	@Override
	public Calendar getAdvisoryTime() {
		return time;
	}
	
	public Calendar getTime() {
		return time;
	}

	@Override
	public int getFcstHr() {
		// TODO Auto-generated method stub
		return 0;
	}

	public void setWaveQuatro(TcmWindQuarters waveQuatro) {
		this.waveQuatro = waveQuatro;
	}

	public TcmWindQuarters getWaveQuarters() {
		return waveQuatro;
	}

	public void setStormType(String stormType) {
		this.stormType = stormType;
	}

	public String getStormType() {
		return stormType;
	}

	public void setStormNumber(int stormNumber) {
		this.stormNumber = stormNumber;
	}

	public int getStormNumber() {
		return stormNumber;
	}

	public void setAdvisoryNumber(int advisoryNumber) {
		this.advisoryNumber = advisoryNumber;
	}

	public int getAdvisoryNumber() {
		return advisoryNumber;
	}

	public void setBasin(String basin) {
		this.basin = basin;
	}

	public String getBasin() {
		return basin;
	}

	public void setEyeSize(int eyeSize) {
		this.eyeSize = eyeSize;
	}

	public int getEyeSize() {
		return eyeSize;
	}

	public int getPositionAccuracy() {
		return positionAccuracy;
	}

	public void setPositionAccuracy(int positionAccuracy) {
		this.positionAccuracy = positionAccuracy;
	}

	public void setCorrection(boolean correction) {
		this.correction = correction;
	}

	public boolean isCorrection() {
		return correction;
	}

	public void setStormName(String stormName) {
		this.stormName = stormName;
	}

	public void setCentralPressure(int centralPressure) {
		this.centralPressure = centralPressure;
	}
	
	@Override
	public ArrayList<Coordinate> getPoints(){
		ArrayList<Coordinate> ret = new ArrayList<Coordinate>();
		for ( TcmFcst fcst : tcmFcst ){
			ret.add(fcst.getLocation());
		}
		return ret;
	}
	
	@Override 
	public Coordinate[] getLinePoints(){
		return (Coordinate[]) this.getPoints().toArray( new Coordinate[ this.getPoints().size()]);
		
	}

}
