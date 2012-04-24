/*
 * gov.noaa.nws.ncep.ui.pgen.sigmet
 * 
 * 15 June 2011
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.sigmet;

import gov.noaa.nws.ncep.ui.pgen.annotation.ElementOperations;
import gov.noaa.nws.ncep.ui.pgen.annotation.Operation;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;
import gov.noaa.nws.ncep.ui.pgen.elements.labeledLines.LabeledLine;

import java.util.Iterator;

/**
 * Element class for CCFP
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 
 * 06/15/11     #252       G. Zhang    Initial creation
 * 
 * 
 * @author gzhang
 * @version 1
 */

@ElementOperations ( {Operation.COPY_MOVE, Operation.EXTRAPOLATE,  Operation.INTERPOLATE} )
public class Ccfp  extends LabeledLine {
	
	//elements: sigmet, area-line, arrowed-line/text-label, speed-arrow, speed-text
	
	private Sigmet sigmet = new Sigmet();
	
	private String tool = "";
	
	private Line areaLine;// area-line, line, and line(Med)
	
	public Ccfp(String name) {
		super(name);		
	}
	
	@Override
	public LabeledLine copy(){
 		
		Ccfp ll = new Ccfp( this.getName());
		ll.setParent( this.parent);
		ll.setPgenCategory(pgenCategory);
		ll.setPgenType(pgenType);

		ll.setSigmet((Sigmet)getSigmet().copy());//if(areaLine!=null){ Line line = (Line)areaLine.copy();ll.setAreaLine(line);ll.addLine(line);}		
		
		Iterator<AbstractDrawableComponent> it = this.getComponentIterator();
		while( it.hasNext() ){
			ll.add(it.next().copy());
		}
		
		return ll;
	}
	
	public void setSigmet(Sigmet s){
		sigmet = s;
	}
	
	public Sigmet getSigmet(){

		sigmet.setPoints(getAreaLine().getPoints());	
		sigmet.setPgenCategory(this.getPgenCategory());
		sigmet.setPgenType(this.getPgenType());
		
		return sigmet;
	}
	
	public void setAreaLine(Line l){
		areaLine = l;
	}
	
	public Line getAreaLine(){
//		for(Line ln : getLines()) if(ln.isClosedLine()) return ln;
		return getLines().get(0);
//		return areaLine;
	}
	
	public void setAttributes(IAttribute attrDlg){
		if ( attrDlg instanceof ICcfp ){
			((ICcfp)attrDlg).copyEditableAttrToAbstractSigmet2(sigmet, this);
		}
		
//		StringBuilder sb = new StringBuilder("CCFP_SIGMET");	
//		
//		sb.append(CcfpInfo.TEXT_SEPERATOR);
//		sb.append(sigmet.getEditableAttrPhenomSpeed()).append(CcfpInfo.TEXT_SEPERATOR);
//		sb.append(sigmet.getEditableAttrPhenomDirection()).append(CcfpInfo.TEXT_SEPERATOR);
//		sb.append(sigmet.getStartTime()).append(CcfpInfo.TEXT_SEPERATOR);
//		sb.append(sigmet.getEndtime());
//
//		this.collectionName = sb.toString();		
	}
	
	public void setCollectionName(String name){
		this.collectionName = name;
	}
	
	public void setCollectionNameWithSigmet(Sigmet sig){

		StringBuilder sb = new StringBuilder("CCFP_SIGMET");	
		
		sb.append(CcfpInfo.TEXT_SEPERATOR);
		sb.append(sig.getEditableAttrPhenomSpeed()).append(CcfpInfo.TEXT_SEPERATOR);
		sb.append(sig.getEditableAttrPhenomDirection()).append(CcfpInfo.TEXT_SEPERATOR);
		sb.append(sig.getEditableAttrStartTime()).append(CcfpInfo.TEXT_SEPERATOR);
		sb.append(sig.getEditableAttrEndTime()).append(CcfpInfo.TEXT_SEPERATOR);
		
		sb.append(sig.getEditableAttrPhenom()).append(CcfpInfo.TEXT_SEPERATOR);
		sb.append(sig.getEditableAttrPhenom2()).append(CcfpInfo.TEXT_SEPERATOR);
		sb.append(sig.getEditableAttrPhenomLat()).append(CcfpInfo.TEXT_SEPERATOR);
		sb.append(sig.getEditableAttrPhenomLon()).append(CcfpInfo.TEXT_SEPERATOR);
		sb.append(sig.getType());		

		setCollectionName( sb.toString() );
	}
	
	public boolean isPrimaryDEClosed(){
		DrawableElement de = this.getPrimaryDE();
		
		if( de != null && (de instanceof Line) )
			return ((Line)de).isClosedLine();		
		
		return false;
	}
	
	public void setToolName(String n){
		tool = n;
	}
	
	public void moveText2Last(){
		
/*		
		java.util.List<Label> ls = this.getLabels();
		if(ls != null && ls.size()>0){
			Label l = ls.get(0);
			DrawableElement de = l.getPrimaryDE();
			if(de instanceof Text)
				l.setSpe( l.getSpe());	
		}
*/		
	}
}
