package gov.noaa.nws.ncep.ui.pgen.sigmet;

import gov.noaa.nws.ncep.ui.pgen.display.ILine;
import gov.noaa.nws.ncep.ui.pgen.elements.labeledlines.LabeledLine;

public interface ICcfp extends ILine{
	public void copyEditableAttrToAbstractSigmet2(AbstractSigmet ba, LabeledLine ll);
	public void copyEditableAttrToAbstractSigmet(AbstractSigmet ba);
	public String getCcfpLineType();
	public boolean isAreaType();
}
