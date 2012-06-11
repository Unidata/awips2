/*
 * gov.noaa.nws.ncep.ui.pgen.tools.ILabeledLine
 * 
 * 08 September 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.elements.labeledlines.LabeledLine;


/**
 * Interface that handles LabeledLine.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 09/09		#305		B. Yin   	Initial Creation.
 * 12/11		?			B. Yin		Added a flag to open/close line 
 * 
 * </pre>
 * 
 * @author	B. Yin
 */

public interface ILabeledLine {
	  public void setAddingLabelHandler();
      public void resetMouseHandler();
      public LabeledLine getLabeledLine();
      public void setLabeledLine( LabeledLine ln );
      public void setDeleteHandler(boolean delLine, boolean flipFlag, boolean openClose);
     // public void setAddLineMode( boolean addLineMode );
}
