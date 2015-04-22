/*
 * gov.noaa.nws.ncep.ui.pgen.tools.IJetBarb
 * 
 * 22 July 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.elements.Jet;

/**
 * Interface that handles adding/deleting barbs for jet.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 07/09		#135		B. Yin   	Initial Creation.
 * 12/10		#366   		B. Yin		Added hash handlers 
 *
 * </pre>
 * 
 * @author	B. Yin
 */

public interface IJetBarb {
	  public void setAddingBarbHandler();
      public void setDeletingBarbHandler();
	  public void setAddingHashHandler();
      public void setDeletingHashHandler();     
      public void resetMouseHandler();
      public Jet getJet();
      public void setJet( Jet aJet);
}
