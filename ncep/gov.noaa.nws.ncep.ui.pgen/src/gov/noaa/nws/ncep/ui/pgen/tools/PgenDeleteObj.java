/*
 * gov.noaa.nws.ncep.ui.pgen.rsc.PgenDeleteObj
 * 
 * 20 May 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;

import com.raytheon.uf.viz.core.rsc.IInputHandler;

/**
 * Implements PGEN "Delete Obj" function.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 05/09			79		B. Yin   	Initial Creation.
 * 04/2014      TTR900      pswamy      R-click cannot return to SELECT from Rotate and DEL_OBJ
 * 
 * </pre>
 * 
 * @author B. Yin
 */

public class PgenDeleteObj extends AbstractPgenTool {

    public PgenDeleteObj() {

        super();

    }

    @Override
    public IInputHandler getMouseHandler() {

        if (inputHandler == null) {
            inputHandler = new PgenDeleteObjHandler();
        }
        return inputHandler;
    }

    /**
     * Implements input handler for mouse events.
     */
    public class PgenDeleteObjHandler extends InputHandlerDefaultImpl {

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int,
         * int, int)
         */
        @Override
        public boolean handleMouseDown(int x, int y, int button) {
            if (!isResourceEditable())
                return false;

            if (button == 3) {
                // set selecting mode
                PgenUtil.setSelectingMode();
                return true;
            }

            return false;
        }

    }
}