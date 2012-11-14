/**
 * 
 */
package gov.noaa.nws.ncep.viz.rsc.ncscat.rsc;

import java.nio.ByteOrder;
import java.util.EnumSet;


/**
 * NcscatMode - Enum class to centralize and encapsulate all the things
 *              that vary among different satellite and data feed types.
 *              
 *              //TODO:  Consider moving this information entirely to the
 *                       bundle and/or preferences (.xml/.prm) files, so
 *                       the Java code can be completely agnostic about
 *                       satellite data types; would allow extended
 *                       'configurability', at the expense of slightly
 *                       longer bundle/preference files...
 * 
 *  This code has been developed by the SIB for use in the AWIPS2 system.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02 Jun 2010  235B       B. Hebbard  Initial creation.
 * 03 Feb 2011  235E       B. Hebbard  Add support for ambiguity variants.
 * 16 Aug 2012             B. Hebbard  Add OSCAT / OSCAT_HI
 * 
 * </pre>
 * 
 * @author bhebbard 
 * @version 1.0
 */

public enum NcscatMode {
    
        QUIKSCAT        (  76, true,  ByteOrder.BIG_ENDIAN ),
        QUIKSCAT_HI     ( 152, true,  ByteOrder.BIG_ENDIAN ),
        ASCAT           (  42, false, ByteOrder.BIG_ENDIAN ),
        ASCAT_HI        (  82, false, ByteOrder.BIG_ENDIAN ),
        EXASCT          (  42, false, ByteOrder.LITTLE_ENDIAN ),
        EXASCT_HI       (  82, false, ByteOrder.LITTLE_ENDIAN ),
        OSCAT           (  36, true,  ByteOrder.BIG_ENDIAN ),
        OSCAT_HI        (  76, true,  ByteOrder.LITTLE_ENDIAN ),
        WSCAT           (  79, true,  ByteOrder.LITTLE_ENDIAN ),
        UNKNOWN         (  76, true,  ByteOrder.BIG_ENDIAN );
        
        private int pointsPerRow ;     //  number of Wind Vector Cell in each scan row across satellite track
        private boolean isWindFrom ;   //  is the numeric wind direction the "from" (meteorological) direction?
        private ByteOrder byteOrder ;  //  endianess of data in the byte stream
        //TODO:  could add more here, to simplify (switch) code with more table driven logic.  But see above note about .xml/.prm...
        
        
        //  Constructor
        NcscatMode (int pointsPerRow, boolean isWindFrom, ByteOrder byteOrder) {
            this.pointsPerRow = pointsPerRow;
            this.isWindFrom   = isWindFrom;
            this.byteOrder    = byteOrder ;
        }
        
        public int getPointsPerRow() {
            return pointsPerRow ;
        }
        
        public boolean isWindFrom() {
            return isWindFrom ;
        }
        
        public ByteOrder getByteOrder() {
            return byteOrder ;
        }
        
        public static NcscatMode stringToMode (String name) {
            //  Given a string, return the corresponding enum
            NcscatMode returnValue = null;
            name = name.toUpperCase();
            name = name.replaceAll("-", "_");
            //TODO:  Remove ambiguity number??
            try {
                returnValue = valueOf(name);
            }
            catch (IllegalArgumentException e) {
                //TODO:  Signal unrecognized Ncscat mode string
                returnValue = UNKNOWN;
            }
            return returnValue;
        }

}
