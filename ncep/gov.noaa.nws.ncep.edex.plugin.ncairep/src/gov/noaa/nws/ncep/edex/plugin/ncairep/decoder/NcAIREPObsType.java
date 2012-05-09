/**
 * This software was modified from Raytheon's airep plugin by
 * NOAA/NWS/NCEP/NCO to order to output point data in HDF5.
 **/
package gov.noaa.nws.ncep.edex.plugin.ncairep.decoder;

import java.util.HashMap;

import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;

/**
 * TODO Change this to a Java 1.5 enum.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date           PTR#     Engineer      Description
 * ------------   -------- ------------- -------------------------------------
 * 04/27/2011              F. J. Yen     Initial creation from airep
 * </pre>
 */
public class NcAIREPObsType
{
    private static final HashMap<String,Integer> AIREP_TYPES =
        new HashMap<String,Integer>();
    static {
        AIREP_TYPES.put("ARP",IDecoderConstants.AIREP_NORMAL);
        AIREP_TYPES.put("AIREP",IDecoderConstants.AIREP_NORMAL);
        AIREP_TYPES.put("ARS",IDecoderConstants.AIREP_SPECIAL);
    }
    
    private final String obsType;
    
    /**
     * 
     * @param aType
     */
    private NcAIREPObsType(String aType) {
        obsType = aType;
    } // NcAIREPObsType()

    /**
     * 
     * @param anObsType
     * @return
     */
    public static NcAIREPObsType obsTypeFactory(String anObsType) {
        NcAIREPObsType obsTypeInstance = null;

        if(AIREP_TYPES.containsKey(anObsType)) {
            obsTypeInstance = new NcAIREPObsType(anObsType);
        }
        return obsTypeInstance;
    } // obsTypeFactory()
    
    /**
     * 
     * @return
     */
    public Integer getValue() {
        return AIREP_TYPES.get(obsType);
    } // getValue()

    public String getType() {
        return obsType;
    }
}
