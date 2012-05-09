package gov.noaa.nws.ncep.edex.plugin.ncpirep.decoder;

//import gov.noaa.nws.ncep.edex.plugin.ncpirep.common.AircraftFlightCondition;

import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.edex.decodertools.aircraft.AircraftFlightCondition;
import com.raytheon.uf.edex.decodertools.aircraft.Entry;
import com.raytheon.uf.edex.decodertools.aircraft.WordTranslator;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 10, 2011            jkorman     Initial creation
 * 2012/02/15   #680       Q.Zhou      Changed CAT and CHOP typo
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class PirepTools  {
    
    private static final char SPACE = ' ';
    
    private static final char DASH = '-';
    

    private static final char SOLIDUS = '/';

    private static final int INT_ID = 1;

    private static final int FRQ_ID = 2;

    private static final int TYP_ID = 3;

    private static final int MOD_ID = 4;

    private static final int SYN_ID = 5;
    
    public static final int TURBC_SYN_NONE = 0;
    
    public static final int TURBC_SYN_DASH = 1;
    
    public static final int TURBC_SYN_SPC = 2;

    public static final int MOD_ID_ABV = 0;

    public static final int MOD_ID_BLO = 1;

    public static final int MOD_ID_UNK = 2;
    
    public static final int TURBC_FRQ_NONE = 0;

    public static final int TURBC_FRQ_OCN = 1;
    
    public static final int TURBC_FRQ_ISO = 2;

    public static final int TURBC_FRQ_INT = 3;
    
    public static final int TURBC_FRQ_CON = 4;
    
    public static final int TURBC_INT_NONE = 0;

    public static final int TURBC_INT_SMOOTHLGT = 1;

    public static final int TURBC_INT_LGT = 2;

    public static final int TURBC_INT_LGTMOD = 3;

    public static final int TURBC_INT_MOD = 4;

    public static final int TURBC_INT_MODSEV = 5;

    public static final int TURBC_INT_SEV = 6;

    public static final int TURBC_INT_EXTRM = 8;

    public static final int TURBC_TYP_NONE = 0;

    public static final int TURBC_TYP_CAT = 1;

    public static final int TURBC_TYP_CHOP = 2;

    public static final int TURBC_TYP_LLWS = 3;

    
    private static final WordTranslator TURBC_WORDS = new WordTranslator();
    static {
        TURBC_WORDS.enter("OCN", "OCN", false, 1, FRQ_ID);
        TURBC_WORDS.enter("OCA", "OCN", false, 1, FRQ_ID);
        TURBC_WORDS.enter("OCNL", "OCN", false, 1, FRQ_ID);
        TURBC_WORDS.enter("ISO", "ISO", false, 1, FRQ_ID);
        TURBC_WORDS.enter("INT", "INT", false, 2, FRQ_ID);
        TURBC_WORDS.enter("STE", "STE", false, 3, FRQ_ID);
        TURBC_WORDS.enter("CON", "CON", false, 3, FRQ_ID);

        TURBC_WORDS.enter("SMTH", "NEG", false, TURBC_INT_NONE, INT_ID);
        TURBC_WORDS.enter("SMT[A-Z]", "NEG", true, TURBC_INT_NONE, INT_ID);
        TURBC_WORDS.enter("SM[A-Z]H", "NEG", true, TURBC_INT_NONE, INT_ID);
        TURBC_WORDS.enter("NONE", "NEG", false, TURBC_INT_NONE, INT_ID);
        TURBC_WORDS.enter("NIL", "NEG", false, TURBC_INT_NONE, INT_ID);
        TURBC_WORDS.enter("NEG", "NEG", false, TURBC_INT_NONE, INT_ID);
        TURBC_WORDS.enter("SMOOTH", "NEG", false, TURBC_INT_NONE, INT_ID);

        TURBC_WORDS.enter("LIG", "LGT", false, TURBC_INT_LGT, INT_ID);
        TURBC_WORDS.enter("LIT", "LGT", false, TURBC_INT_LGT, INT_ID);
        TURBC_WORDS.enter("LGT", "LGT", false, TURBC_INT_LGT, INT_ID);
        TURBC_WORDS.enter("LIGHT", "LGT", false, TURBC_INT_LGT, INT_ID);
        TURBC_WORDS.enter("SLIGHT", "LGT", false, TURBC_INT_LGT, INT_ID);

        TURBC_WORDS.enter("MOD", "MOD", false, TURBC_INT_MOD, INT_ID);
        TURBC_WORDS.enter("MDT", "MOD", false, TURBC_INT_MOD, INT_ID);

        TURBC_WORDS.enter("SEV", "SEV", false, TURBC_INT_SEV, INT_ID);
        TURBC_WORDS.enter("SVR", "SEV", false, TURBC_INT_SEV, INT_ID);
        TURBC_WORDS.enter("HVY", "SEV", false, TURBC_INT_SEV, INT_ID);

        TURBC_WORDS.enter("EXTRM", "EXTRM", false, TURBC_INT_EXTRM, INT_ID);
        TURBC_WORDS.enter("EXTRE", "EXTRM", false, TURBC_INT_EXTRM, INT_ID);
        TURBC_WORDS.enter("XTRM", "EXTRM", false, TURBC_INT_EXTRM, INT_ID);

        TURBC_WORDS.enter("CAT", "CAT", false, TURBC_TYP_CAT, TYP_ID);
        TURBC_WORDS.enter("CHOP", "CHOP", false, TURBC_TYP_CHOP, TYP_ID);
        TURBC_WORDS.enter("LLWS", "LLWS", false, TURBC_TYP_LLWS, TYP_ID);

        TURBC_WORDS.enter("ABV", "ABV", false, MOD_ID_ABV, MOD_ID);
        TURBC_WORDS.enter("ABOVE", "ABV", false, MOD_ID_ABV, MOD_ID);

        TURBC_WORDS.enter("BLO", "BLO", false, MOD_ID_BLO, MOD_ID);
        TURBC_WORDS.enter("BELOW", "BLO", false, MOD_ID_BLO, MOD_ID);

        TURBC_WORDS.enter("UNK", "UNK", false, MOD_ID_UNK, MOD_ID);
        TURBC_WORDS.enter("UNKN", "UNK", false, MOD_ID_UNK, MOD_ID);
        TURBC_WORDS.enter("UNKNOWN", "UNK", false, MOD_ID_UNK, MOD_ID);
        
        TURBC_WORDS.enter("TO", "-", false, TURBC_SYN_DASH, SYN_ID);
        TURBC_WORDS.enter("-", "-", false, TURBC_SYN_DASH, SYN_ID);
    }

    private String layerData;

    private List<String> layers;

    /**
     * 
     * @param data
     */
    public PirepTools(String data) {
        layerData = data;
        separateLayers();
    }

    /**
     * Separate the layer data into individual layers delimited by
     * a solidus "/" character.
     */
    private void separateLayers() {
        layers = new ArrayList<String>();
        if (layerData != null) {
            int pos = layerData.indexOf(SOLIDUS);
            int lastPos = 0;
            while (pos > lastPos) {
                layers.add(layerData.substring(lastPos, pos));
                // move past the solidus
                lastPos = pos + 1;
                pos = layerData.indexOf(SOLIDUS, lastPos);
            }
            // Add the remainder of the data.
            layers.add(layerData.substring(lastPos));
        }
    }
    
    /**
     * Perform the decode function on all layers that were found in the
     * data.
     * @return A list of AircraftFlightConditions for each layer decoded. Return
     * an empty list if no data was found.
     */
    public List<AircraftFlightCondition> decodeTurbulenceData() {
    	
        List<AircraftFlightCondition> tbLayers = new ArrayList<AircraftFlightCondition>();
        if (layers != null) {
            for (String layer : layers) {    	
                AircraftFlightCondition tbLayer = decodeLayer(layer);
                if (tbLayer != null) {
                    tbLayers.add(tbLayer);
                }
            }
        }
        
        return tbLayers;
    }

    /**
     * Perform the actual decode function of a single layer.
     * @param layer One layer of turbulence data to be decoded.
     * @return The decoded turbulence conditions. Returns a null value
     * if proper data was not found or an error occured.
     */
    private AircraftFlightCondition decodeLayer(String layer) {
        AircraftFlightCondition tbLayer = null;
        List<String> words = separateLayer(layer);
        
        for(int i = 0;i < words.size();i++) {
        	String s = words.get(i);
            Entry e = TURBC_WORDS.translate(s);
            if(e != null) {
                words.set(i,e.getTranslatedWord());
            } else {
                if(s.length() > 3) {    	
                    e = TURBC_WORDS.translate(s.substring(0,3));
                    if((e != null)&&(e.getSecondId() == FRQ_ID)) {
                        words.set(i,e.getTranslatedWord());
                    }       
                }
            }
            //System.out.println("words "+words.get(i));
        }
        AircraftFlightCondition afc = new AircraftFlightCondition();
    	
        if(analysis(words, afc)) {
            tbLayer = afc;
        }
       	
        // Ensure the intensities are in the correct order.
        reOrderIntensity(tbLayer);
        // Ensure the heights are in ascending order.
        reOrderHeight(tbLayer);
        
        return tbLayer;
    }

    /**
     * 
     * @param words
     * @param afc Target flight condition object to receive the turbulence
     * information.
     * @return Was the operation successful.
     */
    private boolean analysis(List<String> words, AircraftFlightCondition afc) {
        boolean success = true;
        boolean seenFreq = false;
        boolean intA = false;
        boolean intB = false;
        boolean typ = false;
        boolean flA = false;
        boolean flB = false;
        
        
        boolean modSeen = false;
        
        for(String word : words) {
            Entry e = TURBC_WORDS.translate(word);
            if(e != null) {
                Integer id = e.getSecondId();
                switch(id) {
                case INT_ID: {
                    if (!intA) {
                        afc.setIntensity1(word);
                        intA = true;
                    } else {
                        if (!intB) {
                            afc.setIntensity2(word);
                            intB = true;
                        }
                    }
                    break;
                }
                case FRQ_ID: {
                    if(!seenFreq) {
                        afc.setFrequency(word);
                    }
                    break;
                }
                case TYP_ID: {
                    if (!typ) {
                        afc.setType(word);
                    }
                    break;
                }
                case MOD_ID: {
                    switch(e.getFirstId()) {
                    case MOD_ID_ABV : {
                        afc.setTopHeight(IDecoderConstantsN.UAIR_INTEGER_MISSING); //-9999
                        flB = true;
                        break;
                    }
                    case MOD_ID_BLO : {
                        afc.setBaseHeight(IDecoderConstantsN.UAIR_INTEGER_MISSING);
                        flA = true;
                        break;
                    }
                    }
                    break;
                }
                case SYN_ID: {
                    
                    break;
                }
                }
            } else {
                // check to see if this is a flight level
                try {
                    if (!flA) {
                        int n = Integer.parseInt(word);
                        afc.setBaseHeight(n * 100);
                        flA = true;
                    } else {
                        if (!flB) {
                            int n = Integer.parseInt(word);
                            afc.setTopHeight(n * 100);
                            flB = true;
                        }
                    }
                } catch(NumberFormatException nfe) {
                }
            }
        }
        return success;
    }
    
    /**
     * Separate the layer information into a list of words separated
     * by spaces or a dash "-".
     * @param layer
     * @return
     */
    private List<String> separateLayer(String layer) {
        List<String> words = new ArrayList<String>();
        if((layer != null)&&(layer.length() > 0)) {
            StringBuilder sb = new StringBuilder(layer);
            for(int i = 0;i < sb.length();i++) {
                switch(sb.charAt(i)) {
                case '\n' :
                case '\r' :
                case '\t' : {
                    sb.setCharAt(i, SPACE);
                }
                }
            }
            layer = sb.toString();
            String [] ss = layer.split(String.valueOf(SPACE));
            for(String s : ss) {
                int i = s.indexOf(DASH);
                if(i == 0) {
                    words.add(String.valueOf(DASH));
                    words.add(s.substring(1));
                } else if(i > 0) {
                    words.add(s.substring(0,i));
                    words.add(String.valueOf(DASH));
                    words.add(s.substring(i+1));
                } else {
                    if(!String.valueOf(SPACE).equals(s)) {
                        words.add(s);
                    }
                }
            }
        }
        return words;
    }

    /**
     * If there are two intensity modifiers, reorder them so the the
     * lower intensity occurs first.  
     * @param layer A layer that may contain intensity modifiers.
     */
    private void reOrderIntensity(AircraftFlightCondition layer) {
        if(layer != null) {
            Integer iA = TURBC_WORDS.getFirstId(layer.getIntensity1());
            Integer iB = TURBC_WORDS.getFirstId(layer.getIntensity2());
            if(iA != null) {
                if(iB != null) {
                    if(iA > iB) {
                        String t = layer.getIntensity1();
                        layer.setIntensity1(layer.getIntensity2());
                        layer.setIntensity2(t);
                    }
                }
            }
        }
    }
    
    /**
     * If there are two intensity modifiers, reorder them so the the
     * lower intensity occurs first.  
     * @param layer A layer that may contain intensity modifiers.
     */
    private void reOrderHeight(AircraftFlightCondition layer) {
        if(layer != null) {
            Integer iA = layer.getBaseHeight();
            Integer iB = layer.getTopHeight();
            if(iA != null) {
                if(iB != null) {
                    if((iA > 0) && (iA > iB)) {
                        Integer t = layer.getBaseHeight();
                        layer.setBaseHeight(layer.getTopHeight());
                        layer.setTopHeight(t);
                    }
                }
            }
        }
    }
    
    /**
     * Attempt to convert a string to an integer value.
     * 
     * @param aNumber
     *            A possible integer value.
     * @return The converted number or null if conversion failed.
     */
    public static Integer parseInteger(String aNumber) {
        Integer retValue = null;
        try {
            retValue = new Integer(aNumber.trim());
        } catch (NumberFormatException nfe) {
            retValue = null;
        }
        return retValue;
    } // parseInteger()

    /**
     * @param args
     */
    public static void main(String[] args) {

        // PirepTools tools = new
        // PirepTools("OCNL LGT CHOP/MDT 100-150/LGT 150-200");


//        List<AircraftFlightCondition> list = tools.decodeTurbulenceData();
//        for (AircraftFlightCondition c : list) {
//            System.out.println(c);
//        }

        String [] data = {
                "LGT 100-120",
                "MOD TO SVR 100",
                "SVR OCNL MOD 200-300",
                "LGT CHOP 150 TO 110",
                "LGT-MDT 120-150",
                "MDT BLO\n 100",
                "LIT-MOD CHOP ABOVE 120",
                "CONTINOUS MOD 120-080"};
        for(String s : data) {
            PirepTools tools = new PirepTools(s);

            List<AircraftFlightCondition> list = tools.decodeTurbulenceData();
            for(AircraftFlightCondition afc : list) {
                String ss = afc.getFrequency();
                System.out.print((ss != null) ? ss : "----");
                System.out.print(" ");
                ss = afc.getIntensity1();
                System.out.print((ss != null) ? ss : "----");
                System.out.print(" ");
                ss = afc.getIntensity2();
                System.out.print((ss != null) ? ss : "----");
                System.out.print(" ");
                ss = afc.getType();
                System.out.print((ss != null) ? ss : "----");
                System.out.print(" ");
                Integer n = afc.getBaseHeight();
                System.out.print((n != null) ? n : "----");
                System.out.print(" ");
                n = afc.getTopHeight();
                System.out.print((n != null) ? n : "----");
                System.out.println();
                
            }
        }
        
        
    }

}