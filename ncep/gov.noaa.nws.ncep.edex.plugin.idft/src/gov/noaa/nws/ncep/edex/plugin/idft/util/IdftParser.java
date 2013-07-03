/**
 * 
 * IdftParser
 * 
 * This class provides parser processing utilities for the IDFT Decoder Plug-In.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01Jun2009	   100		F. J. Yen	Initial creation
 * 01Oct2009	   100		F. J. Yen	Allow for multiple reads of idftLoc table with synchronized;
 * 										Remove unneeded imports after using generic StationTable
 * 08Dec2009	   100		F. J. Yen	Modified to11d3 to to11d6
 * 27May2010       100		F. J. Yen	Migrated from to11dr3 to to11dr11
 * Mar152013       982      Archana     get idftLoc.xml from localization correctly and store
 *                                      stns in a map instead of a list
 *
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS system.
 */
package gov.noaa.nws.ncep.edex.plugin.idft.util;

import gov.noaa.nws.ncep.common.dataplugin.idft.IdftRecord;
import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;
import gov.noaa.nws.ncep.edex.common.stationTables.Station;
import gov.noaa.nws.ncep.edex.common.stationTables.StationTable;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;


public class IdftParser {
    private static Map<Integer,Station> idftStnsMap = null;

    public IdftParser() {
    }

    public static synchronized void readIdftLocs() throws Exception {

        final String idftLocTableName = "ncep/stns/idftLoc.xml";

        IPathManager manager = PathManagerFactory.getPathManager();

        LocalizationFile stnsFile = manager.getStaticLocalizationFile(idftLocTableName );
        
        StationTable myloc = new StationTable( stnsFile.getFile().getAbsolutePath() );

        /*
         * Read and put the IdftLocsTable in list.
         */
        List<Station> idftStnsList = myloc.getStationList();
    	idftStnsMap = new HashMap<Integer,Station>();
    	
        if( idftStnsList != null ) {
        	for( Station stn : idftStnsList ) {
        		
        		Integer stnNum = Integer.parseInt( stn.getStnnum() );
        		
        		if( idftStnsMap.containsKey( stnNum ) ) {
        			// LOG a warning msg
                    System.out.println("Duplicate stn num, "+
                    		stn.getStnnum()+ ", in file: "+ stnsFile.getName() );
        		}
        		else {
        			idftStnsMap.put( stnNum, stn );
        		}
        	}
        }
    }

    /**
     * Decode the IDFT report.
     * 
     * @param matchLn
     *            IDFT point record to decode.
     * @param itype
     *            is 0 if no latitude/longitude in record. is 6 if
     *            latitude/longitude in record.
     * @param record
     *            IDFT record.
     * 
     */
    public static void processIdft(Matcher matchLn, int itype, IdftRecord record) {
        /*
         * Regular expression for IDFT report
         */
        Float signLl;
        record.setPointNum(Integer.parseInt(matchLn.group(1)));
        if (itype == 0) {
            /*
             * If itype equals 0, then no lat/lon in raw data for point. So, get
             * lat/lon from unmarshalled idftLoc.xml and then setLat and setLon.
             */
            try {
            	Integer stnNum = Integer.parseInt(matchLn.group(1));
            	
            	if( !idftStnsMap.containsKey( stnNum ) ) {
                	// TODO : change this to discard the record instead??
                    record.setLat(IDecoderConstantsN.FLOAT_MISSING);
                    record.setLon(IDecoderConstantsN.FLOAT_MISSING);
                    System.out.println("Stn Num, "+stnNum +", not found in idftLoc.xml station table.");
            	}
            	else {
            		Station stn = idftStnsMap.get( stnNum );
                
            		record.setLat(stn.getLatitude());
            		record.setLon(stn.getLongitude());
                }
            } catch (IndexOutOfBoundsException e) {
                record.setLat(IDecoderConstantsN.FLOAT_MISSING);
                record.setLon(IDecoderConstantsN.FLOAT_MISSING);
                System.out.println("Bad index to idftPoint created from idftLocs.xml");
            }
        } else {
            /*
             * Else itype is (6) not 0, so lat/lon is given in raw data. Process
             * the lat/lon strings and setLat and setLong
             */
            if (matchLn.group(4).equals("S")) {
                signLl = -1f;
            } else {
                signLl = 1f;
            }
            record.setLat((Integer.parseInt(matchLn.group(2)) + (Integer
                    .parseInt(matchLn.group(3)) * .1f)) * signLl);
            if (matchLn.group(7).equals("W")) {
                signLl = -1f;
            } else {
                signLl = 1f;
            }
            record.setLon((Integer.parseInt(matchLn.group(5)) + (Integer
                    .parseInt(matchLn.group(6)) * .1f)) * signLl);
        }
        record.setDirection(Integer.parseInt(matchLn.group(2 + itype)));
        record.setDistanceNm(Integer.parseInt(matchLn.group(3 + itype))
                + (Integer.parseInt(matchLn.group(4 + itype)) * .1f));
    }
}
