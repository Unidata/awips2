/**
 * This Java class is the JUnit test for the intlsigmet parser.
 *
 * <pre>
 *
 * L. Lin       07/09   Creation
 * </pre>
 *
 */
package gov.noaa.nws.ncep.edex.plugin.intlsigmet.util;

import static org.junit.Assert.assertEquals;
import gov.noaa.nws.ncep.common.dataplugin.intlsigmet.IntlSigmetLocation;
import gov.noaa.nws.ncep.common.dataplugin.intlsigmet.IntlSigmetRecord;

import java.util.Calendar;
import java.util.zip.DataFormatException;

import org.junit.Ignore;
import org.junit.Test;

import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.edex.util.Util;

//TODO fix?
@Ignore
public class IntlSigmetParserTest extends AbstractDecoder {

    @Test
    public void testProcessWMO() {

        final String wmoHeader = "WSNT08";
        final String testBull = "WSNT08 KKCI 081620\n\n\r" + "SIGA0H\n\n\r";

        IntlSigmetRecord record = null;

        record = IntlSigmetParser.processWMO(testBull, null);
        String wmo = record.getWmoHeader();
        assertEquals(wmo, wmoHeader);

        final String issueString = "081620";
        Calendar timeGroup = null;
        try {
            timeGroup = Util.findCurrentTime(issueString);
        } catch (DataFormatException e) {
            System.out.println("Unable to get issue time");
        }
        Calendar issueTime = record.getIssueTime();
        System.out.println("======= This is the issue date:");
        System.out.println("issue date:year= " + timeGroup.get(Calendar.YEAR));
        System.out
                .println("issue date:month= " + timeGroup.get(Calendar.MONTH));
        System.out.println("issue date:day= "
                + timeGroup.get(Calendar.DAY_OF_MONTH));
        System.out.println("issue date:hour= " + timeGroup.get(Calendar.HOUR));
        System.out.println("issue date:minute= "
                + timeGroup.get(Calendar.MINUTE));

        assertEquals(timeGroup, issueTime);
    }

    @Test
    public void testGetHazardType() {

        final String hazardType = "FREQUENT THUNDERSTORMS";
        final String testBull = "WSNT08 KKCI 081620 CCA\n\n\r"
                + "SIGA0H\n\n\r"
                + "KZMA SIGMET HOTEL 1 VALID 081620/082020 KKCI-\n\n\r"
                + "MIAMI OCEANIC FIR FRQ TS OBS AT 1620Z WI N2500 W07400 - N2145\n\n\r"
                + "W07445 - N2315 W07715 - N2500 W07400. TOP FL460. STNR. INTSF.\n\n\r"
                + "\n\n\r";

        String typeRet = IntlSigmetParser.getHazardType(testBull);
        assertEquals(hazardType, typeRet);
    }

    @Test
    public void testGetMessageID() {

        final String messageID = "HOTEL";
        final String testBull = "WSNT08 KKCI 081620 CCA\n\n\r"
                + "SIGA0H\n\n\r"
                + "KZMA SIGMET HOTEL 1 VALID 081620/082020 KKCI-\n\n\r"
                + "MIAMI OCEANIC FIR FRQ TS OBS AT 1620Z WI N2500 W07400 - N2145\n\n\r"
                + "W07445 - N2315 W07715 - N2500 W07400. TOP FL460. STNR. INTSF.\n\n\r"
                + "\n\n\r";

        String idRet = IntlSigmetParser.getMessageID(testBull);
        assertEquals(messageID, idRet);
    }

    @Test
    public void testSequenceNumber() {

        final String sequenceNo = "1";
        final String testBull = "WSNT08 KKCI 081620 CCA\n\n\r"
                + "SIGA0H\n\n\r"
                + "KZMA SIGMET HOTEL 1 VALID 081620/082020 KKCI-\n\n\r"
                + "MIAMI OCEANIC FIR FRQ TS OBS AT 1620Z WI N2500 W07400 - N2145\n\n\r"
                + "W07445 - N2315 W07715 - N2500 W07400. TOP FL460. STNR. INTSF.\n\n\r"
                + "\n\n\r";

        String retSeq = IntlSigmetParser.getSequenceNumber(testBull);
        assertEquals(sequenceNo, retSeq);

    }

    @Test
    public void testGetAtsu() {

        final String atsu = "KZMA ";
        final String testBull = "WSNT08 KKCI 081620 CCA\n\n\r"
                + "SIGA0H\n\n\r"
                + "KZMA SIGMET HOTEL 1 VALID 081620/082020 KKCI-\n\n\r"
                + "MIAMI OCEANIC FIR FRQ TS OBS AT 1620Z WI N2500 W07400 - N2145\n\n\r"
                + "W07445 - N2315 W07715 - N2500 W07400. TOP FL460. STNR. INTSF.\n\n\r"
                + "\n\n\r";

        String retAtsu = IntlSigmetParser.getAtsu(testBull);
        assertEquals(atsu, retAtsu);
    }

    @Test
    public void testGetOmwo() {

        final String omwo = "KKCI";
        final String testBull = "WSNT08 KKCI 081620 CCA\n\n\r"
                + "SIGA0H\n\n\r"
                + "KZMA SIGMET HOTEL 1 VALID 081620/082020 KKCI-\n\n\r"
                + "MIAMI OCEANIC FIR FRQ TS OBS AT 1620Z WI N2500 W07400 - N2145\n\n\r"
                + "W07445 - N2315 W07715 - N2500 W07400. TOP FL460. STNR. INTSF.\n\n\r"
                + "\n\n\r";

        String retOmwo = IntlSigmetParser.getOmwo(testBull);
        assertEquals(omwo, retOmwo);
    }

    @Test
    public void testGetStartTime() {

        final String timeString = "081620";
        final String testBull = "WSNT08 KKCI 081620\n\n\r"
                + "SIGA0H\n\n\r"
                + "KZMA SIGMET HOTEL 1 VALID 081620/082020 KKCI-\n\n\r"
                + "MIAMI OCEANIC FIR FRQ TS OBS AT 1620Z WI N2500 W07400 - N2145\n\n\r"
                + "W07445 - N2315 W07715 - N2500 W07400. TOP FL460. STNR. INTSF.\n\n\r"
                + "\n\n\r";

        Calendar startTime = null;
        try {
            startTime = Util.findCurrentTime(timeString);
        } catch (DataFormatException e) {
            System.out.println("Unable to get start time");
        }
        Calendar timeRet = IntlSigmetParser.getStartTime(testBull, null);

        assertEquals(startTime, timeRet);
    }

    @Test
    public void testGetEndTime() {

        final String timeString = "082020";
        final String testBull = "WSNT08 KKCI 081620\n\n\r"
                + "SIGA0H\n\n\r"
                + "KZMA SIGMET HOTEL 1 VALID 081620/082020 KKCI-\n\n\r"
                + "MIAMI OCEANIC FIR FRQ TS OBS AT 1620Z WI N2500 W07400 - N2145\n\n\r"
                + "W07445 - N2315 W07715 - N2500 W07400. TOP FL460. STNR. INTSF.\n\n\r"
                + "\n\n\r";

        Calendar endTime = null;
        try {
            endTime = Util.findCurrentTime(timeString);
        } catch (DataFormatException e) {
            System.out.println("Unable to get end time");
        }
        Calendar timeRet = IntlSigmetParser.getEndTime(testBull, null);

        assertEquals(endTime, timeRet);

    }

    @Test
    public void testProcessFlightLevel() {

        final int flightLevel = 460;

        final String testBull = "WSNT08 KKCI 081620 CCA\n\n\r"
                + "SIGA0H\n\n\r"
                + "KZMA SIGMET HOTEL 1 VALID 081620/082020 KKCI-\n\n\r"
                + "MIAMI OCEANIC FIR FRQ TS OBS AT 1620Z WI N2500 W07400 - N2145\n\n\r"
                + "W07445 - N2315 W07715 - N2500 W07400. TOP FL460. STNR. INTSF.\n\n\r"
                + "\n\n\r";

        IntlSigmetRecord retRecord = IntlSigmetParser
                .processWMO(testBull, null);
        IntlSigmetParser.processFlightLevels(testBull, retRecord);
        int retLevel = retRecord.getFlightlevel1();

        assertEquals(flightLevel, retLevel);

    }

    @Test
    public void getRemarks() {

        final String remarks = " CCA";
        final String testBull = "WSNT08 KKCI 081620 CCA\n\n\r"
                + "SIGA0H\n\n\r"
                + "KZMA SIGMET HOTEL 1 VALID 081620/082020 KKCI-\n\n\r"
                + "MIAMI OCEANIC FIR FRQ TS OBS AT 1620Z WI N2500 W07400 - N2145\n\n\r"
                + "W07445 - N2315 W07715 - N2500 W07400. TOP FL460. STNR. INTSF.\n\n\r"
                + "\n\n\r";

        String retRemarks = IntlSigmetParser.getRemarks(testBull);
        assertEquals(remarks, retRemarks);

    }

    @Test
    public void testGetSpeed() {

        final int speed = 20;

        final String testBull = "WSNT08 KKCI 081620 CCA\n\n\r"
                + "SIGA0H\n\n\r"
                + "KZMA SIGMET HOTEL 1 VALID 081620/082020 KKCI-\n\n\r"
                + "MIAMI OCEANIC FIR FRQ TS OBS AT 1620Z WI N2500 W07400 - N2145\n\n\r"
                + "W07445 - N2315 W07715 - N2500 W07400. TOP FL460.  MOV NE 20KT NC=\n\n\r"
                + "\n\n\r";

        int retSpeed = IntlSigmetParser.getSpeed(testBull);

        assertEquals(speed, retSpeed);

    }

    @Test
    public void testGetIntensity() {

        final String intensity = "INTSF";
        final String testBull = "WSNT08 KKCI 081620 CCA\n\n\r"
                + "SIGA0H\n\n\r"
                + "KZMA SIGMET HOTEL 1 VALID 081620/082020 KKCI-\n\n\r"
                + "MIAMI OCEANIC FIR FRQ TS OBS AT 1620Z WI N2500 W07400 - N2145\n\n\r"
                + "W07445 - N2315 W07715 - N2500 W07400. TOP FL460. STNR. INTSF.\n\n\r"
                + "\n\n\r";

        String retIntensity = IntlSigmetParser.getIntensity(testBull);
        assertEquals(intensity, retIntensity);
    }

    @Test
    public void testGetDirection() {

        final String direction = "SE";
        final String testBull = "WSNT08 KKCI 081620 CCA\n\n\r"
                + "SIGA0H\n\n\r"
                + "KZMA SIGMET HOTEL 1 VALID 081620/082020 KKCI-\n\n\r"
                + "MIAMI OCEANIC FIR FRQ TS OBS AT 1620Z WI N2500 W07400 - N2145\n\n\r"
                + "W07445 - N2315 W07715 - N2500 W07400. TOP FL460. MOV SE 30KMH NC\n\n\r"
                + "\n\n\r";

        String retDirection = IntlSigmetParser.getDirection(testBull);
        assertEquals(direction, retDirection);
    }

    @Test
    public void testGetDistance() {

        final int distance = 180;

        final String testBull = "WSNT08 KKCI 081620 CCA\n\n\r"
                + "SIGA0H\n\n\r"
                + "KZMA SIGMET HOTEL 1 VALID 081620/082020 KKCI-\n\n\r"
                + "MIAMI OCEANIC FIR FRQ TS OBS AT 1620Z WI N2500 W07400 - N2145\n\n\r"
                + "W07445 - N2315 W07715 - N2500 W07400. TOP FL460. \n\n\r"
                + "WI 180NM OF CENTRE MOV NE 20KT NC=\n\n\r" + "\n\n\r";

        int retDistance = IntlSigmetParser.getDistance(testBull);

        assertEquals(distance, retDistance);

    }

    @Test
    public void testGetNameLocation() {

        final String location = "HURRICANE IRENE LOCATED AT 23.4N 82.6W";
        final String testBull = "WSNT08 KKCI 081620 CCA\n\n\r"
                + "SIGA0H\n\n\r"
                + "KZMA SIGMET HOTEL 1 VALID 081620/082020 KKCI-\n\n\r"
                + "HURRICANE IRENE LOCATED AT 23.4N 82.6W MOVG N AT 2 KT.\n\n\r"
                + "MIAMI OCEANIC FIR FRQ TS OBS AT 1620Z WI N2500 W07400 - N2145\n\n\r"
                + "W07445 - N2315 W07715 - N2500 W07400. TOP FL460. MOV SE 30KMH NC\n\n\r"
                + "\n\n\r";

        String retLocation = IntlSigmetParser.getNameLocation(testBull);
        assertEquals(location, retLocation);
    }

    @Test
    public void testProcessLatLon() {

        final String location = "N2500 W07400";
        final float locLat = (float) 25.0;
        final float locLon = (float) -74.0;
        final String testBull = "WSNT08 KKCI 081620 CCA\n\n\r"
                + "SIGA0H\n\n\r"
                + "KZMA SIGMET HOTEL 1 VALID 081620/082020 KKCI-\n\n\r"
                + "HURRICANE IRENE LOCATED AT 23.4N 82.6W MOVG N AT 2 KT.\n\n\r"
                + "MIAMI OCEANIC FIR FRQ TS OBS AT 1620Z WI N2500 W07400 - N2145\n\n\r"
                + "W07445 - N2315 W07715 - N2500 W07400. TOP FL460. MOV SE 30KMH NC\n\n\r"
                + "\n\n\r";

        IntlSigmetRecord record = IntlSigmetParser.processWMO(testBull, null);
        IntlSigmetLocation locTB = new IntlSigmetLocation();
        Integer index = 0;
        IntlSigmetParser.processLatLon(location, locTB, index, record);
        double lat = locTB.getLatitude();
        double lon = locTB.getLongitude();

        assertEquals(lat, locLat);
        assertEquals(lon, locLon);

    }

    @Test
    public void testGetThunderStorm() {

        final String type = "FREQUENT THUNDERSTORMS";

        final String testBull = "WSNT08 KKCI 081620 CCA\n\n\r"
                + "SIGA0H\n\n\r"
                + "KZMA SIGMET HOTEL 1 VALID 081620/082020 KKCI-\n\n\r"
                + "MIAMI OCEANIC FIR FRQ TS OBS AT 1620Z WI N2500 W07400 - N2145\n\n\r"
                + "W07445 - N2315 W07715 - N2500 W07400. TOP FL460. \n\n\r"
                + "WI 180NM OF CENTRE MOV NE 20KT NC=\n\n\r" + "\n\n\r";

        String retType = IntlSigmetParser.getThunderStorm(testBull);

        assertEquals(type, retType);

    }

    @Test
    public void testRemoveChar() {

        final String type = "FREQUENT THUNDERSTORMS";
        final String retType = "FREQUENTTHUNDERSTORMS";

        String retRemove = IntlSigmetParser.removeChar(type, ' ');

        assertEquals(retRemove, retType);

    }

}